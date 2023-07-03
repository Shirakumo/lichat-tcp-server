(in-package #:org.shirakumo.lichat.tcp-server)

(defvar *default-port* 1111)

(defun ensure-hostname (host-ish)
  (etypecase host-ish
    (string host-ish)
    (array (ensure-hostname (coerce host-ish 'list)))
    (list (format NIL "~{~a~^.~}" host-ish))))

(defclass server (lichat-serverlib:flood-protected-server)
  ((hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)
   (socket :initform NIL :accessor socket)
   (thread :initarg :thread :accessor thread)
   (ping-interval :initarg :ping-interval :accessor ping-interval)
   (lock :initform (bt:make-recursive-lock) :accessor lock)
   (connections :initform () :accessor connections)
   (connection-limit :initarg :connection-limit :accessor connection-limit))
  (:default-initargs
   :name (machine-instance)
   :hostname "localhost"
   :port *default-port*
   :thread NIL
   :ping-interval 10
   :connection-limit 100))

(defclass connection (lichat-serverlib:flood-protected-connection)
  ((hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)
   (socket :initarg :socket :accessor socket)
   (thread :initarg :thread :accessor thread)
   (queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor queue)
   (back-queue :initform (make-array 0 :adjustable T :fill-pointer T) :accessor back-queue)
   (lock :initform (bt:make-recursive-lock) :accessor lock))
  (:default-initargs
   :socket (error "SOCKET required.")))

(defclass channel (lichat-serverlib:backlogged-channel)
  ((lock :initform (bt:make-recursive-lock) :accessor lock)))

(defclass user (lichat-serverlib:user)
  ((lock :initform (bt:make-recursive-lock) :accessor lock)))

(defmethod lichat-serverlib:make-connection ((server server) &rest initargs)
  (apply #'make-instance 'connection initargs))

(defmethod lichat-serverlib:make-channel ((server server) &rest initargs)
  (apply #'make-instance 'channel initargs))

(defmethod lichat-serverlib:make-user ((server server) &rest initargs)
  (apply #'make-instance 'user initargs))

(defmethod open-connection ((server server))
  (when (thread server)
    (error "Connection thread still exists."))
  (let ((socket (usocket:socket-listen (hostname server) (port server)
                                       :reuse-address T)))
    (setf (socket server) socket)
    (setf (thread server)
          (bt:make-thread (lambda ()
                            (unwind-protect
                                 (handle-connection server)
                              (setf (thread server) NIL)))
                          :name (format NIL "Lichat TCP Server ~d:~d"
                                        (hostname server)
                                        (port server))))))

(defmethod close-connection ((server server))
  (unless (thread server)
    (error "No connection thread running."))
  (bt:interrupt-thread (thread server)
                       (lambda () (invoke-restart 'close-connection)))
  (dolist (connection (connections server))
    (close-connection connection)))

(defmethod handle-connection ((server server))
  (v:info :lichat.server.tcp "~a: Listening for incoming connections on ~a:~a"
          server (hostname server) (port server))
  (unwind-protect
       (with-simple-restart (close-connection "Close the connection.")
         (loop for con = (ignore-errors (usocket:socket-accept (socket server)))
               do (when con (handler-case (establish-connection con server)
                              (error (err)
                                (v:debug :lichat.server.tcp err)
                                (v:warn :lichat.server.tcp "~a: Error during connection establishment: ~a" err)
                                (ignore-errors (usocket:socket-close con)))))))
    (usocket:socket-close (socket server))))

(defmethod establish-connection (socket (server server))
  (v:info :lichat.server.tcp "~a: Establishing connection to ~a:~a"
          server (ensure-hostname (usocket:get-peer-address socket)) (usocket:get-peer-port socket))
  (let ((connection (lichat-serverlib:make-connection
                     server
                     :user NIL
                     :socket socket
                     :hostname (ensure-hostname (usocket:get-peer-address socket))
                     :port (usocket:get-peer-port socket)
                     :server server)))
    (cond ((<= (connection-limit server) (length (connections server)))
           (lichat-serverlib:send! connection 'too-many-connections)
           (usocket:socket-close socket))
          (T
           (bt:with-recursive-lock-held ((lock server))
             (push connection (connections server)))
           (setf (thread connection)
                 (bt:make-thread (lambda ()
                                   (unwind-protect
                                        (handle-connection connection)
                                     (setf (thread connection) NIL)
                                     (ignore-errors (usocket:socket-close socket))))
                                 :name (format NIL "Lichat TCP Client ~d:~d"
                                               (hostname connection)
                                               (port connection))))))))

(defmethod handle-connection :around ((connection connection))
  (unwind-protect
       (with-simple-restart (close-connection "Close the connection.")
         (handler-case
             (call-next-method)
           ((or usocket:socket-error cl:stream-error) (err)
             (v:error :lichat.server.tcp err)
             (usocket:socket-close (socket connection)))
           (error (err)
             (v:error :lichat.server.tcp err)
             (ignore-errors
              (lichat-serverlib:send! connection 'failure
                                      :text (princ-to-string err))))))
    (send-queued-messages connection)
    (v:info :lichat.server.tcp "~a: Exiting connection handling." connection)
    (ignore-errors (lichat-serverlib:teardown-connection connection))
    (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
      (setf (connections (lichat-serverlib:server connection))
            (remove connection (connections (lichat-serverlib:server connection)))))))

(defmethod handle-connection ((connection connection))
  (let ((stream (usocket:socket-stream (socket connection)))
        (time (get-universal-time)))
    (handler-case
        (progn (unless (nth-value 1 (usocket:wait-for-input (socket connection) :timeout 10))
                 (error "CONNECT update timeout."))
               (let ((message (lichat-protocol:from-wire stream)))
                 (unless (typep message 'lichat-protocol:connect)
                   (error "Expected CONNECT update."))
                 (lichat-serverlib:process connection message)))
      (lichat-protocol:wire-condition (err)
        (lichat-serverlib:send! connection 'malformed-update
                                :text (princ-to-string err))
        (invoke-restart 'close-connection)))
    (loop (v:trace :lichat.server.tcp "~a: Waiting for message..." connection)
          (cond ((<= (ping-interval (lichat-serverlib:server connection))
                     (- (get-universal-time) time))
                 (setf time (get-universal-time))
                 (lichat-serverlib:send! connection 'lichat-protocol:ping))
                ((< 0 (length (queue connection)))
                 (send-queued-messages connection))
                ((nth-value 1 (usocket:wait-for-input (socket connection) :timeout 1))
                 (v:trace :lichat.server.tcp "~a: Input ready." connection)
                 (lichat-serverlib:process connection stream))))))

(defmethod send-queued-messages ((connection connection))
  (bt:with-recursive-lock-held ((lock connection))
    (rotatef (back-queue connection) (queue connection)))
  (let ((queue (back-queue connection)))
    (loop for i from 0 below (length queue)
          for object = (aref queue i)
          do (v:trace :lichat.server.tcp "~a: Sending ~s to ~a" (lichat-serverlib:server connection) object connection)
             (handler-case (lichat-protocol:to-wire object (usocket:socket-stream (socket connection)))
               (error (err)
                 (v:severe :lichat.server.tcp "Error during sending to ~s: ~s" connection err)))
             (setf (aref queue i) NIL))
    (setf (fill-pointer queue) 0)))

(defmethod close-connection ((connection connection))
  (when (thread connection)
    (if (eql (bt:current-thread) (thread connection))
        (invoke-restart 'close-connection)
        (bt:interrupt-thread (thread connection)
                             (lambda () (invoke-restart 'close-connection)))))
  connection)

(defmethod lichat-serverlib:teardown-connection :after ((connection connection))
  (v:info :lichat.server.tcp "Closing ~a" connection)
  (close-connection connection))

(defmethod lichat-serverlib:send ((object lichat-protocol:object) (connection connection))
  (v:trace :lichat.server.tcp "~a: Queuing ~s for ~a" (lichat-serverlib:server connection) object connection)
  (bt:with-recursive-lock-held ((lock connection))
    (vector-push-extend object (queue connection))))

;;; Handle synchronising
;; FIXME: I'm not entirely convinced the mutual exclusion
;;        implemented in this model is entirely correct.

(defmethod lichat-serverlib:init-connection :around ((connection connection) update)
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (let ((user (lichat-serverlib:find-user (lichat-protocol:from update)
                                            (lichat-serverlib:server connection))))
      (if user
          (bt:with-recursive-lock-held ((lock user))
            (call-next-method))
          (call-next-method)))))

(defmethod lichat-serverlib:teardown-connection :around ((connection connection))
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (let ((user (lichat-protocol:user connection)))
      (if user
          (bt:with-recursive-lock-held ((lock user))
            (call-next-method))
          (call-next-method)))))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:register))
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:create))
  (bt:with-recursive-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:join :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &optional id)
  (declare (ignore id))
  (bt:with-recursive-lock-held ((lock user))
    (bt:with-recursive-lock-held ((lock channel))
      (call-next-method))))

(defmethod lichat-serverlib:leave :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &key id notify-self)
  (declare (ignore id notify-self))
  (bt:with-recursive-lock-held ((lock user))
    (bt:with-recursive-lock-held ((lock channel))
      (call-next-method))))

(defmethod lichat-serverlib:send :around ((object lichat-protocol:object) (channel channel))
  (bt:with-recursive-lock-held ((lock channel))
    (call-next-method)))

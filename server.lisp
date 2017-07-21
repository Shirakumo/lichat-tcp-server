#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
   :ping-interval 60
   :connection-limit 100))

(defclass connection (lichat-serverlib:flood-protected-connection)
  ((hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)
   (socket :initarg :socket :accessor socket)
   (thread :initarg :thread :accessor thread)
   (lock :initform (bt:make-recursive-lock) :accessor lock))
  (:default-initargs
   :socket (error "SOCKET required.")))

(defclass channel (lichat-serverlib:channel)
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
  (let ((socket (usocket:socket-listen (hostname server) (port server))))
    (setf (socket server) socket)
    (setf (thread server)
          (bt:make-thread (lambda ()
                            (unwind-protect
                                 (handle-connection server)
                              (setf (thread server) NIL)))))))

(defmethod close-connection ((server server))
  (unless (thread server)
    (error "No connection thread running."))
  (bt:interrupt-thread (thread server)
                       (lambda () (invoke-restart 'lichat-serverlib:close-connection)))
  (dolist (connection (connections server))
    (close-connection connection)))

(defmethod handle-connection ((server server))
  (v:info :lichat.server.tcp "~a: Listening for incoming connections on ~a:~a"
          server (hostname server) (port server))
  (unwind-protect
       (with-simple-restart (lichat-serverlib:close-connection "Close the connection.")
         (loop for con = (usocket:socket-accept (socket server))
               do (establish-connection con server)))
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
                                     (setf (thread connection) NIL)))))))))

(defmethod handle-connection :around ((connection connection))
  (unwind-protect
       (with-simple-restart (lichat-serverlib:close-connection "Close the connection.")
         (handler-case
             (call-next-method)
           ((or usocket:ns-try-again-condition 
             usocket:timeout-error 
             usocket:shutdown-error
             usocket:connection-reset-error
             usocket:connection-aborted-error
             cl:stream-error) (err)
             (v:error :lichat.server.tcp err))
           (error (err)
             (v:error :lichat.server.tcp err)
             (ignore-errors
              (lichat-serverlib:send! connection 'failure
                                      :text (princ-to-string err))))))
    (v:info :lichat.server.tcp "~a: Exiting connection handling." connection)
    (ignore-errors (usocket:socket-close (socket connection)))))

(defmethod handle-connection ((connection connection))
  (handler-case
      (let ((message (lichat-protocol:from-wire stream)))
        (unless (typep message 'lichat-protocol:connect)
          (error "Expected CONNECT update."))
        (lichat-serverlib:process connection message))
    (lichat-protocol:wire-condition (err)
      (lichat-serverlib:send! connection 'malformed-update
                              :text (princ-to-string err))
      (invoke-restart 'lichat-serverlib:close-connection)))
  (loop (v:trace :lichat.server.tcp "~a: Waiting for message..." connection)
        (cond ((nth-value 1 (usocket:wait-for-input
                             socket :timeout (ping-interval (lichat-serverlib:server connection))))
               (v:trace :lichat.server.tcp "~a: Input ready." connection)
               (lichat-serverlib:process connection stream))
              (T
               (lichat-serverlib:send! connection 'lichat-protocol:ping)))))

(defmethod close-connection ((connection connection))
  (unless (thread connection)
    (error "No connection thread running."))
  (bt:interrupt-thread (thread connection)
                       (lambda () (invoke-restart 'lichat-serverlib:close-connection))))

(defmethod lichat-serverlib:teardown-connection ((connection connection))
  (let ((server (lichat-serverlib:server connection)))
    (v:info :lichat.server.tcp "~a: Closing ~a" server connection)
    (unwind-protect (call-next-method)
      (setf (connections server) (remove connection (connections server)))
      (ignore-errors (close (usocket:socket-stream (socket connection))))
      (ignore-errors (usocket:socket-close (socket connection))))))

(defmethod lichat-serverlib:send ((object lichat-protocol:wire-object) (connection connection))
  (v:trace :lichat.server.tcp "~a: Sending ~s to ~a" (lichat-serverlib:server connection) object connection)
  (bt:with-recursive-lock-held ((lock connection))
    (handler-case
        (lichat-protocol:to-wire object (usocket:socket-stream (socket connection)))
      (error (err)
        (v:severe :lichat.server.tcp "Error during sending to ~s: ~s" connection err)))))

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

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
   (thread :initarg :thread :accessor thread)
   (ping-interval :initarg :ping-interval :accessor ping-interval)
   (lock :initform (bt:make-lock) :accessor lock)
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
   (lock :initform (bt:make-lock) :accessor lock))
  (:default-initargs
   :socket (error "SOCKET required.")))

(defclass channel (lichat-serverlib:channel)
  ((lock :initform (bt:make-lock) :accessor lock)))

(defclass user (lichat-serverlib:user)
  ((lock :initform (bt:make-lock) :accessor lock)))

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
    (setf (thread server)
          (bt:make-thread (lambda ()
                            (unwind-protect
                                 (handle-connection socket server)
                              (setf (thread server) NIL)))))))

(defmethod close-connection ((server server))
  (unless (thread server)
    (error "No connection thread running."))
  (bt:interrupt-thread (thread server)
                       (lambda () (invoke-restart 'lichat-serverlib:close-connection)))
  (dolist (connection (connections server))
    (close-connection connection)))

(defmethod handle-connection (socket (server server))
  (v:info :lichat.server "~a: Listening for incoming connections on ~a:~a"
          server (hostname server) (port server))
  (unwind-protect
       (with-simple-restart (lichat-serverlib:close-connection "Close the connection.")
         (loop for con = (usocket:socket-accept socket)
               do (establish-connection con server)))
    (usocket:socket-close socket)))

(defmethod establish-connection (socket (server server))
  (v:info :lichat.server "~a: Establishing connection to ~a:~a"
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
           (ignore-errors (usocket:socket-close socket)))
          (T
           (push connection (connections server))
           (setf (thread connection)
                 (bt:make-thread (lambda ()
                                   (unwind-protect
                                        (handle-connection socket connection)
                                     (setf (thread connection) NIL)))))))))

(defmethod handle-connection (socket (connection connection))
  (let* ((stream (usocket:socket-stream socket)))
    (unwind-protect
         (with-simple-restart (lichat-serverlib:close-connection "Close the connection.")
           (handler-case
               (let ((message (lichat-protocol:from-wire stream)))
                 (etypecase message
                   (lichat-protocol:connect
                    (lichat-serverlib:process connection message)))
                 (loop while (open-stream-p stream)
                       do (v:trace :lichat.server "~a: Waiting for message..." connection)
                          (cond ((nth-value 1 (usocket:wait-for-input
                                               socket :timeout (ping-interval (lichat-serverlib:server connection))))
                                 (v:trace :lichat.server "~a: Input ready." connection)
                                 (lichat-serverlib:process connection stream))
                                (T
                                 (lichat-serverlib:send! connection 'lichat-protocol:ping)))))
             ((or usocket:ns-try-again-condition 
               usocket:timeout-error 
               usocket:shutdown-error
               usocket:connection-reset-error
               usocket:connection-aborted-error
               cl:end-of-file) (err)
               (v:warn :lichat.server "~a: Encountered fatal error: ~a" connection err))))
      (when (open-stream-p stream)
        (lichat-serverlib:teardown-connection connection)
        (ignore-errors (usocket:socket-close socket))))))

(defmethod close-connection ((connection connection))
  (unless (thread connection)
    (error "No connection thread running."))
  (bt:interrupt-thread (thread connection)
                       (lambda () (invoke-restart 'lichat-serverlib:close-connection))))

(defmethod (setf lichat-serverlib:find-channel) :before (channel name (server server))
  (v:info :lichat.server "~a: Creating channel ~a" server channel))

(defmethod (setf lichat-serverlib:find-user) :before (user name (server server))
  (v:info :lichat.server "~a: Creating user ~a" server user))

(defmethod (setf lichat-serverlib:find-profile) :before (profile name (server server))
  (v:info :lichat.server "~a: Creating profile ~a" server profile))

(defmethod lichat-serverlib:teardown-connection :after ((connection connection))
  (v:info :lichat.server "~a: Closing ~a" (lichat-serverlib:server connection) connection)
  (ignore-errors (usocket:socket-close (socket connection))))

(defmethod lichat-serverlib:send ((object lichat-protocol:wire-object) (connection connection))
  (v:trace :lichat.server "~a: Sending ~s to ~a" (lichat-serverlib:server connection) object connection)
  (bt:with-lock-held ((lock connection))
    (lichat-protocol:to-wire object (usocket:socket-stream (socket connection)))))

;;; Handle synchronising
;; FIXME: I'm not entirely convinced the mutual exclusion
;;        implemented in this model is entirely correct.

;; OPs that need a global lock
(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:connect))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:teardown-connection :around ((connection connection))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:register))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

(defmethod lichat-serverlib:process :around ((connection connection) (update lichat-protocol:create))
  (bt:with-lock-held ((lock (lichat-serverlib:server connection)))
    (call-next-method)))

;; OPs that need a local lock
(defmethod lichat-serverlib:join :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &optional id)
  (declare (ignore id))
  (bt:with-lock-held ((lock channel))
    (bt:with-lock-held ((lock user))
      (call-next-method))))

(defmethod lichat-serverlib:leave :around ((channel lichat-serverlib:channel) (user lichat-serverlib:user) &optional id)
  (declare (ignore id))
  (bt:with-lock-held ((lock channel))
    (bt:with-lock-held ((lock user))
      (call-next-method))))

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

(defclass server (lichat-serverlib:server)
  ((hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)
   (thread :initarg :thread :accessor thread))
  (:default-initargs
   :name (machine-instance)
   :hostname "localhost"
   :port *default-port*
   :thread NIL))

(defclass connection (lichat-serverlib:connection)
  ((socket :initarg :socket :accessor socket)
   (thread :initarg :thread :accessor thread))
  (:default-initargs
   :socket (error "SOCKET required.")))

(defmethod open-connection ((server server))
  (when (thread server)
    (error "Connection thread still exists."))
  (let ((socket (usocket:socket-listen (hostname server) (port server))))
    (setf (thread server)
          (bt:make-thread (lambda ()
                            (unwind-protect
                                 (handle-connection socket server)
                              (setf (thread server) NIL)))))))

(defmethod handle-connection (socket (server server))
  (v:info :lichat.server "~a: Listening for incoming connections on ~a:~a"
          server (hostname server) (port server))
  (unwind-protect
       (loop for con = (usocket:socket-accept socket)
             do (establish-connection con server))
    (usocket:socket-close socket)))

(defmethod establish-connection (socket (server server))
  (v:info :lichat.server "~a: Establishing connection to ~a:~a"
          server (ensure-hostname (usocket:get-peer-address socket)) (usocket:get-peer-port socket))
  (let ((connection (make-instance 'connection
                                   :user NIL
                                   :socket socket
                                   :hostname (ensure-hostname (usocket:get-peer-address socket))
                                   :port (usocket:get-peer-port socket)
                                   :server server)))
    (setf (thread connection)
          (bt:make-thread (lambda ()
                            (unwind-protect
                                 (handle-connection socket connection)
                              (setf (thread connection) NIL)))))))

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
                       do (v:info :lichat.server "~a: Waiting for message..." connection)
                          (if (usocket:wait-for-input socket :timeout 10)
                              (lichat-serverlib:process connection stream)
                              (lichat-serverlib:send! connection 'lichat-protocol:ping))))
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
  (v:info :lichat.server "~a: Sending ~s to ~a" (lichat-serverlib:server connection) object connection)
  (lichat-protocol:to-wire object (usocket:socket-stream (socket connection))))

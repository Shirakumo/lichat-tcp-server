#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat-tcp-server
  (:nicknames #:org.shirakumo.lichat.tcp-server)
  (:use #:cl)
  (:export
   #:*default-port*
   #:ensure-hostname
   #:server
   #:hostname
   #:port
   #:thread
   #:ping-interval
   #:lock
   #:connections
   #:connection
   #:socket
   #:thread
   #:lock
   #:hostname
   #:port
   #:channel
   #:lock
   #:user
   #:lock
   #:open-connection
   #:close-connection
   #:handle-connection
   #:establish-connection))

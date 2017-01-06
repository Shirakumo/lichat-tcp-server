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
   #:server
   #:hostname
   #:port
   #:thread
   #:connection
   #:socket
   #:thread
   #:open-connection
   #:handle-connection
   #:establish-connection))

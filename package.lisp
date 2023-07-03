(in-package #:cl-user)
(defpackage #:lichat-tcp-server
  (:nicknames #:org.shirakumo.lichat.tcp-server)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
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
   #:connection-limit
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

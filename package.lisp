#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat
  (:nicknames #:org.shirakumo.lichat)
  (:use #:cl)
  (:export
   #:*default-port*
   #:server
   #:hostname
   #:port
   #:connection
   #:socket
   #:thread))

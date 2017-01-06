#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem lichat
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An implementation of a simple chat protocol."
  :homepage "https://github.com/Shinmera/lichat"
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "client"))
  :depends-on (:lichat-server
               :usocket
               :bordeaux-threads
               :documentation-utils
               :verbose))

#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem lichat-tcp-server
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple TCP server implementation for lichat."
  :homepage "https://Shirakumo.github.io/lichat-tcp-server/"
  :bug-tracker "https://github.com/Shirakumo/lichat-tcp-server/issues"
  :source-control (:git "https://github.com/Shirakumo/lichat-tcp-server.git")
  :serial T
  :components ((:file "package")
               (:file "server")
               (:file "documentation"))
  :depends-on (:lichat-protocol
               :lichat-serverlib
               :usocket
               :bordeaux-threads
               :documentation-utils
               :verbose))

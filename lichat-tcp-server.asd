(asdf:defsystem lichat-tcp-server
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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

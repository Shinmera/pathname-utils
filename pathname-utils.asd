(asdf:defsystem pathname-utils
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities for pathname manipulation."
  :homepage "https://shinmera.com/docs/pathname-utils/"
  :bug-tracker "https://shinmera.com/project/pathname-utils/issues"
  :source-control (:git "https://shinmera.com/project/pathname-utils.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "documentation"))
  :depends-on (:trivial-features)
  :in-order-to ((asdf:test-op (asdf:test-op :pathname-utils-test))))

(cl:in-package :cl-user)

(defvar *fork-future-library-path* *load-truename*)

(asdf:defsystem fork-future
  :description "Fork-future is a posix fork() based future parallel library"
  :author "Jianshi Huang @ Mathematical Systems Inc. (huang@msi.co.jp)"
  :version "0.2.20091225"
  :depends-on (cl-store cffi)
  :components 
  ((:module src
            :components
            ((:file "package")
             (:file "posix-wrapper" :depends-on ("package"))
             (:file "fork-future" :depends-on ("posix-wrapper")))
            :perform
            (asdf:load-op :after (op c)
                          (when (find-package 'swank)
                            (load (merge-pathnames "src/handle-swank.lisp" *fork-future-library-path*)))))))





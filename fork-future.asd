(cl:in-package :cl-user)

(defvar *fork-future-library-path* *load-truename*)

(asdf:defsystem fork-future
  :description "Fork-future is a posix fork() based future parallel library"
  :author "Jianshi Huang @ Mathematical Systems Inc. (huang@msi.co.jp)"
  :version "0.1.20091224"
  :depends-on (osicat cl-store)
  :components 
  ((:module src
            :components
            ((:file "package")
             (:file "fork-future" :depends-on ("package")))
            :perform
            (asdf:load-op :after (op c)
                          (when (find-package 'swank)
                            (load (merge-pathnames "src/handle-swank.lisp" *fork-future-library-path*)))))))





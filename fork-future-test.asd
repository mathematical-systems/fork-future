(asdf:defsystem fork-future-test
  :depends-on (fork-future stefil)
  :components 
  ((:module test
            :components
            ((:file "package")
             (:file "unit-test" :depends-on ("package"))
             (:file "stress-test" :depends-on ("package"))))))


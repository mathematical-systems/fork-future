(in-package :fork-future-test)

(defsuite stress-test)

(in-suite stress-test)

(deftest 100p-100times ()
  (loop repeat 100
        do
     (progn
       (is (= 0 (hash-table-count fork-future::*futures*)))
       (let ((futures (loop repeat 100
                            collect
                         (future (+ 1 1)))))
         (is (= 100 (hash-table-count fork-future::*futures*)))
         (is (= 200 (reduce '+ futures :key 'touch)))
         (is (= 0 (hash-table-count fork-future::*futures*)))
         (is (> 0 (fork-future::waitpid 0)))))))

(define atomicswap
  '(&& (< (txn fee) (/ (txn amount) 33))
       (|| (&& (= (txn closeremainderto) "Alice")
               (= (txn receiver) "Alice")
               (= (sha256 arg0) "X"))
           (&& (= (txn closeremainderto) "Bob")
               (= (txn receiver) "Bob")
               (> (round) 3000)))))

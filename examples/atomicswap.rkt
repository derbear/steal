(define atomicswap
  '(|| (&& (= (txn closeremainderto) "Alice")
           (= (txn receiver) "Alice")
           (< (txn fee) (/ (txn amount) 33))
           (= (sha256 arg0) "X"))
       (&& (= (txn closeremainderto) "Bob")
           (= (txn receiver) "Bob")
           (> (round) 3000))))

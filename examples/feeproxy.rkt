(define feeproxy
  '(&& (= (txn closeremainderto) "Alice")
       (= (txn receiver) "Alice")
       (ed25519verify arg0 "X" "Bob")))

(define feeproxykeyreg
  '(&& (= (txn closeremainderto) "Zero")
       (= (txn receiver) "Zero")
       (ed25519verify arg0 "X" "Bob")))

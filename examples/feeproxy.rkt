;; TODO will need to reason about the fee over time...
;; (define feeproxy
;;   '(&& (= (txn closeremainderto) "Alice")
;;        (= (txn receiver) "Alice")
;;        (= (len (txn note)) 0)
;;        (= 500 (- (txn firstvalid) (txn lastvalid)))
;;        (ed25519verify "X" (txn firstvalid) "Bob")))

(define feeproxykeyreg
  '(&& (= (txn closeremainderto) "Zero")
       (= (txn receiver) "Zero")
       (= (txn amount) 0)
       (= (txn fee) 1,000)
       (= (len (txn note)) 0)
       (= (txn lastvalid) (+ 500 (txn firstvalid)))
       (= (% (txn firstvalid) 10,000) 0)
       (ed25519verify (txn firstvalid) "Bob" "X")))

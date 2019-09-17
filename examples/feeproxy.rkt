#lang racket

(provide feeproxykeyreg)

;; TODO will need to reason about the fee over time...
;; (define feeproxy
;;   '(&& (= (txn CloseRemainderTo) "Alice")
;;        (= (txn Receiver) "Alice")
;;        (= (len (txn note)) 0)
;;        (= 500 (- (txn firstvalid) (txn lastvalid)))
;;        (ed25519verify "X" (txn firstvalid) "Bob")))

(define feeproxykeyreg
  '(&& (= (txn CloseRemainderTo) (addr "Zero"))
       (= (txn Receiver) (addr "Zero"))
       (= (txn Amount) 0)
       (= (txn Fee) 1000)
       (= (len (txn Note)) 0)
       (= (txn LastValid) (+ 500 (txn FirstValid)))
       (= (% (txn FirstValid) 10000) 0)
       (ed25519verify (txn FirstValid) (addr "Bob") arg0)))

#lang racket

(provide periodicpayment)

(define periodicpayment
  '(&& (= (txn CloseRemainderTo) (global ZeroAddress))
       (= (txn Receiver) (addr Alice))
       (= (txn Amount) 100000)
       (= (txn Fee) 10000)
       (= (% (txn FirstValid) 100000) 0)
       ;; (&& (> (block time) "Tuesday") (< (block time) "Wednesday"))
       (= (txn LastValid) (+ 500 (txn FirstValid)))
       ;; (&& (>= (protocol version) 3.0)
       ;;     (<= (protocol version) 3.4))
       (= (len (txn Note)) 0)))

#lang racket

(provide atomicswap)

;; TODO specify txtype

;; This is an escrow.
(define atomicswap
  '(and (< (txn Fee)
           (+ (global MinTxnFee)
              (/ (txn Amount) 100)))
        (= (txn TypeEnum) 0)
        (or (and (= (txn CloseRemainderTo) (addr Alice))
                 (= (txn Receiver) (addr Alice))
                 (= (hash arg_0) (byte base64 Image)))
            (and (= (txn CloseRemainderTo) (addr Bob))
                 (= (txn Receiver) (addr Bob))
                 (> (global Round) 3000)))))

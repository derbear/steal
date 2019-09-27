#lang racket

(provide feeproxykeyreg)

;; This is delegate logic.
;; arg_0 must be the signature
(define feeproxykeyreg
  '(and (= (txn TypeEnum) 1)
        (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Receiver) (global ZeroAddress))
        (= (txn Amount) 0)
        (= (txn Fee) 1000)
        ;; (= (len (txn Note)) 0)
        (= (txn LastValid) (+ 500 (txn FirstValid)))
        (= (% (txn FirstValid) 10000) 0)
        (= (txn Lease) (byte base32 KEYREGDELEGATE))
        (ed25519verify (txn TxID) arg_0 (addr Bob))))

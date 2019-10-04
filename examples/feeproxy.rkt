#lang racket

(provide feeproxykeyreg)

;; This is delegate logic.
;; arg_0 must be the signature
(define feeproxykeyreg
  '(and (= (txn TypeEnum) 2)
        (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Receiver) (global ZeroAddress))
        (= (txn Amount) 0)
        (< (txn Fee) TMPL_FEE)
        (= (txn LastValid) (+ TMPL_DUR (txn FirstValid)))
        (= (% (txn FirstValid) TMPL_PERIOD) 0)
        (= (txn Lease) (byte base32 TMPL_X))
        (ed25519verify (txn TxID) arg_0 (addr TMPL_AUTH))))

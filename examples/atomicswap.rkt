#lang racket

(provide atomicswap)

;; This is an escrow.
(define atomicswap
  '(and (< (txn Fee) (int TMPL_FEE))
        (= (txn TypeEnum) 1)
        (or (and (= (txn CloseRemainderTo) (global ZeroAddress))
                 (= (txn Receiver) (addr TMPL_RCV))
                 (= (TMPL_HASHFN arg_0) (byte base64 TMPL_HASHIMG)))
            (and (= (txn CloseRemainderTo) (global ZeroAddress))
                 (= (txn Receiver) (addr TMPL_ESC))
                 (> (txn FirstValid) (int TMPL_TIMEOUT))))))

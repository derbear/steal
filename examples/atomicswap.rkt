#lang racket

(provide atomicswap)

;; Implements an atomic swap.
;; This is an escrow.
;;
;; The receiver must be omitted.
;;
;; Money is released under two circumstances:
;; 1. To TMPL_RCV if TMPL_HASHFN(arg_0) = TMPL_HASHIMG
;; 2. To TMPL_OWN if txn.FirstValid > TMPL_TIMEOUT
(define atomicswap
  '(and (< (txn Fee) (int TMPL_FEE))
        (= (txn TypeEnum) 1)
        (= (txn Receiver) (global ZeroAddress))
        (= (txn Amount) 0)
        (or (and (= (txn CloseRemainderTo) (addr TMPL_RCV))
                 (= (TMPL_HASHFN arg_0) (byte base64 TMPL_HASHIMG)))
            (and (= (txn CloseRemainderTo) (addr TMPL_OWN))
                 (> (txn FirstValid) (int TMPL_TIMEOUT))))))

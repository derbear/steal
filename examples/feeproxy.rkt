#lang racket

(provide feeproxykeyreg)

;; Creates a delegate key solely with the authority to
;; register participation keys.
;; This is delegate logic.
;;
;; TMPL_AUTH specifies the delegate public key.  For the
;; transaction to be valid, arg_0 must contain the signature
;; of the private key corresponding to TMPL_AUTH on the
;; key registration transaction.
;;
;; This allows the delegate key to spend TMPL_FEE every
;; TMPL_PERIOD rounds for TMPL_DUR after every multiple
;; of TMPL_PERIOD.
(define feeproxykeyreg
  '(and (= (txn TypeEnum) 2)
        (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Receiver) (global ZeroAddress))
        (= (txn Amount) 0)
        (< (txn Fee) (int TMPL_FEE))
        (= (txn LastValid) (+ (int TMPL_DUR) (txn FirstValid)))
        (= (% (txn FirstValid) (int TMPL_PERIOD)) 0)
        (= (txn Lease) (byte base64 TMPL_X))
        (ed25519verify (txn TxID) arg_0 (addr TMPL_AUTH))))

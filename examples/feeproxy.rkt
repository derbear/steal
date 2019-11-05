#lang racket

(provide feeproxykeyreg)
(provide feeproxykeyreg-doc)
;; (provide feeproxykeyreg-revocable)

(define feeproxykeyreg-doc
"Creates a delegate key solely with the authority to
register participation keys.
This is delegate logic.

TMPL_AUTH specifies the delegate public key.  For the
transaction to be valid, arg_0 must contain the signature
of the private key corresponding to TMPL_AUTH on the
key registration transaction.

This allows the delegate key to spend TMPL_FEE every
TMPL_PERIOD rounds for TMPL_DUR after every multiple
of TMPL_PERIOD.

Parameters:
 - TMPL_AUTH: key authorized with delegation authority
 - TMPL_EXPIRE: round at which key expires
 - TMPL_PERIOD: time between a pair of allowed registration periods
 - TMPL_DUR: duration of an allowed registration period
 - TMPL_LEASE: string to use for the transaction lease
 - TMPL_FEE: maximum fee used by the delegate key registration transaction
")

(define feeproxykeyreg
  '(and (= (txn TypeEnum) 2)
        (< (txn Fee) (int TMPL_FEE))
        (< (txn LastValid) (int TMPL_EXPIRE))
        (= (txn LastValid) (+ (int TMPL_DUR) (txn FirstValid)))
        (= (% (txn FirstValid) (int TMPL_PERIOD)) 0)
        (= (txn Lease) (byte base64 TMPL_LEASE))
        (ed25519verify (txn TxID) arg_0 (addr TMPL_AUTH))))

(define grouped-keyreg
  '(and (= (gtxn 0 TypeEnum) 2)
        (< (gtxn 0 Fee) (int TMPL_FEE))
        (< (gtxn 0 LastValid) (int TMPL_EXPIRE))
        (= (gtxn 0 LastValid) (+ (int TMPL_DUR) (gtxn 0 FirstValid)))
        (= (% (gtxn 0 FirstValid) (int TMPL_PERIOD)) 0)
        (= (gtxn 0 Lease) (byte base64 TMPL_X))
        (ed25519verify (gtxn 0 TxID) arg_0 (addr TMPL_AUTH))))

(define asset-check
  '(and (= (gtxn 1 TypeEnum) 4)
        (< (gtxn 1 Fee) (int TMPL_FEE))
        (= (gtxn 1 XferAsset) (int TMPL_ASSET))
        (= (gtxn 1 Sender) (gtxn 1 AssetReceiver))
        (= (gtxn 1 AssetSender) (global ZeroAddress))
        (> (gtxn 1 AssetAmount) 0)))

;; Like feeproxykeyreg, but the key is also revocable
;; by removing an asset.
;; This is delegate logic.
;;
;; Key registration transactions are allowed on the
;; delegate as long as the account holds at least one of
;; TMPL_ASSET.
(define feeproxykeyreg-revocable
  `(and (= (global GroupSize) 2)
        ,grouped-keyreg
        ,asset-check))

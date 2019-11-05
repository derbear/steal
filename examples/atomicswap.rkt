#lang racket

(provide atomicswap)
(provide atomicswap-doc)

(define atomicswap-doc
"Implements an atomic swap.
This is a contract account.

The receiver must be omitted.

Money is released under two circumstances:
1. To TMPL_RCV if TMPL_HASHFN(arg_0) = TMPL_HASHIMG
2. To TMPL_OWN if txn.FirstValid > TMPL_TIMEOUT

Parameters:
 - TMPL_RCV: the address to send funds to when the preimage is supplied
 - TMPL_HASHFN: the specific hash function (either sha256 or keccak256) to apply
 - TMPL_HASHIMG: the image of the hash function
 - TMPL_TIMEOUT: the round at which the account expires
 - TMPL_OWN: the address to refund funds to on timeout
 - TMPL_FEE: maximum fee used by the atomic swap transaction
")

(define atomicswap
  '(and (< (txn Fee) (int TMPL_FEE))
        (= (txn TypeEnum) 1)
        (= (txn Receiver) (global ZeroAddress))
        (= (txn Amount) 0)
        (or (and (= (txn CloseRemainderTo) (addr TMPL_RCV))
                 (= (TMPL_HASHFN arg_0) (byte base64 TMPL_HASHIMG)))
            (and (= (txn CloseRemainderTo) (addr TMPL_OWN))
                 (> (txn FirstValid) (int TMPL_TIMEOUT))))))

#lang racket

(provide oracle)
(provide oracle-insurance-event)

;; Signs statements to be used as inputs to the TEAL script.
;; This is an ephemeral escrow.
;;
;; To use this oracle, send a transaction to the escrow containing
;; TMPL_OFEE, and then send a transaction from the escrow to TMPL_OWN
;; containing TMPL_OFEE, with the signed statement present in the second
;; transaction's note field, and the oracle's signature on that statement
;; as arg_0.  For security, these two transactions should be present in the
;; same group transaction.
;;
;; TMPL_AUTH is the public key of the oracle.
(define oracle
  '(and (= (txn TypeEnum) 1)
        (= (txn Amount) (int TMPL_OFEE))
        (= (txn Receiver) (addr TMPL_OWN))
        (= (txn CloseRemainderTo) (addr TMPL_OWN))
        (ed25519verify (txn Note) arg_0 (addr TMPL_AUTH))))

;; Pays out depending on a specific bit signed by an oracle.
;; This is an escrow.
;;
;; If the signed bit is set, TMPL_RCV1 receives all money in the escrow.
;; If the signed bit is unset, TMPL_RCV2 receives the money instead.
;;
;; The signed bit is keyed by a 32-bit integer TMPL_OID.  The oracle signs
;; the 64-bit integer (TMPL_OID << 32) | BIT where BIT is the signed bit.
;; This 64-bit integer is encoded as the note of a statement issued by an
;; oracle at the address TMPL_ORCL.
;;
;; TMPL_OWN receives all money when the escrow times out at TMPL_TIMEOUT.
;;
;; This escrow expects to be in a group transaction of size 2 (unless it is
;; closing due to timeout).  The transaction at index 0 pays out to one of
;; the receivers depending on the note field in the transaction at index 1,
;; which is signed by the oracle's public key.
(define oracle-insurance-event
  '(and (< (txn Fee) (int TMPL_FEE))
        (= (txn TypeEnum) 1)
        (if (= (global GroupSize) 1)
            (and (> (txn FirstValid) (int TMPL_TIMEOUT))
                 (= (txn Receiver) (global ZeroAddress))
                 (= (txn CloseRemainderTo) (addr TMPL_OWN)))

            (and (= (global GroupSize) 2)

                 (= (txn GroupIndex) 0)
                 (= (txn Amount) (+ (int TMPL_OFEE) (int TMPL_FEE)))
                 (= (txn Receiver) (addr TMPL_ORCL))

                 (= (gtxn 1 Sender) (addr TMPL_ORCL))
                 (= (/ (btoi (gtxn 1 Note)) 4294967296) (int TMPL_OID))

                 (if (= (% (btoi (gtxn 1 Note)) 2) 1)
                     (= (txn CloseRemainderTo) (addr TMPL_RCV1))
                     (= (txn CloseRemainderTo) (addr TMPL_RCV2)))))))

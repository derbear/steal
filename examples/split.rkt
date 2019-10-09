#lang racket

(provide split)

(define split-core
  '(= (txn TypeEnum) 1))

(define split-transfer
  '(and (= (global GroupSize) 2)
        (= (gtxn 0 Sender) (gtxn 1 Sender))

        (= (gtxn 0 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 0 Receiver) (addr TMPL_RCV1))
        (= (gtxn 1 CloseRemainderTo) (global ZeroAddress))
        (= (gtxn 1 Receiver) (addr TMPL_RCV2))

        (< (gtxn 0 Fee) (int TMPL_FEE))
        (< (gtxn 1 Fee) (int TMPL_FEE))

        (= (gtxn 0 Amount) (/ (* (+ (gtxn 0 Amount) (gtxn 1 Amount)) (int TMPL_RATN)) (int TMPL_RATD)))

        ;; prevent drainage via small fees; loss due to imprecision
        (> (gtxn 0 Amount) (int TMPL_MINPAY))))

(define split-close
  '(and (= (txn CloseRemainderTo) (addr TMPL_OWN))
        (= (txn Receiver) (global ZeroAddress))
        (> (txn FirstValid) (int TMPL_TIMEOUT))))

;; Splits money sent to some account to two recipients at some ratio.
;; This is an escrow.
;;
;; This allows either a two-transaction group, for executing a
;; split, or single transaction, for closing the escrow.
;;
;; Withdrawals from this account are allowed as a group transaction which
;; sends TMPL_RCV1 exactly TMPL_RATN/TMPL_RATD times the sum of amounts
;; across both transactions.  At least TMPL_MINPAY must be sent to TMPL_RCV1.
;; (CloseRemainderTo must be zero.)
;;
;; After TMPL_TIMEOUT passes, all funds can be refunded to TMPL_OWN.
(define split
  `(and ,split-core
        (or ,split-transfer
            ,split-close)))

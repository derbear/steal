#lang racket

(provide periodicpayment)
(provide periodicpayment-doc)
(provide periodicpayment-escrow)
(provide periodicpayment-escrow-doc)

(define periodicpayment-core
  '(and (= (txn TypeEnum) 1)
        (< (txn Fee) (int TMPL_FEE))
        (= (% (txn FirstValid) (int TMPL_PERIOD)) 0)
        (= (txn LastValid) (+ (int TMPL_DUR) (txn FirstValid)))
        (= (txn Lease) (byte base64 TMPL_LEASE))))

(define periodicpayment-transfer
  '(and (= (txn CloseRemainderTo) (global ZeroAddress))
        (= (txn Receiver) (addr TMPL_RCV))
        (= (txn Amount) (int TMPL_AMT))))

(define periodicpayment-close
  '(and (= (txn CloseRemainderTo) (addr TMPL_RCV))
        (= (txn Receiver) (global ZeroAddress))
        (> (txn FirstValid) (int TMPL_TIMEOUT))
        (= (txn Amount) 0)))

(define periodicpayment-doc
"Allows some account to execute periodic withdrawal of funds.
This is delegate logic.

This allows TMPL_RCV to withdraw TMPL_AMT every
TMPL_PERIOD rounds for TMPL_DUR after every multiple
of TMPL_PERIOD.

Parameters:
 - TMPL_RCV: address which is authorized to make withdrawals
 - TMPL_PERIOD: the time between a pair of withdrawal periods
 - TMPL_DUR: the duration of a withdrawal period
 - TMPL_AMT: the maximum number of funds allowed for a single withdrawal
 - TMPL_LEASE: string to use for the transaction lease
 - TMPL_FEE: maximum fee used by the withdrawal transaction
")

(define periodicpayment
  `(and ,periodicpayment-core
        ,periodicpayment-transfer))

(define periodicpayment-escrow-doc
"Allows some account to execute periodic withdrawal of funds.
This is a contract account.

This allows TMPL_RCV to withdraw TMPL_AMT every
TMPL_PERIOD rounds for TMPL_DUR after every multiple
of TMPL_PERIOD.

After TMPL_TIMEOUT, all remaining funds in the escrow
are available to TMPL_RCV.

Parameters:
 - TMPL_RCV: address which is authorized to make withdrawals
 - TMPL_PERIOD: the time between a pair of withdrawal periods
 - TMPL_DUR: the duration of a withdrawal period
 - TMPL_AMT: the maximum number of funds allowed for a single withdrawal
 - TMPL_LEASE: string to use for the transaction lease
 - TMPL_TIMEOUT: the round at which the account expires
 - TMPL_FEE: maximum fee used by the withdrawal transaction
")

(define periodicpayment-escrow
  `(and ,periodicpayment-core
        (or ,periodicpayment-transfer
            ,periodicpayment-close)))

#lang racket

(provide feeproxykeyreg)

;; TODO will need to reason about the fee over time...
;; (define feeproxy
;;   '(&& (= (txn CloseRemainderTo) Alice)
;;        (= (txn Receiver) Alice)
;;        (= (len (txn note)) 0)
;;        (= 500 (- (txn firstvalid) (txn lastvalid)))
;;        (ed25519verify X (txn firstvalid) Bob)))

;; arg_0 must be txn.FirstValid
;; arg_1 must be the signature
(define feeproxykeyreg
  '(&& (= (txn CloseRemainderTo) (global ZeroAddress))
       (= (txn Receiver) (global ZeroAddress))
       (= (txn Amount) 0)
       (= (txn Fee) 1000)
       ;; (= (len (txn Note)) 0)
       (= (txn LastValid) (+ 500 (txn FirstValid)))
       (= (% (txn FirstValid) 10000) 0)
       (= (btoi arg_0) (txn FirstValid))
       (ed25519verify arg_0 arg_1 (addr Bob))))

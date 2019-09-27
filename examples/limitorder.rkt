#lang racket

(provide limitorder)
(provide limitorder-fill)

;; TODO specify txtypes in this script

;; This is delegate logic.
;; This is position 1 in a group txn.
;; Alice is looking for some Bob such that (5 * Bob's DerekCoin) > (2 * Alice's Algos)
;; Alice is looking for a minimum sale of 10000 Algos
(define limitorder
  '(and (= (global GroupSize) 2)
        (= (gtxn 0 CloseRemainderTo) (addr Alice)) ;; erase remainder of account after exchange
        (= (txn 0 TypeEnum) 1)
        (= (txn 1 TypeEnum) 4)

        (< (gtxn 0 Fee) 10000)
        (< (gtxn 1 Fee) 10000)

        ;; exchange Algos for DerekCoin
        ;; (= (gtxn 0 Receiver) (addr Bob)) ; implicit
        (= (gtxn 1 AssetReceiver) (addr Alice))
        (= (gtxn 1 XferAsset) (byte base64 DerekCoin))
        (= (gtxn 1 AssetSender) (global ZeroAddress))

        (> (gtxn 0 Amount) 10000)
        (< (* 2 (gtxn 0 Amount)) (* 5 (gtxn 1 AssetAmount)))))

;; This is delegate logic.
;; This is position 1 in a group txn.
;; Bob is looking for some Alice such that (3 * Alice's Algos) > (5 * Bob's DerekCoin)
;; Bob is looking for a minimum sale of 5000 DerekCoin
(define limitorder-fill
  '(and (= (global GroupSize) 3)
        (= (gtxn 1 AssetCloseTo) (addr Bob)) ;; refund leftover assets to Bob
        (= (txn 0 TypeEnum) 1)
        (= (txn 1 TypeEnum) 4)
        (= (txn 2 TypeEnum) 1)

        ;; erase remainder of account after exchange
        (= (gtxn 2 Sender) (gtxn 1 Sender))
        (= (gtxn 2 Receiver) (global ZeroAddress))
        (= (gtxn 2 CloseRemainderTo) (addr Bob))
        
        (< (gtxn 0 Fee) 10000)
        (< (gtxn 1 Fee) 10000)
        (< (gtxn 2 Fee) 10000)

        ;; exchange Algos for DerekCoin
        (= (gtxn 0 Receiver) (addr Bob))
        ;; (= (gtxn 1 AssetReceiver) (addr Alice)) ; implicit
        (= (gtxn 1 XferAsset) (byte base64 DerekCoin))
        (= (gtxn 1 AssetSender) (global ZeroAddress))

        (> (gtxn 1 AssetAmount) 5000)
        (> (* 3 (gtxn 0 Amount)) (* 5 (gtxn 1 AssetAmount)))))

#lang racket

(provide limitorder)
(provide limitorder-fill)

;; This is an escrow.
;; This is position 0 in a group txn.
;; Alice is looking for some Bob such that (5 * Bob's DerekCoin) > (2 * Alice's Algos)
;; Alice is looking for a minimum sale of 10000 Algos
(define limitorder
  '(and (= (global GroupSize) 2)
        (= (txn GroupIndex) 0)
        (< (gtxn 0 Fee) (int TMPL_FEE))
        (< (gtxn 1 Fee) (int TMPL_FEE))

        ;; gtxn 0
        ;; Escrow -> *: >10000 Algos
        ;; Alice |-> Alice
        (= (gtxn 0 TypeEnum) 1)
        (> (gtxn 0 Amount) (int TMPL_MINTRD))
        (= (gtxn 0 CloseRemainderTo) (addr TMPL_OWN)) ;; erase remainder of account after exchange

        ;; gtxn 1
        ;; * -> Alice: >(ratio * 10000) DerekCoin
        (= (gtxn 1 TypeEnum) 4)
        (= (gtxn 1 XferAsset) (byte base64 TMPL_ASSET))
        (= (gtxn 1 AssetReceiver) (addr TMPL_OWN))
        (= (gtxn 1 AssetSender) (global ZeroAddress))
        (< (* 2 (gtxn 0 Amount)) (* 5 (gtxn 1 AssetAmount)))))

;; This is an escrow.
;; This is position 1 and 2 in a group txn.
;; Bob is looking for some Alice such that (3 * Alice's Algos) > (5 * Bob's DerekCoin)
;; Bob is looking for a minimum sale of 5000 DerekCoin
(define limitorder-fill
  '(and (= (global GroupSize) 3)
        (or (= (txn GroupIndex) 1) (= (txn GroupIndex) 2))
        (< (gtxn 0 Fee) (int TMPL_FEE))
        (< (gtxn 1 Fee) (int TMPL_FEE))
        (< (gtxn 2 Fee) (int TMPL_FEE))

        ;; gtxn 0
        ;; * -> Bob: >(ratio * 5000) Algos
        (= (gtxn 0 TypeEnum) 1)
        (= (gtxn 0 Receiver) (addr TMPL_OWN))
        (> (* 3 (gtxn 0 Amount)) (* 5 (gtxn 1 AssetAmount)))

        ;; gtxn 1
        ;; Escrow -> *: >5000 DerekCoin
        ;; Escrow |-> Bob (DerekCoin)
        (= (gtxn 1 TypeEnum) 4)
        (= (gtxn 1 XferAsset) (byte base64 TMPL_ASSET))
        (= (gtxn 1 AssetSender) (global ZeroAddress))
        (> (gtxn 1 AssetAmount) (int TMPL_MINTRD))
        (= (gtxn 1 AssetCloseTo) (addr TMPL_OWN)) ;; refund leftover assets to Bob

        ;; gtxn 2
        ;; Escrow |-> Bob
        (= (gtxn 2 TypeEnum) 1)
        (= (gtxn 2 Receiver) (global ZeroAddress))
        (= (gtxn 2 CloseRemainderTo) (addr TMPL_OWN))))
        

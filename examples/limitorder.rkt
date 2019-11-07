#lang racket

(provide limitorder)
(provide limitorder-doc)
;; (provide limitorder-fill)

(define limitorder-doc
"Implements a limit order for an asset, given Algos.
This is a contract account.

This allows either a two-transaction group, for executing the
trade, or single transaction, for closing the position.

Let ratio = TMPL_SWAPN / TMPL_SWAPD.

Filling the order requires a group transaction of size two.
More than TMPL_MINTRD can be sent to any address, and more
than ratio * TMPL_MINTRD can be sent to TMPL_OWN. All remaining
funds are refunded to TMPL_OWN.

After TMPL_TIMEOUT passes, all funds can be refunded to TMPL_OWN.

Parameters:
 - TMPL_ASSET: ID of the transferred asset
 - TMPL_SWAPN: exchange rate (for N algos, want rate * N coin) (numerator)
 - TMPL_SWAPD: exchange rate (for N algos, want rate * N coin) (denominator)
 - TMPL_TIMEOUT: the round at which the account expires
 - TMPL_OWN: the address to refund funds to on timeout
 - TMPL_FEE: maximum fee used by the limit order transaction
 - TMPL_MINTRD: the minimum amount (of Algos) to be traded away
")

(define limitorder
  '(and (= (txn GroupIndex) 0)
        (= (txn TypeEnum) 1)
        (< (txn Fee) (int TMPL_FEE))
        (= (txn CloseRemainderTo) (addr TMPL_OWN))
        (< (global GroupSize) 3)

        (if (= (global GroupSize) 1)
            ;; Escrow |-> TMPL_OWN
            (and (> (txn FirstValid) (int TMPL_TIMEOUT))
                 (= (txn Receiver) (global ZeroAddress))
                 (= (txn Amount) 0))

            ;; gtxn 0
            ;; Escrow -> *: >TMPL_MINTRD Algos
            ;; Escrow |-> TMPL_OWN
            ;;
            ;; gtxn 1
            ;; * -> TMPL_OWN: >(ratio * TMPL_ASSET)
            (and (> (txn Amount) (int TMPL_MINTRD))

                 (= (gtxn 1 TypeEnum) 4)
                 (= (gtxn 1 XferAsset) (int TMPL_ASSET))
                 (= (gtxn 1 AssetReceiver) (addr TMPL_OWN))
                 (= (gtxn 1 AssetSender) (global ZeroAddress))
                 (< (* (int TMPL_SWAPN) (txn Amount)) (* (int TMPL_SWAPD) (gtxn 1 AssetAmount)))))))

;; ;; This is an escrow.
;; ;; This is position 1 and 2 in a group txn.
;; ;; Bob is looking for some Alice such that (3 * Alice's Algos) > (5 * Bob's DerekCoin)
;; ;; Bob is looking for a minimum sale of 5000 DerekCoin
;; (define limitorder-fill
;;   '(and (= (global GroupSize) 3)
;;         (or (= (txn GroupIndex) 1) (= (txn GroupIndex) 2))
;;         (< (gtxn 0 Fee) (int TMPL_FEE))
;;         (< (gtxn 1 Fee) (int TMPL_FEE))
;;         (< (gtxn 2 Fee) (int TMPL_FEE))

;;         ;; gtxn 0
;;         ;; * -> Bob: >(ratio * 5000) Algos
;;         (= (gtxn 0 TypeEnum) 1)
;;         (= (gtxn 0 Receiver) (addr TMPL_OWN))
;;         (> (* 3 (gtxn 0 Amount)) (* 5 (gtxn 1 AssetAmount)))

;;         ;; gtxn 1
;;         ;; Escrow -> *: >5000 DerekCoin
;;         ;; Escrow |-> Bob (DerekCoin)
;;         (= (gtxn 1 TypeEnum) 4)
;;         (= (gtxn 1 XferAsset) (int TMPL_ASSET))
;;         (= (gtxn 1 AssetSender) (global ZeroAddress))
;;         (> (gtxn 1 AssetAmount) (int TMPL_MINTRD))
;;         (= (gtxn 1 AssetCloseTo) (addr TMPL_OWN)) ;; refund leftover assets to Bob

;;         ;; gtxn 2
;;         ;; Escrow |-> Bob
;;         (= (gtxn 2 TypeEnum) 1)
;;         (= (gtxn 2 Receiver) (global ZeroAddress))
;;         (= (gtxn 2 CloseRemainderTo) (addr TMPL_OWN))))
        

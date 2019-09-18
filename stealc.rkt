#lang racket

(require racket/trace)

(require "examples/atomicswap.rkt")
(require "examples/split.rkt")
(require "examples/feeproxy.rkt")

(define [stealc prog]
  (string-join (map stealc-postprocess (stealc-lines (stealc-preprocess prog))) "\n"))

(define [stealc-lines prog]
  (if (list? prog)
      (let [(lines (append (stealc-lines-recur (rest prog))
                          (list (stealc-op (first prog)))))]
        (stealc-flatten prog lines))
      (list (stealc-arg prog))))

(define [stealc-lines-recur prog]
  (cond [(null? prog) prog]
        [(list? prog)
         (append (stealc-lines (first prog))
                 (stealc-lines-recur (rest prog)))]
        [else (list (stealc-arg prog))]))

(define [stealc-op op]
  (cond [(eq? op '||) "||"]
        [(eq? op '=) "=="]
        [else (symbol->string op)]))

(define [stealc-arg arg]
  (cond [(symbol? arg) (symbol->string arg)]
        [(number? arg) (number->string arg)]
        [else arg]))

(define [stealc-flatten prog lines]
  (if (list? lines)
      (cond [(eq? (first prog) 'txn) (stealc-flatten-txn lines)]
            [(eq? (first prog) 'gtxn) (stealc-flatten-gtxn lines)]
            [(eq? (first prog) 'addr) (stealc-flatten-addr lines)]
            [(eq? (first prog) 'global) (stealc-flatten-global lines)]
            [(eq? (first prog) 'byte) (stealc-flatten-byte lines)]
            [else lines])
      lines))

(define [stealc-flatten-txn lines]
  (list (string-join (list (second lines) (first lines)) " ")))

(define [stealc-flatten-addr lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-global lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-byte lines]
  (list (string-join (list (third lines) (first lines) (second lines)) " ")))

(define [stealc-flatten-gtxn lines]
  (stealc-flatten-byte lines))

(define [stealc-preprocess prog]
  (if (list? prog)
      (stealc-preprocess-expand prog)
      prog))

(define [stealc-preprocess-expand prog]
  (if (null? prog)
      prog
      (cond [(eq? (first prog) '&&) (stealc-preprocess-distribute prog)]
            [(eq? (first prog) '||) (stealc-preprocess-distribute prog)]
            [else (cons (stealc-preprocess (first prog)) (map stealc-preprocess (rest prog)))])))

(define [stealc-preprocess-distribute prog]
  (cond [(null? (rest prog)) (rest prog)]
        [(null? (rest (rest (rest prog)))) (list (first prog) (stealc-preprocess (second prog)) (stealc-preprocess (third prog)))]
        [else (cons (first prog) (cons (stealc-preprocess (second prog)) (list (map stealc-preprocess (cons (first prog) (rest (rest prog)))))))]))

(define [stealc-postprocess line]
  (if (eq? (string->number line) #f)
      line
      (string-append "int " line)))

(define [stealc-template-populate prog args]
  (if (and (list? prog) (not (null? prog)))
      (cond [(null? (first prog)) prog]
            [(list? (first prog)) (cons (stealc-template-populate (first prog) args) (stealc-template-populate (rest prog) args))]
            [(symbol? (first prog))
             (if (assoc (first prog) args)
                 (cons (second (assoc (first prog) args)) (stealc-template-populate (rest prog) args))
                 (cons (first prog) (stealc-template-populate (rest prog) args)))]
            [else prog])
      prog))

(define args
  '((Alice "YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ")
    (Bob "CVMUT7RKA3XBHQVVTGBV5EKC7M7ZSCHZMHQQ3MCOCWSKWBH7PVIQ43YGGY")
    (hash sha256)
    (Image "uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkk=")))


;; (display "\n")
;; (display "atomic swap\n:::::\n\n")
(display (stealc (stealc-template-populate atomicswap args)))
;; (display "\n")

;; (display "\n")
;; (display "split\n:::::\n\n")
;; (display (stealc split))
;; (display "\n")

;; (display "\n")
;; (display "delegate keyreg\n:::::\n\n")
;; (display (stealc feeproxykeyreg))
;; (display "\n")

#lang racket

(require racket/trace)

(provide stealc)
(provide stealc-bind)

(define [stealc prog]
  (string-join (map stealc-postprocess (stealc-lines (stealc-preprocess prog))) "\n"))

(define [stealc-lines prog]
  (if (list? prog)
      (stealc-lines-special prog)
      (list (stealc-arg prog))))

(define next-label-number 0)
(define (gen-label!)
  (let [(num next-label-number)]
    (set! next-label-number (+ 1 next-label-number))
    (string-append "label" (number->string num))))

(define [stealc-lines-special prog]
  (cond [(eq? (first prog) 'if)
         (let [(then-entry (gen-label!))
               (otherwise-entry (gen-label!))]
           (append (stealc-lines (second prog)) ; test body
                   (list (string-append "bnz " then-entry))
                   (stealc-lines (fourth prog)) ; else body
                   (list "int 1")
                   (list (string-append "bnz " otherwise-entry))
                   (list (string-append "pop // check bug workaround"))
                   (list (string-append then-entry ":"))
                   (stealc-lines (third prog)) ; then body
                   (list (string-append otherwise-entry ":"))))]
        [else
         (let [(lines (append (stealc-lines-recur (rest prog))
                              (list (stealc-op (first prog)))))]
           (stealc-flatten prog lines))]))

(define [stealc-lines-recur prog]
  (cond [(null? prog) prog]
        [(list? prog)
         (append (stealc-lines (first prog))
                 (stealc-lines-recur (rest prog)))]
        [else (list (stealc-arg prog))]))

(define [stealc-op op]
  (cond [(eq? op '||) "||"]
        [(eq? op '=) "=="]
        [(eq? op 'and) "&&"]
        [(eq? op 'or) "||"]
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
            [(eq? (first prog) 'int) (stealc-flatten-int lines)]
            [else lines])
      lines))

(define [stealc-flatten-txn lines]
  (list (string-join (list (second lines) (first lines)) " ")))

(define [stealc-flatten-addr lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-global lines]
  (stealc-flatten-txn lines))

(define [stealc-flatten-int lines]
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
            [(eq? (first prog) 'and) (stealc-preprocess-distribute prog)]
            [(eq? (first prog) 'or) (stealc-preprocess-distribute prog)]
            [else (cons (stealc-preprocess (first prog)) (map stealc-preprocess (rest prog)))])))

(define [stealc-preprocess-distribute prog]
  (cond [(null? (rest prog)) (rest prog)]
        [(null? (rest (rest (rest prog)))) (list (first prog) (stealc-preprocess (second prog)) (stealc-preprocess (third prog)))]
        [else (cons (first prog) (cons (stealc-preprocess (second prog)) (list (stealc-preprocess (cons (first prog) (rest (rest prog)))))))]))

(define [stealc-postprocess line]
  (if (eq? (string->number line) #f)
      line
      (string-append "int " line)))

(define [stealc-bind prog args]
  (if (and (list? prog) (not (null? prog)))
      (cond [(null? (first prog)) prog]
            [(list? (first prog)) (cons (stealc-bind (first prog) args) (stealc-bind (rest prog) args))]
            [(symbol? (first prog))
             (if (assoc (first prog) args)
                 (cons (second (assoc (first prog) args)) (stealc-bind (rest prog) args))
                 (cons (first prog) (stealc-bind (rest prog) args)))]
            [else prog])
      prog))

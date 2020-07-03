#lang racket

(require racket/trace)

(provide stealc)
(provide stealc-bind)

(define teal-target-version 2)

(define [stealc prog]
  (string-join (cons (string-append "#pragma version " (number->string teal-target-version))
                     (stealc-postprocess (stealc-lines (stealc-preprocess prog))))
               "\n"))

(define [stealc-lines prog]
  (if (list? prog)
      (stealc-lines-list prog)
      (list (stealc-atom prog))))

(define [stealc-atom arg]
  (cond [(symbol? arg) (symbol->string arg)]
        [(number? arg) (number->string arg)]
        [else arg]))

(define next-label-number 0)
(define (gen-label! pfx)
  (let ([num next-label-number])
    (set! next-label-number (+ 1 next-label-number))
    (string-append pfx (number->string num))))

(define [stealc-lines-if prog]
  (let ([then-entry (gen-label! "if")]
        [otherwise-entry (gen-label! "if_end")])
    (append (stealc-lines (second prog)) ; test body
            (list (string-append "bnz " then-entry))
            (stealc-lines (fourth prog)) ; else body
            (list "int 1")
            (list (string-append "bnz " otherwise-entry))
            (list (string-append then-entry ":"))
            (stealc-lines (third prog)) ; then body
            (list (string-append otherwise-entry ":")))))
  
(define [stealc-lines-begin prog]
  (if (null? prog)
      prog
      (append (stealc-lines (first prog)) (stealc-lines-begin (rest prog)))))

(define [stealc-lines-cond prog]
  (let ([end-label (gen-label! "cond_end")])
    (append (stealc-lines-cond-recur prog end-label #t)
            (list (string-append end-label ":")))))

(define [stealc-lines-cond-recur prog end-label first?]
  (cond [(null? prog) prog]
        [(eq? (first (first prog)) 'else)
         (append (stealc-lines-begin (rest (first prog)))
                 (if first?
                     '()
                     (list "int 1"
                           (string-append "bnz " end-label))))]
        [else
         (let ([test (first (first prog))]
               [body (rest (first prog))]
               [body-label (gen-label! "cond")])
           (append (stealc-lines test)
                   (list (string-append "bnz " body-label))
                   (stealc-lines-cond-recur (rest prog) end-label #f)
                   (list (string-append body-label ":"))
                   (stealc-lines-begin body)
                   (if first?
                       '()
                       (list "int 1"
                             (string-append "bnz " end-label)))
                   ))]))

(define [stealc-lines-unless prog]
  (let ([test (first prog)]
        [body (rest prog)]
        [after-label (gen-label! "unless")])
    (append (stealc-lines test)
            (list (string-append "bnz " after-label))
            (stealc-lines-begin body)
            (list (string-append after-label ":")))))

(define [stealc-lines-when prog]
  (let ([test (first prog)]
        [body (rest prog)]
        [after-label (gen-label! "when")])
    (append (stealc-lines test)
            (list "!")
            (list (string-append "bnz " after-label))
            (stealc-lines-begin body)
            (list (string-append after-label ":")))))

(define [stealc-lines-error prog]
  (append (list (string-append "// " (if (null? prog) "" (first prog))))
          (stealc-lines '(/ 1 0))))

(define [stealc-lines-assert prog]
  (let ([after-label (gen-label! "assert")])
    (append (stealc-lines (first prog))
            (list (string-append "bnz " after-label))
            (list "err")
            (list (string-append after-label ":")))))

(define stealc-stateful-ops
  '((app-opted-in? app_opted_in)
    (app-global-get app_global_get)
    (app-global-gets app_global_gets)
    (app-local-get app_local_get)
    (app-local-gets app_local_gets)
    (app-global-put! app_global_put)
    (app-local-put! app_local_put)
    (app-global-del! app_global_del)
    (app-local-del! app_local_del)))

(define [stealc-stateful-op? op]
  (assoc op stealc-stateful-ops))

(define [stealc-lines-stateful prog]
  (stealc-lines (cons (second (assoc (first prog) stealc-stateful-ops)) (rest prog))))

(define [stealc-lines-list prog]
  (cond [(eq? (first prog) 'if) (stealc-lines-if prog)]
        [(eq? (first prog) 'begin) (stealc-lines-begin (rest prog))]
        [(eq? (first prog) 'cond) (stealc-lines-cond (rest prog))]
        [(eq? (first prog) 'unless) (stealc-lines-unless (rest prog))]
        [(eq? (first prog) 'when) (stealc-lines-when (rest prog))]
        [(eq? (first prog) 'error) (stealc-lines-error (rest prog))]
        [(eq? (first prog) 'assert) (stealc-lines-assert (rest prog))]
        [(stealc-stateful-op? (first prog)) (stealc-lines-stateful prog)]
        [else
         (let ([lines (append (stealc-lines-recur (rest prog))
                              (list (stealc-op->string (first prog))))])
           (stealc-reorder prog lines))]))

(define [stealc-lines-recur prog]
  (cond [(null? prog) prog]
        [(list? prog)
         (append (stealc-lines (first prog))
                 (stealc-lines-recur (rest prog)))]
        [else (list (stealc-atom prog))]))

(define [stealc-op->string op]
  (cond [(eq? op '||) "||"]
        [(eq? op '=) "=="]
        [(eq? op 'and) "&&"]
        [(eq? op 'or) "||"]
        [(eq? op 'not) "!"]
        [else (symbol->string op)]))

(define [stealc-reorder prog lines]
  (if (and (list? lines) (assoc (first prog) stealc-reorder-ops))
      ((second (assoc (first prog) stealc-reorder-ops)) lines)
      lines))

(define [stealc-reorder-txn lines]
  (list (string-join (list (second lines) (first lines)) " ")))

(define [stealc-reorder-txna lines]
  (list (string-join (list (third lines) (first lines) (second lines)) " ")))

(define [stealc-reorder-addr lines]
  (stealc-reorder-txn lines))

(define [stealc-reorder-global lines]
  (stealc-reorder-txn lines))

(define [stealc-reorder-int lines]
  (stealc-reorder-txn lines))

(define [stealc-reorder-string lines]
  (list (string-join (list (second lines) (first lines)) " ")))

(define [stealc-reorder-byte lines]
  (cond [(= (length lines) 2)
         (list (string-join (list (second lines) (first lines)) " "))]
        [(= (length lines) 3)
         (list (string-join (list (third lines) (first lines) (second lines)) " "))]
        [(= (length lines) 4)
         (list (string-join (list (fourth lines) (first lines) (second lines) (third lines)) " "))]
        [else (raise (format "~a length lines is ~a" (last lines) (length lines)))]))
      
(define [stealc-reorder-gtxn lines]
  (stealc-reorder-byte lines))

(define [stealc-reorder-load lines]
  (stealc-reorder-txn lines))

(define [stealc-reorder-store lines]
  (append (second (dq (rest lines)))
          (list (string-join (list "store" (first lines)) " "))))

(define [stealc-reorder-store2 lines]
  (append (second (dq (rest (rest lines))))
          (list (string-join (list "store" (first lines)) " ")
                (string-join (list "store" (second lines)) " "))))

(define [stealc-reorder-asset-holding-get lines]
  (append (second (dq (second (dq lines))))
          (list (string-join (list (last lines)
                                   (last (second (dq lines))))))))

(define [stealc-reorder-note lines]
  (list (string-join (list "//" (first lines)))))

(define stealc-reorder-ops
  `((txn ,stealc-reorder-txn)
    (gtxn ,stealc-reorder-gtxn)
    (txna ,stealc-reorder-txna)
    (addr ,stealc-reorder-addr)
    (global ,stealc-reorder-global)
    (byte ,stealc-reorder-byte)
    (int ,stealc-reorder-int)
    (string ,stealc-reorder-string)
    (load ,stealc-reorder-load)
    (store! ,stealc-reorder-store)
    (store2! ,stealc-reorder-store2)
    (asset_holding_get ,stealc-reorder-asset-holding-get)
    (note ,stealc-reorder-note)))

;; preprocess

(define stealc-vararg-ops
  '(&& || and or + * concat))

(define [stealc-preprocess prog]
  (cond [(not (list? prog)) prog]
        [(null? prog) prog]
        [(member (first prog) stealc-vararg-ops) (stealc-preprocess-distribute prog)]
        [else (cons (stealc-preprocess (first prog)) (map stealc-preprocess (rest prog)))]))

(define [stealc-preprocess-distribute prog]
  (cond [(null? (rest prog)) (rest prog)]
        [(null? (rest (rest (rest prog)))) (list (first prog) (stealc-preprocess (second prog)) (stealc-preprocess (third prog)))]
        [else (let ([expr (dq prog)])
                (list (first prog)
                      (stealc-preprocess (second expr))
                      (stealc-preprocess (first expr))))]))

(define [dq l]
  (let ([lrev (reverse l)])
    (list (first lrev) (reverse (rest lrev)))))

;; postprocess

(define [stealc-postprocess-line line]
  (if (eq? (string->number line) #f)
      line
      (string-append "int " line)))

(define [stealc-postprocess lines] (map stealc-postprocess-line lines))

;; bind

(define [stealc-bind prog args]
  (cond [(symbol? prog)
         (if (assoc prog args)
             (second (assoc prog args))
             prog)]
        [(null? prog) prog]
        [(list? prog) (cons (stealc-bind (first prog) args) (stealc-bind (rest prog) args))]
        [else prog]))

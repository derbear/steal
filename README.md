# sTEAL

sTEAL provides syntactic sugar for Algorand TEAL contracts, embedded in Scheme.

# Use

## Basic

Let's say we have the following sTEAL contract code:

```
(define sample '(= (txn Sender)
                   (txn Receiver)))
```

We can use the Scheme compiler `stealc` to make TEAL assembly code.
If we have the following code in `example.rkt`

```
#lang racket
(require "stealc.rkt")

(define sample '(= (txn Sender)
                   (txn Receiver)))

(displayln (stealc sample))
```

then running this program produces

```
$ racket example.rkt
txn Sender
txn Receiver
==
```

## Template arguments

sTEAL can be used to make templates for TEAL contracts.

```
#lang racket
(require "stealc.rkt")

(define sample '(= (addr TMPL_RCV)
                   (txn Receiver)))

(displayln (stealc sample))
```

produces

```
addr TMPL_RCV
txn Receiver
==
```

To instantiate the templates with conrete values, use `stealc-bind`:

```
#lang racket
(require "stealc.rkt")

(define sample '(= (addr TMPL_RCV)
                   (txn Receiver)))

(define args
  '((TMPL_RCV "YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ")))

(displayln (stealc (stealc-bind sample args)))
```

produces

```
addr YC3XWSU3EUISB6N4EOGW5NYEMDSSWPGPMN3ZOKD33UDKPNK2HIXYPFLVXQ
txn Receiver
==
```

# Notes

## Variable-length arguments

Certain symbols, such as `and`, compile into TEAL expressions that accept a
variable number of arguments.

```
'(and (= (+ (txn FirstValid) (txn LastValid)) 500)
      (= arg_0 (byte base64 "uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkk="))
      (> (txn Fee) (int TMPL_FEE)))
```

compiles into

```
txn FirstValid
txn LastValid
+
int 500
==
arg_0
byte base64 uFVEhjBpkpKQ8sZaau0qsDsf0eW3oXFEn1Ar5o39vkk=
==
&&
txn Fee
int TMPL_FEE
>
&&
```

## Short-circuiting 

The `if` symbol produces TEAL code which behaves similarly to Lisp's `if`
special form.

```
'(if (= (global GroupSize) 1)
     (= (txn GroupIndex) 6)
     (< (txn Amount) 500))
```

compiles into

```
global GroupSize
int 1
==
bnz label0
txn Amount
int 500
<
int 1
bnz label1
label0:
txn GroupIndex
int 6
==
label1:
```

The labels produced in such a way are all unique.

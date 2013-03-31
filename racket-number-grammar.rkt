#lang racket
;;; racket-number-grammar.rkt
;;; -------------------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; A (full?) decription of the racket number syntax, which produces regular
;;; expressions to put into the racket GeSHi description. Based on the description
;;; of racket's reader in the racket reference
(provide racket-NUMBERS-alist)

(require "grammar-to-regex.rkt")

;;; sort the rules into the order which will lend to the longest match...
(define (racket-number-grammar/n n)
  `(
    #|
      ((number ,n)            (or (inexact ,n) (exact ,n)))
      ((inexact ,n)           (or (inexact-complex ,n) (inexact-real ,n)))
      ((exact  ,n)            (or (exact-complex ,n) (exact-rational ,n) (exact-integer ,n)))
|#
    ((number ,n)            (or
                             (inexact-complex ,n)
                             (exact-complex ,n)
                             (exact-rational ,n)
                             (inexact-real ,n)
                             (exact-integer ,n)))
    ((exact-integer ,n)     (: (? sign) (unsigned-integer ,n)))
    ((unsigned-integer ,n)  (+ (re "[0-9]")))
    ((exact-rational ,n)    (: (? sign) (unsigned-rational ,n)))
    ((unsigned-rational ,n) (: (unsigned-integer ,n) "\\\\/" (unsigned-integer ,n)))
    ((exact-complex ,n)     (: (exact-rational ,n) sign (unsigned-rational ,n) "i"))
    
    ((inexact-real ,n)      (or
                             (: (? sign) (inexact-normal ,n))
                             (: sign inexact-special)
                             ))
    ((inexact-unsigned ,n)  (or (inexact-normal ,n) inexact-special))
    ((inexact-normal ,n)    (: (inexact-simple ,n) (? (: (exp-mark ,n) (exact-integer ,n)))))
    ((inexact-simple ,n)    (or
                             (: (? (unsigned-integer ,n)) (? "\\.") (digits# ,n))
                             (: (digits# ,n) (? "\\.") (* "#"))
                             (: (digits# ,n) "\\\\/" (digits# ,n))))
    ((digits# ,n)           (: (+ (digit ,n)) (* "#")))
    ((inexact-complex ,n)   (or (: (? (inexact-real ,n)) sign (inexact-unsigned ,n) "i")
                                (: (inexact-real ,n) "@" (inexact-real ,n))))
    ((general-number ,n)    (: exactness (number ,n)))))

(define racket-number-grammar
  `((exactness       (or "#e" "#i"))
    (sign            (or "-" "\\+"))
    (inexact-special (: (or "inf\\." "nan\\.") (range "0f")))
    ((digit 2)     (range "0-1"))
    ((digit 8)     (range "0-7"))
    ((digit 10)    (range "0-9"))
    ((digit 16)    (range "0-9" "a-f" "A-F"))
    ((exp-mark 2)  (exp-mark 10))
    ((exp-mark 8)  (exp-mark 10))
    ((exp-mark 10) (or (exp-mark 16) (range "def")))
    ((exp-mark 16) (range "sl"))
    
    ,@(racket-number-grammar/n 10)
    ,@(racket-number-grammar/n  2)
    ,@(racket-number-grammar/n  8)
    ,@(racket-number-grammar/n 16)
    
    ; because the exactness and base can be swapped, we'll handle them here
    ; and therefore NOT use (general-number n)
    (number-2 (: (or "#b#e" "#e#b" "#b#i" "#i#b" "#b") (number 2)))
    (number-8 (: (or "#o#e" "#e#o" "#o#i" "#i#o" "#o") (number 8)))
    (number-16 (: (or "#x#e" "#e#x" "#x#i" "#i#x" "#x") (number 16)))
    (number-10 (: (? (or "#d#e" "#e#d" "#d#i" "#i#d" "#e" "#i" "#d")) (number 10)))
    (number (or number-10 number-2 number-8 number-16))))

(define (number-regexp-for key)
  (with-handlers
      ((exn? (lambda (x) (exn-message x))))
    (string->bytes/utf-8 (grammar->regexp racket-number-grammar key))))

(define racket-NUMBERS-alist
  `((1 . ,(number-regexp-for 'number-16))
    (2 . ,(number-regexp-for 'number-8))
    (3 . ,(number-regexp-for 'number-2))
    (4 . ,(number-regexp-for 'number-10))))

(module+ test
  (number-regexp-for 'number)
  (grammar->regexp racket-number-grammar 'sign)
  )

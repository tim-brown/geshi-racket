#lang racket
;;; racket-keyword-lists.rkt
;;; ------------------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; Provides a list of all defined identifiers - constants, syntaxes and keywords
;;; to the GeSHi language for racket.

(require "geshi-utils.rkt" "tasklist.rkt")
(provide racket-KEYWORDS-alist racket-URLS-alist
         racket-SYMBOL-like-keywords-and-syntaxes)

(todo #<<EOS
URLs should be formed more smartly by discovering which module they came from.
EOS
)

(define (identifiers-from-namespace ns)
  (for*/list
      ((x (in-list (namespace-mapped-symbols ns)))
       (v (in-value (with-handlers ([exn:fail:syntax? (lambda (exn) #f)])
                      (namespace-variable-value x #t #f ns) x))) #:when v) v))

(define (syntaxes-from-namespace ns)
  (for*/list
      ((x (in-list (namespace-mapped-symbols ns)))
       (v (in-value (with-handlers ([exn:fail:syntax? (lambda (exn) x)])
                      (namespace-variable-value x #t #f ns) #f))) #:when v) v))

;;; time for some introspection!
(define-syntax (namespace-require* stx)
  (syntax-case stx ()
    [(_ rq ...)
     #'(begin (namespace-require (quote rq)) ...)]))

(define base-ns (make-base-namespace))
(define racket-ns (make-base-empty-namespace))
(parameterize ((current-namespace racket-ns))
  (namespace-require*
   #;racket/base ;; these identifiers will already be recognised from base-ns
   racket/bool racket/bytes racket/class racket/cmdline racket/contract racket/dict
   racket/file racket/format racket/function racket/future racket/include racket/list
   racket/local racket/match racket/math racket/path racket/place racket/port
   racket/pretty racket/promise racket/sequence racket/set racket/shared racket/stream
   racket/string racket/system racket/tcp racket/udp racket/unit racket/vector))

(define base-identifiers (identifiers-from-namespace base-ns))
(define base-syntaxes    (syntaxes-from-namespace base-ns))
(define racket-identifiers (identifiers-from-namespace racket-ns))
(define racket-syntaxes    (syntaxes-from-namespace racket-ns))

(define (identifiers->KEYWORDS-entry ids)
  (sort (map symbol->geshi-identifier ids) string<?))

(define racket-SYMBOL-like-keywords-and-syntaxes-raw
 (filter
  (lambda (s)
   (not (for/first ((c (in-string (symbol->string s)))
                    #:when (or (char-alphabetic? c)
                               (char-numeric? c)))
                   c)))
  (append base-identifiers base-syntaxes racket-identifiers racket-syntaxes)))

(define racket-SYMBOL-like-keywords-and-syntaxes
 (identifiers->KEYWORDS-entry racket-SYMBOL-like-keywords-and-syntaxes-raw))

(define r-S-l-k-s# (for/hash ((s racket-SYMBOL-like-keywords-and-syntaxes-raw))
                    (values s #t)))

(define (remove-SYMBOL-like list-of-symbols)
 (filter (lambda (s) (not (hash-ref r-S-l-k-s# s #f))) list-of-symbols))

(define keywords-1 (identifiers->KEYWORDS-entry
                    (remove-SYMBOL-like base-identifiers)))
(define keywords-2 (identifiers->KEYWORDS-entry 
                    (remove-SYMBOL-like base-syntaxes)))
(define keywords-3 (identifiers->KEYWORDS-entry 
                    (remove-SYMBOL-like racket-identifiers)))
(define keywords-4 (identifiers->KEYWORDS-entry 
                    (remove-SYMBOL-like racket-syntaxes)))

(define racket-KEYWORDS-alist
  `((1 . ,keywords-1) 
    (2 . ,keywords-2)
    (3 . ,keywords-3)
    (4 . ,keywords-4)))

(define racket-URLS-alist
  `((1 . "http://docs.racket-lang.org/reference/") 
    (2 . "http://docs.racket-lang.org/reference/")
    (3 . "http://docs.racket-lang.org/reference/")
    (4 . "http://docs.racket-lang.org/reference/")))

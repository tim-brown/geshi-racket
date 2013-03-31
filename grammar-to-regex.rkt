#lang racket
;;; racket-number-grammar.rkt
;;; -------------------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; This module defines and uses a grammar to produces regular expressions to match
;;; sentences in the grammar. A sample grammar is the one in racket-number-grammar.rkt

(provide grammar->regexp)
(require "tasklist.rkt")

(todo "most compound regexps are possibly over-bracketed: (or ...)")
(todo "most compound regexps are possibly over-bracketed: (: ...)")              

(define (grammar->regexp G start-token)
  (define sub-G->re
    (match-lambda
      [(? string? s)
       ;; we don't regexp-quote this because with the PHP and other layers we may need finer control over rendering
       s #;(regexp-quote s)]
      
      [`(or ,a) (sub-G->re a)]
      [`(or ,a ...)
       (format "(~a)" (string-join (map (lambda (x) (format "(~a)" (sub-G->re x))) a) "|"))]
      
      [`(range ,(? string? a) ...)
       (string-append "[" (apply string-append a) "]")]
      
      [`(: ,a ...)
       (format "(~a)"
               (string-join
                (map (lambda (x) (format "~a" (sub-G->re x))) a)
                ""))]
      [`(? ,a) (format "(~a)?" (sub-G->re a))]
      [`(+ ,a) (format "(~a)+" (sub-G->re a))]
      [`(* ,a) (format "(~a)*" (sub-G->re a))]
      [`(re ,r) r]
      [(and x (app (lambda (k) (assoc k G)) (list _ v)))
       (sub-G->re v)]))
  
  (sub-G->re start-token))
#lang racket
;;; tasklist.rkt
;;; ------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; Provides a single point of entry for TODO listing in this package.
;;; Dead simple, really!
(provide todo todo-list)
(define todos null) ; literally, an empty list
(define (todo str) (set! todos (append todos (list str))))
(define (todo-list) todos)
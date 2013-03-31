#lang scribble/text
@(begin
;;; racket.php.rkt
;;; --------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; Run this to output your racket geshi grammar!
(require "geshi-template.rkt" (prefix-in racket- "racket-geshi.rkt"))
@(render-geshi-template racket-geshi-parameters racket-geshi-meta)
(flush-output))

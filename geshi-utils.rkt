#lang racket
;;; geshi-utils.rkt
;;; ---------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; Provides (probably) language independent utilities for generating
;;; GeSHi source code. The star is parameters->language which takes a
;;; data structure looking a bit like a recursive alist and produces a
;;; PHP array of arrays from it.

(require net/cgi)
(require "tasklist.rkt")

(provide parameters->language
         symbol->geshi-identifier)

(todo "better handling of empty and short arrays")
(todo "care more about indentation and line lengths")

(define (split-by-width str width)
  (regexp-match* (pregexp (format "(.{1,~a}$|.{,~a}[^\\\\])" width width)) str))

(define (render-list-contents vs indent (max-width 80) (sep ", "))
  (define indent-len (string-length indent))
  (define content-len (max (- max-width indent-len) 20))
  (define sep-len (string-length sep))
  
  (define (inner words remaining rendition beginning-of-line?)
    (if (null? words)
        rendition
        (let* ((tl (cdr words))
               (hd (car words))
               (hd-len (string-length hd)))
          (cond
            [(and beginning-of-line? (> (+ hd-len sep-len) remaining))
             (inner tl content-len
                    (string-append rendition indent
                                   (string-join (split-by-width hd (- content-len 3))
                                                (string-append "'.\n" indent "'"))
                                   sep "\n") #t)]
            [(> (+ hd-len sep-len) remaining)
             (inner words max-width (string-append rendition "\n") #t)]
            
            [beginning-of-line?
              (inner tl (- remaining indent-len hd-len sep-len)
                     (string-append rendition indent hd sep) #f)]
            [else
             (inner tl (- remaining hd-len sep-len)
                    (string-append rendition hd sep) #f)]))))
  
  (inner vs max-width "" #f))

(define symbol->geshi-identifier
  (lambda (i) (format "~a" (string->html (symbol->string i)))))

(define (rejoined-string s indents)
  (string-append "'" (string-join (split-by-width s 60)
                                  (string-append "'.\n" indents "'"))
                 "'"))

(define (string->php-string s)
  (string-append (regexp-replace* #rx"(['\\\\])" s "\\\\\\1")))
(define (php$ s (indents "")) (rejoined-string (string->php-string s) indents))

(define (parameters->language prms (indent "") (indent+ "    ") (indent-suppressed? #f))
  (let ((I (if indent-suppressed? "" indent)))
    (match prms
      ;; list of lists is an associative array      
      [`((,ks . ,vs) ...)
       (string-append I "array(\n"
                      (apply string-append
                             (map (lambda (k v)
                                    (string-append indent indent+
                                                   (parameters->language k indent indent+ #t) " => "
                                                   (parameters->language v (string-append indent indent+) indent+ #t)
                                                   ",\n")) ks vs))
                      indent indent+ ")")]
      
      ;; any other list is a non-associative array
      [`(,(app (lambda (x) (parameters->language x indent indent+ #f)) vs) ...)
       (string-append I "array(" (render-list-contents vs (string-append indent indent+)) "\n" indent indent+ ")")]
      
      ;; atomic values
      ;;; TODO NEED A REGEX TAG... PHP$ escapes within regexps!
      [(? bytes? (app (lambda (x) (rejoined-string (bytes->string/utf-8 x) (string-append indent indent+))) v)) v]
      [(? string? (app (lambda (x) (php$ x (string-append indent indent+))) v)) v]
      [(or (? symbol? (app symbol->string v)) (? number? (app number->string v))) v]
      
      ;; indicate unhandledment
      [x (string-append indent (format "/* <<~a>> */\n" x))])))

(module+ test
  (render-list-contents '("1" "2" "3") "...")
  (parameters->language '( ("NUMBERS" .          ((1 . "(((#x#e)|(#e#x)|(#x#i)|(#i#x)|(#x))((((((((((((-)|(\\+)))?(((((([0-9])+)?(\\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\\.)?(#)*))|(((([0-9a-fA-F])+(#)*)\\\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\\+)))?([0-9])+)))?)))|((((-)|(\\+))(((inf\\.)|(nan\\.))[0f])))))?((-)|(\\+))(((((((([0-9])+)?(\\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\\.)?(#)*))|(((([0-9a-fA-F])+(#)*)\\\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\\+)))?([0-9])+)))?))|((((inf\\.)|(nan\\.))[0f])))i))|((((((((-)|(\\+)))?(((((([0-9])+)?(\\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\\.)?(#)*))|(((([0-9a-fA-F])+(#)*)\\\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\\+)))?([0-9])+)))?)))|((((-)|(\\+))(((inf\\.)|(nan\\.))[0f]))))@((((((-)|(\\+)))?(((((([0-9])+)?(\\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\\.)?(#)*))|(((([0-9a-fA-F])+(#)*)\\\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\\+)))?([0-9])+)))?)))|((((-)|(\\+))(((inf\\.)|(nan\\.))[0f]))))))))|((((((-)|(\\+)))?(([0-9])+\\\\/([0-9])+))((-)|(\\+))(([0-9])+\\\\/([0-9])+)i))|(((((-)|(\\+)))?(([0-9])+\\\\/([0-9])+)))|(((((((-)|(\\+)))?(((((([0-9])+)?(\\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\\.)?(#)*))|(((([0-9a-fA-F])+(#)*)\\\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\\+)))?([0-9])+)))?)))|((((-)|(\\+))(((inf\\.)|(nan\\.))[0f])))))|(((((-)|(\\+)))?([0-9])+))))")))))
  
  (regexp-replace* #rx"(['\\\\])" "\\" "\\\\\\1")
  (regexp-match* (pregexp (format "(.{1,~a}$|.{,~a}[^\\\\])" 20 20)) "\\\\")
  (php$ "\\" "")
  (parameters->language "\\")
  (parameters->language '(("ESCAPE_CHAR" .    "\\")))
  
  (parameters->language '((foo . ("1" "2" "3"))) ">>>")
  (parameters->language '(re "abnmdshjkshjfskfskjhsdkjfhsdkjfhskjfhskjfhksjhfsdkjfhsdkfjhskdjfhskjfshhdskfhdksfhdksjhfdksjhfkdsjhkdsjhfkdsjhfdksh") ">>>")
  )

; vim: colorcolumn=121
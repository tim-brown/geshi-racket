#lang scribble/text
@(begin
;                                                                  
;                                                                  
;                                                                  
;     ;;;   ;;;;;;   ;;;;;    ;;;   ;       ;;;;;;;;;;;;;;  ;;;;;;;
;    ;   ;  ;     ; ;;    ;  ;   ;  ;       ;         ;     ;      
;   ;     ; ;     ; ;       ;     ; ;       ;         ;     ;      
;   ;     ; ;     ; ;;      ;     ; ;       ;         ;     ;      
;   ;     ; ;;;;;;   ;;;;;  ;     ; ;       ;;;;;;;   ;     ;;;;;;;
;   ;     ; ;     ;      ;; ;     ; ;       ;         ;     ;      
;   ;     ; ;     ;       ; ;     ; ;       ;         ;     ;      
;    ;   ;  ;     ; ;    ;;  ;   ;  ;       ;         ;     ;      
;     ;;;   ;;;;;;   ;;;;;    ;;;   ;;;;;;; ;;;;;;;   ;     ;;;;;;;
;                                                                  
;                                                                  
;                                                                  
   
     
   ;;; racket-2.scrbl
   ;;; -------------------------
   ;;; Author: Tim Brown <tim@timb.net>
   ;;; From:   https://github.com/tim-brown/geshi-racket
   ;;;
   ;;; Although labelled racket-2, this was my original attempt.
   ;;; It's a bit monolithic, so I refactored it into what you seen now.
   ;;; However -- if you need a reference output THIS ONE WORKS

   ;;; The rackety stuff is done here, so we can get on with the preprocesed text
   ;;; down at the bottom. Just don't put any blank lines between now and the
   ;;; initial <?php...
   (require racket/pretty)
   (require net/cgi)
   (require racket/match)
   
   (define base-ns (make-base-namespace))
   (define racket-ns (make-base-empty-namespace))
   (parameterize ((current-namespace racket-ns))
     ;; these identifiers will already be recognised from base-ns
     #;(namespace-require 'racket/base)
     (namespace-require 'racket/bool)
     (namespace-require 'racket/bytes)
     (namespace-require 'racket/class)
     (namespace-require 'racket/cmdline)
     (namespace-require 'racket/contract)
     (namespace-require 'racket/dict)
     (namespace-require 'racket/file)
     (namespace-require 'racket/format)
     (namespace-require 'racket/function)
     (namespace-require 'racket/future)
     (namespace-require 'racket/include)
     (namespace-require 'racket/list)
     (namespace-require 'racket/local)
     (namespace-require 'racket/match)
     (namespace-require 'racket/math)
     (namespace-require 'racket/path)
     (namespace-require 'racket/place)
     (namespace-require 'racket/port)
     (namespace-require 'racket/pretty)
     (namespace-require 'racket/promise)
     (namespace-require 'racket/sequence)
     (namespace-require 'racket/set)
     (namespace-require 'racket/shared)
     (namespace-require 'racket/stream)
     (namespace-require 'racket/string)
     (namespace-require 'racket/system)
     (namespace-require 'racket/tcp)
     (namespace-require 'racket/udp)
     (namespace-require 'racket/unit)
     (namespace-require 'racket/vector))
   
   (define (split-into-lines words max-line-len sep)
     (define (inr words remaining this-line lines)
       (if (null? words)
           (if (null? this-line) (reverse lines) (reverse (cons (reverse this-line) lines)))
           (let* ((tl (cdr words))
                  (hd (if (null? tl)
                          (car words)
                          (string-append (car words) sep)))
                  (hd-len (string-length hd)))
             (cond
               [(and (> hd-len max-line-len) (null? this-line))
                (inr tl max-line-len null (cons (list hd) lines))]
               [(> hd-len remaining)
                (inr words max-line-len null
                     (cons (reverse this-line) lines))]
               [else (inr tl (- remaining hd-len) (cons hd this-line) lines)]))))    
     (inr words max-line-len null null))
   
   (define (ids->geshi-array ids max-line-len sep)
     (define quoted-ids (map (lambda (i) (format "'~a'" (string->html (symbol->string i)))) ids))
     
     (split-into-lines (sort quoted-ids string<?) max-line-len sep))
   
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
   
   (define base-identifiers (identifiers-from-namespace base-ns))
   (define base-syntaxes    (syntaxes-from-namespace base-ns))
   
   (define racket-identifiers (identifiers-from-namespace racket-ns))
   (define racket-syntaxes    (syntaxes-from-namespace racket-ns))
   
   (define (list-of-ids->array-lines ids)
     (string-join
      (map (lambda (strs)
             (apply string-append strs))
           (ids->geshi-array ids (- 80 12) ", "))
      "\n            "))
   
   (define keywords-1 (list-of-ids->array-lines base-identifiers))
   (define keywords-2 (list-of-ids->array-lines base-syntaxes))
   
   (define keywords-3 (list-of-ids->array-lines racket-syntaxes))
   (define keywords-4 (list-of-ids->array-lines racket-syntaxes))
   
   ; Styles from racket.css: don't end them in ; -- that's user's problem
   (define racket-css-styles
     (hash
      'RktSym     "color: rgb(38, 38, 128)"
      'RktVal     "color: rgb(34, 139, 34)"
      'RktValLink "color: blue"
      'RktPn      "color: rgb(132, 60,36)"
      'RktMod     "color: black"
      'RktStx     "color: rgb(34, 34, 139)"
      'RktStxLink "color: black"
      ))
   
   (define (style key) (hash-ref racket-css-styles key "color: yellow; background-color: black"))
   
   ;;; sort this in the order which will lend to the longest match...
   (define (number-grammar/n n)
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
                                (exact-integer ,n)
                                ))
       
       ((exact-integer ,n)     (: (? sign) (unsigned-integer ,n)))
       ((unsigned-integer ,n)  (+ (re "[0-9]")))
       ((exact-rational ,n)    (: (? sign) (unsigned-rational ,n)))
       ((unsigned-rational ,n) (: (unsigned-integer ,n) "\\/" (unsigned-integer ,n)))
       ((exact-complex ,n)     (: (exact-rational ,n) sign (unsigned-rational ,n) "i"))
       
       ((inexact-real ,n)      (or
                                (: (? sign) (inexact-normal ,n))
                                (: sign inexact-special)
                                ))
       ((inexact-unsigned ,n)  (or (inexact-normal ,n) inexact-special))
       ((inexact-normal ,n)    (: (inexact-simple ,n) (? (: (exp-mark ,n) (exact-integer ,n)))))
       ((inexact-simple ,n)    (or
                                (: (? (unsigned-integer ,n)) (? ".") (digits# ,n))
                                (: (digits# ,n) (? ".") (* "#"))
                                (: (digits# ,n) "\\/" (digits# ,n))))
       ((digits# ,n)           (: (+ (digit ,n)) (* "#")))
       ((inexact-complex ,n)   (or (: (? (inexact-real ,n)) sign (inexact-unsigned ,n) "i")
                                   (: (inexact-real ,n) "@" (inexact-real ,n))))
       ((general-number ,n)    (: exactness (number ,n)))))
   
   (define number-grammar
     `((exactness       (or "#e" "#i"))
       (sign            (or "-" "+"))
       (inexact-special (: (or "inf." "nan.") (range "0f")))
       ((digit 2)     (range "0-1"))
       ((digit 8)     (range "0-7"))
       ((digit 10)    (range "0-9"))
       ((digit 16)    (range "0-9" "a-f" "A-F"))
       ((exp-mark 2)  (exp-mark 10))
       ((exp-mark 8)  (exp-mark 10))
       ((exp-mark 10) (or (exp-mark 16) (range "def")))
       ((exp-mark 16) (range "sl"))
       
       ,@(number-grammar/n 10)
       ,@(number-grammar/n  2)
       ,@(number-grammar/n  8)
       ,@(number-grammar/n 16)
       
       ; because the exactness and base can be swapped, we'll handle them here
       ; and therefore NOT use (general-number n)
       (number-2 (: (or "#b#e" "#e#b" "#b#i" "#i#b" "#b") (number 2)))
       (number-8 (: (or "#o#e" "#e#o" "#o#i" "#i#o" "#o") (number 8)))
       (number-16 (: (or "#x#e" "#e#x" "#x#i" "#i#x" "#x") (number 16)))
       (number-10 (: (? (or "#d#e" "#e#d" "#d#i" "#i#d" "#e" "#i" "#d")) (number 10)))
       (number (or number-10 number-2 number-8 number-16))))
   
   (define (quote-regexp-range-character c)
     (case c
       ((#\\ #\[  #\] #\-) (format "\\\\~a" c))
       (else (format "~a" c))))
   
   (define (grammar->regexp G start-token)
     
     (define sub-G->re
       (match-lambda
         [(? string? s) (regexp-quote s)]
         
         [`(or ,a) (sub-G->re a)]
         [`(or ,a ...)
          (format "(~a)" (string-join (map (lambda (x) (format "(~a)" (sub-G->re x))) a) "|"))]
         
         [`(range ,(? string? a) ...)
          (string-append "[" (apply string-append a) "]")]
         
         [`(: ,a ...) (format "(~a)"
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
   
   (define (regexp-for key)
     (with-handlers ((exn? (lambda (x) (exn-message x))))
       (grammar->regexp number-grammar key)))
   
   (define (nice-regexp-for key)
     (add-between
      (map (lambda (s) (string-append "'" s "'"))
           (regexp-match* #px".{,60}[^\\\\]" (regexp-for key)))
      ".\n"))
   )<?php
    /*************************************************************************************
    * racket.php
    * ----------
    * Author: Tim Brown (tim@"@"timb.net)
    * Copyright: (c) 2013 Tim Brown (https://github.com/tim-brown/geshi-racket)
    * Release Version: 
    * Date Started: 2013-03-01
    *
    * Racket language file for GeSHi.
    *
    * http://www.racket-lang.org
    *
    * CHANGES
    * -------
    *
    * TODO (updated )
    * ---------------
    *   * vector #(a b c) hash needs highlighting
    *   * all   identifiders
    *   * |...| identifiders
    *
    *************************************************************************************
    *
    *     This file is part of GeSHi.
    *
    *   GeSHi is free software; you can redistribute it and/or modify
    *   it under the terms of the GNU General Public License as published by
    *   the Free Software Foundation; either version 2 of the License, or
    *   (at your option) any later version.
    *
    *   GeSHi is distributed in the hope that it will be useful,
    *   but WITHOUT ANY WARRANTY; without even the implied warranty of
    *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    *   GNU General Public License for more details.
    *
    *   You should have received a copy of the GNU General Public License
    *   along with GeSHi; if not, write to the Free Software
    *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    *
    ************************************************************************************/
    
    $language_data = array (
    'LANG_NAME' => 'Racket',
    'COMMENT_SINGLE' => array(1 => ';'),
    'COMMENT_MULTI' => array('#|' => '|#'),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array('"'),
    'ESCAPE_CHAR' => '\\',
    'KEYWORDS' => array(
    1 => array(
    @|keywords-1|
    ),
    2 => array(
    @|keywords-2|
    ),
    3 => array(
    @|keywords-3|
    ),
    4 => array(
    @|keywords-4|
    )
    ),
    'SYMBOLS' => array(
    0 => array(
    '#fl', '#fx', '#s', '#"',
    '#f', '#F', '#false',
    '#t', '#T', '#true',
    '#lang', '#reader',
    
    '.', "'", '#`', '#,@"@"', '#,',
    "#'", '`', ',@"@"', ',',
    
    '#%', '#$', '#&', '#~',
    '#rx', '#px', '#<<', @; TODO: move to STRINGS(?)
    
    '#;', @; TODO: move to COMMENTS(?)
    
    '#hash',
    '#'
    ),
    ),
    'CASE_SENSITIVE' => array(
    GESHI_COMMENTS => false,
    1 => false,
    2 => false,
    3 => false,
    4 => false
    ),
    'NUMBERS' => array(
    1 => @(nice-regexp-for 'number-16),
    2 => @(nice-regexp-for 'number-8),
    3 => @(nice-regexp-for 'number-2),
    4 => @(nice-regexp-for 'number-10),
    ),
    /* STYLES colour scheme taken from racket.css (generated by scribble). Code
    * won't look like GeSHi, it'll look more like racket docs */
    'STYLES' => array(
    'KEYWORDS' => array(
    1 => '@(style 'RktValLink);',
    2 => '@(style 'RktStx);',
    3 => '@(style 'RktValLink);',
    4 => '@(style 'RktStx);'
    ),
    'COMMENTS' => array(
    1 => 'color: #c2741f;',
    'MULTI' => 'color: #c2741f;'
    ),
    'ESCAPE_CHAR' => array(
    0 => ''
    ),
    'BRACKETS' => array(
    0 => '@(style 'RktPn)'
    ),
    'STRINGS' => array(
    0 => '@(style 'RktVal)'
    ),
    'NUMBERS' => array(
    0 => '@(style 'RktVal);',
    1 => '@(style 'RktVal);',
    2 => '@(style 'RktVal);',
    3 => '@(style 'RktVal);',
    4 => '@(style 'RktVal);',
    ),
    'METHODS' => array(
    0 => 'color: #202020;'
    ),
    'SYMBOLS' => array(
    0 => '@(style 'RktPn);',
    ),
    'REGEXPS' => array(
    5 => '@(style 'RktVal);',
    6 => '@(style 'RktPn);',
    @; 7 => '@(style 'RktVal);',
    8 => '@(style 'RktVal);',
    ),
    'SCRIPT' => array(
    )
    ),
    'URLS' => array(
    1 => ''
    ),
    'OOLANG' => false,
    'OBJECT_SPLITTERS' => array(
    ),
    'REGEXPS' => array(
    5 => '#\\\\(nul|null|backspace|tab|newline|linefeed|vtab|page|return|space|rubout'
    .'|([0-7]{1,3})|(u[[:xdigit:]]{1,4})|(U[[:xdigit:]]{1,6})|[a-z])',
    @; /* note that ; is also a delimiter character (a character that stops an
    @; * identifier. However geshi seems to do an html_quote (or whatever)
    @; * before colouring; the ; at the end of &gt; is then recognised as a
    @; * delimiter, syntax colouring is done on "&gt" which becomes "<span
    @; * ...>&gt</span>;" , and a spurious semicolon appears */
    6 => '#:[^[:space:]()[\\]{}\",\']+', // keyword
    @; 7 => '\'\\|[^|]*\|', // symbol -- doesn't work... seems to conflict with GeSHi's use of |'s elsewhere
    8 => '\'((\\\\ )|([^[:space:]()[\\]{}",\']))+', // symbol
    ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(
    ),
    'HIGHLIGHT_STRICT_BLOCK' => array(
    ),
    'PARSER_CONTROL' => array(
    'KEYWORDS' => array(
    'DISALLOWED_BEFORE' => '[[:space:]()[\\]{}\",\']',
    ),
    'ENABLE_FLAGS' => array(
    /*
    'ALL' => GESHI_NEVER,
    'NUMBERS' => GESHI_ALWAYS,
    'METHODS' => GESHI_NEVER,
    'SCRIPT' => GESHI_NEVER,
    'STRINGS' => GESHI_ALWAYS,
    */
    /* Unfortunately, there is an implode('|'...)
    * in the SYMBOLS handling, which screws up
    * quoted '|...| symbols, so they're all moved
    * into a REGEXPS
    */
    'SYMBOLS' => GESHI_MAYBE,
    'BRACKETS' => GESHI_MAYBE,
    'REGEXPS' => GESHI_MAYBE,
    'ESCAPE_CHAR' => GESHI_MAYBE,
    )
    )
    );
    
    ?>
    @; vim: colorcolumn=121
    @(flush-output)
    
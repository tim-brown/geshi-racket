#lang at-exp racket
;;; geshi-template.rkt
;;; -------------------------
;;; Author: Tim Brown <tim@timb.net>
;;; From:   https://github.com/tim-brown/geshi-racket
;;;
;;; The text template that provides the standard headers etc. for a GeSHi highlighting
;;; PHP source.

(require "geshi-utils.rkt" "tasklist.rkt")
(provide render-geshi-template)

(define geshi-copyright
  #<<EOF
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
EOF
  )

(define (render-geshi-template geshi-parameters geshi-meta)
  (let ((M (lambda (key)
             (cond [(assoc key geshi-meta) => cadr]
                   [else (format "[METADATA KEY: ~s MISSING]" key)]))))
    @list{<?php
          /*************************************************************************************
           * racket.php
           * ----------
           * Author:          @(M 'author) @(M 'email)
           * Copyright:   (c) @(M 'copyright-date) @(M 'author) @(M 'url)
           * Release Version: @(M 'release-version)
           * Date Started:    @(M 'date-started)
           *
           * @(M 'language) language file for GeSHi.
           *
           * @(M 'any-comments)
           *
           * CHANGES
           * -------
           *
           * TODO (updated @(M 'release-date))
           * --------------@(make-string (string-length (M 'release-date)) #\-)-
           *   @(map (lambda (td) (format "* ~a~%" td)) (todo-list))*
           *************************************************************************************
          @geshi-copyright 
           ************************************************************************************/
          
          $language_data = @(parameters->language geshi-parameters "" "    ");
          ?>
          }))
; vim: colorcolumn=121
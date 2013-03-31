<?php
/*************************************************************************************
 * racket.php
 * ----------
 * Author: Tim Brown (tim@timb.net)
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
            '&gt;', '&gt;=', '&lt;', '&lt;=', '*', '+', '-', '/', '=', 
            'abort-current-continuation', 'abs', 'absolute-path?', 'acos', 
            'add1', 'alarm-evt', 'always-evt', 'andmap', 'angle', 'append', 
            'arithmetic-shift', 'arity-at-least-value', 'arity-at-least?', 
            'asin', 'assf', 'assoc', 'assq', 'assv', 'atan', 'banner', 
            'bitwise-and', 'bitwise-bit-field', 'bitwise-bit-set?', 
            'bitwise-ior', 'bitwise-not', 'bitwise-xor', 'boolean?', 
            'bound-identifier=?', 'box', 'box-cas!', 'box-immutable', 'box?', 
            'break-enabled', 'break-thread', 'build-list', 'build-path', 
            'build-path/convention-type', 'build-string', 'build-vector', 
            'byte-pregexp', 'byte-pregexp?', 'byte-ready?', 'byte-regexp', 
            'byte-regexp?', 'byte?', 'bytes&gt;?', 'bytes&lt;?', 'bytes', 
            'bytes-&gt;immutable-bytes', 'bytes-&gt;list', 'bytes-&gt;path', 
            'bytes-&gt;path-element', 'bytes-&gt;string/latin-1', 
            'bytes-&gt;string/locale', 'bytes-&gt;string/utf-8', 
            'bytes-append', 'bytes-close-converter', 'bytes-convert', 
            'bytes-convert-end', 'bytes-converter?', 'bytes-copy!', 
            'bytes-copy', 'bytes-fill!', 'bytes-length', 
            'bytes-open-converter', 'bytes-ref', 'bytes-set!', 
            'bytes-utf-8-index', 'bytes-utf-8-length', 'bytes-utf-8-ref', 
            'bytes=?', 'bytes?', 'caaaar', 'caaadr', 'caaar', 'caadar', 
            'caaddr', 'caadr', 'caar', 'cadaar', 'cadadr', 'cadar', 'caddar', 
            'cadddr', 'caddr', 'cadr', 'call-in-nested-thread', 
            'call-with-break-parameterization', 
            'call-with-composable-continuation', 
            'call-with-continuation-barrier', 'call-with-continuation-prompt', 
            'call-with-current-continuation', 'call-with-escape-continuation', 
            'call-with-exception-handler', 
            'call-with-immediate-continuation-mark', 
            'call-with-parameterization', 'call-with-semaphore', 
            'call-with-semaphore/enable-break', 'call-with-values', 'call/cc', 
            'call/ec', 'car', 'cdaaar', 'cdaadr', 'cdaar', 'cdadar', 'cdaddr', 
            'cdadr', 'cdar', 'cddaar', 'cddadr', 'cddar', 'cdddar', 'cddddr', 
            'cdddr', 'cddr', 'cdr', 'ceiling', 'channel-get', 'channel-put', 
            'channel-put-evt', 'channel-put-evt?', 'channel-try-get', 
            'channel?', 'chaperone-box', 'chaperone-continuation-mark-key', 
            'chaperone-evt', 'chaperone-hash', 'chaperone-of?', 
            'chaperone-procedure', 'chaperone-prompt-tag', 'chaperone-struct', 
            'chaperone-struct-type', 'chaperone-vector', 'chaperone?', 
            'char&gt;=?', 'char&gt;?', 'char&lt;=?', 'char&lt;?', 
            'char-&gt;integer', 'char-alphabetic?', 'char-blank?', 
            'char-ci&gt;=?', 'char-ci&gt;?', 'char-ci&lt;=?', 'char-ci&lt;?', 
            'char-ci=?', 'char-downcase', 'char-foldcase', 
            'char-general-category', 'char-graphic?', 'char-iso-control?', 
            'char-lower-case?', 'char-numeric?', 'char-punctuation?', 
            'char-ready?', 'char-symbolic?', 'char-title-case?', 
            'char-titlecase', 'char-upcase', 'char-upper-case?', 
            'char-utf-8-length', 'char-whitespace?', 'char=?', 'char?', 
            'check-duplicate-identifier', 
            'checked-procedure-check-and-extract', 'choice-evt', 
            'cleanse-path', 'close-input-port', 'close-output-port', 
            'collect-garbage', 'collection-file-path', 'collection-path', 
            'compile', 'compile-allow-set!-undefined', 
            'compile-context-preservation-enabled', 
            'compile-enforce-module-constants', 'compile-syntax', 
            'compiled-expression?', 'compiled-module-expression?', 
            'complete-path?', 'complex?', 'compose', 'compose1', 'cons', 
            'continuation-mark-key?', 'continuation-mark-set-&gt;context', 
            'continuation-mark-set-&gt;list', 
            'continuation-mark-set-&gt;list*', 'continuation-mark-set-first', 
            'continuation-mark-set?', 'continuation-marks', 
            'continuation-prompt-available?', 'continuation-prompt-tag?', 
            'continuation?', 'copy-file', 'cos', 
            'current-break-parameterization', 'current-code-inspector', 
            'current-command-line-arguments', 'current-compile', 
            'current-compiled-file-roots', 'current-continuation-marks', 
            'current-custodian', 'current-directory', 'current-drive', 
            'current-error-port', 'current-eval', 
            'current-evt-pseudo-random-generator', 'current-gc-milliseconds', 
            'current-get-interaction-input-port', 
            'current-inexact-milliseconds', 'current-input-port', 
            'current-inspector', 'current-library-collection-paths', 
            'current-load', 'current-load-extension', 
            'current-load-relative-directory', 'current-load/use-compiled', 
            'current-locale', 'current-logger', 'current-memory-use', 
            'current-milliseconds', 'current-module-declare-name', 
            'current-module-declare-source', 'current-module-name-resolver', 
            'current-namespace', 'current-output-port', 
            'current-parameterization', 'current-preserved-thread-cell-values', 
            'current-print', 'current-process-milliseconds', 
            'current-prompt-read', 'current-pseudo-random-generator', 
            'current-read-interaction', 'current-reader-guard', 
            'current-readtable', 'current-seconds', 'current-security-guard', 
            'current-subprocess-custodian-mode', 'current-thread', 
            'current-thread-group', 'current-thread-initial-stack-size', 
            'current-write-relative-directory', 'custodian-box-value', 
            'custodian-box?', 'custodian-limit-memory', 
            'custodian-managed-list', 'custodian-memory-accounting-available?', 
            'custodian-require-memory', 'custodian-shutdown-all', 'custodian?', 
            'custom-print-quotable-accessor', 'custom-print-quotable?', 
            'custom-write-accessor', 'custom-write?', 'date*-nanosecond', 
            'date*-time-zone-name', 'date*?', 'date-day', 'date-dst?', 
            'date-hour', 'date-minute', 'date-month', 'date-second', 
            'date-time-zone-offset', 'date-week-day', 'date-year', 
            'date-year-day', 'date?', 'datum-&gt;syntax', 
            'datum-intern-literal', 'default-continuation-prompt-tag', 
            'delete-directory', 'delete-file', 'denominator', 
            'directory-exists?', 'directory-list', 'display', 'displayln', 
            'double-flonum?', 'dump-memory-stats', 'dynamic-require', 
            'dynamic-require-for-syntax', 'dynamic-wind', 'eof', 'eof-object?', 
            'ephemeron-value', 'ephemeron?', 'eprintf', 'eq-hash-code', 'eq?', 
            'equal-hash-code', 'equal-secondary-hash-code', 'equal?', 
            'equal?/recur', 'eqv-hash-code', 'eqv?', 'error', 
            'error-display-handler', 'error-escape-handler', 
            'error-print-context-length', 'error-print-source-location', 
            'error-print-width', 'error-value-&gt;string-handler', 'eval', 
            'eval-jit-enabled', 'eval-syntax', 'even?', 'evt?', 
            'exact-&gt;inexact', 'exact-integer?', 
            'exact-nonnegative-integer?', 'exact-positive-integer?', 'exact?', 
            'executable-yield-handler', 'exit', 'exit-handler', 
            'exn-continuation-marks', 'exn-message', 'exn:break-continuation', 
            'exn:break:hang-up?', 'exn:break:terminate?', 'exn:break?', 
            'exn:fail:contract:arity?', 'exn:fail:contract:continuation?', 
            'exn:fail:contract:divide-by-zero?', 
            'exn:fail:contract:non-fixnum-result?', 
            'exn:fail:contract:variable-id', 'exn:fail:contract:variable?', 
            'exn:fail:contract?', 'exn:fail:filesystem:errno-errno', 
            'exn:fail:filesystem:errno?', 'exn:fail:filesystem:exists?', 
            'exn:fail:filesystem:version?', 'exn:fail:filesystem?', 
            'exn:fail:network:errno-errno', 'exn:fail:network:errno?', 
            'exn:fail:network?', 'exn:fail:out-of-memory?', 
            'exn:fail:read-srclocs', 'exn:fail:read:eof?', 
            'exn:fail:read:non-char?', 'exn:fail:read?', 
            'exn:fail:syntax-exprs', 'exn:fail:syntax:unbound?', 
            'exn:fail:syntax?', 'exn:fail:unsupported?', 'exn:fail:user?', 
            'exn:fail?', 'exn:srclocs-accessor', 'exn:srclocs?', 'exn?', 'exp', 
            'expand', 'expand-once', 'expand-syntax', 'expand-syntax-once', 
            'expand-syntax-to-top-form', 'expand-to-top-form', 
            'expand-user-path', 'expt', 'file-exists?', 
            'file-or-directory-identity', 'file-or-directory-modify-seconds', 
            'file-or-directory-permissions', 'file-position', 'file-position*', 
            'file-size', 'file-stream-buffer-mode', 'file-stream-port?', 
            'filesystem-root-list', 'filter', 'find-executable-path', 
            'find-library-collection-paths', 'find-system-path', 'findf', 
            'fixnum?', 'floating-point-bytes-&gt;real', 'flonum?', 'floor', 
            'flush-output', 'foldl', 'foldr', 'for-each', 'format', 'fprintf', 
            'free-identifier=?', 'free-label-identifier=?', 
            'free-template-identifier=?', 'free-transformer-identifier=?', 
            'gcd', 'generate-temporaries', 'gensym', 'get-output-bytes', 
            'get-output-string', 'getenv', 'global-port-print-handler', 
            'guard-evt', 'handle-evt', 'handle-evt?', 'hash', 'hash-&gt;list', 
            'hash-copy', 'hash-count', 'hash-eq?', 'hash-equal?', 'hash-eqv?', 
            'hash-for-each', 'hash-has-key?', 'hash-iterate-first', 
            'hash-iterate-key', 'hash-iterate-next', 'hash-iterate-value', 
            'hash-keys', 'hash-map', 'hash-placeholder?', 'hash-ref!', 
            'hash-ref', 'hash-remove!', 'hash-remove', 'hash-set!', 'hash-set', 
            'hash-set*!', 'hash-set*', 'hash-update!', 'hash-update', 
            'hash-values', 'hash-weak?', 'hash?', 'hasheq', 'hasheqv', 
            'identifier-binding', 'identifier-label-binding', 
            'identifier-prune-lexical-context', 
            'identifier-prune-to-source-module', 
            'identifier-remove-from-definition-context', 
            'identifier-template-binding', 'identifier-transformer-binding', 
            'identifier?', 'imag-part', 'immutable?', 'impersonate-box', 
            'impersonate-continuation-mark-key', 'impersonate-hash', 
            'impersonate-procedure', 'impersonate-prompt-tag', 
            'impersonate-struct', 'impersonate-vector', 'impersonator-of?', 
            'impersonator-prop:application-mark', 
            'impersonator-property-accessor-procedure?', 
            'impersonator-property?', 'impersonator?', 'in-cycle', 
            'in-directory', 'in-hash', 'in-hash-keys', 'in-hash-pairs', 
            'in-hash-values', 'in-parallel', 'in-sequences', 
            'in-values*-sequence', 'in-values-sequence', 'inexact-&gt;exact', 
            'inexact-real?', 'inexact?', 'input-port?', 'inspector?', 
            'integer-&gt;char', 'integer-&gt;integer-bytes', 
            'integer-bytes-&gt;integer', 'integer-length', 'integer-sqrt', 
            'integer-sqrt/remainder', 'integer?', 
            'internal-definition-context-seal', 'internal-definition-context?', 
            'keyword&lt;?', 'keyword-&gt;string', 'keyword-apply', 'keyword?', 
            'kill-thread', 'lcm', 'length', 'liberal-define-context?', 
            'link-exists?', 'list', 'list*', 'list-&gt;bytes', 
            'list-&gt;string', 'list-&gt;vector', 'list-ref', 'list-tail', 
            'list?', 'load', 'load-extension', 'load-on-demand-enabled', 
            'load-relative', 'load-relative-extension', 'load/cd', 
            'load/use-compiled', 'local-expand', 'local-expand/capture-lifts', 
            'local-transformer-expand', 
            'local-transformer-expand/capture-lifts', 'locale-string-encoding', 
            'log', 'log-level?', 'log-max-level', 'log-message', 
            'log-receiver?', 'logger-name', 'logger?', 'magnitude', 
            'make-arity-at-least', 'make-base-empty-namespace', 
            'make-base-namespace', 'make-bytes', 'make-channel', 
            'make-continuation-mark-key', 'make-continuation-prompt-tag', 
            'make-custodian', 'make-custodian-box', 'make-date', 'make-date*', 
            'make-derived-parameter', 'make-directory', 'make-do-sequence', 
            'make-empty-namespace', 'make-ephemeron', 'make-exn', 
            'make-exn:break', 'make-exn:break:hang-up', 
            'make-exn:break:terminate', 'make-exn:fail', 
            'make-exn:fail:contract', 'make-exn:fail:contract:arity', 
            'make-exn:fail:contract:continuation', 
            'make-exn:fail:contract:divide-by-zero', 
            'make-exn:fail:contract:non-fixnum-result', 
            'make-exn:fail:contract:variable', 'make-exn:fail:filesystem', 
            'make-exn:fail:filesystem:errno', 
            'make-exn:fail:filesystem:exists', 
            'make-exn:fail:filesystem:version', 'make-exn:fail:network', 
            'make-exn:fail:network:errno', 'make-exn:fail:out-of-memory', 
            'make-exn:fail:read', 'make-exn:fail:read:eof', 
            'make-exn:fail:read:non-char', 'make-exn:fail:syntax', 
            'make-exn:fail:syntax:unbound', 'make-exn:fail:unsupported', 
            'make-exn:fail:user', 'make-file-or-directory-link', 'make-hash', 
            'make-hash-placeholder', 'make-hasheq', 'make-hasheq-placeholder', 
            'make-hasheqv', 'make-hasheqv-placeholder', 'make-immutable-hash', 
            'make-immutable-hasheq', 'make-immutable-hasheqv', 
            'make-impersonator-property', 'make-input-port', 'make-inspector', 
            'make-keyword-procedure', 'make-known-char-range-list', 
            'make-log-receiver', 'make-logger', 'make-output-port', 
            'make-parameter', 'make-phantom-bytes', 'make-pipe', 
            'make-placeholder', 'make-polar', 'make-prefab-struct', 
            'make-pseudo-random-generator', 'make-reader-graph', 
            'make-readtable', 'make-rectangular', 'make-rename-transformer', 
            'make-resolved-module-path', 'make-security-guard', 
            'make-semaphore', 'make-set!-transformer', 'make-shared-bytes', 
            'make-sibling-inspector', 'make-special-comment', 'make-srcloc', 
            'make-string', 'make-struct-field-accessor', 
            'make-struct-field-mutator', 'make-struct-type', 
            'make-struct-type-property', 'make-syntax-delta-introducer', 
            'make-syntax-introducer', 'make-thread-cell', 'make-thread-group', 
            'make-vector', 'make-weak-box', 'make-weak-hash', 
            'make-weak-hasheq', 'make-weak-hasheqv', 'make-will-executor', 
            'map', 'max', 'mcar', 'mcdr', 'mcons', 'member', 'memf', 'memq', 
            'memv', 'min', 'module-&gt;exports', 'module-&gt;imports', 
            'module-&gt;language-info', 'module-&gt;namespace', 
            'module-compiled-exports', 'module-compiled-imports', 
            'module-compiled-language-info', 'module-compiled-name', 
            'module-compiled-submodules', 'module-declared?', 
            'module-path-index-join', 'module-path-index-resolve', 
            'module-path-index-split', 'module-path-index-submodule', 
            'module-path-index?', 'module-path?', 'module-predefined?', 
            'module-provide-protected?', 'modulo', 'mpair?', 'nack-guard-evt', 
            'namespace-anchor-&gt;empty-namespace', 
            'namespace-anchor-&gt;namespace', 'namespace-anchor?', 
            'namespace-attach-module', 'namespace-attach-module-declaration', 
            'namespace-base-phase', 'namespace-mapped-symbols', 
            'namespace-module-identifier', 'namespace-module-registry', 
            'namespace-require', 'namespace-require/constant', 
            'namespace-require/copy', 'namespace-require/expansion-time', 
            'namespace-set-variable-value!', 'namespace-symbol-&gt;identifier', 
            'namespace-syntax-introduce', 'namespace-undefine-variable!', 
            'namespace-unprotect-module', 'namespace-variable-value', 
            'namespace?', 'negative?', 'never-evt', 'newline', 
            'normal-case-path', 'not', 'null', 'null?', 'number-&gt;string', 
            'number?', 'numerator', 'object-name', 'odd?', 'open-input-bytes', 
            'open-input-string', 'open-output-bytes', 'open-output-string', 
            'ormap', 'output-port?', 'pair?', 'parameter-procedure=?', 
            'parameter?', 'parameterization?', 'path-&gt;bytes', 
            'path-&gt;complete-path', 'path-&gt;directory-path', 
            'path-&gt;string', 'path-add-suffix', 'path-convention-type', 
            'path-element-&gt;bytes', 'path-element-&gt;string', 
            'path-for-some-system?', 'path-list-string-&gt;path-list', 
            'path-replace-suffix', 'path-string?', 'path?', 'peek-byte', 
            'peek-byte-or-special', 'peek-bytes!', 'peek-bytes', 
            'peek-bytes-avail!', 'peek-bytes-avail!*', 
            'peek-bytes-avail!/enable-break', 'peek-char', 
            'peek-char-or-special', 'peek-string!', 'peek-string', 
            'phantom-bytes?', 'pipe-content-length', 'placeholder-get', 
            'placeholder-set!', 'placeholder?', 'poll-guard-evt', 
            'port-closed-evt', 'port-closed?', 'port-commit-peeked', 
            'port-count-lines!', 'port-count-lines-enabled', 
            'port-display-handler', 'port-file-identity', 'port-file-unlock', 
            'port-next-location', 'port-print-handler', 'port-progress-evt', 
            'port-provides-progress-evts?', 'port-read-handler', 
            'port-try-file-lock?', 'port-write-handler', 'port-writes-atomic?', 
            'port-writes-special?', 'port?', 'positive?', 
            'prefab-key-&gt;struct-type', 'prefab-key?', 'prefab-struct-key', 
            'pregexp', 'pregexp?', 'primitive-closure?', 
            'primitive-result-arity', 'primitive?', 'print', 
            'print-as-expression', 'print-boolean-long-form', 'print-box', 
            'print-graph', 'print-hash-table', 'print-mpair-curly-braces', 
            'print-pair-curly-braces', 'print-reader-abbreviations', 
            'print-struct', 'print-syntax-width', 'print-unreadable', 
            'print-vector-length', 'printf', 'procedure-&gt;method', 
            'procedure-arity', 'procedure-arity-includes?', 'procedure-arity?', 
            'procedure-closure-contents-eq?', 'procedure-extract-target', 
            'procedure-keywords', 'procedure-reduce-arity', 
            'procedure-reduce-keyword-arity', 'procedure-rename', 
            'procedure-struct-type?', 'procedure?', 'progress-evt?', 
            'prop:arity-string', 'prop:checked-procedure', 
            'prop:custom-print-quotable', 'prop:custom-write', 
            'prop:equal+hash', 'prop:evt', 'prop:exn:srclocs', 
            'prop:impersonator-of', 'prop:input-port', 
            'prop:liberal-define-context', 'prop:output-port', 
            'prop:procedure', 'prop:rename-transformer', 'prop:sequence', 
            'prop:set!-transformer', 'pseudo-random-generator-&gt;vector', 
            'pseudo-random-generator-vector?', 'pseudo-random-generator?', 
            'putenv', 'quotient', 'quotient/remainder', 'raise', 
            'raise-argument-error', 'raise-arguments-error', 
            'raise-arity-error', 'raise-mismatch-error', 'raise-range-error', 
            'raise-result-error', 'raise-syntax-error', 'raise-type-error', 
            'raise-user-error', 'random', 'random-seed', 'rational?', 
            'rationalize', 'read', 'read-accept-bar-quote', 'read-accept-box', 
            'read-accept-compiled', 'read-accept-dot', 'read-accept-graph', 
            'read-accept-infix-dot', 'read-accept-lang', 
            'read-accept-quasiquote', 'read-accept-reader', 'read-byte', 
            'read-byte-or-special', 'read-bytes!', 'read-bytes', 
            'read-bytes-avail!', 'read-bytes-avail!*', 
            'read-bytes-avail!/enable-break', 'read-bytes-line', 
            'read-case-sensitive', 'read-char', 'read-char-or-special', 
            'read-curly-brace-as-paren', 'read-decimal-as-inexact', 
            'read-eval-print-loop', 'read-language', 'read-line', 
            'read-on-demand-source', 'read-square-bracket-as-paren', 
            'read-string!', 'read-string', 'read-syntax', 
            'read-syntax/recursive', 'read/recursive', 'readtable-mapping', 
            'readtable?', 'real-&gt;decimal-string', 'real-&gt;double-flonum', 
            'real-&gt;floating-point-bytes', 'real-&gt;single-flonum', 
            'real-part', 'real?', 'regexp', 'regexp-match', 
            'regexp-match-exact?', 'regexp-match-peek', 
            'regexp-match-peek-immediate', 'regexp-match-peek-positions', 
            'regexp-match-peek-positions-immediate', 
            'regexp-match-peek-positions-immediate/end', 
            'regexp-match-peek-positions/end', 'regexp-match-positions', 
            'regexp-match-positions/end', 'regexp-match/end', 'regexp-match?', 
            'regexp-max-lookbehind', 'regexp-quote', 'regexp-replace', 
            'regexp-replace*', 'regexp-replace-quote', 'regexp-replaces', 
            'regexp-split', 'regexp-try-match', 'regexp?', 'relative-path?', 
            'remainder', 'remove', 'remove*', 'remq', 'remq*', 'remv', 'remv*', 
            'rename-file-or-directory', 'rename-transformer-target', 
            'rename-transformer?', 'reroot-path', 'resolve-path', 
            'resolved-module-path-name', 'resolved-module-path?', 'reverse', 
            'round', 'seconds-&gt;date', 'security-guard?', 
            'semaphore-peek-evt', 'semaphore-peek-evt?', 'semaphore-post', 
            'semaphore-try-wait?', 'semaphore-wait', 
            'semaphore-wait/enable-break', 'semaphore?', 'sequence-&gt;stream', 
            'sequence-generate', 'sequence-generate*', 'sequence?', 
            'set!-transformer-procedure', 'set!-transformer?', 'set-box!', 
            'set-mcar!', 'set-mcdr!', 'set-phantom-bytes!', 
            'set-port-next-location!', 'shared-bytes', 'shell-execute', 
            'simplify-path', 'sin', 'single-flonum?', 'sleep', 
            'special-comment-value', 'special-comment?', 'split-path', 'sqrt', 
            'srcloc-column', 'srcloc-line', 'srcloc-position', 'srcloc-source', 
            'srcloc-span', 'srcloc?', 'stop-after', 'stop-before', 
            'string&gt;=?', 'string&gt;?', 'string&lt;=?', 'string&lt;?', 
            'string', 'string-&gt;bytes/latin-1', 'string-&gt;bytes/locale', 
            'string-&gt;bytes/utf-8', 'string-&gt;immutable-string', 
            'string-&gt;keyword', 'string-&gt;list', 'string-&gt;number', 
            'string-&gt;path', 'string-&gt;path-element', 'string-&gt;symbol', 
            'string-&gt;uninterned-symbol', 'string-&gt;unreadable-symbol', 
            'string-append', 'string-ci&gt;=?', 'string-ci&gt;?', 
            'string-ci&lt;=?', 'string-ci&lt;?', 'string-ci=?', 'string-copy!', 
            'string-copy', 'string-downcase', 'string-fill!', 
            'string-foldcase', 'string-length', 'string-locale&gt;?', 
            'string-locale&lt;?', 'string-locale-ci&gt;?', 
            'string-locale-ci&lt;?', 'string-locale-ci=?', 
            'string-locale-downcase', 'string-locale-upcase', 
            'string-locale=?', 'string-normalize-nfc', 'string-normalize-nfd', 
            'string-normalize-nfkc', 'string-normalize-nfkd', 'string-ref', 
            'string-set!', 'string-titlecase', 'string-upcase', 
            'string-utf-8-length', 'string=?', 'string?', 'struct-&gt;vector', 
            'struct-accessor-procedure?', 'struct-constructor-procedure?', 
            'struct-info', 'struct-mutator-procedure?', 
            'struct-predicate-procedure?', 'struct-type-info', 
            'struct-type-make-constructor', 'struct-type-make-predicate', 
            'struct-type-property-accessor-procedure?', 
            'struct-type-property?', 'struct-type?', 'struct:arity-at-least', 
            'struct:date', 'struct:date*', 'struct:exn', 'struct:exn:break', 
            'struct:exn:break:hang-up', 'struct:exn:break:terminate', 
            'struct:exn:fail', 'struct:exn:fail:contract', 
            'struct:exn:fail:contract:arity', 
            'struct:exn:fail:contract:continuation', 
            'struct:exn:fail:contract:divide-by-zero', 
            'struct:exn:fail:contract:non-fixnum-result', 
            'struct:exn:fail:contract:variable', 'struct:exn:fail:filesystem', 
            'struct:exn:fail:filesystem:errno', 
            'struct:exn:fail:filesystem:exists', 
            'struct:exn:fail:filesystem:version', 'struct:exn:fail:network', 
            'struct:exn:fail:network:errno', 'struct:exn:fail:out-of-memory', 
            'struct:exn:fail:read', 'struct:exn:fail:read:eof', 
            'struct:exn:fail:read:non-char', 'struct:exn:fail:syntax', 
            'struct:exn:fail:syntax:unbound', 'struct:exn:fail:unsupported', 
            'struct:exn:fail:user', 'struct:srcloc', 'struct?', 'sub1', 
            'subbytes', 'subprocess', 'subprocess-group-enabled', 
            'subprocess-kill', 'subprocess-pid', 'subprocess-status', 
            'subprocess-wait', 'subprocess?', 'substring', 'symbol-&gt;string', 
            'symbol-interned?', 'symbol-unreadable?', 'symbol?', 'sync', 
            'sync/enable-break', 'sync/timeout', 'sync/timeout/enable-break', 
            'syntax-&gt;datum', 'syntax-&gt;list', 'syntax-arm', 
            'syntax-column', 'syntax-disarm', 'syntax-e', 'syntax-line', 
            'syntax-local-bind-syntaxes', 'syntax-local-certifier', 
            'syntax-local-context', 'syntax-local-expand-expression', 
            'syntax-local-get-shadower', 'syntax-local-introduce', 
            'syntax-local-lift-context', 'syntax-local-lift-expression', 
            'syntax-local-lift-module-end-declaration', 
            'syntax-local-lift-provide', 'syntax-local-lift-require', 
            'syntax-local-lift-values-expression', 
            'syntax-local-make-definition-context', 
            'syntax-local-make-delta-introducer', 
            'syntax-local-module-defined-identifiers', 
            'syntax-local-module-exports', 
            'syntax-local-module-required-identifiers', 'syntax-local-name', 
            'syntax-local-phase-level', 'syntax-local-submodules', 
            'syntax-local-transforming-module-provides?', 'syntax-local-value', 
            'syntax-local-value/immediate', 'syntax-original?', 
            'syntax-position', 'syntax-property', 
            'syntax-property-symbol-keys', 'syntax-protect', 'syntax-rearm', 
            'syntax-recertify', 'syntax-shift-phase-level', 'syntax-source', 
            'syntax-source-module', 'syntax-span', 'syntax-taint', 
            'syntax-tainted?', 'syntax-track-origin', 
            'syntax-transforming-module-expression?', 'syntax-transforming?', 
            'syntax?', 'system-big-endian?', 'system-idle-evt', 
            'system-language+country', 'system-library-subpath', 
            'system-path-convention-type', 'system-type', 'tan', 
            'terminal-port?', 'thread', 'thread-cell-ref', 'thread-cell-set!', 
            'thread-cell-values?', 'thread-cell?', 'thread-dead-evt', 
            'thread-dead?', 'thread-group?', 'thread-receive', 
            'thread-receive-evt', 'thread-resume', 'thread-resume-evt', 
            'thread-rewind-receive', 'thread-running?', 'thread-send', 
            'thread-suspend', 'thread-suspend-evt', 'thread-try-receive', 
            'thread-wait', 'thread/suspend-to-kill', 'thread?', 'time-apply', 
            'truncate', 'unbox', 'uncaught-exception-handler', 
            'use-collection-link-paths', 'use-compiled-file-paths', 
            'use-user-specific-search-paths', 'values', 
            'variable-reference-&gt;empty-namespace', 
            'variable-reference-&gt;module-base-phase', 
            'variable-reference-&gt;module-declaration-inspector', 
            'variable-reference-&gt;module-path-index', 
            'variable-reference-&gt;module-source', 
            'variable-reference-&gt;namespace', 'variable-reference-&gt;phase', 
            'variable-reference-&gt;resolved-module-path', 
            'variable-reference-constant?', 'variable-reference?', 'vector', 
            'vector-&gt;immutable-vector', 'vector-&gt;list', 
            'vector-&gt;pseudo-random-generator!', 
            'vector-&gt;pseudo-random-generator', 'vector-&gt;values', 
            'vector-copy!', 'vector-fill!', 'vector-immutable', 
            'vector-length', 'vector-ref', 'vector-set!', 
            'vector-set-performance-stats!', 'vector?', 'version', 'void', 
            'void?', 'weak-box-value', 'weak-box?', 'will-execute', 
            'will-executor?', 'will-register', 'will-try-execute', 'wrap-evt', 
            'write', 'write-byte', 'write-bytes', 'write-bytes-avail', 
            'write-bytes-avail*', 'write-bytes-avail-evt', 
            'write-bytes-avail/enable-break', 'write-char', 'write-special', 
            'write-special-avail*', 'write-special-evt', 'write-string', 'zero?'
            ),
        2 => array(
            '#%app', '#%datum', '#%expression', '#%module-begin', 
            '#%plain-app', '#%plain-lambda', '#%plain-module-begin', 
            '#%provide', '#%require', '#%stratified-body', '#%top', 
            '#%top-interaction', '#%variable-reference', '...', ':do-in', 
            '=&gt;', '_', 'all-defined-out', 'all-from-out', 'and', 'apply', 
            'arity-at-least', 'begin', 'begin-for-syntax', 'begin0', 
            'call-with-input-file', 'call-with-input-file*', 
            'call-with-output-file', 'call-with-output-file*', 'case', 
            'case-lambda', 'combine-in', 'combine-out', 'cond', 'date', 
            'date*', 'define', 'define-for-syntax', 'define-logger', 
            'define-namespace-anchor', 'define-sequence-syntax', 
            'define-struct', 'define-struct/derived', 'define-syntax', 
            'define-syntax-rule', 'define-syntaxes', 'define-values', 
            'define-values-for-syntax', 'do', 'else', 'except-in', 
            'except-out', 'exn', 'exn:break', 'exn:break:hang-up', 
            'exn:break:terminate', 'exn:fail', 'exn:fail:contract', 
            'exn:fail:contract:arity', 'exn:fail:contract:continuation', 
            'exn:fail:contract:divide-by-zero', 
            'exn:fail:contract:non-fixnum-result', 
            'exn:fail:contract:variable', 'exn:fail:filesystem', 
            'exn:fail:filesystem:errno', 'exn:fail:filesystem:exists', 
            'exn:fail:filesystem:version', 'exn:fail:network', 
            'exn:fail:network:errno', 'exn:fail:out-of-memory', 
            'exn:fail:read', 'exn:fail:read:eof', 'exn:fail:read:non-char', 
            'exn:fail:syntax', 'exn:fail:syntax:unbound', 
            'exn:fail:unsupported', 'exn:fail:user', 'file', 'for', 'for*', 
            'for*/and', 'for*/first', 'for*/fold', 'for*/fold/derived', 
            'for*/hash', 'for*/hasheq', 'for*/hasheqv', 'for*/last', 
            'for*/list', 'for*/lists', 'for*/or', 'for*/product', 'for*/sum', 
            'for*/vector', 'for-label', 'for-meta', 'for-syntax', 
            'for-template', 'for/and', 'for/first', 'for/fold', 
            'for/fold/derived', 'for/hash', 'for/hasheq', 'for/hasheqv', 
            'for/last', 'for/list', 'for/lists', 'for/or', 'for/product', 
            'for/sum', 'for/vector', 'gen:custom-write', 'gen:equal+hash', 
            'if', 'in-bytes', 'in-bytes-lines', 'in-indexed', 
            'in-input-port-bytes', 'in-input-port-chars', 'in-lines', 
            'in-list', 'in-mlist', 'in-naturals', 'in-port', 'in-producer', 
            'in-range', 'in-string', 'in-value', 'in-vector', 'lambda', 'let', 
            'let*', 'let*-values', 'let-syntax', 'let-syntaxes', 'let-values', 
            'let/cc', 'let/ec', 'letrec', 'letrec-syntax', 'letrec-syntaxes', 
            'letrec-syntaxes+values', 'letrec-values', 'lib', 'local-require', 
            'log-debug', 'log-error', 'log-fatal', 'log-info', 'log-warning', 
            'module', 'module*', 'module+', 'only-in', 'only-meta-in', 
            'open-input-file', 'open-input-output-file', 'open-output-file', 
            'or', 'parameterize', 'parameterize*', 'parameterize-break', 
            'planet', 'prefix-in', 'prefix-out', 'protect-out', 'provide', 
            'quasiquote', 'quasisyntax', 'quasisyntax/loc', 'quote', 
            'quote-syntax', 'quote-syntax/prune', 'regexp-match*', 
            'regexp-match-peek-positions*', 'regexp-match-positions*', 
            'relative-in', 'rename-in', 'rename-out', 'require', 'set!', 
            'set!-values', 'sort', 'srcloc', 'struct', 'struct-copy', 
            'struct-field-index', 'struct-out', 'submod', 'syntax', 
            'syntax-case', 'syntax-case*', 'syntax-id-rules', 'syntax-rules', 
            'syntax/loc', 'time', 'unless', 'unquote', 'unquote-splicing', 
            'unsyntax', 'unsyntax-splicing', 'when', 'with-continuation-mark', 
            'with-handlers', 'with-handlers*', 'with-input-from-file', 
            'with-output-to-file', 'with-syntax', 'λ'
            ),
        3 => array(
            '&gt;=/c', '&lt;=/c', '-&gt;', '-&gt;*', '-&gt;*m', '-&gt;d', 
            '-&gt;dm', '-&gt;i', '-&gt;m', '=/c', '==', 'absent', 'abstract', 
            'add-between', 'and/c', 'any', 'any/c', 'augment', 'augment*', 
            'augment-final', 'augment-final*', 'augride', 'augride*', 
            'between/c', 'blame-add-context', 'box-immutable/c', 'box/c', 
            'call-with-file-lock/timeout', 'case-&gt;', 'case-&gt;m', 'class', 
            'class*', 'class-field-accessor', 'class-field-mutator', 'class/c', 
            'class/derived', 'command-line', 'compound-unit', 
            'compound-unit/infer', 'cons/c', 'continuation-mark-key/c', 
            'contract', 'contract-out', 'contract-struct', 'contracted', 
            'current-contract-region', 'define-compound-unit', 
            'define-compound-unit/infer', 'define-contract-struct', 
            'define-local-member-name', 'define-match-expander', 
            'define-member-name', 'define-opt/c', 'define-serializable-class', 
            'define-serializable-class*', 'define-signature', 
            'define-signature-form', 'define-struct/contract', 'define-unit', 
            'define-unit-binding', 'define-unit-from-context', 
            'define-unit/contract', 'define-unit/new-import-export', 
            'define-unit/s', 'define-values-for-export', 
            'define-values/invoke-unit', 'define-values/invoke-unit/infer', 
            'define/augment', 'define/augment-final', 'define/augride', 
            'define/contract', 'define/final-prop', 'define/match', 
            'define/overment', 'define/override', 'define/override-final', 
            'define/private', 'define/public', 'define/public-final', 
            'define/pubment', 'define/subexpression-pos-prop', 'delay', 
            'delay/idle', 'delay/name', 'delay/strict', 'delay/sync', 
            'delay/thread', 'dict-&gt;list', 'dict-can-functional-set?', 
            'dict-can-remove-keys?', 'dict-count', 'dict-for-each', 
            'dict-has-key?', 'dict-iterate-first', 'dict-iterate-key', 
            'dict-iterate-next', 'dict-iterate-value', 'dict-keys', 'dict-map', 
            'dict-mutable?', 'dict-ref!', 'dict-ref', 'dict-remove!', 
            'dict-remove', 'dict-set!', 'dict-set', 'dict-set*!', 'dict-set*', 
            'dict-update!', 'dict-update', 'dict-values', 'dict?', 
            'display-lines', 'display-lines-to-file', 'display-to-file', 
            'dynamic-place', 'dynamic-place*', 'eof-evt', 'except', 
            'exn:fail:contract:blame', 'exn:fail:object', 'export', 'extends', 
            'field', 'field-bound?', 'file-&gt;bytes', 'file-&gt;bytes-lines', 
            'file-&gt;lines', 'file-&gt;list', 'file-&gt;string', 
            'file-&gt;value', 'find-relative-path', 'flat-murec-contract', 
            'flat-rec-contract', 'for*/set', 'for*/seteq', 'for*/seteqv', 
            'for/set', 'for/seteq', 'for/seteqv', 'gen:dict', 'gen:stream', 
            'generic', 'get-field', 'get-preference', 'hash/c', 'implies', 
            'import', 'in-set', 'in-stream', 'include', 
            'include-at/relative-to', 'include-at/relative-to/reader', 
            'include/reader', 'inherit', 'inherit-field', 'inherit/inner', 
            'inherit/super', 'init', 'init-depend', 'init-field', 'init-rest', 
            'inner', 'inspect', 'instantiate', 'integer-in', 'interface', 
            'interface*', 'invoke-unit', 'invoke-unit/infer', 'lazy', 'link', 
            'list/c', 'listof', 'local', 'make-handle-get-preference-locked', 
            'make-object', 'make-temporary-file', 'match', 'match*', 
            'match*/derived', 'match-define', 'match-define-values', 
            'match-lambda', 'match-lambda*', 'match-lambda**', 'match-let', 
            'match-let*', 'match-let*-values', 'match-let-values', 
            'match-letrec', 'match/derived', 'match/values', 'member-name-key', 
            'method-contract?', 'mixin', 'nand', 'new', 'non-empty-listof', 
            'none/c', 'nor', 'not/c', 'object-contract', 'object/c', 
            'one-of/c', 'only', 'open', 'opt/c', 'or/c', 'overment', 
            'overment*', 'override', 'override*', 'override-final', 
            'override-final*', 'parameter/c', 'parametric-&gt;/c', 
            'peek-bytes!-evt', 'peek-bytes-avail!-evt', 'peek-bytes-evt', 
            'peek-string!-evt', 'peek-string-evt', 'peeking-input-port', 
            'place', 'place*', 'port-&gt;bytes-lines', 'port-&gt;lines', 
            'prefix', 'private', 'private*', 'procedure-arity-includes/c', 
            'promise/c', 'prompt-tag/c', 'prop:dict/contract', 
            'provide-signature-elements', 'provide/contract', 'public', 
            'public*', 'public-final', 'public-final*', 'pubment', 'pubment*', 
            'read-bytes!-evt', 'read-bytes-avail!-evt', 'read-bytes-evt', 
            'read-bytes-line-evt', 'read-line-evt', 'read-string!-evt', 
            'read-string-evt', 'real-in', 'recursive-contract', 
            'regexp-match-evt', 'remove-duplicates', 'rename', 'rename-inner', 
            'rename-super', 'send', 'send*', 'send+', 'send-generic', 
            'send/apply', 'send/keyword-apply', 'set-field!', 'shared', 
            'stream', 'stream-cons', 'string-join', 'string-len/c', 
            'string-normalize-spaces', 'string-replace', 'string-split', 
            'string-trim', 'struct*', 'struct/c', 'struct/ctc', 'struct/dc', 
            'super', 'super-instantiate', 'super-make-object', 'super-new', 
            'symbols', 'syntax/c', 'tag', 'this%', 'this', 'thunk', 'thunk*', 
            'unconstrained-domain-&gt;', 'unit', 'unit-from-context', 'unit/c', 
            'unit/new-import-export', 'unit/s', 'vector-immutable/c', 
            'vector-immutableof', 'vector/c', 'vectorof', 'with-contract', 
            'with-method', 'write-to-file', '~.a', '~.s', '~.v', '~a', '~e', 
            '~r', '~s', '~v'
            ),
        4 => array(
            '&gt;=/c', '&lt;=/c', '-&gt;', '-&gt;*', '-&gt;*m', '-&gt;d', 
            '-&gt;dm', '-&gt;i', '-&gt;m', '=/c', '==', 'absent', 'abstract', 
            'add-between', 'and/c', 'any', 'any/c', 'augment', 'augment*', 
            'augment-final', 'augment-final*', 'augride', 'augride*', 
            'between/c', 'blame-add-context', 'box-immutable/c', 'box/c', 
            'call-with-file-lock/timeout', 'case-&gt;', 'case-&gt;m', 'class', 
            'class*', 'class-field-accessor', 'class-field-mutator', 'class/c', 
            'class/derived', 'command-line', 'compound-unit', 
            'compound-unit/infer', 'cons/c', 'continuation-mark-key/c', 
            'contract', 'contract-out', 'contract-struct', 'contracted', 
            'current-contract-region', 'define-compound-unit', 
            'define-compound-unit/infer', 'define-contract-struct', 
            'define-local-member-name', 'define-match-expander', 
            'define-member-name', 'define-opt/c', 'define-serializable-class', 
            'define-serializable-class*', 'define-signature', 
            'define-signature-form', 'define-struct/contract', 'define-unit', 
            'define-unit-binding', 'define-unit-from-context', 
            'define-unit/contract', 'define-unit/new-import-export', 
            'define-unit/s', 'define-values-for-export', 
            'define-values/invoke-unit', 'define-values/invoke-unit/infer', 
            'define/augment', 'define/augment-final', 'define/augride', 
            'define/contract', 'define/final-prop', 'define/match', 
            'define/overment', 'define/override', 'define/override-final', 
            'define/private', 'define/public', 'define/public-final', 
            'define/pubment', 'define/subexpression-pos-prop', 'delay', 
            'delay/idle', 'delay/name', 'delay/strict', 'delay/sync', 
            'delay/thread', 'dict-&gt;list', 'dict-can-functional-set?', 
            'dict-can-remove-keys?', 'dict-count', 'dict-for-each', 
            'dict-has-key?', 'dict-iterate-first', 'dict-iterate-key', 
            'dict-iterate-next', 'dict-iterate-value', 'dict-keys', 'dict-map', 
            'dict-mutable?', 'dict-ref!', 'dict-ref', 'dict-remove!', 
            'dict-remove', 'dict-set!', 'dict-set', 'dict-set*!', 'dict-set*', 
            'dict-update!', 'dict-update', 'dict-values', 'dict?', 
            'display-lines', 'display-lines-to-file', 'display-to-file', 
            'dynamic-place', 'dynamic-place*', 'eof-evt', 'except', 
            'exn:fail:contract:blame', 'exn:fail:object', 'export', 'extends', 
            'field', 'field-bound?', 'file-&gt;bytes', 'file-&gt;bytes-lines', 
            'file-&gt;lines', 'file-&gt;list', 'file-&gt;string', 
            'file-&gt;value', 'find-relative-path', 'flat-murec-contract', 
            'flat-rec-contract', 'for*/set', 'for*/seteq', 'for*/seteqv', 
            'for/set', 'for/seteq', 'for/seteqv', 'gen:dict', 'gen:stream', 
            'generic', 'get-field', 'get-preference', 'hash/c', 'implies', 
            'import', 'in-set', 'in-stream', 'include', 
            'include-at/relative-to', 'include-at/relative-to/reader', 
            'include/reader', 'inherit', 'inherit-field', 'inherit/inner', 
            'inherit/super', 'init', 'init-depend', 'init-field', 'init-rest', 
            'inner', 'inspect', 'instantiate', 'integer-in', 'interface', 
            'interface*', 'invoke-unit', 'invoke-unit/infer', 'lazy', 'link', 
            'list/c', 'listof', 'local', 'make-handle-get-preference-locked', 
            'make-object', 'make-temporary-file', 'match', 'match*', 
            'match*/derived', 'match-define', 'match-define-values', 
            'match-lambda', 'match-lambda*', 'match-lambda**', 'match-let', 
            'match-let*', 'match-let*-values', 'match-let-values', 
            'match-letrec', 'match/derived', 'match/values', 'member-name-key', 
            'method-contract?', 'mixin', 'nand', 'new', 'non-empty-listof', 
            'none/c', 'nor', 'not/c', 'object-contract', 'object/c', 
            'one-of/c', 'only', 'open', 'opt/c', 'or/c', 'overment', 
            'overment*', 'override', 'override*', 'override-final', 
            'override-final*', 'parameter/c', 'parametric-&gt;/c', 
            'peek-bytes!-evt', 'peek-bytes-avail!-evt', 'peek-bytes-evt', 
            'peek-string!-evt', 'peek-string-evt', 'peeking-input-port', 
            'place', 'place*', 'port-&gt;bytes-lines', 'port-&gt;lines', 
            'prefix', 'private', 'private*', 'procedure-arity-includes/c', 
            'promise/c', 'prompt-tag/c', 'prop:dict/contract', 
            'provide-signature-elements', 'provide/contract', 'public', 
            'public*', 'public-final', 'public-final*', 'pubment', 'pubment*', 
            'read-bytes!-evt', 'read-bytes-avail!-evt', 'read-bytes-evt', 
            'read-bytes-line-evt', 'read-line-evt', 'read-string!-evt', 
            'read-string-evt', 'real-in', 'recursive-contract', 
            'regexp-match-evt', 'remove-duplicates', 'rename', 'rename-inner', 
            'rename-super', 'send', 'send*', 'send+', 'send-generic', 
            'send/apply', 'send/keyword-apply', 'set-field!', 'shared', 
            'stream', 'stream-cons', 'string-join', 'string-len/c', 
            'string-normalize-spaces', 'string-replace', 'string-split', 
            'string-trim', 'struct*', 'struct/c', 'struct/ctc', 'struct/dc', 
            'super', 'super-instantiate', 'super-make-object', 'super-new', 
            'symbols', 'syntax/c', 'tag', 'this%', 'this', 'thunk', 'thunk*', 
            'unconstrained-domain-&gt;', 'unit', 'unit-from-context', 'unit/c', 
            'unit/new-import-export', 'unit/s', 'vector-immutable/c', 
            'vector-immutableof', 'vector/c', 'vectorof', 'with-contract', 
            'with-method', 'write-to-file', '~.a', '~.s', '~.v', '~a', '~e', 
            '~r', '~s', '~v'
            )
        ),
    'SYMBOLS' => array(
                       0 => array(
                                  '#fl', '#fx', '#s', '#"',
                                  '#f', '#F', '#false',
                                  '#t', '#T', '#true',
                                  '#lang', '#reader',

                                  '.', "'", '#`', '#,@', '#,',
                                  "#'", '`', ',@', ',',

                                  '#%', '#$', '#&', '#~',
                                  '#rx', '#px', '#<<', 
                                  '#;', 
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
         1 => '(((#x#e)|(#e#x)|(#x#i)|(#i#x)|(#x))((((((((((((-)|(\+)))?(((('.
              '(([0-9])+)?(\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)'.
              '(\.)?(#)*))|(((([0-9a-fA-F])+(#)*)\\/(([0-9a-fA-F])+(#)*))))('.
              '([sl]((((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan'.
              '\.))[0f])))))?((-)|(\+))(((((((([0-9])+)?(\.)?(([0-9a-fA-F])+'.
              '(#)*)))|(((([0-9a-fA-F])+(#)*)(\.)?(#)*))|(((([0-9a-fA-F])+(#'.
              ')*)\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\+)))?([0-9])+)))?'.
              '))|((((inf\.)|(nan\.))[0f])))i))|((((((((-)|(\+)))?(((((([0-9'.
              '])+)?(\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\.)?('.
              '#)*))|(((([0-9a-fA-F])+(#)*)\\/(([0-9a-fA-F])+(#)*))))(([sl]('.
              '(((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0'.
              'f]))))@((((((-)|(\+)))?(((((([0-9])+)?(\.)?(([0-9a-fA-F])+(#)'.
              '*)))|(((([0-9a-fA-F])+(#)*)(\.)?(#)*))|(((([0-9a-fA-F])+(#)*)'.
              '\\/(([0-9a-fA-F])+(#)*))))(([sl]((((-)|(\+)))?([0-9])+)))?)))'.
              '|((((-)|(\+))(((inf\.)|(nan\.))[0f]))))))))|((((((-)|(\+)))?('.
              '([0-9])+\\/([0-9])+))((-)|(\+))(([0-9])+\\/([0-9])+)i))|((((('.
              '-)|(\+)))?(([0-9])+\\/([0-9])+)))|(((((((-)|(\+)))?(((((([0-9'.
              '])+)?(\.)?(([0-9a-fA-F])+(#)*)))|(((([0-9a-fA-F])+(#)*)(\.)?('.
              '#)*))|(((([0-9a-fA-F])+(#)*)\\/(([0-9a-fA-F])+(#)*))))(([sl]('.
              '(((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0'.
              'f])))))|(((((-)|(\+)))?([0-9])+))))',
         2 => '(((#o#e)|(#e#o)|(#o#i)|(#i#o)|(#o))((((((((((((-)|(\+)))?(((('.
              '(([0-9])+)?(\.)?(([0-7])+(#)*)))|(((([0-7])+(#)*)(\.)?(#)*))|'.
              '(((([0-7])+(#)*)\\/(([0-7])+(#)*))))(((([sl])|([def]))((((-)|'.
              '(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0f]))))'.
              ')?((-)|(\+))(((((((([0-9])+)?(\.)?(([0-7])+(#)*)))|(((([0-7])'.
              '+(#)*)(\.)?(#)*))|(((([0-7])+(#)*)\\/(([0-7])+(#)*))))(((([sl'.
              '])|([def]))((((-)|(\+)))?([0-9])+)))?))|((((inf\.)|(nan\.))[0'.
              'f])))i))|((((((((-)|(\+)))?(((((([0-9])+)?(\.)?(([0-7])+(#)*)'.
              '))|(((([0-7])+(#)*)(\.)?(#)*))|(((([0-7])+(#)*)\\/(([0-7])+(#'.
              ')*))))(((([sl])|([def]))((((-)|(\+)))?([0-9])+)))?)))|((((-)|'.
              '(\+))(((inf\.)|(nan\.))[0f]))))@((((((-)|(\+)))?(((((([0-9])+'.
              ')?(\.)?(([0-7])+(#)*)))|(((([0-7])+(#)*)(\.)?(#)*))|(((([0-7]'.
              ')+(#)*)\\/(([0-7])+(#)*))))(((([sl])|([def]))((((-)|(\+)))?(['.
              '0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0f]))))))))|(((('.
              '((-)|(\+)))?(([0-9])+\\/([0-9])+))((-)|(\+))(([0-9])+\\/([0-9'.
              '])+)i))|(((((-)|(\+)))?(([0-9])+\\/([0-9])+)))|(((((((-)|(\+)'.
              '))?(((((([0-9])+)?(\.)?(([0-7])+(#)*)))|(((([0-7])+(#)*)(\.)?'.
              '(#)*))|(((([0-7])+(#)*)\\/(([0-7])+(#)*))))(((([sl])|([def]))'.
              '((((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))['.
              '0f])))))|(((((-)|(\+)))?([0-9])+))))',
         3 => '(((#b#e)|(#e#b)|(#b#i)|(#i#b)|(#b))((((((((((((-)|(\+)))?(((('.
              '(([0-9])+)?(\.)?(([0-1])+(#)*)))|(((([0-1])+(#)*)(\.)?(#)*))|'.
              '(((([0-1])+(#)*)\\/(([0-1])+(#)*))))(((([sl])|([def]))((((-)|'.
              '(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0f]))))'.
              ')?((-)|(\+))(((((((([0-9])+)?(\.)?(([0-1])+(#)*)))|(((([0-1])'.
              '+(#)*)(\.)?(#)*))|(((([0-1])+(#)*)\\/(([0-1])+(#)*))))(((([sl'.
              '])|([def]))((((-)|(\+)))?([0-9])+)))?))|((((inf\.)|(nan\.))[0'.
              'f])))i))|((((((((-)|(\+)))?(((((([0-9])+)?(\.)?(([0-1])+(#)*)'.
              '))|(((([0-1])+(#)*)(\.)?(#)*))|(((([0-1])+(#)*)\\/(([0-1])+(#'.
              ')*))))(((([sl])|([def]))((((-)|(\+)))?([0-9])+)))?)))|((((-)|'.
              '(\+))(((inf\.)|(nan\.))[0f]))))@((((((-)|(\+)))?(((((([0-9])+'.
              ')?(\.)?(([0-1])+(#)*)))|(((([0-1])+(#)*)(\.)?(#)*))|(((([0-1]'.
              ')+(#)*)\\/(([0-1])+(#)*))))(((([sl])|([def]))((((-)|(\+)))?(['.
              '0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0f]))))))))|(((('.
              '((-)|(\+)))?(([0-9])+\\/([0-9])+))((-)|(\+))(([0-9])+\\/([0-9'.
              '])+)i))|(((((-)|(\+)))?(([0-9])+\\/([0-9])+)))|(((((((-)|(\+)'.
              '))?(((((([0-9])+)?(\.)?(([0-1])+(#)*)))|(((([0-1])+(#)*)(\.)?'.
              '(#)*))|(((([0-1])+(#)*)\\/(([0-1])+(#)*))))(((([sl])|([def]))'.
              '((((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))['.
              '0f])))))|(((((-)|(\+)))?([0-9])+))))',
         4 => '((((#d#e)|(#e#d)|(#d#i)|(#i#d)|(#e)|(#i)|(#d)))?((((((((((((-'.
              ')|(\+)))?(((((([0-9])+)?(\.)?(([0-9])+(#)*)))|(((([0-9])+(#)*'.
              ')(\.)?(#)*))|(((([0-9])+(#)*)\\/(([0-9])+(#)*))))(((([sl])|(['.
              'def]))((((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(na'.
              'n\.))[0f])))))?((-)|(\+))(((((((([0-9])+)?(\.)?(([0-9])+(#)*)'.
              '))|(((([0-9])+(#)*)(\.)?(#)*))|(((([0-9])+(#)*)\\/(([0-9])+(#'.
              ')*))))(((([sl])|([def]))((((-)|(\+)))?([0-9])+)))?))|((((inf'.
              '\.)|(nan\.))[0f])))i))|((((((((-)|(\+)))?(((((([0-9])+)?(\.)?'.
              '(([0-9])+(#)*)))|(((([0-9])+(#)*)(\.)?(#)*))|(((([0-9])+(#)*)'.
              '\\/(([0-9])+(#)*))))(((([sl])|([def]))((((-)|(\+)))?([0-9])+)'.
              '))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0f]))))@((((((-)|(\+)))'.
              '?(((((([0-9])+)?(\.)?(([0-9])+(#)*)))|(((([0-9])+(#)*)(\.)?(#'.
              ')*))|(((([0-9])+(#)*)\\/(([0-9])+(#)*))))(((([sl])|([def]))(('.
              '((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((inf\.)|(nan\.))[0f'.
              ']))))))))|((((((-)|(\+)))?(([0-9])+\\/([0-9])+))((-)|(\+))((['.
              '0-9])+\\/([0-9])+)i))|(((((-)|(\+)))?(([0-9])+\\/([0-9])+)))|'.
              '(((((((-)|(\+)))?(((((([0-9])+)?(\.)?(([0-9])+(#)*)))|(((([0-'.
              '9])+(#)*)(\.)?(#)*))|(((([0-9])+(#)*)\\/(([0-9])+(#)*))))(((('.
              '[sl])|([def]))((((-)|(\+)))?([0-9])+)))?)))|((((-)|(\+))(((in'.
              'f\.)|(nan\.))[0f])))))|(((((-)|(\+)))?([0-9])+))))',
         ),
    /* STYLES colour scheme taken from racket.css (generated by scribble). Code
     * won't look like GeSHi, it'll look more like racket docs */
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => 'color: blue;',
            2 => 'color: rgb(34, 34, 139);',
            3 => 'color: blue;',
            4 => 'color: rgb(34, 34, 139);'
            ),
        'COMMENTS' => array(
            1 => 'color: #c2741f;',
            'MULTI' => 'color: #c2741f;'
            ),
        'ESCAPE_CHAR' => array(
            0 => ''
            ),
        'BRACKETS' => array(
            0 => 'color: rgb(132, 60,36)'
            ),
        'STRINGS' => array(
            0 => 'color: rgb(34, 139, 34)'
            ),
        'NUMBERS' => array(
            0 => 'color: rgb(34, 139, 34);',
            1 => 'color: rgb(34, 139, 34);',
            2 => 'color: rgb(34, 139, 34);',
            3 => 'color: rgb(34, 139, 34);',
            4 => 'color: rgb(34, 139, 34);',
            ),
        'METHODS' => array(
            0 => 'color: #202020;'
            ),
        'SYMBOLS' => array(
                           0 => 'color: rgb(132, 60,36);',
            ),
        'REGEXPS' => array(
            5 => 'color: rgb(34, 139, 34);',
            6 => 'color: rgb(132, 60,36);',
            8 => 'color: rgb(34, 139, 34);',
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
        6 => '#:[^[:space:]()[\\]{}\",\']+', // keyword
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

<?php
include_once 'geshi/geshi.php';
$path = 'geshi/geshi/';

$source_php = 'echo "hello, world!";
// weeeeee!!!!';

$source_rkt = '#lang racket
#|
   here is a multi-line comment
|#
(define x 32)
#f
#\newline
#:exist

  (define (list-of-ids->array-lines ids)
   (string-join
    (map (lambda (strs)
          (apply string-append strs))
     (ids->geshi-array ids (- 80 12) ", "))
    "\n            "))

  woo

   (define (split-into-lines qids (remaining max-line-len) (this-line null) (lines null))
    (if (null? qids)
     (if (null? this-line) (reverse lines) (reverse (cons (reverse this-line) lines)))
     (let* ((tl (cdr qids))
            (hd (if (null? tl)
                 (car qids)
                 (string-append (car qids) sep)))
            (hd-len (string-length hd)))
      (cond
       [(and (> hd-len max-line-len) (null? this-line))
       (split-into-lines tl max-line-len null (cons (list hd) lines))]
       [(> hd-len remaining)
       (split-into-lines qids max-line-len null
        (cons (reverse this-line) lines))]
       [else (split-into-lines tl (- remaining hd-len) (cons hd this-line) lines)]
      ))))

-1 ; reads equal to -1 
1/2 ; reads equal to (/ 1 2)
1.0 ; reads equal to (exact->inexact 1)
1+2i ; reads equal to (make-complex 1 2)
1/2+3/4i ; reads equal to (make-complex (/ 1 2) (/ 3 4))
1.0+3.0e7i ; reads equal to (exact->inexact (make-complex 1 30000000))
2e5 ; reads equal to (exact->inexact 200000)
#i5 ; reads equal to (exact->inexact 5)
#e2e5 ; reads equal to 200000
#x2e5 ; reads equal to 741
#b101 ; reads equal to 5

+inf.0
-inf.0
+inf.f
-inf.f
A pipe : |
\'a-symbol
\'another-symbol
\'>woo<
\'>yay-woo<
\'||
\'|woo|
\'|woo yay|
\'woo\ yay

(test-equal? (cons a b) (quote (a . b)))

(define spliced-list
 `(
   ,woo
   ,@(foo bar)))

; a vector
#(1 2 3 4)
#fl(1 2 3 4)

; characters
#\nul
#\077
#\u1234
#\u123456 ; expect 56 to miss
#\U123456
#\a #\b #\c
#\n
';

$geshi_php = new GeSHi($source_php, "php", $path);
$geshi_scm = new GeSHi($source_rkt, "scheme", $path);
$geshi_rkt = new GeSHi($source_rkt, "racket", ".");
// $geshi_rkt2 = new GeSHi($source_rkt, "racket-2", ".");

$geshi_php->enable_classes();
$geshi_scm->enable_classes();
$geshi_rkt->enable_classes();
// $geshi_rkt2->enable_classes();

?>
<html>
<head>
	<title>PHP Test</title>
	<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">

  <style type="text/css">
  <?php
  echo $geshi_php->get_stylesheet();
  echo $geshi_scm->get_stylesheet();
  echo $geshi_rkt->get_stylesheet();
  // echo $geshi_rkt2->get_stylesheet();
  ?>
  </style>
</head>
<body>
<?php
echo '<h1>GeSHi test</h1>';
echo $geshi_php->get_version();
echo $geshi_php->parse_code();

echo '<h2>as racket...</h2>';
echo $geshi_rkt->parse_code();

// echo '<h2>as racket/2...</h2>';
// echo $geshi_rkt2->parse_code();

// echo '<h1>as scheme...</h1>';
// echo $geshi_scm->parse_code();

echo '<hr><footer>yay</footer>';
?>
</body>
</html>

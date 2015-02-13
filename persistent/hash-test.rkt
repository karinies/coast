#lang racket/base

(require
 rackunit "hash.rkt")

(define (less? alpha beta)
  (string<? (symbol->string (car alpha)) (symbol->string (car beta))))

;; The gold standard test result.
(define alphabet '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
                     (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                     (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26)))

(define consonants '((b . 2) (c . 3) (d . 4) (f . 6) (g . 7) (h . 8) (j . 10)
                     (k . 11) (l . 12) (m . 13) (n . 14) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
                     (v . 22) (w . 23) (x . 24) (y . 25) (z . 26)))

(define (hash/sort h) (sort (hash=>pairs h) less?))

;; Test article for (vector/hash ...).
(define vector/hash/subject
  (vector/hash
   hash/eq/null
   '#(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
      k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
      u 21 v 22 w 23 x 24 y 25 z 26)))

;; Test article for (vectors/hash ...).
(define vectors/hash/subject
  (vectors/hash
   hash/eq/null
   '#(a b c d e f g h i j k l m n o p q r s t u v w x y z)
   '#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)))

;; Confirm that vector/hash constructs a proper hash table.
(check-equal? (hash/sort vector/hash/subject) alphabet "vector/hash")

;; Confirm that vectors/hash constructs a proper hash table.
(check-equal? (hash/sort vectors/hash/subject) alphabet "vectors/hash")

;; Confirm that hash/vector/remove correctly removes key/value pairs from a hash table.
(check-equal? (hash/sort (hash/vector/remove vector/hash/subject '#(a e i o u))) consonants "hash/vector/remove")


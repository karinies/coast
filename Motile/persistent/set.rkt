#lang racket/base

;; Copyright 2010 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Contact: mgorlick@acm.org

(require "ordered_trie.rkt")

(provide
 set/persist?
 set/eq/null
 set/eqv/null
 set/equal/null
 set/eq?
 set/eqv?
 set/equal?
 
 list/set
 set/new
 set/fold
 set/for-each
 set/length
 set/empty?
 set/list
 set/cons
 set/remove
 set/contains?
 set/car
 set/cdr
 set/map
 set/subset?
 set/union
 set/intersection
 set/difference
 set/filter
 set/partition
 set=>vector/racket
 vector/set
 
 ; Exported for the sake of serialize/deserialize.
 set/equality
 set/hash
 set/root
 set/construct)
 

;; A persistent unordered set is a four element vector v:
;; v[0] - the literal symbol <set/persist>
;; v[1] - the key equality test, one of eq?, eqv?, or equal?
;; v[2] - the key hash function, one of eq-hash-code, eqv-hash-code, or equal-hash-code
;; v[3] - the top level trie, the root of the set.
(define-accessor set/equality  1)
(define-accessor set/hash      2)
(define-accessor set/root      3)

(define (set/construct equality? hash root)
  (vector '<set/persist> equality? hash root))

(define set/eq/null    (set/construct eq?    eq-hash-code    trie/empty))
(define set/eqv/null   (set/construct eqv?   eqv-hash-code   trie/empty))
(define set/equal/null (set/construct equal? equal-hash-code trie/empty))

(define (set/persist? s)
  (and
   (vector? s)
   (= (vector-length s) 4)
   (eq? (vector-ref s 0) '<set/persist>)))

(define (set/eq? s)    (eq? (set/equality s) eq?))
(define (set/eqv? s)   (eq? (set/equality s) eqv?))
(define (set/equal? s) (eq? (set/equality s) equal?))

;; Effectively a set is a persistent hash table whose key/value pairs are e_i/#t
;; for each set member e_i.

;; Given set s and a list of elements (e_0 ... e_{n-1}) return the union of
;; s and elements e_0, ..., e_{n-1}.
(define (list/set s elements)
  (let ((equality? (set/equality s)) ; Set equality test.
        (hasher    (set/hash s)))    ; Set element hash.
    (let loop ((elements elements)
               (t (set/root s)))
      (if (null? elements)
          (set/construct equality? hasher t)
          (let ((key (car elements)))
            (loop (cdr elements) (trie/key/with equality? hasher t key #t (hasher key) 0)))))))

;; Given set s and vector v of set elements #(e_0 ... e_{n-1}) return the union of
;; s and elements e_0, ..., e_{n-1}.
(define (vector/set s v)
  (let ((equality? (set/equality s))
        (hasher    (set/hash s))
        (n         (vector-length v)))
    (let loop ((i 0)
               (t (set/root s)))
      (if (< i n)
          (let ((key (vector-ref v i)))
            (loop (add1 i) (trie/key/with equality? hasher t key #t (hasher key) 0)))
          (set/construct equality? hasher t)))))


(define (set/new s . rest) (list/set s rest))

;; Fold function function f over set s with the given initial seed value.
;; f takes two arguments, an element of set s and the current seed, and returns
;; the new seed value.
;; The value of the fold is the final seed value.
(define (set/fold s f seed)
  (trie/flat/fold (lambda (e _ignore seed) (f e seed)) seed (set/root s))) ; trie/flat/fold passes key, value, and seed to its fold function.

(define (set/for-each s f)
  (trie/for-each (set/root s) (lambda (k _v) (f k))))

(define (set/length s)
  (trie/pairs/total (set/root s)))

(define (set/empty? s)
  (eq? (set/root s) trie/empty))

(define (set/list s)
  (set/fold s (lambda (x seed) (cons x seed)) null))

;; Convert set s to a Racket vector with one set element per slot.
(define (set=>vector/racket s)
  (let ((v (make-vector (set/length s) #f)))
    (set/fold s (lambda (e seed) (vector-set! v s e) (add1 s)) 0)
    v))

(define (set/cons s element)
  (let* ((equality? (set/equality s))
         (hasher    (set/hash s))
         (t (trie/key/with equality? hasher (set/root s) element #t (hasher element) 0)))
    (set/construct equality? hasher t)))

(define (set/remove s element)
  (let* ((equality? (set/equality s))
         (hasher    (set/hash s))
         (t (trie/without equality? (set/root s) element (hasher element) 0)))
    (if (eq? t (set/root s)) s (set/construct equality? hasher t))))

(define (set/contains? s element)
  (if (trie/get (set/equality s) (set/root s) element ((set/hash s) element) 0) #t #f))

(define (set/car s)
  (let ((t (set/root s)))
    (if (eq? t trie/empty)
        (error 'set/car "car of empty set")
        (car (trie/car t)))))

(define (set/cdr s)
  (let ((t (set/root s)))
    (if (eq? t trie/empty)
        (error 'set/cdr "cdr of empty set")
        (set/construct (set/equality s) (set/hash s) (trie/cdr t)))))

(define (set/map s f)
  (let* ((equality? (set/equality s))
        (hasher    (set/hash s))
        (map
         (lambda (e _ignore seed)
           (let ((x (f e)))
             ;              equality? hasher trie key value  hash      shift) 
             (trie/key/with equality? hasher seed x   #t    (hasher x) 0)))))

    (set/construct
     equality?
     hasher
     (trie/flat/fold map trie/empty (set/root s))))) ; (trie/flat/fold f seed trie)

;; Returns #t if set beta is a subset of set alpha and #f otherwise.
(define (set/subset? alpha beta)
  (let ((root      (set/root alpha))
        (equality? (set/equality alpha))
        (hasher    (set/hash alpha)))
    ; Blow out of the trie/for-each the instant we discover an element in beta that is not in alpha.
    (let/cc abort
      (trie/for-each (set/root beta) (lambda (e _ignore) (unless (trie/get equality? root e (hasher e) 0) (abort #f))))
      #t)))

;       (trie/flat/fold
;        ; Continue in the fold until we exhaust beta or prove that beta is not a subset.
;        (lambda (e _ignore seed)
;          (if (trie/get equality? root e (hasher e) 0)
;              #t
;              (k #f))) ; Blow out of the fold immediately.
;        #t                   ; Seed.
;        (set/root beta)))))) ; Source of pairs.

;; Returns the union of sets alpha and beta.
;; The resulting set is defined with the equality? and hash functions of set alpha.
(define (set/union alpha beta)
  (let* ((equality? (set/equality alpha))
         (hasher    (set/hash alpha))
         (union
          (trie/flat/fold
           (lambda (e _ignore t) (trie/key/with equality? hasher t e #t (hasher e) 0))
           (set/root alpha)   ; Seed
           (set/root beta)))) ; Starting point for trie/flat/fold traversal.
    ; Return a set representing the union.
    (set/construct equality? hasher union)))

;; Returns the intersection of sets alpha and beta.
;; The resulting set is defined with the equality? and hash functions of set alpha.
(define (set/intersection alpha beta)
  (let* ((equality? (set/equality alpha))
         (hasher    (set/hash alpha))
         (root      (set/root alpha))
         (f (lambda (e _ignore t)
              (let ((hash (hasher e)))
                (if (trie/get equality? root e hash 0)
                    (trie/key/with equality? hasher t e #t hash 0) ; alpha contains e from beta so generate a successor trie containing e.
                    t)))) ; alpha does not contain e from beta so leave the trie alone.
         (intersection (trie/flat/fold f trie/empty (set/root beta)))) ; Generate a trie containing the intersection.
    ; Return a set representing the intersection.
    (set/construct equality? hasher intersection)))

;; Return the subset, S, of alpha such that no member of S is also a member of beta.
(define (set/difference alpha beta)
  (let* ((equality? (set/equality alpha))
         (hasher    (set/hash alpha))
         (difference
          (trie/flat/fold
           (lambda (e _ignore t) (trie/without equality? t e (hasher e) 0))
           (set/root alpha)   ; Seed.
           (set/root beta)))) ; Fold over contents of beta. 
    (set/construct equality? hasher difference)))

;; Return the subset of s such that for all elements x in s (f x) holds.
(define (set/filter s f)
  (let* ((equality? (set/equality s))
         (hasher    (set/hash s))
         (subset
          (trie/flat/fold
           (lambda (e _ignore t) (if (f e) (trie/key/with equality? hasher t e #t (hasher e) 0) t))
           trie/empty
           (set/root s))))
    (set/construct equality? hasher subset)))

;; Partition set s into two disjoint subsets A and B of s where f holds for all members of A
;; and f does not hold for all members of B.
;; Returns the partition as a pair (A . B).
(define (set/partition s f)
  (let* ((equality? (set/equality s))
         (hasher    (set/hash s))
         (f (lambda (e _ignore partition)
              (let ((i (if (f e) 0 1)))
                (vector-set! partition i (trie/key/with equality? hasher (vector-ref partition i) e #t (hasher e) 0))
                partition)))
         (final (trie/flat/fold f (vector trie/empty trie/empty) (set/root s))))
    (cons
     (set/construct equality? hasher (vector-ref final 0))
     (set/construct equality? hasher (vector-ref final 1)))))

;; Testing ephemera.

;(define s/26
;  (list/set
;   set/eq/null
;   '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
;     k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
;     u 21 v 22 w 23 x 24 y 25 z 26)))
;
;(define (set/car/test s)
;  (let loop ((s s)
;             (outcome null))
;    (if (set/empty? s)
;        outcome
;        (loop (set/cdr s) (cons (set/car s) outcome)))))
;
;(define (set/map/test s)
;  (set/map
;   s
;   (lambda (element)
;     (if (symbol? element) 999 element))))
;  

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

;; Persistent hash table based on the "Ideal Hash Tries" of Phil Bagwell using the Java implementation of
;; Andrew McKinlay as a starting point.
;; (See http://thesoftwarelife.blogspot.com/2009/10/java-immutable-persistent-map.html
;;  for a brief overview of McKinlay's implementation. The source code is available at
;;  http://suneido.svn.sourceforge.net/viewvc/suneido/jsuneido/src/suneido/util/PersistentMap.java?view=markup ).

(require
 ;"trie.rkt")
 "ordered_trie.rkt")

(require "tuple.rkt") ; TEMPORARY! For tracking down weird tuple problem ONLY!

(provide
 ; Type test.
 hash/persist?
 hash/eq?
 hash/eqv?
 hash/equal?
 
 ; Empty hash tables.
 hash/eq/null
 hash/eqv/null
 hash/equal/null

 ; Hash table constructors.
 hash/new
 list/hash
 pairs/hash
 vector/hash
 vectors/hash
 hash/cons
 hash/remove
 hash/list/remove
 hash/vector/remove
 hash/merge
 
 ; Hash table deconstructors.
 hash=>list
 hash=>pairs
 hash=>vector/racket
 ;hash/insertion=>vector
 hash/keys
 hash/values
 
 ; Hash table lookup.
 hash/ref
 hash/car
 hash/cdr

 hash/length
 hash/empty?
 hash/contains?

 ; Hash table combinators.
 hash/fold
 hash/map
 hash/for-each
 hash/filter
 hash/partition
 
 ; Exported solely for the sake of serialize/deserialize.
 hash/equality
 hash/hash
 hash/root
 hash/construct
 hash/persist/flatten
 hash/equality/decode)

;; A persistent hash table is a four element vector v:
;; v[0] - the literal symbol hash
;; v[1] - the key equality test, one of eq?, eqv?, or equal?
;; v[2] - the key hash function, one of eq-hash-code, eqv-hash-code, or equal-hash-code
;; v[3] - the top level trie, the root of the hash table.
(define-accessor hash/equality  1)
(define-accessor hash/hash      2)
(define-accessor hash/root      3)

;; Returns a new virgin hash table for Mischief.
(define (hash/construct equality? hasher root)
  (vector '<hash/persist> equality? hasher root))

;; Type test for Mischief.
(define (hash/persist? h)
  (and
   (vector? h)
   (= (vector-length h) 4)
   (eq? '<hash/persist> (vector-ref h 0))))

;; The constant empty hash tables that are the starting points for any persistent hash table instance.
(define hash/eq/null    (hash/construct eq?    eq-hash-code    trie/empty))
(define hash/eqv/null   (hash/construct eqv?   eqv-hash-code   trie/empty))
(define hash/equal/null (hash/construct equal? equal-hash-code trie/empty))

;; Return #t if h is an instance of an empty (null) hash table and #f otherwise.
(define (hash/null? h)
  (and (hash/persist? h) (eq? trie/empty (hash/root h))))

;; These three predicates detect the underlying equality test of hash table h.
(define (hash/eq? h)    (eq? (hash/equality h) eq?))
(define (hash/eqv? h)   (eq? (hash/equality h) eqv?))
(define (hash/equal? h) (eq? (hash/equality h) equal?))

;; User functions.

;; Nondestructively creates a fresh hash table h' that is the union of the contents of hash table
;; h and the key/value pairs (k_0 v_0 k_1 v_1 ...) given as input.
;; If h contains k_i/x for some value x then it will be replaced by k_i/v_i in h'.
(define (list/hash h input)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let loop ((input input)
               (t (hash/root h)))
      (if (null? input)
          (hash/construct equality? hasher t)
          (let ((key (car input))
                (value (cadr input)))
            (loop (cddr input) (trie/with equality? hasher t key value (hasher key) 0)))))))

;; (hash/new h k_0 v_0 ... k_i v_i) is equivalent to (list/hash h (k_0 v_0 ... k_i v_i)).
(define (hash/new h . rest)
  (list/hash h rest))

;; Nondestructively creates a fresh hash table h' that is the union of the contents of hash table
;; h and the the key value pairs ((k_0 . v_0) ... (k_i . v_i)) given as input.
;; Equivalent to (list/hash h k_0 v_0 ... k_i v_i).
(define (pairs/hash h input)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let loop ((pairs input)
               (t (hash/root h)))
      (if (null? pairs)
          (hash/construct equality? hasher t)
          (let ((pair (car pairs)))
            (loop (cdr pairs) (trie/with equality? hasher t (car pair) (cdr pair) (hasher (car pair)) 0)))))))

;; Nondestructively create a fresh hash table h' that is the union of the contents of the hash table h
;; and the contents of vector v = #(k_0 v_0 k_1 v_1 ... k_{n-1} v_{n-1}).
;; The vector equivalent of (pairs/hash h ((k_0 . v_0) ... (k_{n-1} . v_{n-1}))).
(define (vector/hash h v)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h))
        (n         (vector-length v))) ; n must be even.
    (let loop ((i 0)
               (t (hash/root h)))
      (if (= i n)
          (hash/construct equality? hasher t) ; Return the merged hash table.
          (let ((key   (vector-ref v i))
                (value (vector-ref v (add1 i))))
            (loop
             (+ i 2)
             (trie/with equality? hasher t key value (hasher key) 0)))))))

(define (vectors/hash h keys values)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h))
        (n         (vector-length keys))) ; The lengths of keys and values must be identical.
    (let loop ((i 0)
               (t (hash/root h)))
      (if (= i n)
          (hash/construct equality? hasher t) ; Return the merged hash table.

          ; Add key_i/value_i to the hash table.
          (let ((key (vector-ref keys i)))
            (loop
             (add1 i)
             (trie/with equality? hasher t key (vector-ref values i) (hasher key) 0)))))))
  

;; Fold function f with seed over all pairs appearing in hash table h.
;; f accepts three arguments: key, value, and the most recently computed seed.
(define (hash/fold h f seed)
  (trie/flat/fold f seed (hash/root h)))

;; Return the total number of key/value pairs in hash table h. 
(define (hash/length h)
  (trie/pairs/total (hash/root h)))

(define (hash/empty? h)
  (eq? (hash/root h) trie/empty))

;; Return all key/value pairs in h as an association list ((k_0 . v_0) ...)
(define (hash=>pairs h)
  (hash/fold h (lambda (key value seed) (cons (cons key value) seed)) null))

;; Given the hash contents of h as k_0/v_0, ..., k_N/v_N return a flat list
;; (k_0 v_0 ... k_N v_N).
(define (hash=>list h)
  (hash/fold h (lambda (key value seed) (cons key (cons value seed))) null))

;; Given the hash contents of h as k_0/v_0, ..., k_N/v_N return
;; #(k_0 v_0 ... k_N v_N).
(define (hash=>vector/racket h)
  (let* ((n (hash/length h))
         (v (make-vector (* 2 n) #f)))
    (hash/fold
     h
     (lambda (key value seed)
       (vector-set! v seed        key)
       (vector-set! v (add1 seed) value)
       (+ seed 2))
     0)
    v))

;; Return the contents of h as #(k_1 v_1 ... k_n v_n) but respect the insertion order
;; of the key/value pairs k_i/v_i, that is, k_i v_i precedes k_j v_j if k_i/v_i were
;; added to the hash table before k_j/v_j.
;;; This routine is here for the sake of the serializer.
;(define (hash/insertion=>vector h)
;  (trie/sort/insertion=>vector (hash/root h)))

;; Given hash table h return a successor hash table whose contents is the merge of h and the key/value pair.
;; If h contains key then that pair is replaced by the arguments in the successor.
(define (hash/cons h key value)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let ((t (trie/with equality? hasher (hash/root h) key value (hasher key) 0)))
      (hash/construct equality? hasher t))))

;; Return a successor to h whose contents are identical to h less the pair in h indexed by key.
(define (hash/remove h key)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (let ((t (trie/without equality? (hash/root h) key (hasher key) 0)))
      (if (eq? t (hash/root h))
          h
          (hash/construct equality? hasher t)))))

;; Return a succesor to h whose contents are identical to h less the n pairs in h indexed by keys = #(k_0 ... k_{n-1}).
(define (hash/vector/remove h keys)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h))
        (n         (vector-length keys)))
    (let loop ((i 0)
               (t (hash/root h)))
      (if (= i n)
          (if (eq? t (hash/root h)) h (hash/construct equality? hasher t))
          (let ((key (vector-ref keys i)))
            (loop (add1 i) (trie/without equality? t key (hasher key) 0)))))))

;; Return a successor to h whose contents are to h less the pairs in h indexed by keys = (k_0 ... k_{n-1}).
(define (hash/list/remove h keys)
    (let ((equality? (hash/equality h))
          (hasher    (hash/hash h)))
    (let loop ((keys keys)
               (t (hash/root h)))
      (if (null? keys)
          (if (eq? t (hash/root h)) h (hash/construct equality? hasher t))
          (let ((key (car keys)))
            (loop (cdr keys) (trie/without equality? t key (hasher key) 0)))))))
                     

;; Returns #t if hash table h contains key and #f otherwise.
(define (hash/contains? h key)
  (if (trie/get (hash/equality h) (hash/root h) key ((hash/hash h) key) 0)
      #t
      #f))
    
;; If hash table h contains key then return the value of the key/value pair otherwise return failure.
(define (hash/ref h key failure)
  (let ((pair (trie/get (hash/equality h) (hash/root h) key ((hash/hash h) key) 0)))
    (if pair
        (trie/pair-value pair)
        failure)))

;; Return the set of keys in hash table h as a list.
(define (hash/keys h)
  (hash/fold h (lambda (key _value seed) (cons key seed)) null))

;; Return the set of values in hash table h as a list.
(define (hash/values h)
  (hash/fold h (lambda (_key value seed) (cons value seed)) null))

;; Return the "first" key/value pair of hash table h.
;; hash/car is the hash table equivalent of car for lists.
(define (hash/car h)
  (let ((t (hash/root h)))
    (if (eq? trie/empty t)
        (error 'hash/car "car of empty hash")
        (trie/car t))))

;; Return the successor of hash table h in which the key/value pair returned by (hash/car h)
;; is absent in the successor. Equivalent to (hash/remove h (car (hash/car h))) but considerably
;; more efficient.
(define (hash/cdr h)
  (let ((t (hash/root h)))
    (if (eq? trie/empty t)
        (error 'hash/cdr "cdr of empty hash")
        (hash/construct (hash/equality h) (hash/hash h) (trie/cdr t)))))

;; Apply function f to each key/value pair in h and return a hash table containing the results.
;; f accepts two arguments, a key and a value.
(define (hash/map h f)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (define (map key value seed)
      (let ((x (f key value)))
        (if (pair? x) ; (key . value)
            (trie/with equality? hasher seed (car x) (cdr x) (hasher (car x)) 0)
            (error 'hash/map "pair required from function: ~s" x))))          
    
    (hash/construct
     equality?
     hasher
     (trie/flat/fold map trie/empty (hash/root h)))))

;; Apply function f, as (f key value) to each key/value pair in h and discard all results.
;(define (hash/for-each h f)
;  (cond [(hash/empty? h) (void)]
;        [else (f (hash/car h))
;              (hash/for-each (hash/cdr h) f)]))

;; Apply function f, as (f key value) to each key/value pair in h and discard all results.
(define (hash/for-each h f)
  (trie/for-each (hash/root h) f))

;; Return a hash table containing only those key/value pairs in h for which (p key value) is true.
(define (hash/filter h p)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    (define (filter key value seed)
      (if (p key value)
          (trie/with equality? hasher seed key value (hasher key) 0)
          seed)) ; Return the seed trie unchanged.
    
    (hash/construct
     (hash/equality h)
     (hash/hash h)
     (trie/flat/fold filter trie/empty (hash/root h)))))

;; Return a pair of hash tables (true . false) in which hash table true contains only those pairs of
;; hash table h for which predicate p returns #t and hash table false contains only those pairs of
;; h for which predicate p returns #f.
;; p is a predicate of two arguments (p key value)
(define (hash/partition h p)
  (let ((equality? (hash/equality h))
        (hasher    (hash/hash h)))
    ; The trick here is that seed is a two element vector for which element 0 is the root trie for the
    ; "true" partition and element 1 is the root trie for the "false" partition.
    (define (action key value seed)
      (let ((i (if (p key value) 0 1)))
        (vector-set!
         seed i
         (trie/with equality? hasher (vector-ref seed i) key value (hasher key) 0))
        seed))
    
    (let ((partition (trie/flat/fold action (vector trie/empty trie/empty) (hash/root h))))
      (cons
       (hash/construct equality? hasher (vector-ref partition 0))
       (hash/construct equality? hasher (vector-ref partition 1))))))
          

;; Return a hash table that is is the merge of beta INTO alpha. Any key/value pair in beta whose key
;; duplicates a key appearing in alpha will replace the corresponding key/value pair in alpha.
;; Consequently the length of the resulting hash table will be at most (+ (hash/length alpha) (hash/length beta)).
(define (hash/merge alpha beta)
  (let ((equality? (hash/equality alpha))
        (hasher    (hash/hash     alpha)))
  (hash/construct
   equality?
   hasher
   (trie/flat/fold
    (lambda (key value seed)
      (trie/with equality? hasher seed key value (hasher key) 0))
    (hash/root alpha)    ; Seed.
    (hash/root beta))))) ; Source of pairs.


(define (hash/equality/flatten h)
  (cond
    ((hash/eq?  h) 'eq)
    ((hash/eqv? h) 'eqv)
    (else          'equal)))

(define (hash/persist/flatten v loop)
  (let* ((r (make-vector (* 2 (hash/length v)) #f))
         (f (lambda (k x i)
              (vector-set! r i        (loop k)) ; Key.
              (vector-set! r (add1 i) (loop x)) ; Value.
              (+ 2 i))))
    ; Fill r with the serialized keys and values K_1 X_1 ... K_N X_N contained as k_i/x_i pairs in hash table v.
    (hash/fold v f 0)
    (vector 'H (hash/equality/flatten v) r)))

(define (hash/equality/decode x)
  (case x
    ((eq)    hash/eq/null)
    ((eqv)   hash/eqv/null)
    ((equal) hash/equal/null)))

;; unfold p f g seed tail -> list
;; unfold is best described by its basic recursion:
;(define (hash/unfold h stop f g seed)
;  (if (stop seed)
;      h
;      (let ((pair (f seed)))
;        (hash/cons (hash/unfold h stop f g (g seed)) (car pair) (cdr pair)))))
;        

;; Ephemera for testing.

(define h/26
  (list/hash
   hash/eq/null
   '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
     k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
     u 21 v 22 w 23 x 24 y 25 z 26)))
;
;(define list/100
;    (let loop ((i 0)
;             (outcome null))
;    (if (< i 100)
;        (loop (add1 i) (cons i (cons i outcome)))
;        outcome)))
;
;(define h/100 (list/hash hash/eqv/null list/100))
;
;(define list/1000
;  (let loop ((i 0)
;             (outcome null))
;    (if (< i 1000)
;        (loop (add1 i) (cons i (cons i outcome)))
;        outcome)))
;
;(define h/1000 (list/hash hash/eq/null list/1000))
;
;(define (symbols/sort symbols)
;  (sort
;   symbols
;   (lambda (alpha beta) (string<? (symbol->string alpha) (symbol->string beta)))))
;
;(define (hash/car/test h)
;  (let loop ((h h)
;             (outcome null))
;    (if (hash/empty? h)
;        outcome
;        (loop (hash/cdr h) (cons (hash/car h) outcome)))))

(define XPATHS
  (list
   (tuple)           1
   (tuple 'foo 'bar) 2
   (tuple 'foo)      3
   (tuple 'bar)      4))

(define YPATHS
  (list
   '(<tuple>)          1
   '(<tuple> foo bar)  2
   '(<tuple> foo)      3
   '(<tuple> bar)      4))

(define bad (list/hash hash/equal/null XPATHS))
(define bady (list/hash hash/equal/null YPATHS))

;#(<hash/persist>
;   #<procedure:equal?>
;   #<procedure:equal-hash-code>
;   #(2113538 (#(<tuple> foo bar) . 2)
;             #(131072
;               #(32768
;                 #(128
;                   #(2048
;                     #(2097152
;                       #(1 #((#(<tuple> foo) . 3) (#(<tuple> bar) . 4))))))))
;             (#(<tuple>) . 1)))

;; Helper function used in hash/vector/test below.
(define (vector/pairs v)
  (let loop ((pairs null)
             (i 0)
             (n (vector-length v)))
    (if (< i n)
        (loop
         (cons (cons (vector-ref v i) (vector-ref v (add1 i))) pairs)
         (+ i 2)
         n)
        pairs)))

(define (less? alpha beta)
  (string<? (symbol->string (car alpha)) (symbol->string (car beta))))

(define (hash/vector/test)
  (let ((v (hash=>vector/racket h/26)))
    
    (sort (vector/pairs v) less?)))
;    '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8) (i . 9) (j . 10)
;      (k . 11) (l . 12) (m . 13) (n . 14) (o . 15) (p . 16) (q . 17) (r . 18) (s . 19) (t . 20)
;      (u . 21) (v . 22) (w . 23) (x . 24) (y . 25) (z . 26))))

(define (hash/for-each/test)
  (hash/for-each h/26 (lambda (k v) (write (cons k v)) (newline))))

(define (test/hash/list/remove)
  (sort
   (vector/pairs (hash=>vector/racket (hash/list/remove h/26 '(a x y junk))))
   less?))

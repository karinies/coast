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
 racket/contract/base
 [only-in racket/vector vector-map]
 "../getters.rkt"
 "ordered_trie.rkt")

;; A type contract that weakly captures #(k_1 v_1 ... k_n v_n).
(define vector-key-value/c
  (flat-named-contract
   'vector-key-value
   (lambda (x) (and (vector? x) (even? (vector-length x))))))

(provide
 ; Type test.
 (rename-out (hash? hash/persist?))
 (contract-out
  [hash/eq? (hash/persist? . -> . boolean?)]
  [hash/eqv? (hash/persist? . -> . boolean?)]
  [hash/equal? (hash/persist? . -> . boolean?)])
 
 ; Empty hash tables.
 hash/eq/null
 hash/eqv/null
 hash/equal/null

 ; Hash table constructors.
 (contract-out
  [hash/new (->* (hash/persist?) #:rest (listof any/c) hash/persist?)]
  [list/hash (hash/persist? (or/c null (listof any/c)) . -> . hash/persist?)]
  [pairs/hash (hash/persist? (or/c null (listof (cons/c any/c any/c))) . -> . hash/persist?)]
  [vector/hash (hash/persist? vector-key-value/c . -> . hash/persist?)]
  [vectors/hash (hash/persist? vector? vector? . -> . hash/persist?)]
  [hash/cons (hash/persist? any/c any/c . -> . hash/persist?)]
  [hash/remove (hash/persist? any/c . -> . hash/persist?)]
  [hash/list/remove (hash/persist? (or/c null (listof any/c)) . -> . hash/persist?)]
  [hash/vector/remove (hash/persist? vector? . -> . hash/persist?)]
  [hash/merge (hash/persist? hash/persist? . -> . hash/persist?)])
 
 ; Hash table deconstructors.
 (contract-out
  [hash=>list (hash/persist? . -> . (or/c null (listof any/c)))]
  [hash=>pairs (hash/persist? . -> . (or/c null (listof (cons/c any/c any/c))))]
  [hash=>vector/racket (hash/persist? . -> . vector?)]
  [hash/keys (hash/persist? . -> . (or/c null (listof any/c)))]
  [hash/values (hash/persist? . -> . (or/c null (listof any/c)))])
 
 ; Hash table lookup.
 (contract-out
  [hash/ref (hash/persist? any/c any/c . -> . any/c)]
  [hash/car (hash/persist? . -> . (cons/c any/c any/c))]
  [hash/cdr (hash/persist? . -> . hash/persist?)])

 ; Membership tests.
 (contract-out
  [hash/length (hash/persist? . -> . exact-nonnegative-integer?)]
  [hash/empty? (hash/persist? . -> . boolean?)]
  [hash/contains? (hash/persist? any/c . -> . boolean?)])

 ; Hash combinators.
 (contract-out
  [hash/and (hash/persist? (-> any/c any/c boolean?) . -> . boolean?)]
  [hash/fold (hash/persist? (-> any/c any/c any/c any/c) any/c . -> . any/c)]
  [hash/map (hash/persist? (-> any/c any/c (cons/c any/c any/c)) . -> . hash/persist?)]
  [hash/for-each (hash/persist? (-> any/c any/c void?) . -> . void?)]
  [hash/filter (hash/persist? (-> any/c any/c boolean?) . -> . hash/persist?)]
  [hash/or (hash/persist? (-> any/c any/c boolean?) . -> . boolean?)]
  [hash/partition (hash/persist? (-> any/c any/c boolean?) . -> . (cons/c hash/persist? hash/persist?))])
 
 ; Exported solely for the sake of serialize/deserialize.
 (contract-out
  [hash/persist/flatten (hash/persist? (-> any/c any/c) . -> . vector?)]
  [hash/persist/unflatten (hash/persist/flat/c procedure? . -> . hash?)])
 )
 
 

;; A persistent hash table is a four element vector v:
;; v[0] - the literal symbol hash
;; v[1] - the key equality test, one of eq?, eqv?, or equal?
;; v[2] - the key hash function, one of eq-hash-code, eqv-hash-code, or equal-hash-code
;; v[3] - the top level trie, the root of the hash table.
;(define-accessor hash/equality  1)
;(define-accessor hash/hash      2)
;(define-accessor hash/root      3)

(struct hash
  (equality ; Equality test for keys.
   hash     ; Hashing function for keys.
   root)    ; Root of trie.
  #:transparent)
(struct/getters/define hash equality hash root)

;; Returns a new virgin hash table for Motile.
(define (hash/construct equality? hasher root)
  ;(vector '<hash/persist> equality? hasher root))
  (hash equality? hasher root))

;; Type test for Motile.
(define hash/persist? hash?)
;(define (hash/persist? h)
;  (and
;   (vector? h)
;   (= (vector-length h) 4)
;   (eq? '<hash/persist> (vector-ref h 0))))

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

;; Returns #t if (f k v) returns true for every pair k/v in h and #f otherwise.
(define (hash/and h f)
  (trie/flat/and f (hash/root h)))

;; Returns #t if (f k v) returns true for some pair k/v in h and #f otherwise.
(define (hash/or h f)
  (trie/flat/or f (hash/root h)))

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
        (trie/pair/value pair)
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

;(define (hash/persist/flatten v loop)
;  (let* ((r (make-vector (* 2 (hash/length v)) #f))
;         (f (lambda (k x i)
;              (vector-set! r i        (loop k)) ; Key.
;              (vector-set! r (add1 i) (loop x)) ; Value.
;              (+ 2 i))))
;    ; Fill r with the serialized keys and values K_1 X_1 ... K_N X_N contained as k_i/x_i pairs in hash table v.
;    (hash/fold v f 0)
;    (vector 'H (hash/equality/flatten v) r)))

;; Flatten the persistent hash k_1/v_1 ... k_n/v_n
;; (where k_i/v_i precedes k_j/v_j iff k_i/v_i was inserted prior to k_j/v_j)
;; into a vector #(struct:hash <eq | eqv | equal> <k_1> <v_1> ... <k_n> <v_n>)
;; where <k_>i is the serialization of key k_i and <v_i> is the serialization of value <v_i>.
(define (hash/persist/flatten v loop)
  (let* ((all (trie/sort/insertion (hash/root v))) ; A list of all trie/pairs in insertion order.
         (n   (length all))
         (pairs (make-vector (* n 2) #f))
         (x (make-vector 3 #f)))
    (vector-set! x 0 'struct:hash)
    (vector-set! x 1 (hash/equality/flatten v))
    (let scan ((i 0) (all all))
      (when (pair? all)
        (vector-set! pairs i        (loop (trie/pair/key   (car all))))
        (vector-set! pairs (add1 i) (loop (trie/pair/value (car all))))
        (scan (+ i 2) (cdr all))))
    (vector-set! x 2 pairs)
    x))

(define (hash/equality/decode x)
  (case x
    ((eq)    hash/eq/null)
    ((eqv)   hash/eqv/null)
    ((equal) hash/equal/null)))

(define (hash/persist/flatten? x)
  (and
   (vector? x)
   (= (vector-length x) 3)
   (eq?  (vector-ref x 0) 'struct:hash)
   (memq (vector-ref x 1) '(eq eqv equal))
   (let ((pairs (vector-ref x 2)))
     (and (vector? pairs) (even? (vector-length pairs))))))

(define hash/persist/flat/c (flat-named-contract 'hash/persist/flat hash/persist/flatten?))

;; v - #(struct:hash <equality> #(K_1 X_1 ... K_n X_n))
(define (hash/persist/unflatten v f)
  (let ((pairs (vector-map f (vector-ref v 2))))
    (vector/hash (hash/equality/decode (vector-ref v 1)) pairs)))

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

;(define XPATHS
;  (list
;   (tuple)           1
;   (tuple 'foo 'bar) 2
;   (tuple 'foo)      3
;   (tuple 'bar)      4))
;
;(define YPATHS
;  (list
;   '(<tuple>)          1
;   '(<tuple> foo bar)  2
;   '(<tuple> foo)      3
;   '(<tuple> bar)      4))
;
;(define bad (list/hash hash/equal/null XPATHS))
;(define bady (list/hash hash/equal/null YPATHS))

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

(define (test/hash/and/01)
  (hash/and h/26 (lambda (key value) (positive? value))))

(define (test/hash/and/02)
  (hash/and h/26 (lambda (key value) (zero? value))))

(define (test/hash/and/03)
  (let ((h (hash/cons h/26 '0 -14)))
    (hash/and h (lambda (key value) (positive? value)))))

(define (test/hash/or/01)
  (hash/or h/26 (lambda (key value) (negative? value))))
(define (test/hash/or/02)
  (hash/or h/26 (lambda (key value) (= value 26))))

(define (test/flatten/01)
  (trie/sort/insertion (hash/root h/26)))

(define (test/flatten/02)
  (trie/sort/insertion=>vector (hash/root h/26)))

(define (test/flatten/03)
  (let ((h (hash/cons (hash/remove h/26 'a) 'a 1)))
    (trie/sort/insertion=>vector (hash/root h))))

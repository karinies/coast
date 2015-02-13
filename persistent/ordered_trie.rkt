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

;; A library for constructing the "Ideal Hash Tries" of Phil Bagwell and inspired by Andrew McKinlay's
;; Java implementation of persistent hash tables.
;; (See http://thesoftwarelife.blogspot.com/2009/10/java-immutable-persistent-map.html
;;  for a brief overview of McKinlay's implementation. The source code is available at
;;  http://suneido.svn.sourceforge.net/viewvc/suneido/jsuneido/src/suneido/util/PersistentMap.java?view=markup ).

(require
 (only-in ffi/unsafe/atomic start-atomic end-atomic)
 (only-in "../getters.rkt" struct/getters/define)
 (only-in racket/vector vector-copy)
 (only-in rnrs/arithmetic/bitwise-6 
          bitwise-bit-count
          bitwise-first-bit-set))

(provide
 define-inline
 define-accessor
 
 trie/car
 trie/cdr
 trie/empty
 trie/get
 trie/key/with
 trie/with
 trie/without
 trie/pairs/total
 trie/fold      ; Fold over the trie/pair representation as (f trie/pair seed).
 trie/flat/fold ; Fold over a key/value as in (f key value seed).
 trie/and
 trie/flat/and
 trie/or
 trie/flat/or
 trie/for-each
 trie/sort/insertion         ; Return a list of trie/pairs sorted in insertion order (from first to last).
 trie/sort/insertion=>vector ; Return #(k_1 v_1 ... k_n v_n) in insertion order for the sake of the serializer.
 
 trie/pair?
 trie/pair/order
 trie/pair/key
 trie/pair/value)

(define-syntax define-inline
  ;; Work around the lack of an inliner.
  (syntax-rules ()
    ((_ (name formals ...) body ...)
     (define-syntax name
       (syntax-rules ()
         ((_ formals ...)
          (begin body ...)))))))

(define-syntax define-accessor
  (syntax-rules ()
    ((_ name index)
     (define-inline (name block)
       (vector-ref block index)))))

;; A trie is a vector #(map slot ...), where map is a 32-bit unsigned integer and the vector
;; contains anywhere between 0 ... 32 slots inclusive.
;; If bit i is set in the map then slot_i of the trie of the trie is occupied. To conserve
;; space only those slots whose bits are 1 in the map are actually allocated. Thus the
;; total length of a trie t is n+1 where n is the total number of 1-bits present in the
;; map of t.
(define-accessor trie/map 0)
(define TRIE/SLOT/OFFSET  1)

;; Handy inline shorthand for common manipulations.

;; Access the i'th slot of trie t where i = 0 corresponds to the first (lowest order) bit
;; set in the map, i = 1 corresponds to the second bit set, and so on.
(define-syntax-rule (trie/slot t i)
  (vector-ref t (add1 i)))
;; Set the i'th slot of trie t to value x. Again, i has the meaning given above for trie/slot.
(define-syntax-rule (trie/slot! t i x)
  (vector-set! t (add1 i) x))
;; The total number of slots allocated in trie t.
(define-syntax-rule (trie/length t)
  (sub1 (vector-length t)))
;; Return a skeleton trie with the given map and n slots (one slot for each 1-bit appearing in the map).
(define (trie/construct map n)
  (let ((t (make-vector (add1 n) #f)))
    (vector-set! t 0 map)
    t))

;; Type test for trie nodes.
(define-syntax-rule (trie? t)
  (and
   (vector? t)
   (integer? (vector-ref t 0)))) ; map

;; Generator for order of insertion values in trie.
(define (counter/shared/new)
  (let ((n 1)) ; We'll count 1, 2, 3, ...
    (lambda ()
      (start-atomic)
      (let ((i n))
        (set! n (add1 n))
        (end-atomic)
        i))))

(define counter/shared (counter/shared/new)) ; Monotonic counter for order of insertion of trie key/value pairs.

;; For the sake of signing hash maps and binding environments, both of which are built with the trie library,
;; we must guarantee that every serialization of a particular hash map X or binidng environment Y always presents its key/value
;; pairs for serialization in exactly in the same order. To do this we associate an order = 1, ... with each key/value
;; pair and then present the key/value pairs to the serializer in insertion order. The insertion order may contain
;; holes (as a consequence of deletions) but all we care is that the order is monotonically increasing.
(struct trie/pair (order key value) #:transparent) ; A key/value pair.
(struct/getters/define trie/pair order key value)

(define (trie/pair/new key value) (trie/pair (counter/shared) key value))

;; Convert a trie/pair into a cons pair.
(define-syntax-rule (trie/pair=>cons x) (cons (trie/pair/key x) (trie/pair/value x)))

;; Overflow nodes are just vectors #(t_0 t_1 ... t_n) of trie pairs t_i.
;; Overflow nodes are created when the depth of the trie exceeds 6 (including the root).
;; Type test for overflow nodes.
(define-syntax-rule (overflow? o)
  (and
   (vector? o)
   (> (vector-length o) 1)
   (trie/pair? (vector-ref o 0))))

;; Used during debugging to dump a trie map.
;(define (bits/on x)
;  (let loop ((shift 0)
;             (x x)
;             (outcome null))
;    (if (< shift 32)
;        (if (zero? (&& x #x01))
;            (loop (add1 shift) (>>> x 1) outcome)
;            (loop (add1 shift) (>>> x 1) (cons shift outcome)))
;        (reverse outcome))))
           
;; Returns #t if the trie t is empty and #f otherwise.
(define (trie/empty? t) (= (vector-length t) 1))

;; Allocate an empty trie node with a map of zero and no allocated slots.
(define (trie/empty/new) (trie/construct 0 0)) ; Equivalently just #(0).

;; The constant denoting the universal shared empty trie node.
(define trie/empty (trie/empty/new))

;; Shorthand for the nitty gritty bit manipulation required for trie nodes.

;; Bitwise arithmetic right shift.
(define-syntax-rule (>>> x shift)
  (arithmetic-shift x (- shift)))
;; Bitwise arithmetic left shift.
(define-syntax-rule (<<< x shift)
  (arithmetic-shift x shift))
;; Bitwise and.
(define-syntax-rule (&& x y)
  (bitwise-and x y))
;; Bitwise or.
(define-syntax-rule (|| x y)
  (bitwise-ior x y))
;; Bitwise not.
(define-syntax-rule (~ x) 
  (bitwise-not x))

(define HASH-BITS 32)

;; Given a 32-bit hash code and a shift = 0, 5, 10, ..., 30 return 32-bit mask 1 << n
;; where n = 0, 1, ..., 31.
(define-syntax-rule (slot/mask hash shift)
  (<<< 1 (&& (>>> hash shift) #x01f)))

;; The number of occupied slots preceeding the slot denoted by the mask
(define-syntax-rule (slots/before map mask)
  (bitwise-bit-count (&& map (sub1 mask))))
;; The total number of allocated slots in trie node t.
(define-syntax-rule (slots/occupied t)
  (bitwise-bit-count (trie/map t)))
;; Return #t if the slot denoted by the mask is available for allocation.
;; Return #f if the slot denoted by the mask is already in use.
(define-syntax-rule (slot/free? t mask)
  (zero? (&& (trie/map t) mask)))
;; Given a mask for a particular slot remove that slot indicator from the map of trie node t.
(define-syntax-rule (trie/unmap t mask)
  (&& (trie/map t) (~ mask)))

;; Locate trie/pair in trie node t of hash h whose key matches.
;; If found return the trie/pair.
;; If not found return #f.
(define (trie/get equality? t key hash shift)
  (let ((mask (slot/mask hash shift))) ; Which hash slot 0 ... 31 should we try?
    (if (slot/free? t mask)
        #f ; That trie slot is empty. No such key in this trie.

        ; The key/value pair may be here. Look further.
        (let* ((i    (slots/before (trie/map t) mask))
               (slot (trie/slot t i)))
          (cond
            ((trie/pair? slot)
             (if (equality? key (trie/pair/key slot)) slot #f))
             
;            ((pair? slot)
;             (if (equality? key (car slot)) slot #f))

            ((trie? slot)
             (trie/get equality? slot key hash (+ shift 5)))

            ((overflow? slot)
             (overflow/get equality? slot key))

            (else
             (error 'trie/get "unknown node type ~a" slot)))))))

;; Non-destructively add the key/value pair to trie node t returning the successor trie.
;; equality? - equality predicate for keys
;; hasher - hash function for keys
;; t - trie node
;; key - key portion of key/value pair
;; value - value portion of key/value pair
;; hash - hash code of key, that is, return value of (hasher key)
;; shift - rightward shift to compute trie map mask
(define (trie/with equality? hasher t key value hash shift)
  (let* ((mask (slot/mask hash shift))
         (i    (slots/before (trie/map t) mask)) ; Number of occupied slots preceeding slot denoted by mask.
         (midpoint (+ i TRIE/SLOT/OFFSET)))
    (if (slot/free? t mask)
        ; No such key in this trie.
        (let ((extension
               (trie/construct (|| (trie/map t) mask) (add1 (trie/length t)))))
          (vector-copy! extension TRIE/SLOT/OFFSET t TRIE/SLOT/OFFSET midpoint)
          ;(vector-set!  extension midpoint (cons key value))
          (vector-set!  extension midpoint (trie/pair/new key value))
          (when (< i (slots/occupied t))
            (vector-copy! extension (add1 midpoint) t midpoint))
          extension)

        ; Slot i for the key is occupied.
        (let ((entry (trie/slot t i))
              (clone (vector-copy t))) ; Replacement trie node.
          (cond
            ((trie/pair? entry)
             ; Slot i contains some trie entry <order, key, value>.
             (if (equality? key (trie/pair/key entry))
                 ; The two keys match. Replace the old key/value pair with the new key/value pair.
                 (trie/slot! clone i (trie/pair/new key value))
                 ; Collision (two distinct keys with the same hash)!
                 (trie/slot! clone i (child/new entry (hasher (trie/pair/key entry)) key value hash (+ shift 5))))
             clone)

            ((trie? entry)
             ; Slot contains a a (sub)trie node.
             (trie/slot! clone i (trie/with equality? hasher entry key value hash (+ shift 5)))
             clone)

            ((overflow? entry)
             ; Slot contains an overflow node.
             (trie/slot! clone i (overflow/with equality? entry key value))
             clone)

            (else
             (error 'trie/with "unknown node type ~s" entry)))))))

;; A specialized version of trie/with for the case in which only the key is significant and the
;; accompanying value is immaterial, for example, when using hash tables to implement unordered sets.
;; This allows us to optimize the special case in which the key already appears in trie t or its subnodes
;; by eliminating the expense of generating a new trie or overflow node needlessly.
(define (trie/key/with equality? hasher t key value hash shift)
  (let* ((mask (slot/mask hash shift))
         (i    (slots/before (trie/map t) mask)) ; Number of occupied slots preceeding slot denoted by mask.
         (midpoint (+ i TRIE/SLOT/OFFSET)))
    (if (slot/free? t mask)
        ; No such key in this trie.
        (let ((extension
               (trie/construct (|| (trie/map t) mask) (add1 (trie/length t)))))
          (vector-copy! extension TRIE/SLOT/OFFSET t TRIE/SLOT/OFFSET midpoint)
          (vector-set!  extension midpoint (trie/pair/new key value)) ; (vector-set!  extension midpoint (cons key value))
          (when (< i (slots/occupied t))
            (vector-copy! extension (add1 midpoint) t midpoint))
          extension)

        ; Slot i for the key is occupied.
        (let ((entry (trie/slot t i)))
          (cond
            ((trie/pair? entry)
             ; Slot i contains some (k . v) pair.
             (if (equality? key (trie/pair/key entry))
                 t ; The two keys match and that is all we care about.

                 ; Collision (two distinct keys with the same hash).
                 (let ((clone (vector-copy t)))
                   (trie/slot! clone i (child/new entry (hasher (trie/pair/key entry)) key value hash (+ shift 5)))
                   clone)))

;            ((pair? entry)
;             ; Slot i contains some (k . v) pair.
;             (if (equality? key (car entry))
;                 t ; The two keys match and that is all we care about.
;
;                 ; Collision (two distinct keys with the same hash).
;                 (let ((clone (vector-copy t)))
;                   (trie/slot! clone i (child/new entry (hasher (car entry)) key value hash (+ shift 5)))
;                   clone)))

            ((trie? entry)
             ; Slot contains a a (sub)trie node.
             (let ((x (trie/key/with equality? hasher entry key value hash (+ shift 5))))
               (if (eq? x entry)
                   t ; key already appears in the (sub)trie node or its descendants.
                   (let ((clone (vector-copy t)))
                     (trie/slot! clone i x)
                     clone))))

            ((overflow? entry)
             ; Slot contains an overflow node.
             (let ((o (overflow/key/with equality? entry key value)))
               (if (eq? o entry)
                   entry ; key already appears in the overflow node (entry).
                   (let ((clone (vector-copy t)))
                     (trie/slot! clone i o)
                     clone))))

            (else
             (error 'trie/with "unknown node type ~s" entry)))))))



;; The hash table equivalent of car for lists.
;; Assuming that trie node t is nonempty return the first key/value pair found in a left-to-right
;; depth-first search of the trie rooted at t.
(define (trie/car t)
  (let ((entry (trie/slot t 0)))
    (cond
      ((trie/pair? entry) (trie/pair=>cons entry))
      ;((pair? entry) entry)
      ((trie? entry) (trie/car entry))
      ((overflow? entry) (vector-ref entry 0))
      (else (error 'trie/car "unknown node type ~s" entry)))))
  
;; The hash table equivalent of cdr for lists.
;; Assuming that trie node t is nonempty non-destructively return a replacement trie node t'
;; such that t' is the root of a trie constituting the "rest" of t.
(define (trie/cdr t)
  ;; The mask for the first (lowest order) bit set in a trie map.
  (define-syntax-rule (mask/first t) (<<< 1 (bitwise-first-bit-set (trie/map t))))

  (let ((entry (trie/slot t 0)))
    (cond
      ((trie/pair? entry) ;(pair? entry)
       (trie/shrink t 0 (mask/first t)))
      
      ((trie? entry)
       (let ((replacement (trie/cdr entry)))
         (cond
           ((eq? replacement entry) t)

           ((eq? trie/empty replacement)
            (trie/shrink t 0 (mask/first t)))

           (else
            (trie/replace t 0 replacement)))))
      
      ((overflow? entry)
       (let ((replacement
              ; At a minimum any overflow contains at least one key/value pair.
              (if (= (vector-length entry) 1)
                  (vector-ref entry 1)
                  (vector-copy entry 1)))) ; Shrink the overflow by discarding the first key/value pair.
         (trie/replace t 0 replacement)))
      
      (else
       (error 'trie/cdr "unknown node type ~s" entry)))))

;; Non-destructively delete slot i from trie t
;; Note: shrinking may leave us with a chain of trie nodes of which only the last in the chain
;; contains a key/value pair. It isn't clear that eliminating such chains is worth the bother.
(define (trie/shrink t i mask)
  (let ((n (trie/length t)))
    (cond
      ((= n 1)
       trie/empty)
      ((= n 2)
       ; A trivial optimization since the new trie node contains exactly one key/value pair.
       (let ((other (trie/slot t (if (zero? i) 1 0))))
         (vector (trie/unmap t mask) other)))
      (else
        (let ((contraction (trie/construct (trie/unmap t mask) (sub1 n)))
              (midpoint (+ i TRIE/SLOT/OFFSET)))
          (vector-copy! contraction TRIE/SLOT/OFFSET t TRIE/SLOT/OFFSET midpoint)
          (vector-copy! contraction midpoint t (add1 midpoint))
          contraction)))))

;; Non-destructively replace slot i of trie t with the given child node.
(define (trie/replace t i child)
  (let ((clone (vector-copy t)))
    (trie/slot! clone i child)
    clone))

;; Remove (non-destructively) the given key from the trie whose root is t.
(define (trie/without equality? t key hash shift)
  (let* ((mask (slot/mask hash shift))
         (i    (slots/before (trie/map t) mask)))
    (if (slot/free? t mask)
        t ; Key is not in this trie nor any of its descendants.

        (let ((entry (trie/slot t i)))
          (cond
            ((trie/pair? entry) ; (pair? entry)
             (if (equality? (trie/pair/key entry) key) ; (if (equality? (car entry) key)
                 (trie/shrink t i mask) ; Generate smaller trie.
                 t))                    ; Key not found. Return original trie.

            ((trie? entry)
             (let ((child (trie/without equality? entry key hash (+ shift 5))))
               (cond
                 ((eq? child entry) t)

                 ((eq? trie/empty child)
                  (trie/shrink t i mask))
                 
                 (else
                  (trie/replace t i child)))))

            ((overflow? entry)
             (let ((child (overflow/without equality? entry key)))
               (cond
                 ((eq? child entry) t)

                 ((or (trie? child) (overflow? child))
                  (trie/replace t i child))
                 (else
                  (trie/shrink t i mask)))))

            (else
             (error 'trie/without "unknown node type ~s" entry)))))))


;; Generates a new child node in event of a key collision between two distinct keys k_1, k_2.
;; That is, for a prior (lower) shift value S = 0, 5, 10, ... we had:
;;     (hash(k_1) >> S) & #x01f = (hash(k_2) >> S) & #x01f
;; in other words, both keys got hashed to the same slot i in a parent trie node t.

;; Returns a subtrie (at worst just a single overflow node) that resolves the collision.
;; h - parent hash table
;; alpha - key/value pair (k_1 . v_1)
;; key - key k_2
;; value - value v_2
;; hash - hash code hash(k_1)
;; shift - shift value S + 5
(define (child/new alpha alpha/hash key value key/hash shift)
  (if (>= shift HASH-BITS)
      ; We've exhausted the depth of the trie. Shove both pairs into an overflow node.
      (overflow/new alpha (trie/pair/new key value)) ;(overflow/new alpha (cons key value))
      
      (let ((alpha/slot (&& (>>> alpha/hash shift) #x01f))
            (beta/slot  (&& (>>> key/hash   shift) #x01f)))
        (if (= alpha/slot beta/slot)
            ; Collision again so generate a (sub)trie and try again.
            (let ((subtrie (trie/construct (<<< 1 alpha/slot) 1)))
              (trie/slot! subtrie 0 (child/new alpha alpha/hash key value key/hash (+ 5 shift)))
              subtrie)
            
            ; No collision (each "hashed" to a distinct map slot).
            ; Generate a trie containing each pair in a distinct slot.
            (let ((subtrie (trie/construct (|| (<<< 1 alpha/slot) (<<< 1 beta/slot)) 2)))
              (cond
                ((< alpha/slot beta/slot)
                 (trie/slot! subtrie 0 alpha)
                 ;(trie/slot! subtrie 1 (cons key value))) ; FIX!!!!
                 (trie/slot! subtrie 1 (trie/pair/new key value)))
                (else
                 ;(trie/slot! subtrie 0 (cons key value))  ; FIX!!!
                 (trie/slot! subtrie 0 (trie/pair/new key value))  ; FIX!!!
                 (trie/slot! subtrie 1 alpha)))
              subtrie)))))
           
;; Methods for the overflow node.

;; Locate a key within overflow node o.
;; Return the (key . value) pair if present and #f otherwise.
(define (overflow/get equality? o key)
  (let ((i (overflow/find equality? o key)))
    (if i (vector-ref o i) #f)))

;; Return a new overflow node containing key/value pairs alpha and beta.
(define (overflow/new alpha beta)
  (vector alpha beta))

;; Persistent (non-destructive) addition of a key/value pair to overflow node o.
(define (overflow/with equality? o key value)
  (cond
    ((overflow/find equality? o key)
     =>
     (lambda (i)
       (let ((clone  (vector-copy o)))
         ;(vector-set! clone i (cons key value))
         (vector-set! clone i (trie/pair/new key value))
         clone)))
    (else
     (let* ((n (vector-length o))
            (expansion (make-vector (add1 n) #f)))
       (vector-copy! expansion 0 o)
       ;(vector-set!  expansion n (cons key value))
       (vector-set!  expansion n (trie/pair/new key value))
       expansion))))

;; A specialized version of overflow/with where the value affilliated with the key is irrelevant
;; (useful, for example, when implementing unordered sets with hash tries).
(define (overflow/key/with equality? o key value)
  (if (overflow/find equality? o key)
      o ; key already in overflow node.
      (let* ((n (vector-length o))
             (expansion (make-vector (add1 n) #f)))
        (vector-copy! expansion 0 o)
        ;(vector-set!  expansion n (cons key value))
        (vector-set!  expansion n (trie/pair/new key value))
       expansion)))


;; Persistent (non-destructive) removal of key/value pair from overflow node o.
(define (overflow/without equality? o key)
  (let ((i (overflow/find equality? o key)))
    (if i
        ; Overflow o contains the (key . value) pair.
        (let ((n (vector-length o)))
          (if (= n 2)
              ; Return the solitary remaining (key . value) pair in overflow o.
              (vector-ref o (if (zero? i) 1 0))
              ; Return an overflow without that (key . value) pair.
              (let ((contraction (make-vector (sub1 n) #f)))
                (vector-copy! contraction 0 o 0 i)
                (vector-copy! contraction i o (add1 i))
                contraction)))
        o)))

;; Search for the given key among a set of overflows all of which hash to the same value
;; as the given key.
;; If the key matches overflow i = 0, 1, ... return i and if no match return #f.
(define (overflow/find equality? o key)
  (let ((slots o)
        (n (vector-length o)))
    (let loop ((i 0))
      (if (< i n)
          (if (equality? (trie/pair/key (vector-ref slots i)) key) ; (car (vector-ref slots i)) key)
              i
              (loop (add1 i)))
          #f))))

;; Fold function f with seed over all trie/pairs appearing in the trie rooted at x.
;; f accepts two arguments: a trie/pair and the most recent folded value.
;; The first invocation of f is given the seed as the initial folded value.
(define (trie/fold f seed x)
  (let loop ((entry x)
             (outcome seed))
    (cond
      ((trie/pair? entry)
       ; Entry is a trie/pair so just feed it to the fold function.
       (f entry outcome))
      
      ((trie? entry)
       ; Visit each trie slot.
       (let ((n (trie/length entry)))
         (let loop/trie ((i 0) (inner outcome))
           (if (< i n)
               (loop/trie (add1 i) (loop (trie/slot entry i) inner))
               inner))))
      
      ((overflow? entry)
       ; Visit each overflow slot and apply f to each trie/pair.
       (let ((n (vector-length entry)))
         (let loop/overflow ((i 0) (inner outcome))
           (if (< i n)
               (loop/overflow (add1 i) (f (vector-ref entry i) inner))
               outcome))))
      
      (else
       (error 'trie/fold "unknown node type: ~s" entry)))))

;; And function f over all trie/pairs appearing in the trie rooted at x.
;; f accepts a single trie/pair entry.
;; Returns #t if f returns #t for every trie/pair in x and #f otherwise.
;; Performs short-circuit evalution in that the combinator stops immediately
;; when f returns #f.
(define (trie/and f x)
  (let loop ((entry x))
    (cond
      ((trie/pair? entry) (f entry)); Entry is a trie/pair so just feed it to f.
      
      ((trie? entry)
       ; Visit each trie slot.
       (let ((n (trie/length entry)))
         (let loop/trie ((i 0))
           (if (< i n)
               (if (loop (trie/slot entry i)) (loop/trie (add1 i)) #f)
               #t))))
      
      ((overflow? entry)
       ; Visit each overflow slot and apply f to each trie/pair.
       (let ((n (vector-length entry)))
         (let loop/overflow ((i 0))
           (if (< i n)
               (if (f (vector-ref entry i)) (loop/overflow (add1 i)) #f)
               #t))))
      
      (else
       (error 'trie/fold "unknown node type: ~s" entry)))))

;; As in trie/and above except f is called as (f key value) for each key/value
;; pair in the trie rooted at x.
(define (trie/flat/and f x)
  (trie/and (lambda (e) (f (trie/pair/key e) (trie/pair/value e))) x))

;; Returns #t if (f x) is #t for at least one trie entry and
;; returns #f otherwise.
;; If the trie is empty then #f is returned.
(define (trie/or f x)
  (let loop ((entry x))
    (cond
      ((trie/pair? entry) (f entry)); Entry is a trie/pair so just feed it to f.
      
      ((trie? entry)
       ; Visit each trie slot.
       (let ((n (trie/length entry)))
         (let loop/trie ((i 0))
           (if (< i n)
               (if (loop (trie/slot entry i)) #t (loop/trie (add1 i)))
               #f))))
      
      ((overflow? entry)
       ; Visit each overflow slot and apply f to each trie/pair.
       (let ((n (vector-length entry)))
         (let loop/overflow ((i 0))
           (if (< i n)
               (if (f (vector-ref entry i)) #t (loop/overflow (add1 i)))
               #f))))
      
      (else
       (error 'trie/fold "unknown node type: ~s" entry)))))

(define (trie/flat/or f x)
  (trie/or (lambda (e) (f (trie/pair/key e) (trie/pair/value e))) x))

;; Apply function f, as (f key value), to each key/value in the trie.
(define (trie/for-each x f)
  (let loop ((item x))
    (cond
      ((trie/pair? item) (f (trie/pair/key item) (trie/pair/value item)))

      ((trie? item)
       ; Visit each slot of the trie.
       (let loop/trie ((i 0) (n (trie/length item)))
         (when (< i n)
           (loop (trie/slot item i))
           (loop/trie (add1 i) n))))
      
      ((overflow? item)
       ; Visit each overflow slot.
       (let loop/overflow ((i 0) (n (vector-length item)))
         (when (< i n)
           (loop (vector-ref item i))
           (loop/overflow (add1 i) n))))
      
      (else
       (error 'trie/for-each "unknown node type: ~s" item)))))

;; Similar to trie/pairs/fold above except that f is given three arguments:
;; key, value, and the most recent folded value.
;; This function helps to insulate higher-level structures such as hash maps and binding environments
;; from the internal representation that the trie uses for key/value pairs.
(define (trie/flat/fold f seed x)
  (trie/fold (lambda (e s) (f (trie/pair/key e) (trie/pair/value e) s)) seed x))

;; Return the contents of the hash table rooted at x as a list of trie/pair where the list is sorted
;; in increasing insertion order of the pairs.
(define (trie/sort/insertion x)
  (let ((all (trie/fold (lambda (pair seed) (cons pair seed)) null x))) ; Generate a list of all trie/pairs in the the trie.
    (sort all < #:key trie/pair/order)))

;; Like trie/sort/insertion but returns the contents in insertion order as a flat vector #(k_1 v_1 ... k_n v_n)
;; where pair k_i/v_i was added to the trie before pair k_{i+1}/v_{i+1}.
(define (trie/sort/insertion=>vector x)
  (let* ((all (trie/sort/insertion x))
         (v   (make-vector (* 2 (length all)) #f)))
    (let loop ((all all) (i 0))
      (cond
        ((null? all) v)
        (else
         (vector-set! v i        (trie/pair/key   (car all)))
         (vector-set! v (add1 i) (trie/pair/value (car all)))
         (loop (cdr all) (+ i 2)))))))
          
;; Return the total number of trie pairs in the trie rooted at x.
(define (trie/pairs/total x)
  (let loop ((entry x) (count 0))
    (cond
      ((trie/pair? entry) (add1 count)) ;(pair? entry) (add1 count))
      
      ((trie? entry)
       ; Visit each trie slot ... sigh.
       (let ((max (trie/length entry))) ; Total number of slots
         (let loop/trie ((i 0) (inner count))
           (if (< i max)
               (loop/trie (add1 i) (loop (trie/slot entry i) inner))
               inner))))
      
      ((overflow? entry) (+ (vector-length entry) count)) ; Overflow nodes contain nothing but key/value pairs.
      
      (else (error 'trie/count "unknown node type: ~s" entry)))))

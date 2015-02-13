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
 pairs/count
 pairs/fold)

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

;; Overflow nodes are just vectors #((k_0 . v_0) ... (k_j . v_j)) of key/value pairs.
;; Overflow nodes are created when the depth of the trie exceeds 6 (including the root).
;; Type test for overflow nodes.
(define-syntax-rule (overflow? o)
  (and
   (vector? o)
   (> (vector-length o) 1)
   (pair? (vector-ref o 0))))

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
(define (trie/empty? t)
  (= (vector-length t) 1))

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



;; Locate key/value pair in trie node t of hash h.
;; If found return the cons cell (key . value)
;; If not found return #f.
(define (trie/get equality? t key hash shift)
  (let ((mask (slot/mask hash shift))) ; Which hash slot 0 ... 31 should we try?
    (if (slot/free? t mask)
        #f ; That trie slot is empty. No such key in this trie.

        ; The key/value pair may be here. Look further.
        (let* ((i    (slots/before (trie/map t) mask))
               (slot (trie/slot t i)))
          (cond
            ((pair? slot)
             (if (equality? key (car slot)) slot #f))

            ((trie? slot)
             (trie/get equality? slot key hash (+ shift 5)))

            ((overflow? slot)
             (overflow/get equality? slot key))

            (else
             (error 'trie/get "unknown node type ~a" slot)))))))

;; Non-persistent (destructive) addition of key/value pair to trie node t of hash table h.
;(define (trie/add h t key value hash shift)
;  (let* ((mask (slot/mask hash shift))
;         (i    (slots/before (trie-map t) mask))) ; Number of occupied slots preceeding slot denoted by mask.
;    (if (zero? (&& (trie-map t) mask))
;        ; No such key in this trie so increase the number of slots by one.
;        (let ((extension (make-vector (add1 (vector-length (trie-slots t))))))
;          (vector-copy! extension 0 (trie-slots t) 0 i)
;          (vector-set!  extension i (cons key value))
;          (when (< i (bitwise-bit-count (trie-map t)))
;            (vector-copy! extension (add1 i) (trie-slots t) i))
;          (set-trie-map!   t (|| (trie-map t) mask))
;          (set-trie-slots! t extension))
;        
;        (let ((entry (vector-ref (trie-slots t) i)))
;          (cond
;            ((pair? entry)
;             (if (equality? h key (car entry))
;                 ; Replace existing key/value pair.
;                 (vector-set! (trie-slots t) i (cons key value))
;                 ; Collision (two distinct keys with the same hash).
;                 (vector-set! (trie-slots t) i (child/new h t entry key value hash (+ 5 shift)))))
;            ((trie? entry)
;             ; Slot is a trie node.
;             (vector-set!
;              (trie-slots t) i
;              (trie/with h entry key value hash (+ shift 5))))
;            ((overflow? entry)
;             ; Slot is an overflow node.
;             (vector-set!
;              (trie-slots t) i
;              (overflow/with h entry key value)))
;            (else
;             (error 'trie/add "unknown node type ~s" entry)))))))


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
          (vector-set!  extension midpoint (cons key value))
          (when (< i (slots/occupied t))
            (vector-copy! extension (add1 midpoint) t midpoint))
          extension)

        ; Slot i for the key is occupied.
        (let ((entry (trie/slot t i))
              (clone (vector-copy t))) ; Replacement trie node.
          (cond
            ((pair? entry)
             ; Slot i contains some (k . v) pair.
             (if (equality? key (car entry))
                 ; The two keys match. Replace prior k/v pair with key/value pair.
                 (trie/slot! clone i (cons key value))
                 ; Collision (two distinct keys with the same hash).
                 (trie/slot! clone i (child/new entry (hasher (car entry)) key value hash (+ shift 5))))
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
          (vector-set!  extension midpoint (cons key value))
          (when (< i (slots/occupied t))
            (vector-copy! extension (add1 midpoint) t midpoint))
          extension)

        ; Slot i for the key is occupied.
        (let ((entry (trie/slot t i)))
          (cond
            ((pair? entry)
             ; Slot i contains some (k . v) pair.
             (if (equality? key (car entry))
                 t ; The two keys match and that is all we care about.

                 ; Collision (two distinct keys with the same hash).
                 (let ((clone (vector-copy t)))
                   (trie/slot! clone i (child/new entry (hasher (car entry)) key value hash (+ shift 5)))
                   clone)))

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
      ((pair? entry) entry)
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
      ((pair? entry)
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
            ((pair? entry)
             (if (equality? (car entry) key)
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
      (overflow/new alpha (cons key value))
      
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
                 (trie/slot! subtrie 1 (cons key value)))
                (else
                 (trie/slot! subtrie 0 (cons key value))
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
         (vector-set! clone i (cons key value))
         clone)))
    (else
     (let* ((n (vector-length o))
            (expansion (make-vector (add1 n) #f)))
       (vector-copy! expansion 0 o)
       (vector-set!  expansion n (cons key value))
       expansion))))

;; A specialized version of overflow/with where the value affilliated with the key is irrelevant
;; (useful, for example, when implementing unordered sets with hash tries).
(define (overflow/key/with equality? o key value)
  (if (overflow/find equality? o key)
      o ; key already in overflow node.
      (let* ((n (vector-length o))
             (expansion (make-vector (add1 n) #f)))
        (vector-copy! expansion 0 o)
        (vector-set!  expansion n (cons key value))
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
          (if (equality? (car (vector-ref slots i)) key)
              i
              (loop (add1 i)))
          #f))))

;; Fold function f with seed over all pairs appearing in the trie rooted at x.
(define (pairs/fold f seed x)
  (let loop ((entry x)
             (outcome seed))
    (cond
      ((pair? entry)
       ; Entry is a (key . value) pair so just feed it to the fold function.
       (f entry outcome))
      
      ((trie? entry)
       ; Visit each trie slot.
       (let ((n (trie/length entry)))
         (let loop/trie ((i 0) (inner outcome))
           (if (< i n)
               (loop/trie (add1 i) (loop (trie/slot entry i) inner))
               inner))))
      
      ((overflow? entry)
       ; Visit each overflow slot and apply f to each (key . value) pair.
       (let ((n (vector-length entry)))
         (let loop/overflow ((i 0) (inner outcome))
           (if (< i n)
               (loop/overflow (add1 i) (f (vector-ref entry i) inner))
               outcome))))
      
      (else
       (error 'pairs/fold "unknown node type: ~s" entry)))))

;; Return the total number of key/value pairs in the trie rooted at x.
(define (pairs/count x)
  (let loop ((entry x)
             (count 0))
    (cond
      ((pair? entry) (add1 count))
      
      ((trie? entry)
       ; Visit each trie slot ... sigh.
       (let ((max (trie/length entry)))
         (let loop/trie ((i 0) (inner count))
           (if (< i max)
               (loop/trie (add1 i) (loop (trie/slot entry i) inner))
               inner))))
      
      ((overflow? entry) (+ (vector-length entry) count)) ; Overflow nodes contain nothing but key/value pairs.
      
      (else
       (error 'pairs/count "unknown node type: ~s" entry)))))

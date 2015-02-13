#lang racket

;; This implementation of "vector lists" is based upon a Guile implementation by Ludovic Courtès <ludo@gnu.org>
;; of a data structure invented by Phil Bagwell and described in his paper 
;; "Fast Functional Lists, Hash-Lists, Dequeues and Variable-Length Arrays",
;; Proceedings of the Implementation of Functional Languages, 14th International Workshop,
;; Madrid, Spain, September 2002.

;; Copyright and license of the Courtès implementation are:

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;; I (Michael Gorlick, mgorlick@acm.org) ported it to Racket 5.0, modified the basic block structure
;; to eliminate the indirect vector used by Courtès for the velist content, added the requisite
;; locking to make the implementation thread-safe, and eliminated the accomodations included in the basic
;; velist structure for vellist-based hash tables.

;; Copyright (C) 2010 Michael M. Gorlick

(require
 profile
 (only-in srfi/1 reverse!))

(provide
 velist?
 velist/cons velist/car velist/cdr velist/null?
 velist/null
 list/velist velist/ref velist/drop velist/take
 velist/length velist/fold/left velist/fold/right velist/map
 velist/append
 velist/reverse velist/filter velist/delete velist/list
 velist/for-each)


;; Multiplicative factor for block size as velist capacity increases.
(define block-growth-factor 2)

(define-syntax define-inline
  ;; Work around the lack of an inliner.
  (syntax-rules ()
    ((_ (name formals ...) body ...)
     (define-syntax name
       (syntax-rules ()
         ((_ formals ...)
          (begin body ...)))))))

;; Block constructor.
;; The base, offset, and size of a block are constant over the block lifespan.
;; The value of the "free" index may increase monotonically from 0 ... size over time.
;; The values in the content portion of the block, once set, are immutable over the block lifespan.
(define (block/new base offset size)
  (let ((v (make-vector (+ BLOCK/OFFSET/MIN size) #f)))
    (vector-set! v 0 base)      ; Prior block.
    (vector-set! v 1 offset)    ; Offset into content of prior block.
    (vector-set! v 2 size)      ; Size in content slots of this block.
    (vector-set! v 3 0)         ; Next free content slot 0, ...., size-1.
    (vector-set! v 4 (make-semaphore 1)) ; For thread-safety.
    ; Actual block content follows at slots [BLOCK/OFFSET/MIN ... BLOCK/OFFSET/MIN + size).
    v))

;; The universal (and unique) null block that terminates all velists.
(define BLOCK/NULL
  (vector
   #f   ; No prior block.
   #f   ; No offset into prior block
   0    ; Zero content slots allocated in this block
   #f   ; No free content slots in this block
   #f)) ; No semaphore in this block

(define-syntax define-block-accessor
  (syntax-rules ()
    ((_ name index)
     (define-inline (name block)
       (vector-ref block index)))))

(define-block-accessor block/base      0)
(define-block-accessor block/offset    1)
(define-block-accessor block/size      2)
(define-block-accessor block/free      3)
(define-block-accessor block/semaphore 4)

;; A block has the structure:
;;    base      --> immediately prior (and smaller, by factor of 2) block
;;    offset    --> slot index of last occupied slot in immediately prior block
;;    size      --> total number of content slots allocated in block (always 2^N, N = 0, 1, ...)
;;    free      --> index = 0, 1, ..., 2^N - 1 of next available content slot relative to start of slots
;;    semaphore --> binary semaphore for thread safety when adding content to block
;;    slot      --> content slot 0
;;    ...
;;    slot      --> content slot size-1 (2^N - 1 for some N = 0, 1, ...)

(define BLOCK/SIZE/MIN   1) ; Mininum capacity for any block other than BLOCK/NULL.
(define BLOCK/OFFSET/MIN 5) ; Smallest offset for any block.
(define BLOCK/GROWTH     2) ; Growth factor for all blocks, hence blocks have capacity 2^N, N = 0, 1, ...

;; Helper code.

;; Increment the block/free field of the block.
(define (block/free/increment! block)
  (vector-set! block 3 (add1 (block/free block))))

;; Append a value to the content portion of the block.
(define (block/append! block value)
  (vector-set! block (+ (block/free block) BLOCK/OFFSET/MIN) value)
  (block/free/increment! block)
  #t)

;; Fetch a value from the content portion of the block.
(define (block/content block offset)
  (vector-ref block (+ BLOCK/OFFSET/MIN offset)))

;; User-level opaque handle for a velist object.
;(struct velist (base offset) #:constructor-name velist/new)

(define (velist/new base offset) (vector '<velist> base offset))
(define-syntax-rule (velist-base v)   (vector-ref v 1))
(define-syntax-rule (velist-offset v) (vector-ref v 2))

(define (velist? v)
  (and (vector? v) (= (vector-length v) 3) (eq? (vector-ref v 0) '<velist>)))

;; The canonical empty velist.
(define velist/null (velist/new BLOCK/NULL 0))

;; Add a value to the head of a velist returning a fresh velist (the analogue of cons in the list domain).
(define (block/cons value velist)
  (let ((base (velist-base velist)))
    (cond 
      ((eq? base BLOCK/NULL)
       (let ((b (block/new BLOCK/NULL 0 BLOCK/SIZE/MIN)))
         (block/append! b value)
         (velist/new b 0)))
      (else
       (let ((semaphore (block/semaphore base)))
         (semaphore-wait semaphore)
         (let ((offset (add1 (velist-offset velist)))) ; Next free slot in block.
           (cond
             ((and (< offset (block/size base))
                   (= offset (block/free base))
                   (block/append! base value))
              (semaphore-post semaphore)
              (velist/new base offset))
              
              ((< offset (block/free base)) ; Offset of velist references some "tail" of the content.
               (semaphore-post semaphore)
               (let ((b (block/new base (sub1 offset) BLOCK/SIZE/MIN)))
                 (block/append! b value)
                 (velist/new b 0)))
               
              (else ; No more space in block b.
               (semaphore-post semaphore)
               (let ((b (block/new base (sub1 offset) (* BLOCK/GROWTH (block/size base)))))
                 (block/append! b value)
                 (velist/new b 0))))))))))

;; Velist user functions.

(define (velist/null? v)
  (eq? (velist-base v) BLOCK/NULL))

(define (velist/cons item v) ; SWAPPED!
  (block/cons item v))

(define (velist/car velist)
  (let ((base (velist-base velist)))
    (if (eq? base BLOCK/NULL)
        (error 'velist/car "expects argument of type <velist>; given velist/null")
        (vector-ref base (+ (velist-offset velist) BLOCK/OFFSET/MIN)))))

(define (velist/cdr velist)
  (let ((base (velist-base velist))
        (offset (velist-offset velist)))
    (cond
      ((eq? base BLOCK/NULL)
       (error "velist/cdr: expects argument of type <velist>; given velist/null"))
      ((positive? offset)
       (velist/new base (sub1 offset)))
      (else
       (velist/new (block/base base) (block/offset base))))))

;; Fold function f with the seed over velist v from left to right (in order of increasing index).
(define (velist/fold/left f seed v)
  (let loop ((base    (velist-base v))
             (offset  (velist-offset v))
             (outcome seed))
    (if (eq? base BLOCK/NULL)
        outcome
        (let* ((i (sub1 offset))
               (done? (negative? i)))
          (loop
           (if done? (block/base base)   base)         ; base
           (if done? (block/offset base) i)            ; offset
           (f (vector-ref base (+ offset BLOCK/OFFSET/MIN)) outcome))))))  ; outcome

;; Given a velist v with contents a_0, a_1, ..., a_{n-1) return a new velist u with contents a_{n-1}, ..., a_1, a_0.
(define (velist/reverse v)
  (velist/fold/left velist/cons velist/null v))

;; Given list L (a_0 a_1 ... a_{n-1}) return a new velist with contents a_0, a_1, ..., a_{n-1}.
(define (list/velist L)
  (velist/reverse (foldl velist/cons velist/null L)))

;; Given a velist v with contents a_0, a_1, ..., a_{n-1} return a list (a_0 a_1 ... a_{n-1}).
(define (velist/list v)
  (reverse! (velist/fold/left cons null v))) ; Destructive reverse perfectly safe here.

;; Fold function f with the seed over velist v from  right to left (in order of decreasing index).
(define (velist/fold/right f seed v)
  (velist/fold/left f seed (velist/reverse v)))

;; Return the value at reference position n = 0, 1, ... for velist v.
(define (velist/ref v n)
  (let loop ((index n)
             (base   (velist-base v))
             (offset (velist-offset v)))
    (cond
      ((eq? base BLOCK/NULL)
       (error 'velist/ref "index ~s out of range" n))
      ((<= index offset)
       (vector-ref base  (+ (- offset index) BLOCK/OFFSET/MIN))) ; Work backwards.
      (else
       ; The element is in a prior block.
       (loop
        (- index offset 1)
        (block/base base)
        (block/offset base))))))
    
;; Returns the total number of blocks in velist v including the null block.
(define (velist/block/count v)
  (let loop ((n 1)
             (base (velist-base v)))
    (if (eq? base BLOCK/NULL)
        n
        (loop (add1 n) (block/base base)))))

;; The velist analogue of list length.
(define (velist/length v)
  (let loop ((base    (velist-base v))
             (length  (velist-offset v)))
    (if (eq? base BLOCK/NULL)
        length
        (loop
         (block/base base)
         (+ length 1 (block/offset base))))))

;; Map f over the contents, a_0, a_1, ..., a_{n-1} of velist v and
;; return a new velist containing f(a_0), f(a_1), ..., f(a_{n-1}).
(define (velist/map f v)
  (velist/fold/left
   (lambda (x result) (velist/cons (f x) result))
   velist/null
   (velist/reverse v)))

;; Return a new velist that does not contain the first count elements of v.
(define (velist/drop v count)
  (let loop ((n  count)
             (base   (velist-base v))
             (offset (velist-offset v)))
    (cond
      ((eq? base BLOCK/NULL)
       (error 'velist/drop "count ~s too large for velist ~s" count (velist/list v)))
      ((<= n offset)
       (velist/new base (- offset n)))
      (else
       (loop
        (- n offset 1)
        (block/base base)
        (block/offset base))))))

;; Return a new velist containing the first count elements of v.
(define (velist/take v count)
  (let loop ((n count)
             (tail v)
             (final velist/null))
    (cond
      ((zero? n)
       (velist/reverse final))
      ((velist/null? tail)
       (error 'velist/take "count ~s too large for velist ~s" count (velist/list v)))
      (else
       (loop
        (sub1 n)
        (velist/cdr tail)
        (velist/cons (velist/car tail) final))))))

;; Used for debugging the implementation. May be handy for users.
(define (velist/dump v)
  (display (format "offset: ~a\n" (velist-offset v)))
  (let loop ((block (velist-base v)))
    (if (eq? block BLOCK/NULL)
        (display "null block\n")
        (begin
          (pretty-display (cdr (vector->list block)))
          (loop (block/base block))))))
        
;; Return a velist containing only those elements of source that satisfy the predicate.
(define (velist/filter predicate source)
  (velist/fold/right
   (lambda (e v) (if (predicate e) (velist/cons e v) v)) ; Filter
   velist/null                                           ; Seed
   source))

;; Return a velist containing only those elements that do not satisfy the predicate.
(define (velist/delete predicate source)
  (velist/fold/right
   (lambda (e v) (if (predicate e) v (velist/cons e v)))
   velist/null
   source))

(define (velist/append . velists)
  "Append the given lists."
  (if (null? velists)
      velist/null
      (foldr
       (lambda (velist result)
         (velist/fold/right
          (lambda (e v) (velist/cons e v)) result velist))
       velist/null
       velists)))

;; Invoke the procedure on each element of velist v in order.
(define (velist/for-each procedure v)
  (velist/fold/left
   (lambda (item _) (procedure item))
   (void)
   v))

(define (velist/build f n)
  (let loop ((n n) (v velist/null))
    (if (zero? n)
        v
        (loop (sub1 n) (velist/cons (f n) v)))))
        

(define list/1000 (build-list 1000 (lambda (i) i)))
;(define vector/100000 (make-vector 1000 #f))
;(define vector/1000 (make-vector 1000 #f))
(define vector/10000 (make-vector 10000 #f))
(define velist/1000 (list/velist list/1000))

(define x (list/velist '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) ; For debugging.

;; Index a vector from front to back 10,000 times.
(define (test/vector/index v)
  (let loop ((i 0)
             (max (vector-length v)))
    (when (< i max)
      (vector-ref v i)
      (loop (add1 i) max))))

(define (test/velist/index v)
  (let loop ((i 0)
             (max (velist/length v)))
    (when (< i max)
      (velist/ref v i)
      (loop (add1 i) max))))

;; Vector indexing is so cheap that it is essentially invisible, probably only a few nanoseconds per index.
;(profile-thunk (lambda () (test/vector/index vector/10000)) #:repeat 1000)

;; 4614 ms = 10^7 total indexes = 461.4 nanosecs/index
;; 29,387 ms = 10^8 total indexes = 294 nanosecs/index
;(profile-thunk (lambda () (test/velist/index velist/1000)) #:repeat 100000)

;(define (test/velist)
;  ; Construct a list of 1000 elements.
;  (define (test/velist/1)
;    (let ((ordinary (build-list 1000 values)))
    

                        
 

 


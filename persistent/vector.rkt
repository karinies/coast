#lang racket/base

;; For performance testing only.
;(require
; profile
; srfi/42)

(require (only-in racket/vector vector-copy))
(provide
 list/vector
 vector/build
 vector/fold/left
 vector/fold/right
 vector/persist?
 vector/length
 vector/list
 vector/null
 vector/null?
 vector/cons
 vector/cdr
 vector/filter
 vector/map
 vector/ref
 vector/subvector
 vector/update
 vector=>vector/racket
 vector/racket=>vector/persist)

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

;; An implementation of the persistent vector, invented by Rich Hickey and
;; first implemented in Clojure (www.clojure.org). The persistent vector has its
;; roots in the "Ideal Hash Tries" described by Phil Bagwell.

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


;; count - Total number of values in persistent vector
;; shift - 5*(h + 1) where h is the height of the trie (h = 0 for an empty trie)
;; root  - an array of subtries
;; tail  - a vector cache of upto the last 32 values in the persistent vector
(define-accessor vepersist/count 1)
(define-accessor vepersist/shift 2)
(define-accessor vepersist/root  3)
(define-accessor vepersist/tail  4)
(define vector/zero #())
(define-syntax-rule (vepersist/new count shift root tail)
  (vector '<vector/persist> count shift root tail))

(define-inline (vepersist/count++ v)
  (vector-set! v 1 (add1 (vepersist/count v))))
(define-inline (vepersist/tail! v t)
  (vector-set! v 4 t))

(define (vector/persist? v)
  (and
   (vector? v)
   (= (vector-length v) 5)
   (eq? (vector-ref v 0) '<vector/persist>)))



;(struct vepersist (count shift root tail) #:constructor-name vepersist/construct)

(define vector/null (vepersist/new 0 5 vector/zero vector/zero))

(define (vector/null/new) (vepersist/new 0 5 vector/zero vector/zero)) ; Used by the Motile deserializer.

(define (vector/null? v) (eq? v vector/null))

;; Grow vector r by 1 with x as its new last value.
(define-syntax-rule (root/expand r x)
  (let* ((n (vector-length r))
         (successor (make-vector (add1 n) #f)))
    (vector-copy! successor 0 r)
    (vector-set!  successor n x)
    successor))

;; Append the value to the end of persistent vector v.
(define (vector/cons v value)
  (let ((n (vector-length (vepersist/tail v))))
    (if (< n 32)
        ; The tail cache has room for the additional element.
        (let ((tail (make-vector (add1 n) #f)))
          (vector-copy! tail 0 (vepersist/tail v))
          (vector-set! tail n value)
          (vepersist/new (add1 (vepersist/count v)) (vepersist/shift v) (vepersist/root v) tail))
        
        ; No room in the tail cache so we must adjust the root.
        (let-values
            ([(root expansion)
              (tail/push 
               (- (vepersist/shift v) 5)
               (vepersist/root v)
               (vepersist/tail v)
               #f)])
          (if expansion
              ; The tree is one level deeper and has expanded on the right.
              (vepersist/new
               (add1 (vepersist/count v)) ; count
               (+ (vepersist/shift v) 5)  ; shift (increased by one level)
               (vector root expansion)    ; prior root pushed down one level 
               (vector value))            ; tail (new)
              
              ; There was room in the root for a branch comprising the old tail.
              (vepersist/new
               (add1 (vepersist/count v)) ; count
               (vepersist/shift v)        ; shift
               root                       ; root
               (vector value)))))))       ; tail (new)

;; Append the value to the end of persistent vector v.
(define (vector/cons/destructive v value)
  (let ((n (vector-length (vepersist/tail v))))
    (if (< n 32)
        ; The tail cache has room for the additional element.
        (let ((tail (make-vector (add1 n) #f)))
          (vector-copy! tail 0 (vepersist/tail v))
          (vector-set! tail n value)
          (vepersist/count++ v)
          (vepersist/tail! v tail)
          v)
        
        ; No room in the tail cache so we must adjust the root.
        (let-values
            ([(root expansion)
              (tail/push 
               (- (vepersist/shift v) 5)
               (vepersist/root v)
               (vepersist/tail v)
               #f)])
          (if expansion
              ; The tree is one level deeper and has expanded on the right.
              (vepersist/new
               (add1 (vepersist/count v)) ; count
               (+ (vepersist/shift v) 5)  ; shift (increased by one level)
               (vector root expansion)    ; prior root pushed down one level 
               (vector value))            ; tail (new)
              
              ; There was room in the root for a branch comprising the old tail.
              (vepersist/new
               (add1 (vepersist/count v)) ; count
               (vepersist/shift v)        ; shift
               root                       ; root
               (vector value)))))))       ; tail (new)


(define (list/vector v values)
  (let loop ((result (vector-copy v)) (values values))
    (if (null? values)
        result
        (loop (vector/cons/destructive result (car values)) (cdr values)))))

;; Convert a Racket vector to a persistent vector. Used by the Motile deserializer.
(define (vector/racket=>vector/persist r)
  (let loop ((i 0) (n (vector-length r)) (result (vector/null/new)))
    (when (< i n)
      (loop (add1 i) n (vector/cons/destructive result (vector-ref r i))))
    result))

(define-syntax-rule (tail/offset v)
  (- (vepersist/count v) (vector-length (vepersist/tail v))))

;; Return the value of element i in persistent vector v.
(define (vector/ref v i)
  (if (and (<= 0 i) (< i (vepersist/count v)))
      (if (<= (tail/offset v) i)
          ; The item we want is in the tail cache.
          (vector-ref (vepersist/tail v) (bitwise-and i #x01f))
          
          (do ((root 
                (vepersist/root v)
                (vector-ref root (bitwise-and (arithmetic-shift i (- level)) #x01f)))
               (level
                (vepersist/shift v)
                (- level 5)))
            ((= level 0) (vector-ref root (bitwise-and i #x01f)))))
          
          (error 'vector/ref "index ~s out of range [0, ~s)" i (vepersist/count v))))
  
(define (vector/length v) (vepersist/count v))
  
;; Remove the tail (last) element of persistent vector v.
(define (vector/cdr v)
  (cond
    ((vector/null? v)
     ; v is the empty vector.
     (error 'vector/cdr "persistent vector is null"))
    ((= 1 (vepersist/count v))
     ; v is the unit vector, that is, a vector with only one element
     vector/null)
    (else
     ; v is a non-trivial vector.
     (let ((n (vector-length (vepersist/tail v))))
       (if (> n 1)
           (vepersist/new
            (sub1 (vepersist/count v))
            (vepersist/shift v)
            (vepersist/root v)
            (vector-copy (vepersist/tail v) 0 (sub1 n)))
           
           (let-values
               ([(root tail) (tail/pop (- (vepersist/shift v) 5) (vepersist/root v) #f)])
             (if (and (> (vepersist/shift v) 5) (= (vector-length root) 1))
                 (vepersist/new
                  (sub1 (vepersist/count v)) ; count
                  (- (vepersist/shift v) 5)  ; shift
                  vector/zero                ; root
                  tail)                      ; tail
                 
                 (vepersist/new
                  (sub1 (vepersist/count v)) ; count
                  (vepersist/shift v)        ; shift
                  root                       ; root
                  tail))))))))               ; tail
  
;(define (list/vector values)
;  (let loop ((v vector/null)
;             (values values))
;    (if (null? values)
;        v
;        (loop (vector/cons (car values) v) (cdr values)))))

(define-syntax-rule (vector/ref/last v) (vector-ref v (sub1 (vector-length v))))

(define-syntax-rule (root/clone r x)
  (let ((clone (vector-copy r))
        (last  (sub1 (vector-length r))))
    (vector-set! clone last x)
    clone))

  
;; Called when the tail cache is exhausted and we need to adjust the tree to make room for an additional
;; vector element.
;; Returns the values r, e where if e is #f then r is the new top-level root of the successor persistent vector.
;; Otherwise the new top-level root of the successor will be #(r e) where r is the original root and e the
;; rightmost expansion.
(define (tail/push level root tail expansion)
  (cond
    ((zero? level)
     (if (= 32 (vector-length root))
         ; root = #(s_0 ... s_31) where each s_i is a subtree.
         ; tail = #(e_0 ... e_31) where e_i is a leaf value.
         ; Inform the caller to construct the successor root as #(#(s_0 ... s_31) #(#(e_0 ... e_31)))
         ; In other words, increase the depth of the tree by one level and expand on the rightmost side.
         (values root (vector tail))
         
         ; root = #(s_0 ... s_m), m < 31 where each s_i is a subtree.
         ; tail = #(e_0 ... e_31) where each e_i is a leaf value.
         ; The successor root will be #(s_0 ... s_m tail).
         ; No expansion is required as the original root had sufficient room.
         (values (root/expand root tail) #f)))
    
    (else ; level > 0
     (let-values
         ; Go down one level of the tree expanding into the rightmost subtree of this root.
         ([(child subexpansion)
           (tail/push (- level 5) (vector/ref/last root) tail expansion)])
       (cond
         ((not subexpansion)
          ; Replace the rightmost subtree of the root with an expanded subtree.
          (values (root/clone root child) #f))
         
         ((= 32 (vector-length root))
          ; Root is full so push it down one level.
          (values root (vector subexpansion)))
         
         (else
          ; There is sufficient room in the root for an additional subtree.
          (values (root/expand root subexpansion) #f)))))))

(define (tail/pop shift root tail)
  (cond
    ((positive? shift)
     (let-values
         ([(child subtail)
           (tail/pop (- shift 5) (vector/ref/last root) tail)])
       (if child
           (let ((clone (make-vector (add1 (vector-length root)))))
             (vector-copy! clone 0 root)
             (vector-set! clone (vector-length root) child)
             (values clone subtail))
           
           (if (= 1 (vector-length root))
               (values vector/zero subtail)
               (values
                (vector-copy root 0 (sub1 (vector-length root)))
                subtail)))))
    ((zero? shift)
     (if (= 1 (vector-length root))
         (values vector/zero (vector/ref/last root))
         (values
          (vector-copy root 0 (sub1 (vector-length root)))
          (vector/ref/last root))))
    (else
     (if (= 1 (vector-length root))
         (values vector/zero tail)
         (values
          (vector-copy root 0 (sub1 (vector-length root)))
          tail)))))

(define (vector/update v i value)
  (cond
    ((and (<= 0 i) (< i (vepersist/count v)))
     (if (<= (tail/offset v) i)
         ; The vector index to update is in the tail cache.
         (let ((tail (vector-copy (vepersist/tail v) 0)))
           (vector-set! tail (bitwise-and i #x01f) value)
           (vepersist/new
            (vepersist/count v)
            (vepersist/shift v)
            (vepersist/root v)
            tail))
         
         ; The vector index to update is deep within the tree.
         (vepersist/new
          (vepersist/count v)
          (vepersist/shift v)
          (update/deep (vepersist/shift v) (vepersist/root v) i value) ; root replacement
          (vepersist/tail v))))
    
    ((= i (vepersist/count v))
     ; Simply extend the persistent vector with the value.
     (vector/cons v value))
    
    (else
     (error 'vector/update "index ~s out of range [0, ~s)" i (vepersist/count v)))))

(define (update/deep level root i value)
  (let ((clone (vector-copy root 0)))
    (if (zero? level)
        (vector-set! clone (bitwise-and i #x01f) value)
        (let ((subindex (bitwise-and (arithmetic-shift i (- level)) #x01f)))
          (vector-set!
           clone subindex
           (update/deep (- level 5) (vector-ref root subindex) i value))))
    clone))

;; Returns a vector w containing those elements e, in index order, iff (p e) is not #f.
(define (vector/filter v p)
  (let loop ((i 0)
             (n (vector/length v))
             (outcome vector/null))
    (if (< i n)
        (let ((e (vector/ref v i)))
          (if (p e)
              (loop (add1 i) n (vector/cons outcome e))
              (loop (add1 i) n outcome)))
        outcome)))

(define (vector/map v f)
  (let loop ((i 0)
             (n (vector/length v))
             (outcome vector/null))
    (if (< i n)
        (loop (add1 i) n (vector/cons outcome (f (vector/ref v i))))
        outcome)))

(define (vector/fold/left v f seed)
  (let loop ((i 0)
             (n (vector/length v))
             (outcome seed))
    (if (< i n)
        (loop (add1 i) n (f (vector/ref v i) outcome))
        outcome)))

(define (vector/fold/right v f seed)
  (let loop ((i 0)
             (n (vector/length v))
             (outcome seed))
    (if (< i n)
        (loop (add1 i) n (f (vector/ref v (- n i 1)) outcome))
        outcome)))

(define (vector/list v)
  (vector/fold/right v (lambda (x tail) (cons x tail)) null))

(define (vector=>vector/racket v)
  (let ((r (make-vector (vector/length v) #f)))
    (vector/fold/left v (lambda (x i) (vector-set! r i x) (add1 i)) 0)
    r))

(define vector/subvector
  (case-lambda
    ((v from)
     (let ((end (vector/length v)))
       (if (<= 0 from end)
           (let loop ((i from)
                      (outcome vector/null))
             (if (< i end)
                 (loop (add1 i) (vector/cons outcome (vector/ref v i)))
                 outcome))
           
           (error 'vector/subvector "invalid range [~s .. ~s)" from end))))
    ((v from end)
     (if (<= 0 from end (vector/length v))
         (let loop ((i from)
                    (outcome vector/null))
           (if (< i end)
               (loop (add1 i) (vector/cons outcome (vector/ref v i)))
               outcome))
         
         (error 'vector/subvector "invalid range [~s .. ~s)" from end)))))

(define (vector/build n f)
  (let loop ((i 0)
             (outcome vector/null))
    (if (< i n)
        (loop (add1 i) (vector/cons outcome (f i)))
        outcome)))

;  (define  (vector/dump v)
;    (let ((root (vepersist-root v))
;          (shift (vepersist-shift v)))
;      (pretty-display
;       (list
;        'count (vepersist-count v)
;        'shift (vepersist-shift v)
;        'tail  (vepersist-tail v)))
;      (let loop ((shift shift)
;                 (root root)
;                 (i 0)
;                 (n (vector-length root)))
;        (if (= shift 5)
;            (display (format "~a: ~a\n" (vector-length root) root))
;            
;            (when (< i n)
;              (loop (- shift 5) (vector-ref root i) 0 (vector-length (vector-ref root i)))
;              (loop shift root (add1 i) n))))))
;  
;(define x (list/vector vector/null
;           '(0 1 2 3 4 5 6 7 8 9
;               10 11 12 13 14 15 16 17 18 19
;               20 21 22 23 24 25 26 27 28 29
;               30 31 32 33 34 35 36 37 38 39)))
;  
;  (define y (list/vector (build-list 1080 (lambda (i) i))))
;  (define vepersist/10000 (vector/build 10000 (lambda (i) i)))
;  (define vector/10000 (vector->immutable-vector (make-vector 10000 #f)))
;  
;  (define (test/vector/ref v)
;    (let loop ((i 0)
;               (max (vector-length v)))
;      (when (< i max)
;        (nothing (vector-ref v i))
;        (loop (add1 i) max))))
;  
;  (define (nothing x) x)
;  
;  
;  (define (test/vector/skeleton v)
;    (let loop ((i 0)
;               (max (vector-length v)))
;      (when (< i max)
;        ;(vector-ref v i)
;        (loop (add1 i) max))))
;  
;  
;  (define (test/vepersist/ref v)
;    (let loop ((i 0)
;               (max (vector/length v)))
;      (when (< i max)
;        (vector/ref v i)
;        (loop (add1 i) max))))
;  
;  
;  (define (repeat n thunk)
;    (when (positive? n)
;      (thunk)
;      (repeat (sub1 n) thunk)))
;  
;  ; 7593 msecs for 10^7 accesses
;  ; 77430 msecs for 10^8 accesses
;  ; 75608 msecs for 10^8 accesses
;  ;(time-apply repeat (list 10000 (lambda () (test/vepersist/ref vepersist/10000))))
;  
;  
;  ; 398 msecs for 10^7 accesses
;  ; 3991 msecs for 10^8 accesses
;  ; 4599 msecs for 10^8 accesses for immutable vector
;  ;(time-apply repeat (list 10000 (lambda () (test/vector/ref vector/10000))))
;  
;  ; 2681 msecs for 10^8 repetitions
;  ;(time-apply repeat (list 10000 (lambda () (test/vector/skeleton vector/10000))))
;  
;  (define matrix/size 46)
;  
;  (define matrix/vector
;    (vector->immutable-vector
;     (build-vector (* matrix/size matrix/size) (lambda (i) i))))
;  ;     (vector-ec
;  ;      (:range x 0 matrix/size)
;  ;      (:range y 0 matrix/size)
;  ;      (* x y))))
;  
;  (define matrix/vepersist
;    (list/vector (vector->list matrix/vector)))
;  
;  (define (vector/multiply)
;    (sum-ec
;     (:range row 0 matrix/size)
;     (:range col 0 matrix/size)
;     (:range elt 0 matrix/size)
;     (* (vector-ref matrix/vector (+ (* row matrix/size) elt))
;        (vector-ref matrix/vector (+ (* elt matrix/size) col)))))
;  
;  (define (vepersist/multiply)
;    (sum-ec
;     (:range row 0 matrix/size)
;     (:range col 0 matrix/size)
;     (:range elt 0 matrix/size)
;     (* (vector/ref matrix/vepersist (+ (* row matrix/size) elt))
;        (vector/ref matrix/vepersist (+ (* elt matrix/size) col)))))
;  
;  (time (vector/multiply))
;  (time (vepersist/multiply))
;  
;  (define (inorder v)
;    (let loop ((i 0)
;               (max (sub1 (vector/length v))))
;      (if (< i max)
;          (if (= (add1 (vector/ref v i)) (vector/ref v (add1 i)))
;              (loop (add1 i) max)
;              (display (format "out of order ~a: ~a ~a: ~a" i (vector/ref v i) (add1 i) (vector/ref v (add1 i)))))
;          #t)))
;  
;  
;  (define x/1056 (vector/build 1056 (lambda (i) i))) ; shift = 5
;  (define x/1057 (vector/build 1057 (lambda (i) i)))
;  (define x/2048 (vector/build 2048 (lambda (i) i))) ; shift = 10
;  (define x/2049 (vector/build 2049 (lambda (i) i)))
;  (define x/2080 (vector/build 2080 (lambda (i) i)))
;  (define x/2081 (vector/build 2081 (lambda (i) i)))
;  
;  (list
;   (inorder x/1056)
;   (inorder x/1057)
;   (inorder x/2048)
;   (inorder x/2049)
;   (inorder x/2080)
;   (inorder x/2081))
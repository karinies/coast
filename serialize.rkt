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

;; This implementation borrows heavily from Racket v5.0/collects/racket/private/serialize.rkt
;; with numerous modifications to accommodate the serialized representations of Motile mobile
;; closures, continuations, and binding environments.
;; Many of the comments are paraphrases of the Racket Reference, Section 12.11, "Serialization."

(require racket/pretty) ; For test and debugging only.

(require
 racket/contract/base
 racket/port
 (only-in "getters.rkt" struct/getters/define)
 (only-in "Motile/generate/baseline.rkt" motile/decompile)
 (only-in "Motile/compile/recompile.rkt" motile/recompile motile/recompile/active?)

 ;"bindings/fastlz/fastlz.rkt"
 "bindings/libsodium/crypto.rkt"
 "curl/base.rkt"
 "curve.rkt"
 "Island/accessor.rkt"
 "Island/keystore.rkt"
 "time.rkt"
 ;"this.rkt"
 "islet.rkt"
 "promise/base.rkt"
 "zpl.rkt"

 "persistent/hash.rkt"
 "persistent/environ.rkt"
 "persistent/set.rkt"
 (only-in "persistent/vector.rkt" vector/persist? vector/length vector/fold/left vector/racket=>vector/persist)
 "comet/comet-types.rkt"
 [only-in "Island/island-comet.rkt" island/monitoring/log])
 
(provide 
 motile/serializable?
 motile/serialize
 (contract-out
  [struct
   motile/flat
   ((version serialize/version/c)
    (total exact-nonnegative-integer?)
    (objects vector?) (fixups list?) (final any/c))]
  [motile/flat/reconstruct (-> motile/flat/reconstruct/c  motile/flat?)]
  [motile/flat-to-bytes (-> motile/flat? bytes?)]
  [motile/deserialize (-> motile/flat? any/c)]
  [motile/deserialize/offline
   (-> motile/flat? (or/c accessor/c hash/eq?) curve? (or/c keystore/c hash/equal?) any/c)])
 motile/serialize/version)

(define serialize/version/c
  (flat-named-contract
   'serialize/version
   (vector/c
    exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?
    #:immutable #t #:flat? #t)))

(define (motile/serialize/version) #(1 0 0)) ; major/minor/patch

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; serialize
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (motile/serializable? v)
  (or 
   ; Primitive values.
   (boolean? v)
   (null? v)
   (number? v)
   (char? v)
   (symbol? v)
   (string? v)
   (bytes? v)

   ; Basic persistent functional data structures.
   (vector/persist? v) ; Persistent functional vector.
   (hash/persist? v)   ; Persistent functional hash table.
   (environ? v)        ; Binding environment.
   (set/persist? v)    ; Persistent functional set.

   ; Capability URLs (CURLs).
   (and (curl? v) (not (curl/embargo? v)))
   
   (time/utc? v)

   ; Fundamental structured values.
   (vector? v) ; Covers Motile tuples as well since all tuples are immutable vectors.
   (pair? v)
   (box? v)

   (void? v)
   (procedure? v)))

;; In Motile a mutable object is any box or vector
;; that is not immutable.
(define (mutable? o)
  (and
   (or (box? o) (vector? o))
   (not (immutable? o))))

(define (vector/map v f)
  (let ((n (vector-length v)))
    (let loop ((i 0) (result (make-vector n #f)))
      (cond
        ((< i n)
         (vector-set! result i (f (vector-ref v i)))
         (loop (add1 i) result))
        (else result)))))

(define (vector/for-each v f)
  (let loop ((i 0) (n (vector-length v)))
    (when (< i n)
      (f (vector-ref v i))
      (loop (add1 i) n))))

;; Find a mutable object among those that contained in the current cycle.
;; v - an object known to be immutable
;; cycle/stack - a non-empty list of candidate mutable objects (at a minimum it contains v).
;;   Anything above v in the stack is from the traversal of the substructure of v.
;;   To reconstruct a cycle containing v we must locate a mutable x that we can "patch"
;;   during deserialization to reconstruct the cycle.
(define (mutable/find v stack) 
  (let ([o (car stack)])
    (cond
      [(eq? o v) (error 'motile/serialize "cannot serialize cycle of immutable values: ~e" v)] ; Everything above v in the stack is immutable.
      [(mutable? o) o]
      [else (mutable/find v (cdr stack))])))

;; Compute the next share id, an index into the share portion of the serialization structure.
;(define (share/id/new share cycle)
;  (+ (hash-count share) (hash-count cycle)))

;; Traverse object v depth-first to identify cycles, sharing and to collect all actors and locatives.
;; Shared objects go in the `share' table, and mutable objects that are
;; cycle-breakers go in `cycle' table.
;; In each case, the object is mapped to a number that is
;; incremented as shared/cycle objects are discovered, so
;; when the objects are deserialized, we build them in reverse order.
;;
;; cycle - Hash map containing all objects o that are members of cycles. It maps o => <share-id>
;;   where share-id is a unique integer i, the index of o in the share portion of the serialization structure.
;; share - Hash map containing all shared objects o discovered in the depth-first tour rooted at v.
;; active - The set of all locatives and actors discovered over the course of the traversal.
;;    If #f then we don't bother capturing actors and locatives.
;
;(define (xtraverse v cycle share actors)
;  (let loop ((v v)
;             (cycle cycle)
;             (cycle/candidates hash/eq/null)
;             (share share)
;             (share/candidates hash/eq/null)
;             (actors actors)
;             (cycle/stack null))
;    
;    (cond
;      [(or
;        (boolean? v)
;        (number? v)
;        (char? v)
;        (symbol? v)
;        (null? v)
;        (void? v))
;       (values cycle share actors)]
;      
;      [(actor? v)
;       (values
;        cycle cycle/candidates
;        share share/candidates
;        (hash/cons actors (actor/id v) v)
;        cycle/stack)]
;      
;      [(locative? v)
;       (values
;        cycle cycle/candidates
;        share share/candidates
;        (hash/cons actors (locative/id v) v)
;        cycle/stack)]
;      
;      [(hash/contains? cycle v)
;       ; v is part of a cycle so it is already accounted for.
;       (values
;        cycle cycle/candidates
;        share share/candidates
;        actors
;        cycle/stack)]
;      
;      [(hash/contains? cycle/candidates v)
;       ; We've seen v earlier in our depth-first search so it must be a member of a cycle.
;       (let* ([mutable (if (mutable? v) v (mutable/find v cycle/stack))]  
;              ; A mutable will be used to break the cycle.
;              [cycle (hash/cons cycle mutable (share/id/new share cycle))]
;              [share
;               (if (eq? mutable v)
;                   v
;                   ; As v and mutable are distinct v is potentially shared (refered to) by other objects.
;                   (hash/cons share v (share/id/new share cycle)))])
;         (values cycle cycle/candidates share share/candidates actors cycle/stack))]
;      
;      [(hash/contains? share v)
;       ; Since v is known to be shared ignore it.
;       (values cycle cycle/candidates share share/candidates actors cycle/stack)]
;      
;      [(hash/contains? share/candidates v)
;       (values
;        cycle cycle/candidates
;        ; Object v is shared (refered to) by at least two objects.
;        (hash/cons share v (share/id/new share cycle)) share/candidates
;        actors
;        cycle/stack)]
;      
;      [else
;       ; v is not known to be shared among any objects or a member of a cycle.
;       (let
;           ((cycle/candidates (hash/cons cycle/candidates v #t))
;            (share/candidates (hash/cons share/candidates v #t))
;            (cycle/stack (cons v cycle/stack)))
;         
;         ; Now deconstruct v and account for sharing (if any) of its substructures.
;         (cond
;           [(or (string? v) (bytes? v)) (void)] ; No substructure.
;
;           [(hash/persist? v)
;            (hash/for-each
;             v
;             (lambda (k v)
;               (loop k cycle cycle/candidates share share/candidates actors cycle/stack)
;               (loop v cycle cycle/candidates share share/candidates actors cycle/stack)))]
;
;           [(vector/persist? v)
;            (vector/for-each
;             v
;             (lambda (x) (loop x cycle cycle/candidates share share/candidates actors cycle/stack)))]
;
;           [(set/persist? v)
;            (set/for-each
;             v
;             (lambda (e) (loop e cycle cycle/candidates share share/candidates actors cycle/stack)))]    
;           
;           [(curl? v)
;            (loop (curl/pk       v) cycle cycle/candidates share share/candidates actors cycle/stack)  ; Likely to be shared.
;            (loop (curl/receiver v) cycle cycle/candidates share share/candidates actors cycle/stack)  ; For both the active map and sharing.
;            (loop (curl/path     v) cycle cycle/candidates share share/candidates actors cycle/stack)  ; Vector of arbitrary values.
;            (loop (curl/metadata v) cycle cycle/candidates share share/candidates actors cycle/stack)] ; Arbitrary value.
;
;           ; No sharable substructure for these values.
;           [(actor? v) #t]
;           [(locative? v) #t]
;           [(time/unix? v) #t]
;           [(date/utc? v) #t]
;   
;           [(vector? v)
;            (vector/for-each
;             v
;             (lambda (x) (loop x cycle cycle/candidates share share/candidates actors cycle/stack)))]
;
;           [(pair? v)
;            (loop (car v) cycle cycle/candidates share share/candidates actors cycle/stack)
;            (loop (cdr v) cycle cycle/candidates share share/candidates actors cycle/stack)]
;
;           [(box? v)
;            (loop (unbox v) cycle cycle/candidates share share/candidates actors cycle/stack)]           
;
;           ; Motile code/procedure/continuation descriptors are Racket vectors.
;           [(procedure? v)
;            (loop (motile/decompile v) cycle cycle/candidates share share/candidates actors cycle/stack)]
;
;           [else (raise-type-error 'motile/serialize "not a serializable object" v)])
;         
;         ; No more opportunities for this object to appear in a cycle as the depth-first search has returned.
;         (values
;          cycle (hash/remove cycle/candidates v)
;          share share/candidates
;          actors
;          (cdr cycle/stack)))])))

(define-syntax-rule (primitive? v)
  (or
   (boolean? v)
   (number? v)
   (char? v)
   (symbol? v)
   (null? v)
   (void? v)))
(define-syntax-rule (shared? share x) (hash-ref share x #f))
(define-syntax-rule (cyclic? cycle x) (hash-ref cycle x #f))
(define-syntax-rule (stack/pop s) (set! s (cdr s)))
(define-syntax-rule (stack/push s x) (set! s (cons x s)))

(define-syntax-rule (candidates/add candidates x)
  (hash-set! candidates x #t))
(define-syntax-rule (candidates/remove candidates x)
  (hash-remove! candidates x))

;; Given an arbitrary Motile value v extract all elements appearing in cycles
;; or shared by two or more subelements placing all cycle elements in
;; cycle map and all shared elements in the share map.
(define (traverse v cycle share)
  (let ([cycle/candidates (make-hasheq)]  ; Set of candidates for sharing
        [share/candidates (make-hasheq)]  ; Set of candidates for cycles
        [cycle/stack null])               ; Candidates for breaking cycles

    (define (share/id/new) (+ (hash-count cycle) (hash-count share)))

    (define-syntax-rule (cycle/add cycle x)
      (hash-set! cycle x (share/id/new)))
    (define-syntax-rule (share/add share x)
      (hash-set! share x (share/id/new)))

    (define-syntax-rule (cycle/break v cycle share cycle/stack)
      ; Break a cycle.
      (let ([mutable (if (mutable? v) v (mutable/find v cycle/stack))])
        ; mutable will be used to break the cycle in (de)serialization.
        (cycle/add cycle mutable)
        (unless (eq? mutable v)
          ; v and mutable are distinct.
          ; We've seen mutable earlier in the traversal but v is potentially shared.
          (share/add share v))))

    ; Deconstruct v into its constituent parts.
    (let loop ([v v])
      (cond
        [(primitive? v) (void)]
        
        ;[(actor? v) (void)] ; Special cases handled elsewhere in the serializer.

        [(cyclic? cycle v) (void)] ; v is part of a cycle so it is already accounted for.

        [(cyclic? cycle/candidates v) (cycle/break v cycle share cycle/stack)]
;         ; We have seen object v earlier in our depth-first search, hence it must be a member of a cycle.
;         (let ([mutable (if (mutable? v) v (mutable/find v cycle/stack))])
;           (hash-set! cycle mutable (share/id/new share cycle)) ; mutable will be used to break the cycle.
;           (unless (eq? mutable v)
;             ; As v and mutable are distinct v is potentially shared (refered to) by other objects.
;             (hash-set! share v (share/id/new share cycle))))]

        [(shared? share v) (void)] ; Since we already know that v is shared ignore it.

        [(shared? share/candidates v)
         ; We encountered v earlier in the tour so it must be shared.
         (share/add share v)]

        [else
         ; v is not known to be either shared or a member of a cycle.
         (candidates/add share/candidates v)
         (candidates/add cycle/candidates v)
         ;(hash-set! share/candidates v #t)
         ;(hash-set! cycle/candidates v #t)
         (stack/push cycle/stack v)
         (deconstruct v loop)
         ; No more opportunities for v to appear in a cycle as the depth-first search has returned.
         (candidates/remove cycle/candidates v)
         (stack/pop cycle/stack)]))))

; Now deconstruct v and account for sharing (if any) of its substructures.
(define (deconstruct v loop)
  (cond
    [(or (string? v) (bytes? v)) (void)] ; No substructure.
    [(hash/persist? v)   (hash/for-each    v (lambda (k v) (loop k) (loop v)))]
    [(environ? v)        (environ/for-each v (lambda (k v) (loop k) (loop v)))]
    [(vector/persist? v) (vector/for-each  v loop)]
    [(set/persist? v)    (set/for-each     v loop)]
    [(curl? v)           (loop (curl/origin v)) (loop (curl/zpl/signed v))]
    [(time/utc? v) (void)] ; No sharable substructure.
    [(date? v)     (void)] ; No sharable substructure.
    ; Accounts for tuples as well since all tuples are
    ; just Racket vectors with a type tag in element 0.
    ; Note: In the future a tuple will just be a Racket immutable vector.
    [(vector? v) (vector/for-each v loop)]
    [(pair? v) (loop (car v)) (loop (cdr v))]
    [(box? v)  (loop (unbox v))]
    ; Motile code/procedure/continuation descriptors are Racket vectors.
    [(procedure? v) (loop (motile/decompile v))]
    [else (raise-type-error 'motile/serialize "not a serializable object" v)]))

(define (set/equality/serialize s)
  (cond
    ((set/eq?  s) 'eq)
    ((set/eqv? s) 'eqv)
    (else         'equal)))
  
(define (set/persist/serialize v loop)
  (let ((r (make-vector (set/length v) #f)))
    ; Fill r with the serialized members of set v.
    (set/fold v (lambda (e i) (vector-set! r i (loop e)) (add1 i)) 0)
    (vector 'S (set/equality/serialize v) r)))

;; Produces #(struct:time/unix <n>) where n is Unix epoch time in seconds.
(define (time/unix/serialize v loop)
  (struct->vector v)) 

;; Generate the serialization description (known as a "serial") for the given object v.
;; v      - an object for which we require a "serial"
;; share  - a hash table of shared objects (objects refered to by two or more objects)
;; share? - #t if the share table should be consulted in constructing the "serial" and #f otherwise
(define (value/flatten v share share?)
  (let loop ([v v] [share? share?])
    (let ([reloop (lambda (x) (loop x #t))])
      (cond
        [(or
          ; In Motile we assume that every character and byte string is immutable.
          (string? v)
          (bytes? v)   
          (boolean? v)
          (number? v)
          (char? v)
          (null? v)
          (symbol? v)) v]
        [(void? v) (cons 'void null)]
        
        ; Shared object v is (? . id) where id is the share id of v.
        [(and share? (hash-ref share v #f)) => (lambda (id) (cons '? id))]
        
        ; Immutable character string or bytes string is itself.
        ; Mutable character string or bytes string v is (u . v).
        ;[(or (string? v) (bytes? v)) (if (immutable? v) v (cons 'u v))]
        
        ; Persistent vector is (V . #(s_0 ... s_{n-1})) where s_i is the serial of element x_i of v.
        [(vector/persist? v)
         (let ((r (make-vector (vector/length v) #f)))
           (vector/fold/left v (lambda (x i) (vector-set! r i (value/flatten x share #t)) (add1 i)) 0)
           (cons 'V r))]
        
        ; Persistent hash is #(sruct:hash <kind> K_1 X_1 ... K_N X_N) where K_i, X_i are the serials of pair k_i/x_i
        ;; respectively in the hash table. K_i/X_i appear in insertion order, that is, if K_i/X_i precede K_j/X_j
        ;; in the flattened representation then pair k_i/x_i was inserted into the hash table prior to the insertion
        ;; of k_j/x_j.
        [(hash/persist? v) (hash/persist/flatten v reloop)]
        
        ; #(struct:environ #(struct:hash ...)).
        [(environ? v) (environ/flatten v reloop)]
        
        ; Persistent set is #(S <flavor> #(E_1 ... E_N)) where E_i is the serial of set element e_i.
        [(set/persist? v) (hash/persist/flatten v reloop)]
        
        ; Immutable Racket vector x is (v .  #(s_0 ... s_{n-1}))  where s_i is the serial of element i of x.
        ; Mutable Racket vector   x is (v! . #(s_0 ... s_{n-1}))  where s_i is the serial of element i of x.
        [(vector? v) (cons (if (immutable? v) 'v 'v!) (vector/map v reloop))]
        
        ; Pair (x . y) is #(c X Y) where X, Y is the "serial" of x, y respectively. 
        [(pair? v) (vector 'c (reloop (car v)) (reloop (cdr v)))]
        
        ; Immutable box is (b .  X) where X is the serial of the contents of box v.
        ; Mutable box is   (b! . X) where X is the serial of the contents of box v.
        [(box? v) (cons (if (immutable? v) 'b 'b!) (reloop (unbox v)))]
        
        ; Motile closure is (M . D) where D is the serial of the Motile closure descriptor.
        [(procedure? v) (cons 'M (reloop (motile/decompile v)))]
        
        [(curl? v)
         (unless (or (symbol? (curl/access v)) (curl/embargo? v))
           (accessor/add (this/accessors) (curl/access v)))
         (let ([flat-curl (vector-immutable 'struct:curl (reloop (curl/origin v)) (reloop (curl/zpl/signed v)))])
           (island/monitoring/log #:type COMET/CURL/TRANSFER
                                  #:place (if (symbol? (curl/access v)) INTER INTRA) 
                                  #:curl v)
           flat-curl)]

        [(time/utc? v) (time/flatten  v)]
        [(date? v)     (date/flatten  v)]
        
        [else (raise-type-error 'motile/serialize "unknown type ~a" v)]))))

;; Return the encoding for a cyclic graph point for value v.
;; Only a Racket vector or a box may contribute to a cycle.
(define (serial/wrap v)
  (cond
    [(vector? v) (cons 'v (vector-length v))]
    [(box? v) 'b]
    [else (raise-type-error 'motile/serialize "can not construct cycle with ~s" v)]))
 
;(struct motile/flat (version total objects fixups final signature bytes) #:transparent)

;; A root object is deconstructed in a depth-first tour which first accumulates all shared
;; (sub)objects and all (sub)objects appearing in cycles.
;; Each shared and cycle object is assigned a unique tour index i = 0, 1, ... taken from
;; the first appearance of the object in the depth-first tour.
(struct
  motile/flat
   ; Version of serialization format given as #(<major> <minor> <patch>).
  (version
   ; Number of shared objects + number of objects appearing in cycles.
   total
   ; Vector of the serial forms of all shared and cycle objects in discovery order where
   ; objects[i] = serial form of object whose discovery order index is i = 0, 1, ...
   objects
   ; List of serial forms of all objects appearing in cycles, given as (i . s_i),
   ; where i is the tour index and s_i is the serial form of the cyclic object.
   ; The list is not in tour index order.
   fixups
   ; The root object.
   final)
  #:transparent)
(struct/getters/define motile/flat version total objects fixups final)

(define motile/flat/reconstruct/c
  (flat-named-contract
   'motile/flat/reconstruct
   (and/c
    vector?
    (lambda (x) (and (= (vector-length x) 6) (eq? (vector-ref x 0) 'struct:motile/flat))))))

;(define motile/flat/reconstruct/c
;  (flat-named-contract
;   'motile/flat/reconstruct
;   (vector/c
;    'struct:motile/flat
;    serialize/version/c ; version
;    exact-nonnegative-integer? ; total
;    vector? ; objects
;    list?   ; fixups
;    any/c   ; root object
;    #:immutable #t
;    #:flat? #t)))
         
(define (motile/flat/reconstruct v)
  (motile/flat (vector-ref v 1) (vector-ref v 2) (vector-ref v 3) (vector-ref v 4) (vector-ref v 5)))

(define (motile/flat-to-bytes f)
  (call-with-output-bytes (lambda (out) (write (struct->vector f) out))))

;; Merge the set of objects participating in cycles in the set of shared objects.
(define (cycle/merge! cycle share)
  (hash-for-each cycle (lambda (o id) (hash-set! share o id)))
  share)

;; Generate a vector #(o_0 ... o_{N-1}) of the contents of the union of share and cycle sets where o_i is the object
;; in the union whose share-id was i. In other words the objects are sorted in discovery order (given by their share-ids)
;; in the depth-first traversal of the serialized root object v.
(define (share/objects/sort share)
  (let* ([associations (sort (hash-map share cons) (lambda (a b) (< (cdr a) (cdr b))))] ; Sort by ascending share id.
         [ordered (list->vector (map car associations))])
    ordered))

(define (motile/serialize root)
  (let ([cycle  (make-hasheq)]
        [share  (make-hasheq)])
    ; Traverse object v to find cycles and shared objects within v.
    (traverse root cycle share)
    ; Merge the set of cycle objects into the set of shared objects (cycle remains unchanged and share becomes the union of both).
    (cycle/merge! cycle share)
    
    (let* ([ordered (share/objects/sort share)] ; Produces all share and cycle objects as a vector in depth-first discovery order: 0, 1, ...
           [ordered/boxed ; Box all cycle objects and serialize all shared objects.
            (vector/map ordered (lambda (v) (if (hash-ref cycle v #f) (box (serial/wrap v)) (value/flatten v share #f))))]
           [fixups ; Serialize all cycle objects.
            (hash-map cycle (lambda (v n) (cons n (value/flatten v share #f #f))))]
           [final (value/flatten root share #t)])           
      (motile/flat
       (motile/serialize/version) ; Version of serialization format.
       (vector-length ordered)    ; Total number of cycle and share objects.
       ordered/boxed              ; Serialized cycle and share in order of discovery in depth-first tour.
       fixups                     ; For cycles.
       final))))                  ; Serialization of root object passed to motile/serialize.


;(define (motile/flat/ok? x)
;  (and
;   (motile/flat? x)
;   ; Version must be (<major> <minor> <patch>).
;   (vector? (motile/flat/version x))
;   (= (vector-length (motile/flat/version x)) 3)
;   ; 0 or more cycle + share objects.
;   (exact-nonnegative-integer? (motile/flat/total x))
;   ; The set of cycle + share objects is represented as a vector.
;   (vector? (motile/flat/objects x))
;   ; Length and total must match.
;   (= (vector-length (motile/flat/objects x)) (motile/flat/total x))
;   ; Fixups are always a list.
;   (list? (motile/flat/fixups x))))

;   (or (not (motile/flat/bytes x))     (bytes? (motile/flat/bytes x)))     ; Either #f or #"..."
;   (or (not (motile/flat/signature x)) (bytes? (motile/flat/signature x))) ; Either #f or #"..."
;   (if (not (motile/flat/bytes x)) (not (motile/flat/signature x)) #t)))   ; Signature only if we have the wire-line bytes representation.
       

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deserialize
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The unready structure contains two fields:
;; shares - The set of all shared objects plus all cycle objects given as a vector
;;    #((v_0 . 0) ... (v_{n-1} . n))
;;    where for all (v_i . i) v is either a shared or cycle object
;;    and i = 0, ..., n-1 is a tour index giving the order in which
;;   the object was discovered in a depth-first traversal.

;; fixers - a hash map from discovery index i to an anonymous closure for
;;    each cycle object with discovery index i.
;;    The fixers are generated in preparation for deserialization.
(struct unready (shares fixers) #:transparent)
(struct/getters/define unready shares fixers)

;; Allocate a bytes string that contains at least n bits.
(define (used/new n)
  (let-values ([(q r) (quotient/remainder n 8)])
    (let ((m (+ q (if (> r 0) 1 0))))
      (make-bytes m 0))))

(define BYTE-SET (bytes 128 64 32 16 8 4 2 1)) 

(define BITS-USED-BYTE (bytes 0 #b10000000 #b11000000 #b11100000 #b11110000 #b11111000 #b11111100 #b11111110))

;(define BITS-USED-BYTE
;  (let ((b (make-bytes 7 0)))
;    (bytes-set! b 0 (bytes-ref BYTE-SET 0))
;    (let loop ((i 1))
;      (when (< i 7)
;        (bytes-set! b i (bitwise-ior (bytes-ref b (sub1 i)) (bytes-ref BYTE-SET i)))
;        (loop (add1 i))))
;    b))

;; Turn on bit i in bytes sequence b.
(define (bitwise/set! b i)
  (let-values ([(q r) (quotient/remainder i 8)])
    ; Set bit r in byte q on (where bit r = 0 is the high order bit of the byte and r = 7 is the low order).
    (bytes-set! b q (bitwise-ior (bytes-ref b q) (bytes-ref BYTE-SET r)))))

(define (bits/used? b n)
  (let-values ([(q r) (quotient/remainder n 8)]) ; q bytes + r bits.
    (and
     (let loop ((i 0))
       (cond
         ((i < q)
          (if (= (bytes-ref b i) 256)
              (loop (add1 i))
              #f))
         (else #t)))
     (if (> r 0) (= (bytes-ref b q) (bytes-ref BITS-USED-BYTE (sub1 r))) #t))))
       

;; The share vector is not necessarily in order of refereds before referees. A `unready' object
;;  indicates a reference before a value is ready,
;;  so we need to recur to make it ready. Cycles
;;  have been broken, though, so we don't run into
;;  trouble with an infinite loop here.

;; Return the i-th shares value, reconstructing it if necessary.
;; share - a vector #(u u ... u) of n unready structures where all slots i = 0, ... n-1 are occupied
;;   by the same unready structure, that is, for any 0 <= i <= j <= n-1,  share[i] is eq? to share[j].
;; i     - an index 0, ..., n-1
(define (share/rebuild share i)
  ; v is either a value that has already been reconstructed or the single instance of the unready structure.
  (let ((v (vector-ref share i)))
    ;(display (format "inside share/rebuild: ~a" v)) (newline)
    (if (unready? v) ; v is an unready structure (shares fixers).
        ; Reconstruct the shared or cycle object.
        ; x (from shares[i] of the serialization) either a shared object or a cycle member.
        (let* ([x (vector-ref (unready/shares v) i)]
               [reconstruction
                (if (box? x)
                    (shared/unflatten (unbox x) (unready/fixers v) i) ; Reconstruct a shared/cyclic value.
                    (value/unflatten x share))])
          (vector-set! share i reconstruction)
          reconstruction)

        v)))

(define (set/equality/decode x)
  (case x
    ((eq)    set/eq/null)
    ((eqv)   set/eqv/null)
    ((equal) set/equal/null)))

(define (motile/deserialize/raise message v)
  (raise-type-error 'motile/deserialize message v))

;; (? . i)
(define (?/rebuild x share)
  ;(display (format "inside ?/rebuild: ~a" x)) (newline)
  (let ((i (cdr x)))
    (unless (and (exact-nonnegative-integer? i) (< i (vector-length share)))
      (motile/deserialize/raise "(? . i) where i >= 0" x))
    (share/rebuild share i)))

;; Deserialization for the void value.
(define (void/rebuild x)
  (unless (null? (cdr x))
    (motile/deserialize/raise "singleton list (void)" x))
  (void)) ; Strictly speaking unncessary since (unless ...) returns void.
  
;; Deserialization for mutable strings and bytes.
(define (u/rebuild x)
  (let ((s (cdr x)))
    (cond
      ((string? s) (string-copy s))
      ((bytes?  s) (bytes-copy  s))
      (else (motile/deserialize/raise "(u . <string>) or (u . <bytes>)" x)))))
  
;; Deserialization for immutable vectors.
(define (v/rebuild v f)
  (let ((x (cdr v)))
    (unless (vector? x)
      (motile/deserialize/raise "(v . #(X_0 ...))" v))
    (vector->immutable-vector (vector/map x f))))

;; Deserialization for mutable vectors.
(define (v!/rebuild v f)
  (let ((x (cdr v)))
    (unless (vector? x)
      (motile/deserialize/raise "(v! . #(X_0 ...))" v))
    (vector/map x f)))

;; Deserialization for persistent vectors.
(define (V/rebuild v f)
  (let ((x (cdr v)))
    (unless (vector? x)
      (motile/deserialize/raise "(V . #(X_0 ...))" v))
    (vector/racket=>vector/persist (vector/map x f))))

;; Deserialization for immutable cons cells #(c X Y).
(define (c/rebuild v f)
  (unless (= (vector-length v) 3)
    (motile/deserialize/raise "#(c X Y)" v))
  (cons (f (vector-ref v 1)) (f (vector-ref v 2))))

(define (descriptor/hash/ok? v)
  (and
   (= (vector-length v) 3)
   (memq (vector-ref v 1) '(eq eqv equal))
   (vector? (vector-ref v 2))
   (even?   (vector-length (vector-ref v 2)))))

;; Deseriaization for persistent hash tables.
;(define (H/rebuild v f)
;  (unless (descriptor/hash/ok? v)
;    (motile/deserialize/raise "#(H <equality> #(K_1 V_1 ...))" v))
;  (vector/hash
;   (hash/equality/decode (vector-ref v 1))
;   (vector/map (vector-ref v 2) f))) ; Reconstruct the key/value pairs from their serialized form.
    
(define (descriptor/set/ok? v)
  (and
   (= (vector-length v) 3)
   (memq (vector-ref v 1) '(eq eqv equal))
   (vector? (vector-ref v 2))))

;; Deserilization for persistent sets.
(define (S/rebuild v f)
  (unless (descriptor/set/ok? v)
    (motile/deserialize/raise "#(S <equality> #(K_1 V_1 ...))" v))
  (vector/set
   (set/equality/decode (vector-ref v 1))
   (vector/map (vector-ref v 2) f)))

;; Locate by name a ZPL node x (an immediate child of parent ZPL node z) and return the value of x.
;; If no such child node exists return #f.
(define (zpl/locate* z name)
  (cond
    [(zpl/locate z name) => (lambda (x) (zpl/value x))]
    [else #f]))
;; Given a bytes input port whose contents is a curl/zpl/signed that has been stripped
;; of its signing hash reconstruct a curl/core instance.
(define (zpl-to-curl/core in)
  (let* ([tree (zpl/load in)]
         [CURL (zpl/locate tree 'CURL)])
    (and
     CURL
     (let ([id        (zpl/locate* CURL 'id)]
           [origin    (zpl/locate* CURL 'origin)]
           [path      (zpl/locate* CURL 'path)]
           [access/id (zpl/locate* CURL 'access/id)]
           [created   (ISO8601-to-time (zpl/locate* CURL 'created))]
           [metadata
            (let ([m (zpl/locate* CURL 'metadata)])
              (if m (motile/deserialize (motile/flat/reconstruct m)) #f))])
       (curl/core id origin path access/id created metadata)))))
       
;; OLD code for zpl-to-curl/core that replaced an access/id with an access:send point
;; from the set of island accessors. We don't do this any longer and for incoming CURLs
;; the island router is responsible for associating the access/id with the correct
;; access:send point. 2014-05-06
;       (if (bytes=? origin (curve/kp/base64 (this/curve)))
;           ; The CURL originated on this island.
;           (let ([a (accessor/look (this/accessors) access/id)])
;             (and a (curl/core id origin path a created metadata)))
;           ; The CURL originated on a remote island so we can't possibly hold the access:send point.
;           (curl/core id origin path access/id created metadata))))))

(define (descriptor/curl/ok? v) (= (vector-length v) 3))

;; Rebuild a CURL from the serialization v using rebuild function f for the subcomponents.
(define (curl/rebuild v f)
  (unless (descriptor/curl/ok? v)
    (motile/deserialize/raise "#(struct:curl <origin> <curl/zpl/signed>)"))
  (let ([origin (f (vector-ref v 1))])     ; kp/base64 of island of origin of curl.
    (if (kp/base64? origin)
        (let ([zpl/signed (f (vector-ref v 2))] ; curl/zpl/signed representation of curl/core.
              [kp/sign ; kp/sign key for island of origin (if we can find it).
               (if (bytes=? origin (curve/kp/base64 (this/curve)))
                   (curve/kp/sign (this/curve))
                   (keystore/kp/sign/look (this/keystore) origin))])
          (let ([c ; curl/core for the curl we are trying to reconstruct.
                 (if kp/sign
                     ; Verify the core/zpl/signed byte string.
                     (let ([z (crypto/sign/verify zpl/signed kp/sign)])
                       ; If it verified unpack z to a curl/core instance.
                       (and z (call-with-input-bytes z zpl-to-curl/core)))
                     ; Was signed by a foreign island whose kp/sign is unavailable at this time.
                     (call-with-input-bytes
                      zpl/signed
                      (lambda (in)
                        (read-bytes CURVE/SIGNING/LENGTH in) ; Discard the signing.
                        (zpl-to-curl/core in))))]) ; Convert the remaining ZPL bytes text to a curl/core.
            (and
             c
             ; Guarantee that island of origin given in serialization agrees with island of origin given in zpl/signed.
             (bytes=? origin (curl/core/origin c))
             (curl c kp/sign zpl/signed))))

        (motile/deserialize/raise "bad CURL origin"))))

;; v      - a "serial" generated by object/serialize
;; shares - a vector #(u_0 ... u_{n-1}) where each u_i contains the SAME unready structure.
(define (value/unflatten v share)
  (let loop ([v v])
    ;(display (format "inside value/unflatten: ~a" v)) (newline)
    (cond
      ; Fundamental primitive values are trivially deserialized as themselves.
      [(or (boolean? v) (number? v) (char? v) (symbol? v) (null? v)) v]
      ; A standalone character string "..." denotes the equivalent immutable character string.
      [(string? v) (string->immutable-string v)]
      ; A standalone bytes string #"..." denotes the equivalent immutable bytes string.
      [(bytes? v) (bytes->immutable-bytes v)]
      [(pair? v)
       (case (car v)
         ; (? . i) where i is an index into the shares.
         [(?) (?/rebuild v share)]
         ; (u . "...") or (u . #"..."). A mutable character string or bytes sequence respectively.
         [(u) (u/rebuild v)]
         ; (v . #(e_0 ... e_{N-1})). Immutable vector #(e_0 ... e_{N-1}).
         [(v) (v/rebuild v loop)]
         ; (v! . #(e_0 ... e_{N-1})). Mutable vector #(e_0 ... e_{N-1}).
         [(v!) (v!/rebuild v loop)]
         ; (M . <code-descriptor>). Motile closure.
         [(M) (motile/recompile (loop (cdr v)))] ; Note - recall (when (motile/recompile/active? descriptor) (hash-set! procedures code descriptor))
         ;; (V . #(X_0 ... X_{N-1}). Persistent vector #(x_0 ... x_{N-1}).
         [(V) (V/rebuild v loop)]
         ; The singleton list (void) containing the symbol void, denoting the Racket void value.
         [(void) (void/rebuild v)]
         ; (b . x). Immutable box #&(x).
         [(b) (box-immutable (loop (cdr v)))]
         ; (b! . x). Mutable box #&(x).
         [(b!) (box (loop (cdr v)))]
         [else (motile/deserialize/raise "unknown serialized object" (car v))])]
      [(vector? v)
       (case (vector-ref v 0)
         ; #(c X Y). Pair (x . y) where X and Y are the serializations of x and y respectively.
         [(c) (c/rebuild v loop)]
         ; #(struct:hash <equality> #(K_1 X_1 ... K_n X_n)) where K_i/X_i are the serialization of key/value pair k_i/x_i.
         [(struct:hash) (hash/persist/unflatten v loop)]
         ; #(struct:environ #(struct:hash ...))
         [(struct:environ) (environ/unflatten v loop)]
         ; (S <equality> #(K_1 X_e1 ... K_n X_n) where K_i/X_i are the serialization of key/value pair k_i/x_i.
         [(S) (S/rebuild v loop)]

         [(struct:time) (time/unflatten v)]
         [(struct:date) (date/unflatten v)]
         [(struct:curl) (curl/rebuild v loop)]

         [else (motile/deserialize/raise "unknown serialized object" (vector-ref v 0))])])))

;; x     - (v . m) where v is the symbol "v" and m is an exact positive integer
;; fixup - vector for the fixup shells generated here
;; i     - index into fixup
(define (deserial/vector/unwrap x fixers i)
  (let ((m (cdr x))) ; Size of vector to reconstruct.
    (unless (exact-positive-integer? m)
      (motile/deserialize/raise "should be (v . m), m an exact positive integer" x))
    
    (let* ((skeleton (make-vector m #f)) ; Create a skeleton vector.
           (shell (lambda (v) ; Anonymous shell function will be called later to patch cycles.
                    (let loop ((i 0))
                      (when (< i m)
                        (vector-set! skeleton i (vector-ref v i))
                        (loop (add1 i)))))))
      ; Shell (fixup[i]) when called will copy elements of vector v into the skeleton.
      (hash-set! fixers i shell)

      skeleton))) ; Return skeleton vector as value.

(define (deserial/pair/unwrap v fixers i)
  (let ([reconstruction (cons #f #f)]
        [f (lambda (p) (motile/deserialize/raise "cannot restore pair in cycle" ))])
    (hash-set! fixers i f)
    reconstruction))
  
(define (deserial/box/unwrap v fixers i)
  (let* ([reconstruction (box #f)]
         [f (lambda (b) (set-box! reconstruction (unbox b)))])
    (hash-set! fixers i f)
    reconstruction))

;; v     - value to be reconstructed
;; fixup - vector to hold callbacks for cycle reconstructions
;; i     - index into fixup
(define (shared/unflatten v fixup i)
  (cond
    [(pair? v)
     (case (car v)
       [(v)  (deserial/vector/unwrap v fixup i)]
       [else (motile/deserialize/raise "expected (<symbol v> . m), m an exact positive integer" v)])]
    [else
     (case v
       [(c)  (deserial/pair/unwrap v fixup i)]
       [(b)  (deserial/box/unwrap  v fixup i)]
       [else (motile/deserialize/raise "expected c or b serial descriptor" v)])]))

(define (flat/deserialize _version flat) ; _version to accomodate version-specific deserialization in the future.
  (let ([n       (motile/flat/total   flat)]  ; Total number of share + cycle objects.
        [shares  (motile/flat/objects flat)]  ; A vector of all shared and cycle objects in serialization.
        [fixups  (motile/flat/fixups  flat)]  ; List of (id . <serial>) templates for patching cycles in the graph of objects. 
        [final   (motile/flat/final   flat)]) ; The "root" object of the graph of objects.
        
    ; Fixers below is a map from tour index -> anonymoous fixer closure for each cycle object with tour index i.
    ; In Motile cycle objects will be fairly rare.
    (let* ([fixers  (and fixups (make-hasheq))] ; #f if no cycle objects.
           [unready (unready shares fixers)]
           [share   (make-vector n unready)]) ; Each share[i] (i = 0, ..., n-1) contains the SAME single unready structure.
      ; Deserialize into the share vector defined above.
      ; The anonymous "fixer" closures for cycle repair are generated here as well.
      (let loop ([i 0])
        (when (< i n)
          (share/rebuild share i)
          (loop (add1 i))))
      
      ; Fixup shell for the cycles.
      (for-each
       (lambda (pair)
         (let ((i      (car pair))                  ; Tour index of cycle object.
               (serial (cdr pair)))                 ; Full serial representation of cycle object.
           (let ([v (value/unflatten serial share)] ; Reconstruct the cycle object.
                 [fixer (hash-ref fixers i)])       ; Anonymous closure to patch the cycle object.
             (fixer v))))                           ; Apply the fixer to the reconstructed object to repair cycles.
       fixups)

      ; Deserialize final result (if there's no sharing, then all the work is actually here).
      (value/unflatten final share))))

(define (motile/deserialize flat)
  ;(display flat) (newline)
  (let ((version (motile/flat/version flat)))
    (if (equal? version (motile/serialize/version))
        (flat/deserialize version flat)
        (motile/deserialize/raise "unknown version of flat"))))

(define (motile/deserialize/offline flat accessors curve keystore)
  (let ([this/accessors (make-parameter #f)]
        [this/curve     (make-parameter #f)]
        [this/keystore  (make-parameter #f)])
    (parameterize
        [(this/accessors (if (box? accessors) accessors (box accessors)))
         (this/curve curve)
         (this/keystore (if (box? keystore) keystore (box keystore)))]
      (motile/deserialize flat))))


;; ----------------------------------------

;; Test two serializations flat_1 and flat_2 for equality.
;(define (motile/serialize/equal? flat_1 flat_2)
;  (let ([v_1 (motile/deserialize flat_1 #f)]
;        [v_2 (motile/deserialize flat_2 #f)])
;    (equal? v_1 v_2)))

(define (share/test)
  (let* ((shared '(a b c))
         (v_1 (vector 1 2 shared 3))
         (v_2 (vector 101 102 shared 103)))
    (write (motile/serialize v_1)) (newline) (newline)
    (write (motile/serialize v_2)) (newline) (newline)
    (write (motile/serialize (cons v_1 v_2))) (newline)))

;; Produces #(struct:motile/flat #(1 0 0) 1 #(#&(v . 3)) ((0 v! . #((? . 0) (? . 0) (? . 0)))) (? . 0))
;; version - #(1 0 0)
;; total   - 1
;; objects - #(#&(v . 3))
;; fixups  - ((0 v! . #((? . 0) (? . 0) (? . 0))))
;; final   - (? . 0)
(define (cycles/test/1a)
  (let ((v (make-vector 3 #f)))
    (vector-set! v 0 v)
    (vector-set! v 1 v)
    (vector-set! v 2 v)
    (write (motile/serialize v)) (newline)
    (motile/deserialize (motile/serialize v))))

;; Produces #(struct:motile/flat #(1 0 0) 2 #("long silly string" #&(v . 3)) ((1 v! . #((? . 0) (? . 0) (? . 1)))) (? . 1))
;; version - #(1 0 0)
;; total   - 2
;; objects - #("long silly string" #&(v . 3))
;; fixups  - ((1 v! . #((? . 0) (? . 0) (? . 1))))
;; final   - (? . 1)
(define (cycles/test/1b)
  (let ((v (make-vector 3 #f))
        (s "long silly string"))
    (vector-set! v 0 s)
    (vector-set! v 1 s)
    (vector-set! v 2 v)
    (write (motile/serialize v)) (newline)
    (motile/deserialize (motile/serialize v))))

;#((1 0 0) 1 (#&(v . 4)) ((0 v! . #(1 (? . 0) 3 4))) (? . 0))
; version - (1 0 0)
; total shareds and cycles               - 1
; list of serials of all shared + cycles - (#&(v . 4))
; list of fixups for cycles              - ((0 v! . #(1 (? . 0) 3 4)))
; root object                            - (? . 0)
(define (cycles/test/2)
    (let ((v (vector 1 #f 3 4)))
    (vector-set! v 1 v) ; Create a cycle.
    (write (motile/serialize v)) (newline)))

; #((1 0 0) 2 (#&b (c 1 c 2 c (? . 0) c 3)) ((0 b! ? . 1)) (? . 1))
(define (cycles/test/3)
  (let* ((b (box #f))
         (v (list 1 2 b 3)))
    (set-box! b v) ; Create a cycle.
    (write (motile/serialize v)) (newline)
    (motile/deserialize (motile/serialize v))))

(define (cycles/test/4)
  (let* ((b (box #f))
         (v (list 1 2 b b)))
    (set-box! b v) ; Create a cycle.
    (write (motile/serialize v)) (newline)
    (motile/deserialize (motile/serialize v))))

(define h/26
  (list/hash
   hash/eq/null
   '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
     k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
     u 21 v 22 w 23 x 24 y 25 z 26)))

(define e/26
  (list/environ
   environ/null
   '(a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 8 i 9 j 10
     k 11 l 12 m 13 n 14 o 15 p 16 q 17 r 18 s 19 t 20
     u 21 v 22 w 23 x 24 y 25 z 26)))
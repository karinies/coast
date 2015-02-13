#lang racket/base

(require
 (only-in racket/vector vector-copy)
 (only-in "hash.rkt" hash/cons hash/eq/null hash/keys hash/ref hash/contains? vectors/hash))

(provide
 ;; The following functions are exported for installation into the Motile baseline.
 record?
 record/contains?
 record/signed?
 record/kind
 record/keys
 record/unsign
  
 ;; The following functions are used only in Racket infrastructure code supporting Motile.
 record/raw/kind
 record/raw/hash
 record/raw/signature
 error/field/unknown
 error/unrecord
 
 ;; The following functions are the Racket emulations of the base Motile record primitives.
 record/new   ; Racket equivalent of Motile (record ...) special form.
 record/cons  ; Racket equivalent of Motile (record/cons ...) special form.
 record/ref   ; Racket equivalent of Motile (record/ref ....) special form
)
 

;; Self-describing record sructure based on persistent hash tables.
;; #(<record> <kind> <hash> <signature>)

(define-syntax-rule (record/raw/kind  r)     (vector-ref r 1))
(define-syntax-rule (record/raw/hash  r)     (vector-ref r 2))
(define-syntax-rule (record/raw/signature r) (vector-ref r 3))

(define (record? r)
  (and (vector? r)
       (= (vector-length r) 4)
       (eq? (vector-ref r 0) '<record>)))

(define (record/signed? r)
  (bytes? (record/raw/signature r)))

;; Produce a copy of record r that is unsigned.
(define (record/unsign r)
  (if (not (record/signed? r))
      r
      (let ((u (vector-copy r)))
        (vector-set! u 3 #f)
        u)))

;; Return the kind of record r as a symbol.
(define (record/kind r)
  (if (record? r)
      (record/raw/kind r)
      (error/unrecord 'record/kind r)))

;; Return the field names of record r as symbols in a tuple.
(define (record/keys r)
  (if (record? r)
      (hash/keys (record/raw/hash r))
      (error/unrecord 'record/keys r)))

(define (record/contains? r tag)
  (if (record? r)
      (hash/contains? (record/raw/hash r) tag)
      #f))

;; The given field tag is unknown in record r.
(define (error/field/unknown use r field)
  (error use "no such field ~s in record type ~s" field (record/raw/kind r)))

;; Raise an error as x is not a record.
(define (error/unrecord use x)
  (error use "not a record ~s" x))

;; All of this is an emulation of Motile records in the base Scheme.

(define-syntax record/new
  (syntax-rules ()
    [(_ name (field value) ...)
     (vector '<record> 'name (vectors/hash hash/eq/null (vector 'field ...) (vector value ...)) #f)]))

(define-syntax record/cons
  (syntax-rules ()
    [(_ r (field value) ...) (record/cons/N r (vector 'field ...) (vector value ...))]
    [(_ r field value)       (record/cons/1 r 'field value)]))

(define (record/cons/1 r field value)
  (vector '<record> (record/raw/kind r) (hash/cons (record/raw/hash r) field value) #f))

(define (record/cons/N r fields values)
  (vector '<record> (record/raw/kind r) (vectors/hash (record/raw/hash r) fields values) #f))

(define-syntax record/ref
  (syntax-rules ()
    [(_ record field)
     (if (hash/contains? (record/raw/hash record) 'field)
         (hash/ref (record/raw/hash record) 'field #f)
         (error 'record/ref "no field ~s in record ~s" 'field (record/raw/kind r)))]

    [(_ record alpha beta ...)
     (if (hash/contains? (record/raw/hash record) 'alpha)
         (record/ref (hash/ref (record/raw/hash record) 'alpha #f) beta ...)
         (error 'record/ref "no field ~s in record ~s" 'alpha (record/raw/kind r)))]

     [(_ record field failure) (hash/ref (record/raw/hash record) 'field failure)]))


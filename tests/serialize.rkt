#lang racket/base

(require
  "../serialize.rkt"
  "../this.rkt"
  "../time.rkt"
  )

;; Serialization of cons
(define (test/cons/01)
  (display (motile/serialize (cons 18 24)))
  (newline)
  (display (motile/deserialize (motile/serialize (cons 18 24))))
  (newline)
  (let ((x (cons 18 24)))
    (display (motile/serialize (cons x x)))
    (newline)
    (display (motile/deserialize (motile/serialize (cons x x))))
    (newline)))

;; Serialization of time structures.
(define (test/time/01)
  (let* ((d (time/now)))
    (display (motile/serialize d))
    (newline)
    (display (motile/serialize (cons d d)))
    (newline)))

;; Deserialization of time structures.
(define (test/time/02)
  (let* ((d (time/now)))
    (and
     (equal? d (motile/deserialize (motile/serialize d)))
     (equal? (cons d d) (motile/deserialize (motile/serialize (cons d d))))
     )))

;;; Serialization of island/address.
;(define (test/address/01)
;  (let ((a (island/address/new #"foobar" "www.example.com" 10001)))
;    (display (motile/serialize a))
;    (newline)
;    (display (motile/serialize (cons a a)))
;    (newline)))
;
;;; Serialization of island/address.
;(define (test/address/02)
;  (let ((a (island/address/new #"foobar" "www.example.com" 10001)))
;    (display (motile/deserialize (motile/serialize a)))
;    (newline)
;    (display (motile/deserialize (motile/serialize (cons a a))))
;    (newline)))
#|

(define (test/actor/01a)
  (this/island #"a long public key in base64 format")
  (actor/root/new)
  (display (motile/serialize (this/actor)))
  (newline))

(define (test/actor/01b)
   (this/island #"a long public key in base64 format")
  (actor/root/new)
  (display (motile/deserialize (motile/serialize (this/actor))))
  (newline))

;; Shared actor vales.
(define (test/actor/02)
  (this/island #"a long public key in base64 format")
  (actor/root/new)
  (display (motile/serialize (cons (this/actor) (this/actor))))
  (newline))
|#
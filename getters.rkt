#lang racket

;; Copyright 2013 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require
 compatibility/defmacro)

(provide
 struct/getters/define
 struct/setters/define
 struct/++/define
 ::)

;; t       - type name of struct as symbol
;; getters - field_1 field_2 ... as symbols
;;           Note: fields may be in any order
;; Generates
;; (begin
;;    (define t/field_1 t-field_1)
;;    ...
;;    (define t/field_m t-field_m))
(define-macro (struct/getters/define t . getters)
  (define (getter/slashify t s) (string->symbol (format "~a/~a" t s)))
  (define (getter/classic t s)  (string->symbol (format "~a-~a" t s)))

  ;; t - type name of struct as symbol
  ;; g - field name as symbol
  ;; Generates (define t/g t-g).
  (define (getter/define t g) `(define ,(getter/slashify t g) ,(getter/classic t g)))

  ;; t       - type name of struct as symbol
  ;; getters - list of field names (field_1 ... field_m) as symbols
  ;; Generates ((define t/field_1 t-field_1) ... (define t/field_m t-field_m)).
  (define (getters/define t getters) (map (lambda (g) (getter/define t g)) getters))
  
  `(begin ,@(getters/define t getters)))

;; The setters equivalent of the getters macro above.
(define-macro (struct/setters/define t . setters)
  (define (setter/slashify x y) (string->symbol (format "~a/~a!" x y)))
  (define (setter/setify x y)   (string->symbol (format "~a/~a/set" x y)))
  (define (setter/classic t s) (string->symbol (format "set-~a-~a!" t s)))
  ;(define (setter/define t s) `(define ,(setter/slashify t s) ,(setter/classic t s)))
  (define (setter/define t s) `(define ,(setter/setify t s) ,(setter/classic t s)))
  (define (setters/define t getters) (map (lambda (g) (setter/define t g)) getters))
  
  `(begin ,@(setters/define t setters)))

(define (getter/slashify x y) (string->symbol (format "~a/~a" x y)))
(define (setter/slashify x y) (string->symbol (format "~a/~a!" x y)))
(define (setter/classic t s) (string->symbol (format "set-~a-~a!" t s)))
(define (getter/classic t s) (string->symbol (format "~a-~a" t s)))
(define (symbol/++ t s)      (string->symbol (format "~a/~a++" t s)))
(define (++/define t s)
  `(define (,(symbol/++ t s) x) (,(setter/classic t s) x (add1 (,(getter/classic t s) x)))))
(define (++/defines t setters) (map (lambda (s) (++/define t s)) setters))

(define-macro (struct/++/define t . setters)
  (define (setter/classic t s) (string->symbol (format "set-~a-~a!" t s)))
  (define (getter/classic t s) (string->symbol (format "~a-~a" t s)))
  (define (symbol/++ t s)      (string->symbol (format "~a/~a++" t s)))
  (define (++/define t s)
    `(define (,(symbol/++ t s) x) (,(setter/classic t s) x (add1 (,(getter/classic t s) x)))))
  (define (++/defines t setters) (map (lambda (s) (++/define t s)) setters))

  `(begin ,@(++/defines t setters)))  
  
;; (:: <instance> t/field)        => (t/field <instance>).
;; (:: <instance> t/field <value) => (t/field! <instance> <value>)
(define-macro (:: instance accessor . value)
  (if (null? value)
      (list accessor instance)
      (let ((setter (string->symbol (format "~a!" accessor))))
        `(,setter ,instance ,(car value)))))

;(define-syntax-rule (:: t f x)

;; Given struct type t with fields f_1, f_2, ...
;; generate
;; (define t/f_1/index 1) (define t/f_2/index 2) ...
;; These definitions correspond to the format  v = (struct->vector x)
;; where v_0 is the struct type (as a symbol) and v_i is the ith field value.
;; The argument fields must enumerate all fields in the order in which they
;; appear.
;(define-macro (struct/fields/index t . fields)
;  (define (index/define t g i)
;    (let ((t/string (symbol->string t))
;          (g/string (symbol->string g)))
;      `(define
;         ,(string->symbol (string-append t/string "/" g/string "/index"))
;         ,i)))
;
;  (define (indices/define t fields)
;    (let loop ((i 1) (fields fields) (defines null))
;      (cond
;        ((null? fields) defines)
;        (else
;         (loop (add1 i) (cdr fields) (cons (index/define t (car fields) i) defines))))))
;  
;  `(begin ,@(indices/define t fields)))

;; This macro assumes that struct/fields/index has been applied previously to structure type t and its fields.
;; Given structure type t and structure instance x convert x to a vector (using struct->vector) and
;; for each mutation (field expression) replace the old value of the field in the vector with the
;; value of the expression.
;; For example:
;;    (struct foo (a b c d) #:transparent)
;;    (struct/fields/index foo a b c d)
;;    (define FOO (foo 1 2 3 4))
;;    (struct=>vector* foo FOO (c 33) (a 17))
;(define-macro (struct=>vector* t x . mutations)
;  (define (index/assemble t field)
;    (string->symbol (string-append (symbol->string t) "/" (symbol->string field) "/index")))
;  
;  (let ((local (gensym)))
;    (let loop ((mutations mutations) (resets null))
;      (cond
;        ((null? mutations) `(let ((,local (struct->vector ,x))) ,@resets ,local))
;        
;        (else
;         (let* ((mutation (car mutations))
;                (field (list-ref mutation 0))
;                (reset (list-ref mutation 1)))
;           (loop (cdr mutations)
;                 (cons `(vector-set! ,local ,(index/assemble t field) ,reset) resets))))))))


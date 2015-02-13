#lang racket/base

;; Copyright 2011 Michael M. Gorlick

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;       http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

; Utility routines used to construct the BASELINE and other principal island environs.

(require
 
 (for-syntax racket/base)
 
 (only-in "../persistent/environ.rkt" pairs/environ)
 
 (only-in
  "frame.rkt"
  arguments/pack
  arguments/list/pack
  a/1 a/2 a/3
  a/EMPTY
  arity/verify
  vector/list/copy!)
 (only-in
  "utility.rkt"
  decompile?
  error/motile/internal/call
  k/RETURN))

(provide
 motile/global/0
 motile/global/1
 motile/global/2
 motile/global/3
 motile/global/N
 define/combinator/2
 define/combinator/2-2
 define/combinator/3
 define/combinator/3-3
 define/global/0
 define/global/1
 define/global/2
 define/global/3
 define/global/N
 descriptor/global
 motile/call
 motile/call/3
 motile/decompile
 ;pairs/environ
 ++
 global-defines
 require-spec->global-defines)

(define (descriptor/global symbol)
  (vector-immutable 'reference/global symbol))

;; Many of the persistent functional data structures implement combinators of the form (C d u) where:
;;    C is a combinator of the persistent functional data structure,
;;    d is an instance of the data structure, and 
;;    u a function applied by the combinator.
;; These combinators must be wrapped before they can be used within Motile.

;; Wrapper for combinator that takes two arguments:
;; a data structure instance and a single-argument Motile function.
;; symbol - Motile name for the combinator being wrapped
;; combinator - Racket implementation of the combinator
(define (motile/combinator/2 symbol combinator)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 2 symbol)
         (let* ((instance (a/1  a)) ; Data structure instance.
                (f        (a/2 a)) ; Motile function to be applied by combinator.
                (h (lambda (x) (f k/RETURN (arguments/pack x) g))))
           (k (combinator instance h))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for combinator that takes two arguments:
;; a data structure instance and a two-argument Motile function.
;;
;; symbol     - Motile name for the combinator being wrapped
;; combinator - Racket implementation of the combinator
(define (motile/combinator/2-2 symbol combinator)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 2 symbol)
         (let* ((instance (a/1 a)) ; Data structure instance.
                (f        (a/2 a)) ; Motile function to be applied by combinator.
                (h (lambda (x y) (f k/RETURN (arguments/pack x y) g))))
           (k (combinator instance h))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for combinator that takes three arguments:
;; a data structure instance, a two-argument Motile function, and a seed value.
;; symbol - Motile name for the combinator being wrapped
;; combinator - Racket implementation of the combinator
(define (motile/combinator/3 symbol combinator)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 3 symbol)
         (let* ((instance (a/1  a))
                (f        (a/2 a))
                (seed     (a/3  a))
                (h (lambda (x y) (f k/RETURN (arguments/pack x y) g))))
           (k (combinator instance h seed))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for combinator that takes three arguments:
;; a data structure instance, a three-argument Motile function, and a seed value
;;
;; symbol     - Motile name for the combinator being wrapped
;; combinator - Racket implementation of the combinator
(define (motile/combinator/3-3 symbol combinator) ; 3-3 reflects 3-argument combinator + 3-argument function
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 3 symbol)
         (let* ((instance (a/1 a))
                (f        (a/2 a))
                (seed     (a/3 a))
                (h (lambda (x y z) (f k/RETURN (arguments/pack x y z) g))))
           (k (combinator instance h seed))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; ----- BEGIN KYLE -----
;; The following functions wrap host Scheme primitives allowing those primitives to be invoked
;; within Motile and to be properly "decompiled" for network transmission.

#|(define (symbol->motile-procedure-name symbol n)
  (string->symbol (format "@motile-global@/~a/~a" n symbol)))

(define (motile-named-procedure symbol n wrapper-lambda)
  (cons symbol (procedure-rename wrapper-lambda (symbol->motile-procedure-name symbol n))))

; a special case for procedures whose arities are unconstrainted at environment-construction time.
(define (define/global/N symbol procedure)
  (let ((descriptor (vector 'reference/global symbol)))
    (motile-named-procedure symbol 'N (lambda (k _rte _global . arguments)
                                        (if k
                                            (k (apply procedure arguments))
                                            descriptor)))))

; procedure arities for calls to define-global/K-definer ===> some define/global/K definition
(define global-define-dispatch (make-hash))

(define-syntax (define-global/K-definer stx)
  (syntax-case stx ()
    [(k n) ; n can ONLY be a number literal
     (with-syntax 
         ([id:define/global
           (datum->syntax #'k ; produces define/global/K as an identifier
                          (string->symbol (format "define/global/~a" (syntax->datum #'n))))]
          [(id:global-args ...) ; generated identifiers for use inside define/global/K definition - see below
           (generate-temporaries (build-list (syntax->datum #'n) values))])
       #'(begin
           (define (id:define/global symbol procedure)
             (motile-named-procedure
              symbol n (case-lambda
                         [(k _rte _global id:global-args ...) (k (procedure id:global-args ...))]
                         [(k _rte _global) (unless k (vector 'reference/global symbol))])))
           (hash-set! global-define-dispatch n id:define/global)
           (provide id:define/global)))]))

(define-global/K-definer 0)
(define-global/K-definer 1)
(define-global/K-definer 2)
(define-global/K-definer 3)
(define-global/K-definer 4)
(define-global/K-definer 5)
(define-global/K-definer 6)
(define-global/K-definer 7)
(define-global/K-definer 8)
(define-global/K-definer 9)
(define-global/K-definer 10)
------ END KYLE ----------|#

;; The following helper functions: motile/global/0, ..., motile/global/3
;; wrap host Scheme functions so that they can be called from Motile programs.

;; Wrapper for zero-argument host procedures.
(define (motile/global/0 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 0 symbol)
         (k (procedure)))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for one-argument host procedures.
(define (motile/global/1 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 1 symbol)
         (k (procedure (a/1 a))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for two-argument host procedures.
(define (motile/global/2 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 2 symbol)
         (k (procedure (a/1 a) (a/2 a))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for three-argument host procedures.
(define (motile/global/3 symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (arity/verify a 3 symbol)
         (k (procedure (a/1 a) (a/2 a) (a/3 a))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Wrapper for host procedures that accept more than three arguments
;; or accept a variable number of arguments.
(define (motile/global/N symbol procedure)
  (let ((descriptor (descriptor/global symbol)))
    (lambda (k a g)
      (cond
        ((procedure? k)
         (k (apply procedure (cdr (vector->list a)))))
        ((decompile? k a g) descriptor)
        (else (error/motile/internal/call symbol))))))

;; Convenient helper macros for building global binding environments.
(define-syntax-rule (define/global/0 symbol procedure)
  (cons symbol (motile/global/0 symbol procedure)))

(define-syntax-rule (define/global/1 symbol procedure)
  (cons symbol (motile/global/1 symbol procedure)))

(define-syntax-rule (define/global/2 symbol procedure)
  (cons symbol (motile/global/2 symbol procedure)))

(define-syntax-rule (define/global/3 symbol procedure)
  (cons symbol (motile/global/3 symbol procedure)))

(define-syntax-rule (define/global/N symbol procedure)
  (cons symbol (motile/global/N symbol procedure)))

(define-syntax-rule (define/combinator/2 symbol combinator)
  (cons symbol (motile/combinator/2 symbol combinator)))

(define-syntax-rule (define/combinator/2-2 symbol combinator)
  (cons symbol (motile/combinator/2-2 symbol combinator)))

(define-syntax-rule (define/combinator/3 symbol combinator)
  (cons symbol (motile/combinator/3 symbol combinator)))

(define-syntax-rule (define/combinator/3-3 symbol combinator)
  (cons symbol (motile/combinator/3-3 symbol combinator)))

; global-defines: for each function P in the arguments list:
; - if P has arity K and only K, produce a call to the arity-correct define/global/K function to wrap P.
; - if P is a multiple-arity function wrap it with define/global/N.
; - if P is not a function just cons it with its symbol.
; result: form a list of all produced global definitions, as expected by ++
(define global-define-dispatch (vector motile/global/0 motile/global/1 motile/global/2 motile/global/3))
(define-syntax global-defines
  (syntax-rules ()
    [(_ p ...)
     (list (cond [(not (symbol? 'p))
                  (error (format "error: unnameable procedure ~a given to global-defines" p))]
                 [(procedure? p)
                  (define arity (procedure-arity p))
                  (cond [(and (exact-nonnegative-integer? arity) 
                              (< arity (vector-length global-define-dispatch)))
                         (cons 'p ((vector-ref global-define-dispatch arity) 'p p))]
                        [else 
                         (define/global/N 'p p)])]
                 [else 
                  (cons 'p p)])
           ...)]))

; require-spec->global-defines:
; take the imports instantiated from a module via a require-spec and produce global-defines out of them.
; note: current implementation requires callee to use `(except-in spec macro1 macro2 macro3)'
; since there appears to be no way to filter out macro identifiers at macro processing time.
(require (for-syntax racket/require-transform))
(define-syntax (require-spec->global-defines stx)
  (syntax-case stx ()
    [(k spec)
     ; process the require-spec to get imported IDs
     (let-values ([(imports import-sources) (expand-import #'spec)])
       (with-syntax ([(names ...) (map import-local-id imports)]) ; pull the IDs out
         ; rewrite in terms of a call to global-defines
         #'(global-defines names ...)))]))

; add one or more (list (define/global/A 'foo foo) (define/global/B 'bar bar) ...)
; to an environment yielding a new environment
(define (++ base-environment . global-defines-lists)
  (define (flip f) (λ (b a) (f a b)))
  (foldl (flip pairs/environ) base-environment global-defines-lists))

;; This is the inverse of the helper macros above.
;; It allows a Scheme host function to a call a Motile function f in the context of global binding environ g.
;; The value returned is the value computed by f.
(define (motile/call f g . arguments)
  (f k/RETURN (if (null? arguments) #f (arguments/list/pack arguments)) g))

;; Like motile/call but the arguments to f must be assembled as a list.
(define (motile/call/3 f g arguments)
  (f k/RETURN (if (null? arguments) #f (arguments/list/pack arguments)) g))

;; Given a Motile code closure c returns the decompilation of c as a
;; Motile Assembly Graph (MAG).
(define (motile/decompile c) (c #f #f #f))

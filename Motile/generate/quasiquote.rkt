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

;; Code generation for quasiquotation.

(require
 (only-in "utility.rkt"             bind/return! decompile? error/motile/internal/call)
 (only-in "baseline.rkt"            motile/decompile)
 (only-in "../persistent/tuple.rkt" list/tuple))

(provide
 quasiquote/append/generate
 quasiquote/cons/generate
 quasiquote/tuple/generate)

(define (descriptor/quasiquote/append head tail)
  (vector-immutable 'quasiquote/append (motile/decompile head) (motile/decompile tail)))

(define (quasiquote/append/generate head tail)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (head
          (lambda (h) 
            (tail (lambda (t) (k (append h t))) e g))
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/quasiquote/append head tail)))
        (else (error/motile/internal/call 'quasiquote/append/generate))))))

;(define (form/append/generate head tail)
;  (lambda (rtk rte)
;    (head
;     (lambda (h)
;       (tail
;        (lambda (t) (rtk (append h t)))
;        rte))
;     rte)))

(define (descriptor/quasiquote/cons head tail)
  (vector-immutable 'quasiquote/cons (motile/decompile head) (motile/decompile tail)))

(define (quasiquote/cons/generate alpha beta)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (alpha
          (lambda (a)
            (beta (lambda (b) (k (cons a b))) e g))
          e g))
         ((decompile? k e g) (bind/return! descriptor (descriptor/quasiquote/cons alpha beta)))
         (else (error/motile/internal/call 'quasiquote/cons/generate))))))

;(define (form/cons/generate alpha beta)
;  (lambda (rtk rte)
;    (alpha
;     (lambda (a)
;       (beta
;        (lambda (b) (rtk (cons a b)))
;        rte))
;     rte)))

(define (descriptor/quasiquote/tuple t)
  (vector-immutable 'quasiquote/tuple (motile/decompile t)))

(define (quasiquote/tuple/generate t)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (t (lambda (x) (k (list/tuple x))) e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/quasiquote/tuple t)))
        (else (error/motile/internal/call 'quasiquote/tuple/generate))))))

;(define (form/vector/generate c)
;  (lambda (rtk rte)
;    (c
;     (lambda (x) (rtk (list->vector x)))
;     rte)))

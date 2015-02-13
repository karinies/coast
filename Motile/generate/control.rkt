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

;; Code generation for the basic control special forms.

(require
 (only-in racket/vector vector-map)

 (only-in
  "utility.rkt"
  bind/return!
  decompile?
  error/motile/internal/call)
 (only-in "baseline.rkt" motile/decompile))

(provide
 and/generate
 if/generate
 or/generate
 sequence/generate
 when/generate
 unless/generate)

(define-syntax-rule (1+ n) (add1 n))
(define-syntax-rule (1- n) (sub1 n))

(define (descriptor/if test then else)
  (vector-immutable 'if (motile/decompile test) (motile/decompile then) (motile/decompile else)))

;; Code generation for (if <test> <then> <else>).
(define (if/generate test then else)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (test
          (lambda (x) (if x (then k e g) (else k e g))) ; Continuation for <test>.
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/if test then else)))
        (else (error/motile/internal/call 'if/generate))))))

(define (descriptor/when test thens)
  (vector-immutable 'when (motile/decompile test) (motile/decompile thens)))

;; Code generation for (when <test> <thens>).
(define (when/generate test thens)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (test
          (lambda (x) (if x (thens k e g) (k (void)))) ; Continuation for <test>.
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/when test thens)))
        (else (error/motile/internal/call 'when/generate))))))

(define (descriptor/unless test thens)
  (vector-immutable 'unless (motile/decompile test) (motile/decompile thens)))

;; Code generation for (unless <test> <thens>).
(define (unless/generate test thens)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (test
          (lambda (x) (if (not x) (thens k e g) (k (void)))) ; Continuation for <test>.
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/unless test thens)))
        (else (error/motile/internal/call 'unless/generate))))))


(define (OLD/descriptor/sequence n codes)
  (vector-immutable
   'sequence n
   (list->vector (map (lambda (c) (motile/decompile c)) codes))))

;; Code generation for a nonempty sequence of closures (c_1 c_2 ... c_N) ensuring
;;   left-to-right evaluation of closures c_i, and
;;   return of the value of the evaluation of c_N.
(define (OLD/sequence/generate n codes)
  (if (= n 1)
      (car codes)

      (let ((descriptor #f)
            (c (car codes))
            (tail (sequence/generate (sub1 n) (cdr codes))))
        (lambda (k e g)
          (cond
            ((procedure? k)
             (c (lambda (_) (tail k e g)) e g))
            ((decompile? k e g) (bind/return! descriptor (descriptor/sequence n codes)))
            (else (error/motile/internal/call 'sequence/generate)))))))

(define (descriptor/sequence n codes)
  (vector-immutable
   'sequence n
   (vector-map (lambda (c) (motile/decompile c)) codes)))

(define (sequence/generate n codes)
  (let loop ((i 0) (last (1- n)))
    (if (= i last)
        (vector-ref codes i)

        (let ((descriptor #f)
              (c (vector-ref codes i))
              (tail (loop (1+ i) last)))
          (lambda (k e g)
            (cond
              ((procedure? k)
               (c (lambda (_) (tail k e g)) e g))
              ((decompile? k e g) (bind/return! descriptor (descriptor/sequence n codes)))
              (else (error/motile/internal/call 'sequence/generate))))))))
          


(define (descriptor/and head tail)
  (vector-immutable 'and (motile/decompile head) (motile/decompile tail)))

;; Code generation for (and <head> . <tail>)
(define (and/generate head tail)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (head (lambda (x) (if x (tail k e g) (k #f))) e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/and head tail)))
        (else (error/motile/internal/call 'and/generate))))))

(define (descriptor/or head tail)
  (vector-immutable 'or (motile/decompile head) (motile/decompile tail)))

;; Code generation for (or <head> . <tail>)
(define (or/generate head tail)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (head (lambda (x) (if x (k x) (tail k e g))) e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/or head tail)))
        (else (error/motile/internal/call 'or/generate))))))
    


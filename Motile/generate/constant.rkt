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

;; Code generation for literal constants.

(require
 (only-in
  "utility.rkt"
  bind/return!
  decompile?
  error/motile/internal/call))

(provide
 constant/generate)

(define (descriptor/constant c)
  (vector-immutable 'constant c))

(define-syntax-rule (macro/constant/generate c)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k) (k c))
        ((decompile? k e g) (bind/return! descriptor (descriptor/constant c)))
        (else (error/motile/internal/call 'constant/generate))))))
       
;; Generate a closure for a constant value.
(define (constant/generate value)
  (cond
    ((eqv? value 0)      (macro/constant/generate 0))
    ((eqv? value 1)      (macro/constant/generate 1))
    ((eqv? value 2)      (macro/constant/generate 2))
    ((eqv? value null)   (macro/constant/generate null))
    ((eqv? value #t)     (macro/constant/generate #t))
    ((eqv? value #f)     (macro/constant/generate #f))
    ((eqv? value (void)) (macro/constant/generate (void)))
    (else                (constant/generate/value value))))
    
(define (constant/generate/value x)
  (macro/constant/generate x))
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

;; Miscellaneous utility routings shared by the code generators.


(provide
 bind!
 bind/return!
 decompile?
 error/closure/arity
 error/closure/arity/any
 error/motile/internal/call
 error/motile/type
 k/RETURN
 macro/motile/call
 motile/decompile)
 
;; Used by lambdas and closures at run-time to raise an error when an
;; invocation is neither a proper application or a request for decompilation.
(define (error/motile/internal/call tag)
  (error/motile/internal tag "not call or decompile"))

;; Raises a Motile run-time internal error.
;; tag - symbol giving hint where error arose
;; message - brief informative text
(define-syntax-rule (error/motile/internal tag message)
  (error tag (format "Motile internal error: ~a" message)))

;; Convenient shorthand for reporting arity mismatch.
;; tag - symbolic name of primitive Motile function
;; m - number of user arguments expected
;; n - number of user arguments actually received
(define-syntax-rule (error/closure/arity tag m n)
  (error tag "expects ~a argument(s) but got ~a" m n))

(define-syntax-rule (error/motile/type symbol type where got)
  (error symbol "expected type ~a at argument ~a but got ~a" type where got))

;; Catchall version of the above.
;; tag - symbolic name of primitive Motile function
;; m - number of user arguments expected
;; arguments - list of all arguments to primitive
(define-syntax-rule (error/closure/arity/any tag m arguments)
  (error/closure/arity tag m (- (length arguments) 3))) ; Discount k, e, and g, the first three "hidden" arguments of any Motile closure.

;; Return #t if a decompile is requested and #f otherwise.
;; The compiler convention is that if the continuation, lexical run-time environment, and the global binding environment
;; are all #f then the Motile closure should return its decompilation.
(define (decompile? k e g) (and (not k) (not e) (not g)))

;; Ask Motile code to decompile itself and return the decompilation graph.
(define (motile/decompile x) (x #f #f #f))

;; Helper macro which sets a variable iff the variable is #f.
(define-syntax-rule (bind! variable value)
  (unless variable (set! variable value)))
;; An extension of bind! which, upon setting the variable, also returns its value.
(define-syntax-rule (bind/return! variable value)
  (begin
    (unless variable (set! variable value))
    variable))

;; Convenience macro for a host Scheme function to call a Motile function f
;; with continuation k, global binding environment g, and arguments x ... .
(define-syntax-rule (macro/motile/call f k g x ...) (f k (arguments/pack x ...) g))

;; The trivial continuation.
(define (k/RETURN x) x)
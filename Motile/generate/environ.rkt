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

;; Code generation for binding environment special forms.

(require
 (only-in racket/vector vector-map)

 (only-in
  "utility.rkt"
  bind/return!
  decompile?
  error/motile/internal/call
  k/RETURN)

 (only-in "baseline.rkt" motile/decompile)
 
 (only-in "../../persistent/environ.rkt" environ/ref/symbol environ/ref/list environ/ref/vector environ/vector/remove vectors/environ))

(provide
 environ/cons/generate
 environ/reflect/generate
 environ/remove/generate
 environ/ref/path/generate
 environ/ref/symbol/generate)

;; environ - closure whose evaluation returns an environ.
;; identfiers - vector of symbols
;; values - vector of closures for identifier values
(define (descriptor/environ/cons environ identifiers values)
  (vector-immutable
   'environ/cons
   (vector-length identifiers)
   (motile/decompile environ)
   identifiers
   (vector-map (lambda (x) (motile/decompile x)) values)))

;; environ - closure whose evaluation returns a Motile binding environment
;; symbols - a vector of symbols s_1 ... s_n
;; values - a vector of Motile closures v_1 ... v_n
(define (environ/cons/generate environ symbols values)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (let ((Vs (vector-map (lambda (x) (x k/RETURN e g)) values))) ; Just lexical lookup at the Motile-level.
           (environ
            (lambda (E) (k (vectors/environ E symbols Vs))) ; Continuation for environ Motile closure.
            e g)))
        ((decompile? k e g) (bind/return! descriptor (descriptor/environ/cons environ symbols values)))
        (else (error/motile/internal/call 'environ/cons/generate))))))
        
;; environ - closure whose evaluation returns an environ
;; symbol - name for binding in environ
;; failure - closure for value if symbol not present in environ
(define (descriptor/environ/ref environ symbol failure)
  (vector-immutable 'environ/ref (motile/decompile environ) symbol (motile/decompile failure)))

(define (environ/ref/symbol/generate environ symbol failure)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         ; (1) Evaluate the environ/code to obtain an environ E.
         ; (2) Evaluate the failure/code to obtain a failure value F.
         ; (3) Lookup the symbol in environ E.
         (environ
          (lambda (E) ; Continuation for evaluation of environ.
            (failure 
             (lambda (F) (k (environ/ref/symbol E symbol F))) ; Continuation for evaluation of failure value.
             e g))
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/environ/ref environ symbol failure)))
        (else (error/motile/internal/call 'environ/ref/symbol/generate))))))

;; environ - closure whose evaluation returns an environ
;; path - literal list of symbols (NOT quoted since environ/ref is a special form) OR
;;        literal vector of symbols
;; failure - closure whose evaluation returns a failure value
(define (environ/ref/path/generate environ path failure)
  (let ([environ/ref/path (if (pair? path) environ/ref/list environ/ref/vector)]
        [descriptor #f])
    (lambda (k e g)
      (cond
        ((procedure? k)
         (environ
          (lambda (E) ; Continuation for evaluation of environ.
            (failure
             (lambda (F) (k (environ/ref/path E path F))) ; Continuation for evaluation of failure value.
             e g))
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/environ/ref environ path failure)))
        (else (error/motile/internal/call 'environ/ref/path/generate))))))


;; environ - closure whose evaluation returns an environ
;; symbols - vector of symbols
(define (descriptor/environ/remove environ symbols)
  (vector-immutable 'environ/remove (vector-length symbols) (motile/decompile environ) symbols))

;; environ - a closure that returns a Motile binding environment
;; symbols - a non-empty vector of symbols
(define (environ/remove/generate environ symbols)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (environ (lambda (E) (k (environ/vector/remove E symbols))) e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/environ/remove environ symbols)))
        (else (error/motile/internal/call 'environ/remove/generate))))))

(define (descriptor/environ/reflect global body)
  (vector-immutable 'environ/reflect (motile/decompile global) (motile/decompile body)))

;; environ - closure whose evaluation returns a Motile binding environment E
;; body - a code body to be evaluated in the context of E
(define (environ/reflect/generate environ body)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k)
         (environ
          (lambda (G) (body k e G)) ; Evaluate the body in the context of the reflected global binding environment.
          e g))
        ((decompile? k e g) (bind/return! descriptor (descriptor/environ/reflect environ body)))
        (else (error/motile/internal/call 'environ/reflect/generate))))))


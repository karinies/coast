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

(require
 (only-in
  "frame.rkt"
  a/1 a/2 a/3
  a/1! a/2! a/3! a/4!
  a/rest
  frame/load! frame/load/rest! frame/push reframe
  arity/verify arity/rest/verify
  reference/free)
 
 (only-in
  "utility.rkt"
  bind!
  bind/return!
  decompile?
  error/motile/internal/call
  motile/decompile))

(provide
 closure/generate
 closure/rest/generate)


;; In all of these closure code generators we delay construction of the closure bindings until the
;; closure is called for the first time, thereby guaranteeing that any letrec bindings on which the closure
;; may depend hav been fully resolved.

;; Generate decompilation descriptors for inner and outer closures.
(define (descriptor/closure/inner m n e addresses body)
  (vector-immutable 'closure/inner m n e addresses (motile/decompile body)))

(define (descriptor/closure/outer m n addresses body)
  (vector-immutable 'closure/outer m n addresses (motile/decompile body)))

;; These are the decompilation descriptors for closures with a rest parameter.
(define (descriptor/closure/rest/inner m n e addresses body)
  (vector-immutable 'closure/rest/inner m n e addresses (motile/decompile body)))

(define (descriptor/closure/rest/outer m n addresses body)
  (vector-immutable 'closure/rest/outer m n addresses (motile/decompile body)))


;; Let s be a frame skeleton where slot 0 is reserved for the lexical environment at point of definition,
;; slots 1, ..., offset-1, are reserved for the calling arguments,
;; and slots offset, offset+1, ... are reserved for the closed variable bindings.
;; Insert the closed variable bindings into the tail of skeleton s and return s.
(define (skeleton/fill! s offset n e addresses)
  (vector-set! s 0 e) ; Push the skeleton onto the stack.
  ; Fill in the skeleton with the bindings of the closed variables.
  (let loop ((i 0))
    (unless (>= i n)
      (vector-set! s (+ i offset) (reference/free e (vector-ref addresses i)))
      (loop (add1 i))))
  s)

;; Return a fresh frame frame skeleton with all closure binding slots initialized.
(define (frame/skeleton/new m n e addresses)
  (let ((offset (add1 m)))
    (skeleton/fill! (skeleton/new m n) offset n e addresses)))

;; Create a fresh empty frame skeleton.
;; m - the number of arguments >= 0
;; n - the number of closed variables > 0
;; A frame skeleton has the structure:
;;   slot 0 - prior lexical environ
;;   slots 1 ... m - the m lambda arguments
;;   slots m+1 ... m+n - the n closed variable bindings
(define-syntax-rule (skeleton/new m n) (make-vector (+ 1 m n) #f))

;; Code generation for closures without a rest argument.

;; One or more closed variables in lambda body.
;; m: total number of formal parameters
;; n: total number of closed variables
;; addresses: vetor of frame addresses of closed variables
;; body: code body of lambda expression
(define (closure/generate m n addresses body)
    (cond
      ((= m 0) (closure/0/N/generate   n addresses body))
      ((= m 1) (closure/1/N/generate   n addresses body))
      ((= m 2) (closure/2/N/generate   n addresses body))
      ((= m 3) (closure/3/N/generate   n addresses body))
      (else    (closure/M/N/generate m n addresses body))))

;; The special cases closure/0/N/... to /closure/3/N/... are simply loop unrolled versions
;; of the general case closure/M/N/... .
;; I have not established by experiment that this actually yields any performance improvement over simply doing
;; a vector-copy!

;; Recall that the arguments vector a passed to (lambda (k a g) ...) has the structure #(#f a_1 ... a_m).
;; This is an optimization exploited by lambdas without any closed variables that allows them to reuse the argument vector
;; itself as a stack frame (thereby avoiding the cost of allocating a fresh stack frame and copying the arguments to it).
;; In the case of closures (lambdas with closed variables) the frame includes the bindings for the closed variables so this
;; trick just won't work.
;; Again, at this point, I don't have any measurements to validate this optimization.

;; Zero arguments + n > 0 free variables.
(define (closure/0/N/generate n addresses body)
  (lambda (k e _) ; k - continuation, e - lexical scope run-time stack, _ - ignore.
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g) ; k - continuation, a - vector of arguments, g - global binding environ.
             (cond
               ((procedure? k)
                (arity/verify a 0 'closure/0/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 0 n e addresses)) ; Construct closure bindings.
                (body k (frame/push e (reframe frame/skeleton)) g))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/inner 0 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/0/N/generate))))))
        
        ; We are decompiling the closure at point of definition. 
        (descriptor/closure/outer 0 n addresses body))))
        
;; One argument + n > 0 free variables.
(define (closure/1/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/verify a 1 'closure/1/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 1 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/1 a))
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/inner 1 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/1/N/generate))))))
        
        ; We are decompiling the closure at point of definition. 
        (descriptor/closure/outer 1 n addresses body))))

;; Two arguments + n > 0 free variables.
(define (closure/2/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/verify a 2 'closure/2/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 2 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/1 a))
                  (a/2! frame (a/2 a))
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/inner 2 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/2/N/generate))))))
        
        ; We are decompiling the closure at point of definition. 
        (descriptor/closure/outer 2 n addresses body))))

;; Three arguments + n > 0 free variables.
(define (closure/3/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/verify a 3 'closure/3/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 3 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/1 a))
                  (a/2! frame (a/2 a))
                  (a/3! frame (a/3 a))
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/inner 3 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/3/N/generate))))))
        
        ; We are decompiling the closure at point of definition. 
        (descriptor/closure/outer 3 n addresses body))))

;; m > 3 arguments + n > 0 free variables.
(define (closure/M/N/generate m n addresses body)
    (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/verify a m 'closure/M/N/generate)
                (bind! frame/skeleton (frame/skeleton/new m n e addresses)) ; Construct closure bindings.
                ; Copy all m arguments into the new body frame.
                (body k (frame/push e (frame/load! (reframe frame/skeleton) a)) g))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/inner m n e addresses body)))
               (else
                (error/motile/internal/call 'closure/M/N/generate))))))
        
        ; We are decompiling the closure at point of definition. 
        (descriptor/closure/outer m n addresses body))))

;; Code generation for closures with a rest argument.

;; One or closed variables in the lambda body and the last formal parameter is a rest argument.
;; m: total number of formal parameters including the rest argument
;; addresses: list of lexical addresses of closed variables
;; body: code body of lambda expression
(define (closure/rest/generate m n addresses body)
  (cond
    ((= m 1) (closure/rest/1/N/generate   n addresses body))
    ((= m 2) (closure/rest/2/N/generate   n addresses body))
    ((= m 3) (closure/rest/3/N/generate   n addresses body))
    ((= m 4) (closure/rest/4/N/generate   n addresses body))
    (else    (closure/rest/M/N/generate m n addresses body))))

;; Zero arguments + rest parameter + n > 0 closed variables.
(define (closure/rest/1/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (bind! frame/skeleton (frame/skeleton/new 1 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/rest a 1))
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/rest/inner 1 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/rest/0/N/generate))))))

        (descriptor/closure/rest/outer 1 n addresses body))))

;; One argument + rest parameter + n > 0 closed variables.
(define (closure/rest/2/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/rest/verify a 2 'closure/rest/1/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 2 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/1 a))
                  (a/2! frame (a/rest a 2)) ; (a_2 a_3 ...) is the rest argument.
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/rest/inner 2 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/rest/2/N/generate))))))

        (descriptor/closure/rest/outer 2 n addresses body))))

;; Two arguments + rest parameter + n > 0 closed variables.
(define (closure/rest/3/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/rest/verify a 3 'closure/rest/3/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 3 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/1 a))
                  (a/2! frame (a/2 a))
                  (a/3! frame (a/rest a 3)) ; (a_3 a_4 ...) is the rest argument.
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/rest/inner 3 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/rest/3/N/generate))))))

        (descriptor/closure/rest/outer 3 n addresses body))))

;; Three arguments + rest parameter + n > 0 closed variables.
(define (closure/rest/4/N/generate n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/rest/verify a 4 'closure/rest/3/N/generate)
                (bind! frame/skeleton (frame/skeleton/new 4 n e addresses)) ; Construct closure bindings.
                (let ((frame (reframe frame/skeleton)))
                  (a/1! frame (a/1 a))
                  (a/2! frame (a/2 a))
                  (a/3! frame (a/3 a))
                  (a/4! frame (a/rest a 4)) ; (a_4 a_5 ...) is the rest argument.
                  (body k (frame/push e frame) g)))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/rest/inner 4 n e addresses body)))
               (else
                (error/motile/internal/call 'closure/rest/3/N/generate))))))

        (descriptor/closure/rest/outer 4 n addresses body))))

;; m > 4 total arguments including a rest parameter + n > 0 closed variables.
(define (closure/rest/M/N/generate m n addresses body)
  (lambda (k e _)
    (if (procedure? k)
        (k
         (let ((frame/skeleton #f)
               (descriptor     #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/rest/verify a m 'closure/rest/M/N/generate)
                (bind! frame/skeleton (frame/skeleton/new m n e addresses)) ; Construct closure bindings.
                (body k (frame/push e (frame/load/rest! (reframe frame/skeleton) a m)) g))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/closure/rest/inner m n e addresses body)))
               (else
                (error/motile/internal/call 'closure/rest/M/N/generate))))))

        (descriptor/closure/rest/outer m n addresses body))))

  
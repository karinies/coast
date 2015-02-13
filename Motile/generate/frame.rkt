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

;; Stack frame manipulation and accessors.

(require
 (only-in racket/vector vector-copy)
 (only-in "utility.rkt" bind/return! decompile? error/motile/internal/call)
 (only-in "../../persistent/environ.rkt" environ/ref/symbol))

(provide
 arity/verify
 a/1 a/2 a/3 a/last
 a/1! a/2! a/3! a/4! a/n!
 a/arity
 a/EMPTY
 a/flatten
 a/rest
 arguments/pack
 arguments/list/pack
 arity/rest/verify
 frame/new frame/pop frame/push reframe frame/load! frame/load/rest!
 reference/free global/get/generate variable/get/generate
 stack/depth
 vector/list/copy!)

;; Boilerplate for confirming arity for lambdas and closures with a fixed number of arguments.
;; a - argument frame passed to function
;; n - expected arity
;; tag - symbol (name) for function in question
(define (arity/verify a n tag)
  (unless (= n (a/arity a))
    ; Raise an error if the actual number of arguments doesn't match the expectations.
    (error tag "expected ~a argument(s) but got ~a" n (a/arity a))))

;; Boilerplate for confirming arity for lambdas and closures with a total of n arguments, one of which is a
;; rest parameter.
(define (arity/rest/verify a n tag)
  (when (< (a/arity a) (sub1 n)) ; There are n-1 required arguments.
    (error tag "expected at least ~a argument(s) but got only ~a" (sub1 n) (a/arity a))))

;; Pack a series of arguments into a vector.
;; As an optimization for calling lambdas in which no free variables appear in the body
;; we reserve slot 0 of the vector for the lexical stack.
;; This allows lambdas to forego copying the argument vector and instead simply set slot 0
;; to their lexical stack instance.
(define-syntax-rule (arguments/pack x ...) (vector #f x ...))
;; Given a list of items pack the contents of that list as an argument frame.
(define-syntax-rule (arguments/list/pack items)     (list->vector (cons #f items)))

;; Compute the total number of arguments in the pack accounting for the fact that slot 0 is
;; reserved for the prior lexical stack.
(define-syntax-rule (a/arity v) ;(sub1 (vector-length v)))
  (if (not v) 0 (sub1 (vector-length v))))

;; Accessors for picking apart an argument frame.
(define-syntax-rule (a/1 a)    (vector-ref a 1))
(define-syntax-rule (a/2 a)    (vector-ref a 2))
(define-syntax-rule (a/3 a)    (vector-ref a 3))
(define-syntax-rule (a/last a) (vector-ref a (a/arity a)))

(define-syntax-rule (a/1! a v) (vector-set! a 1 v))
(define-syntax-rule (a/2! a v) (vector-set! a 2 v))
(define-syntax-rule (a/3! a v) (vector-set! a 3 v))
(define-syntax-rule (a/4! a v) (vector-set! a 4 v))
(define-syntax-rule (a/n! a n v) (vector-set! a n v))

;; The universal empty argument frame.
;; It is immutable because we should always optimize it away and if we fail to do so then
;; a some point a run-time error will be generated.
(define a/EMPTY #(#f))

;; Given an argument frame #(#f x_1 ... x_m (y_1 ... y_n)) where the last argument is a list (y_1 ... y_m)
;; and the first k <= m arguments are to be skipped over
;; return a flattened argument frame #(#f x_{k+1} ... x_m y_1 ... y_n).
;; a - argument vector
;; skip - number of arguments >= 0 to be skipped over
(define (a/flatten a skip)
  (let* ((arity (a/arity a))
         (rest  (a/last a))
         (m     (sub1 arity))
         (n     (length rest))
         (v     (make-vector (+ 1 (- m skip) n))))
    (vector-set!  v 0 #f)
    (vector-copy! v 1 a (add1 skip) (add1 m)) ; Copy x_{k+1} ... x_m into v at slots 1, ..., m-k
    (vector/list/copy! v (add1 (- m skip)) rest) ; Copy y_1 ... y_n into v at slots m-k+1, ..., m-k+n.
    v))

;; Utility routine for constructing the value of a rest parameter from an argument frame.
;; Returns the list (a[start] ... a[n-1]) where n is the length of a.
(define (a/rest a start)
  (let loop ((i (sub1 (vector-length a))) (rest null))
    (if (>= i start)
        (loop (sub1 i) (cons (vector-ref a i) rest))
        rest)))


;; Create an argument frame with slots for n argument values.
;; Slot 0 is always reserved for a reference to the prior frame.
(define (frame/new n) (make-vector (add1 n)))

;; Return a duplicate of frame f.
(define (reframe f) (vector-copy f))

;; Destructively load frame f with the contents of argument frame a.
;; Return the loaded frame f as the value.
(define (frame/load! f a)
  (vector-copy! f 1 a 1)
  f)

;; Frame loading when there are a total of m arguments including a rest argument.
;; Destructively load frame f with the first m-1 bindings of argument frame a and
;; then load frame f slot m with the list (a_m ... a_n) where n is the total
;; number of arguments in a.
;; Return the loaded frame f as the value.
(define (frame/load/rest! f a m)
  (vector-copy! f 1 a 1 m)
  (vector-set!  f m (a/rest a m))
  f)

;; Push frame f onto stack e returning f as the new top of stack e.
(define (frame/push e f)
  (vector-set! f 0 e)
  f)

;; Pop a frame off stack e returning the new top of stack.
(define-syntax-rule (frame/pop e) (vector-ref e 0))

;; Fetch a binding from a frame on the run-time lexical stack.
;; Each frame is a vector #(prior b_1 b_2 ... b_n) where
;;   prior is a reference to the previous frame on the stack (or #f) if the frame is the bottom frame
;;   b_i is a binding at slot i of the frame.
;; Frames are addressed as 0, 1, ... starting with 0 for the topmost frame.
;; Bindings within a frame are addressed as 1, 2, ...
;; e - run-time lexical stack
;; address - pair (frame . offset) giving the frame number and offset within frame of a binding
(define (reference/free e address)
  (let loop ((e e)
             (frame  (car address))
             (offset (cdr address)))
    (cond
      ((= frame 0) (vector-ref e offset))
      ((= frame 1) (vector-ref (frame/pop e) offset))
      ((= frame 2) (vector-ref (frame/pop (frame/pop e)) offset))
      ((= frame 3) (vector-ref (frame/pop (frame/pop (frame/pop e))) offset))
      (else        (loop (frame/pop e) (sub1 frame) offset)))))

(define (descriptor/variable/get offset) (vector-immutable 'variable/get offset))

;; Access the value of an argument or a closed variable within the body of a lambda or closure.
;; In other words, the offset is an index into the topmost frame of the run-time stack.
(define (variable/get/generate offset)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k) (k (vector-ref e offset)))
        ((decompile? k e g) (bind/return! descriptor (descriptor/variable/get offset)))
        (else (error/motile/internal/call 'variable/get/generate))))))

(define (descriptor/global/get symbol) (vector-immutable 'global/get symbol))

(define (global/get/generate symbol)
  (let ((descriptor #f))
    (lambda (k e g)
      (cond
        ((procedure? k) (k (global/find g symbol)))
        ((decompile? k e g) (bind/return! descriptor (descriptor/global/get symbol)))
        (else (error/motile/internal/call 'global/get/generate))))))

(define UNDEFINED (gensym 'undefined.))

(define (global/find g symbol)
  (let ((binding (environ/ref/symbol g symbol UNDEFINED)))
    (if (eq? binding UNDEFINED)
        (error 'motile/global/find "undefined: ~s" symbol)
        binding)))

;; Destructive copy into vector v starting at index i of the contents of list L.
(define (vector/list/copy! v i L)
  (cond
    ((null? L) v)
    (else
     (vector-set! v i (car L))
     (vector/list/copy! v (add1 i) (cdr L)))))

(define (stack/depth e)
  (let loop ((i 0) (e e))
    (if (not e) i (loop (add1 i) (frame/pop e)))))

;; Dump the stack e as a list of frames (f_0 f_1 ... f_m).
(define (stack/dump e)
  (let loop ((dump null) (e e))
    (if (not e)
        (reverse dump)
        (loop
         (cons (list->vector (cdr (vector->list e))) dump)
         (frame/pop e)))))
        
        


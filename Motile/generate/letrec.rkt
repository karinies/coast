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

(require
 (only-in "utility.rkt" decompile? error/motile/internal/call)

 (only-in
  "frame.rkt"
  a/1 a/2 a/3
  a/1! a/2! a/3! a/n!
  arity/verify
  frame/pop
  frame/push))

(provide letrec/set/generate letrec*/set/generate)

(define (letrec/set/generate n)
  (cond
    ((= n 1) (letrec/set/1/generate))
    ((= n 2) (letrec/set/2/generate))
    ((= n 3) (letrec/set/3/generate))
    (else    (letrec/set/N/generate n))))

(define descriptor/letrec/set/1 (vector-immutable 'letrec/set 1))

(define (letrec/set/1/generate)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k (a/1! (frame/pop e) (a/1 e)))) ; slot (1 . 1) := slot (0 . 1).
      ((decompile? k e g) descriptor/letrec/set/1)
      (else (error/motile/internal/call 'letrec/set/1)))))

(define descriptor/letrec/set/2 (vector-immutable 'letrec/set 2))

(define (letrec/set/2/generate)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k
        (let ((prior (frame/pop e)))
          (a/1! prior (a/1 e))    ; slot (1 . 1) := slot (0 . 1).
          (a/2! prior (a/2 e))))) ; slot (1 . 2) := slot (0 . 2).
      ((decompile? k e g) descriptor/letrec/set/2)
      (else (error/motile/internal/call 'letrec/set/2)))))
      
(define descriptor/letrec/set/3 (vector-immutable 'letrec/set 3))

(define (letrec/set/3/generate)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k
        (let ((prior (frame/pop e)))
          (a/1! prior (a/1 e))    ; slot (1 . 1) := slot (0 . 1).
          (a/2! prior (a/2 e))    ; slot (1 . 2) := slot (0 . 2).
          (a/3! prior (a/3 e))))) ; slot (1 . 3) := slot (0 . 3).
      ((decompile? k e g) descriptor/letrec/set/3))))

(define (descriptor/letrec/set/N n) (vector-immutable 'letrec/set n))

(define (letrec/set/N/generate n)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k (vector-copy! (frame/pop e) 1 e 1)))
      ((decompile? k e g) (descriptor/letrec/set/N n))
      (else (error/motile/internal/call 'letrec/set/N)))))

(define (letrec*/set/generate i)
  (lambda (k e g)
    (cond
      ((procedure? k)
       (k
        (lambda (k a _)
          (k
           (begin
             (arity/verify a  1 'letrec*/set/generate)
             (a/n! e i (a/1 a)))))))

      ((decompile? k e g) (vector-immutable 'letrec*/set i))

      (else (error/motile/internal/call 'letrec*/set/generate)))))


;; letrec*/generate solves a problem that has been bothering me for sometime with regard to letrec,
;; namely the letrec/set/N descriptor which is just an invitation to abuse in which a malicious island
;; misuses letrec/set/... to trick an innocent island into (re)constructing a devious and malicious closure.
;; letrec*/generate illustrates how this problem can be eliminated altogther and when I have some more time
;; I will rewrite the letrec code in the compiler in a style similar to what is done here - 2011.12.05

;; n - total number of bound variables in letrec*, x_1, ..., x_n
;; values - A vector of closures #(v_1 ... v_n) for binding expressions e_1, ..., e_n respectively
;;          Note: If n = 1 then values is just v_1 and NOT the unitary vector #(v_1).
;; body - closure for letrec* body
;; The code generated here is equivalent to:
;;  (lambda (x_1 x_2 ... x_n)
;;     (set! x_1 e_1)
;;     ...
;;     (set! x_n e_n)
;;     body)
;(define (letrec*/generate n values body)
;  (cond
;    ((= n 1) (letrec*/1/generate   values body))   ; values = v_1.
;    ((= n 2) (letrec*/2/generate   values body))   ; values = #(v_1 v_2).
;    ((= n 3) (letrec*/3/generate   values body))   ; values = #(v_1 v2 v_3).
;    (else    (letrec*/N/generate n values body)))) ; values = #(v1 v2 v3 ... v_n)
;
;(define (letrec*/1/generate value body)
;  (lambda (k a g)
;    (cond
;      ((procedure? k)
;       (arity/verify a 1 'letrec*/1/generate)
;       (a/1! a #f)
;       (a/1! a (value k/RETURN a g))
;       (body k a g))
;      ((decompile? k a g)
;       (vector-immutable 'letrec* 1 (motile/decompile value) (motile/decompile body)))
;      (else (error/motile/internal/call 'letrec*/1/generate)))))
;
;;(define (letrec*/1/generate value body)
;;  (lambda (k e g)
;;    (cond
;;      ((procedure? k)
;;       (let ((e (frame/push e (frame/fill/new 1 #f))))
;;         (a/1! e (value k/RETURN e g))
;;         (body k e g)))
;;      ((decompile? k e g)
;;       (vector-immutable 'letrec* 1 (motile/decompile value) (motile/decompile body)))
;;      (else (error/motile/internal/call 'letrec*/1/generate)))))
;
;(define (letrec*/2/generate values body)
;  (lambda (k e g)
;    (cond
;      ((procedure? k)
;       (let ((e (frame/push e (frame/fill/new 2 #f))))
;         (a/1! e ((vector-ref values 0) k/RETURN e g))
;         (a/2! e ((vector-ref values 1) k/RETURN e g))
;         (body k e g)))
;      ((decompile? k e g)
;       (vector-immutable 'letrec* 2 (vector-map motile/decompile values) (motile/decompile body)))
;      (else (error/motile/internal/call 'letrec*/2/generate)))))
;
;(define (letrec*/3/generate values body)
;  (lambda (k e g)
;    (cond
;      ((procedure? k)
;       (let ((e (frame/push e (frame/fill/new 3 #f))))
;         (a/1! e ((vector-ref values 0) k/RETURN e g))
;         (a/2! e ((vector-ref values 1) k/RETURN e g))
;         (a/3! e ((vector-ref values 2) k/RETURN e g))
;         (body k e g)))
;      ((decompile? k e g)
;       (vector-immutable 'letrec* 3 (vector-map motile/decompile values) (motile/decompile body)))
;      (else (error/motile/internal/call 'letrec*/3/generate)))))
;
;(define (letrec*/N/generate n values body)
;  (lambda (k e g)
;    (cond
;      ((procedure? k)
;       (let loop ((e (frame/push e (frame/fill/new n #f))) ; Push a frame with n variable slots, all initialized to #f, onto the stack.
;                  (i 1))
;           (cond
;             ((<= i n)
;              (vector-set! e i ((vector-ref values (sub1 i)) k/RETURN e g)) ; Initialize binding x_i with the outcome of the i'th value closure.
;              (loop e (add1 i)))
;             (else (body k e g))))) ; All the bindings are initalized so invoke the letrec* body.
;      
;      ((decompile? k e g)
;       (vector-immutable 'letrec* n (vector-map motile/decompile values) (motile/decompile body)))
;      (else (error/motile/internal/call 'letrec*/generate)))))
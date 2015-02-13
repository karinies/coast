#lang racket/base

(require
 (only-in
  "frame.rkt"
  a/1! a/2! a/3!
  frame/load/rest! frame/new frame/push
  arity/verify arity/rest/verify)

 (only-in
  "utility.rkt"
  bind/return!
  decompile?
  error/motile/internal/call)

 (only-in "baseline.rkt" motile/decompile))

(provide
 lambda/generate
 lambda/rest/generate)

;; Lambda code generation.

(define (descriptor/lambda/inner n e body)
  (vector-immutable 'lambda/inner n e (motile/decompile body)))

(define (descriptor/lambda/outer n body)
  (vector-immutable 'lambda/outer n (motile/decompile body)))

(define (descriptor/lambda/rest/inner n e body)
  (vector-immutable 'lambda/rest/inner n e (motile/decompile body)))

(define (descriptor/lambda/rest/outer n body)
  (vector-immutable 'lambda/rest/outer n (motile/decompile body)))

;; No closed variables in lambda body.
;; n - total number of formal parameters
;; body - code body of lambda expression.
(define (lambda/generate n body)
  (if (zero? n)
      (lambda/0/generate   body)
      (lambda/N/generate n body)))

;; Zero arguments.
(define (lambda/0/generate body)
  (lambda (k e g)
    (if (procedure? k)
        (k
         (let ((descriptor #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/verify a 0 'lambda/0/generate)
                (body k e g)) ; No arguments so don't bother to push a new frame.
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/lambda/inner 0 e body)))
               (else
                (error/motile/internal/call 'lambda/0/generate))))))

        (descriptor/lambda/outer 0 body))))

;; n > 0 arguments.
(define (lambda/N/generate n body)
  (lambda (k e g)
    (if (procedure? k)
        (k
         (let ((descriptor #f))
           (lambda (k a g)
             (cond
               ((procedure? k)
                (arity/verify a n 'lambda/N/generate)
                (body k (frame/push e a) g))
               ((decompile? k a g)
                (bind/return! descriptor (descriptor/lambda/inner n e body)))
               (else
                (error/motile/internal/call 'lambda/N/generate))))))

        (descriptor/lambda/outer n body))))


;; No closed variables in the lambda body and the last formal parameter is a rest argument.
;; n > 0: total number of formal parameters including the rest argument
;; body: code body of lambda expression
;; A legal call has n-1 arguments a[1] ... a[n-1] plus zero or more arguments,
;; a[n], a[n+1] ..., that must be gathered into a rest parameter.
(define (lambda/rest/generate n body)
  (lambda (k e g)
    (if (procedure? k)
        (k
         (let ((descriptor #f))
           (lambda (k a g)
           (cond
             ((procedure? k)
              (arity/rest/verify a n 'lambda/rest/generate)
              (body k (frame/push e (frame/load/rest! (frame/new n) a n)) g))
             ((decompile? k a g)
              (bind/return! descriptor (descriptor/lambda/rest/inner n e body)))
             (else
              (error/motile/internal/call 'lambda/rest/generate))))))

        (descriptor/lambda/rest/outer n body))))


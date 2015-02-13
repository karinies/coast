#lang racket/base

(require
 (only-in racket/vector  vector-map)
 (only-in "frame.rkt"    arguments/list/pack arguments/pack)
 (only-in "utility.rkt"  bind/return!)
 (only-in "baseline.rkt" motile/decompile))

(provide
 combination/generate)

;; The following five generators produce the code for a combination: (operator a_1 ... a_N).
;; where operator is a source procedure.

;; Used for when a Motile closure is invoked with zero user arguments.
(define a/EMPTY #(#f))

;; Generate a descriptor for a combination of n >= 0 arguments.
(define (descriptor/combination/N n operator . arguments)
  (vector-immutable
   'combination
   n
   (motile/decompile operator)
   (if (zero? n) null (map motile/decompile arguments))))

(define (descriptor/combination operator arguments)
  (vector-immutable
   'combination
   (length arguments)
   (motile/decompile operator)
   (if (null? arguments) null (map motile/decompile arguments))))

;; Map function f over list l but return a vector with the results.
(define (map/list/vector f l)
  (let ((v (make-vector (length l) #f)))
    (let loop ((i 0) (elements l))
      (cond
        ((null? elements) v)
        (else
         (vector-set! v i (f (car elements)))
         (loop (add1 i) (cdr elements)))))))

;; Given an operator and the generated code for each of the arguments a_1 ... a_n, n >= 0
;; generate the code for the combination (operator a_1 ... a_n).
(define (combination/generate operator arguments)
  (let ((n (length arguments)))
    (cond
      ((= n 0)
       (combination/0/generate operator))
      ((= n 1)
       (combination/1/generate operator (car arguments)))
      ((= n 2)
       (combination/2/generate operator (car arguments) (cadr arguments)))
      ((= n 3)
       (combination/3/generate operator (car arguments) (cadr arguments) (caddr arguments)))
      (else
       (combination/N/generate operator arguments)))))

;; Zero-argument application of an operator.
(define (combination/0/generate operator)
  (let ((descriptor #f))
    (lambda (k e g)
      (if (procedure? k)
          (operator (lambda (o) (o k a/EMPTY g)) e g)
          (bind/return! descriptor (descriptor/combination/N 0 operator))))))

;; The pattern in the combinations following is to evaluate the operator first,
;; then evaluate the arguments in order of appearance, and
;; finally apply the evaluated operator to the evaluated arguments.
;; It helps to recall that the operator and each of its arguments are Motile closures
;; that are evaluated with three arguments: k, a continuation; e, a lexical run-time stack;
;; and g, a global binding environment.
;; The evaluated operator o is finally applied to the evaluated arguments x_1 ... x_n, n >= 0
;; by invoking (o k (arguments/pack x_1 ... x_n) g).

;; One-argument application of an operator.
(define (combination/1/generate operator x)
  (let ((descriptor #f))
    (lambda (k e g)
      (if (procedure? k)
          (operator
           (lambda (o) (x (lambda (a) (o k (arguments/pack a) g)) e g)) ; Continuation for operator.
           e g)
          
          (bind/return! descriptor (descriptor/combination/N 1 operator x))))))

;; Two-argument application of an operator.
(define (combination/2/generate operator x y)
  (let ((descriptor #f))
    (lambda (k e g)
      (if (procedure? k)
          (operator
           (lambda (o)
             (x
              (lambda (a)
                (y (lambda (b) (o k (arguments/pack a b) g)) e g))
              e g))
           e g)
          
          (bind/return! descriptor (descriptor/combination/N 2 operator x y))))))

;; Three-argument application of an operator.
(define (combination/3/generate operator x y z)
  (let ((descriptor #f))
    (lambda (k e g)
      (if (procedure? k) 
          (operator
           (lambda (o)
             (x
              (lambda (a)
                (y
                 (lambda (b) (z (lambda (c) (o k (arguments/pack a b c) g)) e g)) ; Continuation for y.
                 e g)) 
              e g))
           e g)
          
          (bind/return! descriptor (descriptor/combination/N 3 operator x y z))))))

;; The classic (map f l) in continuation-passing style (map f L k) where:
;; f - function applied by map to each member of list l. f accepts two arguments: a value and a continuation
;; l - the subject list of the map
;; k - continuation of the map
(define (map/k f l k)
  (if (null? l)
      (k null)

      (f
       (car l)
       (lambda (v1) (map/k f (cdr l) (lambda (v2) (k (cons v1 v2))))))))

;; N-argument (N > 3) application of an operator.
(define (combination/N/generate operator arguments)
  (let ((descriptor #f))
    (lambda (k e g)
      (if (procedure? k)
          (operator
           (lambda (o)
             (map/k
              (lambda (a k) (a k e g)) ; f for map/k which evaluates Motile argument a from the list of arguments.
              arguments
              (lambda (values) (o k (arguments/list/pack values) g)))) ; The continuation for map/k which applies the evaluated operator.
           e g)
          
          (bind/return! descriptor (descriptor/combination operator arguments))))))

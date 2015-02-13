#! /usr/bin/env racket
#lang racket/base

(require "../../compile/compile.rkt"
         "../../generate/baseline.rkt"
         "../../baseline.rkt")

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
       (lambda (k)
         (ctak-aux
          k
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- x 1) y z)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- y 1) z x)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define motile-compiled-source
  (motile/compile
   '(let ()
      (define (ctak x y z)
  (call-with-current-continuation
   (lambda (k) (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
       (lambda (k)
         (ctak-aux
          k
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- x 1) y z)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- y 1) z x)))
          (call-with-current-continuation
           (lambda (k) (ctak-aux k (- z 1) x y))))))))
      (ctak 18 12 6))))

(define (racket-version)
  (ctak 18 12 6))
(define (motile-version)
  (motile/call motile-compiled-source BASELINE))

(define (iterations n f)
  (for ([i (in-range n)])
    (f)))

(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)
(time (iterations 100 racket-version))
(collect-garbage)(collect-garbage)(collect-garbage)(collect-garbage)
(time (iterations 100 motile-version))
(collect-garbage)
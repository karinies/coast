#lang racket/base

(require
 (only-in racket/vector vector-count))

(provide
 vector/all?)

;; Returns #t iff every element of v satisfies the predicate.
(define (vector/all? v predicate)
  (= (vector-length v) (vector-count predicate v)))

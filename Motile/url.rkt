#lang racket/base

(provide
 curl/new
 curl/thread/new
 curl?
 curl/agency
 curl/path
 curl/meta
 curl/path/length)
 


;; scheme - symbol (usually crest)
;; agency - may be an island (DNS . port) pair, a thread, a promise, a stream
;; path - tuple of path components where each path component may be any Motile data type
;; meta - metadata (unused for now)
(define (curl/new agency path meta)
  (vector-immutable '<curl> agency path meta)) ; No doubt subject to change.

(define (curl? u)
  (and
   (vector? u)
   (= (vector-length u) 4)
   (eq? (vector-ref u 0) '<curl>)))
(define-syntax-rule (curl/agency u) (vector-ref u 1))
(define-syntax-rule (curl/path u)   (vector-ref u 2))
(define-syntax-rule (curl/meta u)   (vector-ref u 3))

(define (curl/thread/new t path)
  (curl/new 'crest t path #f))

(define-syntax-rule (curl/path/length u) (tuple/length (curl/path u)))
  
  
  


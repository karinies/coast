#lang racket/base

;; Safe multithreaded update and lookup for a map from
;; island kp/z85 to kp/sign, that is for each known island
;; a map from its unique id to its public signing key.

(require
 racket/contract/base
 "curve.rkt"
 "persistent/hash.rkt")

(provide
 (contract-out
  [foreign/kp/sign/new    (-> (box/c hash/persist? #:flat? #t))]
  [foreign/kp/sign/add    (-> (box/c hash/persist? #:flat? #t) kp/z85/c kp/sign/c void?)]
  [foreign/kp/sign/look   (-> (box/c hash/persist? #:flat? #t) kp/z85/c (or/c kp/sign/c #f))]
  [foreign/kp/sign/remove (-> (box/c hash/persist? #:flat? #t) kp/z85/c void?)]))
 

;; Construct a fresh foreign/kp/sign map.
(define (foreign/kp/sign/new) (box hash/equal/null))
;; Update a foreign/kp/sign map with the pair origin:kp/sign where
;; origin is a kp/z85 island public key and kp/sign is that island's
;; public signing key.
(define (foreign/kp/sign/add b origin kp/sign)
  (let loop ([old (unbox b)])
    (when (not (box-cas! b old (hash/cons old origin kp/sign)))
      (loop (unbox b)))))
;; Lookup the public signing key of island origin.
;; Returns the islands kp/sign key or #f.
(define (foreign/kp/sign/look b origin) (hash/ref (unbox b) origin #f))

;; Remove an island's public signing key from the map.
(define (foreign/kp/sign/remove b origin)
  (let loop ([old (unbox b)])
    (when (not (box-cas! b old (hash/remove old origin)))
      (loop (unbox b)))))
    
             

#lang racket

(provide
 INTER
 INTRA
 ANYWHERE
 place/c
 (contract-out
  [place/intra? (-> place/c boolean?)]
  [place/inter? (-> place/c boolean?)]
  [place/anywhere? (-> place/c boolean?)]))

(define place/c (flat-named-contract 'place (or/c 'INTER 'INTRA 'ANYWHERE)))
(define (place/intra? place)    (eq? place INTRA))
(define (place/inter? place)    (eq? place INTER))
(define (place/anywhere? place) (eq? place ANYWHERE))

(define INTER    'INTER)
(define INTRA    'INTRA)
(define ANYWHERE 'ANYWHERE)
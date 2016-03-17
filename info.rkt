#lang info
(define collection "Island")
(define deps '("racket/base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/Motile-Island.scrbl" ())))
(define pkg-desc "Motile/Island implementation of the COmputationAl State Transfer (COAST) architectural style")
(define version "0.0")
(define pkg-authors '("Michael Gorlick"))

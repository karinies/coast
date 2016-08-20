#lang racket/base

(require
  racket/contract/base
  "../../include/base.rkt")

(provide
 (contract-out
  
  [example/island/new (-> symbol? string? procedure? island?)])
 
 CERTIFICATE/PUBLIC
 CERTIFICATE/SECRET
 KEYSTORE)


(define CERTIFICATE/PUBLIC "./certificates/public/")

(define CERTIFICATE/SECRET "./certificates/secret/")

(define KEYSTORE ((lambda () 
                   (let ([k (keystore/new)])
                     (keystore/load k CERTIFICATE/PUBLIC)
                     k))))

(define (example/island/new nickname filename bootstrap)
  (let* ([ISLAND/SECRET/PATH   (string-append CERTIFICATE/SECRET filename)]
         [ISLAND/CURVE/SECRET   (path-to-curve ISLAND/SECRET/PATH)]
         [island (island/new nickname ISLAND/CURVE/SECRET bootstrap)])
    (island/keystore/set island KEYSTORE)
    island)
  )

#lang racket/base

(require 
  racket/set
  racket/string
  racket/contract/base
  "../Island/logger.rkt"
  "../islet.rkt"
  "../include/base.rkt")

(provide
 (contract-out
  [islet/log/info (-> string? void?)]
  [islet/log/debug (-> string? void?)]
  [islet/log/warning (-> string? void?)]
  [islet/log/error (-> string? void?)]
  [islet/log/silence (-> symbol? void?)]
  [islet/log/unsilence (-> symbol? void?)]
  
  [example/island/new (-> symbol? string? procedure? island?)])
 
 CERTIFICATE/PUBLIC
 CERTIFICATE/SECRET
 KEYSTORE)

(define silenced-islands (mutable-set))

(define (islet/log/info message)
  (when (unsilenced (this/island/nickname))
    (log/info (this/islet/nickname) message #f)))

(define (islet/log/debug message)
  (when (unsilenced (this/island/nickname))
    (log/debug (this/islet/nickname) message #f)))

(define (islet/log/warning message)
  (when (unsilenced (this/island/nickname))
    (log/warning (this/islet/nickname) message #f)))

(define (islet/log/silence island)
  (set-add! silenced-islands island))

(define (islet/log/unsilence island)
  (set-remove! silenced-islands island))

(define (unsilenced island)
  (not (set-member? silenced-islands island)))


(define (islet/log/error message)
  (log/error (this/islet/nickname) message #f))

(define CERTIFICATE/PUBLIC "./certificates/public/")

(define CERTIFICATE/SECRET "./certificates/secret/")

(define KEYSTORE (lambda () 
                   (let ([k (keystore/new)])
                     (keystore/load KEYSTORE CERTIFICATE/PUBLIC)
                     k)))

(define (example/island/new nickname filename bootstrap)
  (let* ([ISLAND/SECRET/PATH   (string-append CERTIFICATE/SECRET filename)]
         [ISLAND/CURVE/SECRET   (path-to-curve ISLAND/SECRET/PATH)]
         [island (island/new nickname ISLAND/CURVE/SECRET bootstrap)])
    (island/keystore/set island KEYSTORE)
    island)
  )
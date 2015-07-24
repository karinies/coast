#lang racket/base

(require 
  racket/set
  racket/string
  racket/contract/base
  racket/format
  ;"../Island/logger.rkt"
  "../islet.rkt"
  "../include/base.rkt"
  "../comet/stomp-transport.rkt"
  "../Island/island-comet.rkt"
  )

(provide
 (contract-out
  ;[islet/log/info (-> string? void?)]
  [islet/log/debug (-> string? void?)]
  [islet/log/warning (-> string? void?)]
  [islet/log/error (-> string? void?)]
  [islet/log/silence (-> symbol? void?)]
  [islet/log/unsilence (-> symbol? void?)]
  
  [example/island/new (-> symbol? string? procedure? island?)])
 islet/log/info
 
 CERTIFICATE/PUBLIC
 CERTIFICATE/SECRET
 KEYSTORE)

(define silenced-islands (mutable-set))

(define (log/display* template args)
  (displayln (apply format (string-append "~a: " (~a template)) (this/islet/nickname) args)))

(define (islet/log/info template . args)
  (when (unsilenced (this/island/nickname))
    (log/display* template args)))

(define (islet/log/debug template . args)
  (when (unsilenced (this/island/nickname))
    ;(log/debug (this/islet/nickname) message #f)))
    (log/display* template args)))

(define (islet/log/warning template . args)
  (when (unsilenced (this/island/nickname))
    ;(log/warning (this/islet/nickname) message #f)))
    (log/display* template args)))

(define (islet/log/error template . args)
  (when (unsilenced (this/island/nickname))
    ;(log/error (this/islet/nickname) message #f)))
    (log/display* template args)))

(define (islet/log/silence island)
  (set-add! silenced-islands island))

(define (islet/log/unsilence island)
  (set-remove! silenced-islands island))

(define (unsilenced island)
  (not (set-member? silenced-islands island)))

(define CERTIFICATE/PUBLIC "./certificates/public/")

(define CERTIFICATE/SECRET "./certificates/secret/")

(define KEYSTORE ((lambda () 
                    (let ([k (keystore/new)])
                      (keystore/load k CERTIFICATE/PUBLIC)
                      k))))

(define (example/island/new nickname filename bootstrap)
  (let* ([ISLAND/SECRET/PATH   (string-append CERTIFICATE/SECRET filename)]
         [ISLAND/CURVE/SECRET   (path-to-curve ISLAND/SECRET/PATH)]
         [island (island/new nickname ISLAND/CURVE/SECRET bootstrap)]
         [messenger (stomp-messenger-new #:host "peru.local"
                                       #:login "coastdev"
                                       #:pass "Hi123"
                                       #:destination "/queue/coast")])

    (island/keystore/set island KEYSTORE)
    (island/monitoring/start nickname messenger)
    island))
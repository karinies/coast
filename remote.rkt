#lang racket/base

(require
 racket/contract/base
 "Motile/generate/baseline.rkt"
 "curl/base.rkt"
 "islet.rkt"
 "send.rkt")

;; We can't use thunk/c because every Motile procedure takes at least 3 arguments:
;; continuation, lexical scope, and global binding environment, so procedure? will
;; have to do for now.
;; Todo: define motile/thunk/c
(provide
 (contract-out
  [remote (-> islet? procedure? curl? (>/c 0) thread?)]
  [spawn  (-> islet? procedure?       (>/c 0) thread?)]))

;; Safely execute the given Motile thunk in the context
;; of the global binding environment and transmit the
;; return value of that execution via CURL u.
;; The maximum lifespan of the evaluation is given in real
;; fractional seconds.
;; Returns the minder thread that is watching over the worker thread however
;; it is perfectly safe to ignore the return value.

(define (remote worker thunk u lifespan)
  (islet/jumpstart
   worker
   (lambda () (send u (motile/call thunk (islet/site/global worker)))))
  ; The assassin thread that kills a remote evaluation when it overstays its welcome.
  (thread
   (lambda ()
     (unless (sync/timeout lifespan (event/islet/dead worker)) (islet/kill worker)))))

;; worker - worker islet to execute Motile thunk
;; lifespan - max lifespan in fractional seconds of spawn
(define (spawn worker thunk lifespan)
  (islet/jumpstart
   worker
   (lambda () (motile/call thunk (islet/site/global worker))))
  ; The assassin thread that kills a spawn when it overstays its welcome.
  (thread
   (lambda ()
     (unless (sync/timeout lifespan (event/islet/dead worker)) (islet/kill worker)))))
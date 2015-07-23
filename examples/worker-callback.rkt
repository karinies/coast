#lang racket/base

(require
  "../include/base.rkt"
  "../baseline.rkt"
  "../islet-utils.rkt"
  "examples-base.rkt"
  "examples-env.rkt")

; Defines a motile callback that will print a message when a new event is received by the islet's worker.
(define MOTILE/THUNK
  (island/compile
   '(lambda (event)
      (log/info "Event received" ))))

; Bob's bootstrapping code.
(define (bob/boot)
  (islet/log/info "Booting bob...")
  
  ; Creates and executes bob's main islet.
  (let ([x (islet/new (this/island) 'bob TRUST/MODERATE environ/null environ/null)])
    (islet/jumpstart
     x
     (lambda ()
       ; Bob's main islet will create a worker to work for it.
       (let* ([p (subislet/callback/new 'bob-1 (environ/merge BASELINE/SPAWN EXAMPLES/ENVIRON) MOTILE/THUNK)]
              [c (cdr p)])
         ; The subislet worker is created and we can communicate with it via CURL c.
         (send c 1)
         (send c 2)
         (send c 3))))))

;; Silence bob.
;(islet/log/silence 'bob)

;; Island-specific code.
(define bob (example/island/new 'bob "bob_secret" bob/boot))

;; Logging
(island/log/level/set 'info)
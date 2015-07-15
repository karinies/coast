#lang racket/base

(require
  "../include/base.rkt"
  "../baseline.rkt"
  "../islet-utils.rkt")

;; Certificates initialization.

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))

(define KEYSTORE (keystore/new))
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))

;; Application code.

; Defines a motile callback that will print a message when a new event is received by the islet's worker.
(define MOTILE/THUNK
  (island/compile
   '(lambda (event)
      (display "Worker received: ")(display event)(display "\n"))))

; Bob's bootstrapping code.
(define (bob/boot)
  (displayln "Booting bob...")
  
  ; Creates and executes bob's main islet.
  (let ([x (islet/new (this/island) 'bob TRUST/MODERATE environ/null environ/null)])
    (islet/jumpstart
     x
     (lambda ()
       ; Bob's main islet will create a worker to work for it.
       (let* ([p (subislet/callback/new 'bob-1 BASELINE/SPAWN MOTILE/THUNK)]
              [c (cdr p)])
         ; The subislet worker is created and we can communicate with it via CURL c.
         (send c 1)
         (send c 2)
         (send c 3))))))

;; Island-specific code.
(define bob   (island/new 'bob   BOB/CURVE/SECRET   bob/boot))
(island/keystore/set bob   KEYSTORE)

;; Logging
(island/log/level/set 'warning)
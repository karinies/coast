#lang racket/base


(require
 "../include/base.rkt"
 "../baseline.rkt"
 "market-server-env.rkt"
 "market-server-register.rkt"
 [only-in "../curl/base.rkt" curl/origin curl/path curl/metadata]
 "../promise.rkt"
 "../remote.rkt"
 "../spawn.rkt"
 "../transport/gate.rkt"
 "../transport/gates/challenge.rkt"
 "../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))

;; Returns <islet>@<island> where <islet> is the given name
;; and <island> is the island nickname.
;; For example (islent/name "service.foo") returns the symbol service.foo@alice
;; when called on the island alice.
(define (islet/name islet . rest)
  (string->symbol
   (format "~a@~a" islet (if (null? rest) (this/island/nickname) (car rest)))))

(define (service/notification)
  (displayln "Running Notification Service...")
  (define (market/notify/all)
    (if (zero? (market/subs/count))
        (displayln "No clients to notify!")
        (market/subs/apply (lambda (k v) (send v "Test")))))
    
  (let loop ()
    (sleep 1)
    (market/notify/all)
    (loop)))

(define (service/register) ; Registration service (It spawns computations for registration).
  (display "Running server's spawned service.\n")
           (let* ([d (islet/curl/known/new '(service spawn) 'access:send.service.spawn GATE/ALWAYS environ/null)])
           ; Wait for a spawn request.
             (display "Waiting for a spawn request...\n")
           (let loop ([m (duplet/block d)])
             (display "Request received.\n")
             (let ([payload (murmur/payload m)])
               (when (procedure? payload)
                 (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST MARKET/SERVER/ENV #f)])
                   ; Task and start the worker with a 30.0 second deadline.
                   (display "Spawning worker for incoming request.\n")
                   (spawn worker payload 30.0))))
             (loop (duplet/block d)))))

(define (server/boot)
  (define (registration/spawn) ; This function spawns the registration service.
    (let* ([server/name (islet/name "server.registration")] ; server.registration@alice
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; We create a new islet.
      (islet/jumpstart
       x
       (service/register)))) ; Executes service/register in the new islet.
  
  (define (notification/spawn) ; This function spawns the registration service.
    (let* ([server/name (islet/name "server.notification")] ; server.registration@alice
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; We create a new islet.
      (islet/jumpstart
       x
       (service/notification)))) ; Executes service/register in the new islet.
         
  (display "Running server's boot function\n")
  
  (thread (lambda () (registration/spawn)))  ; Why (thread)??
  (thread (lambda () (notification/spawn))))  ; Why (thread)??

(define alice (island/new 'alice ALICE/CURVE/SECRET server/boot))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/log/level/set 'info)

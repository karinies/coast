#lang racket/base

(require 2htdp/batch-io)
(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./robot-server-env.rkt"
  "./robot-server-comm.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata curl/access]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../spawn.rkt"
  "../../transport/access.rkt"
  "../../transport/gate.rkt"
  "../../transport/gates/challenge.rkt"
  "../../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ROBOT-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "robot_server_secret"))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ROBOT-SERVER/CURVE/SECRET (path-to-curve ROBOT-SERVER/SECRET/PATH))

;; Returns <islet>@<island> where <islet> is the given name
;; and <island> is the island nickname.
;; For example (islent/name "service.foo") returns the symbol service.foo@robot-server
;; when called on the island robot-server.
(define (islet/name islet . rest)
  (string->symbol
   (format "~a@~a" islet (if (null? rest) (this/island/nickname) (car rest)))))
  

(define (service/spawn/trader) ; A Service to spawn stock trader computations
  (display "Running Robot Server's spawning service.\n")
  (let* ([d (islet/curl/known/new '(service spawn) 'access:send.service.spawn GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST ROBOT/SERVER/ENV #f)]) ; Spawn the computation with a Binding Environment prepared (only) for registration.
            (display "Robot Server spawning computation...")
            (spawn worker payload 900.0)))) ; There shouldn't be a timeout for this.
      (loop (duplet/block d)))))

(define (server/boot)
  (define (trader/spawn) ; This function creates an islet that will receive spawn requests to register for notifications.
    (let* ([server/name (islet/name "server.registration")] ; server.registration@robot-server
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/spawn/trader))))) ; Executes service/spawn/registration in the new islet.
  
  
  (display "Running robot server's boot function\n")
  
  (thread (lambda () (trader/spawn))))  

(define robot-server (island/new 'robot-server ROBOT-SERVER/CURVE/SECRET server/boot))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set robot-server KEYSTORE)
(island/log/level/set 'warning)

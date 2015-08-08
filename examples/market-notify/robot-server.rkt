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
  "../../transport/gate.rkt"
  "../examples-base.rkt"
  "../examples-env.rkt")

(provide robot-server)

(define (service/spawn/trader) ; A Service to spawn stock trader computations
  (islet/log/info "Running Robot Server's spawning service.")
  (let* ([d (islet/curl/known/new '(service spawn) 'access:send.service.spawn GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST (environ/merge ROBOT/SERVER/ENV EXAMPLES/ENVIRON) #f)]) ; Spawn the computation with a Binding Environment prepared (only) for registration.
            (islet/log/info "Robot Server spawning computation...")
            (spawn worker payload 900.0)))) ; There shouldn't be a timeout for this.
      (loop (duplet/block d)))))

(define (service/spawn/trader/premium) ; A Service to spawn stock trader computations
  (islet/log/info "Running Robot Server's Premium spawning service.")
  (let* ([d (islet/curl/known/new '(register) 'access:send:service.register GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST (environ/merge ROBOT/SERVER/ENV EXAMPLES/ENVIRON) #f)]) ; Spawn the computation with a Binding Environment prepared (only) for registration.
            (islet/log/info "Robot Server spawning computation...")
            (spawn worker payload 900.0)))) ; There shouldn't be a timeout for this.
      (loop (duplet/block d)))))

(define (server/boot)
  (islet/log/info "Running Robot server's boot function.")
  
  (islet/log/info "Waiting for Market Data Server and Order Router...")
  (islands/enter/wait (list (curl/origin (robot/get-curl/market-server)) (curl/origin (robot/get-curl/order-router))))
  (islet/log/info "Market Data Server and Order Router have been seen.")
  
  ;; Commented because Kari doesn't run the Risk Server every time.
  ; (islet/log/info "Waiting for Risk Server...")
  ; (islands/enter/wait (curl/origin (robot/get-curl/risk-server)))
  ; (islet/log/info "Risk Server has been seen.")
  
  (define (trader/spawn) ; This function creates an islet that will receive spawn requests to register for notifications.
    (let ([x (islet/new (this/island) 'server.registration TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/spawn/trader))))) ; Executes service/spawn/registration in the new islet.

  (define (trader/spawn/premium) ; This function creates an islet that will receive spawn requests to register for notifications.
    (let ([x (islet/new (this/island) 'server.registration.premium TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/spawn/trader/premium))))) ; Executes service/spawn/registration in the new islet.

  (trader/spawn)
  (trader/spawn/premium))

(define robot-server (example/island/new 'robot-server "robot_server_secret" server/boot))

(island/log/level/set 'warning)

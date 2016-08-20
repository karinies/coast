#lang racket

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./sim-server-env.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata curl/access]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../transport/gate.rkt"
  "coastapp-utils.rkt")

(provide sim-server)

(define (service/spawn/simulation) ; A Service to spawn a simulation
  (display "Running Simulation Server's spawning service.\n")
  (let ([d (islet/curl/known/new '(service spawn) 'ingress.service.spawn GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subislet/new 'simulation TRUST/LOWEST SIM/SERVER/ENV)])
            ; Spawn the simulation with a Binding Environment.
            (display "Simulation Server spawning simulation...\n")
            (spawn worker payload 900.0)))) ; There shouldn't be a timeout for this.
            (display "Simulation spawned.\n")
      (loop (duplet/block d)))))

(define (sim-server/boot)
  (display "Running simulation server's boot function.\n")
  
  (define (sim/spawn) ; This function creates an islet that will receive spawn requests to run simulations.
    (let ([x (islet/new (this/island) 'server.simulation TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       service/spawn/simulation))) ; Executes service/spawn/simulation in the new islet.

  (sim/spawn)
  (display "Simulation server boot completed.\n"))

;fire up the sim-server island
(define sim-server (example/island/new 'sim-server "sim_server_secret" sim-server/boot))

(island/log/level/set 'warning)
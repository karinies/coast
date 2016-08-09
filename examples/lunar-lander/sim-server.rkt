#lang racket

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./sim-server-env.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata curl/access]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../transport/gate.rkt"
  "../examples-base.rkt"
  "../examples-env.rkt")

(provide sim-server)

(define (service/spawn/simulation) ; A Service to spawn a simulation
  (islet/log/info "Running Simulation Server's spawning service.")
  (let ([d (islet/curl/known/new '(service spawn) 'ingress.service.spawn GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subislet/new 'simulation TRUST/LOWEST (environ/merge SIM/SERVER/ENV EXAMPLES/ENVIRON))])
            ; Spawn the simulation with a Binding Environment.
            (islet/log/info "Simulation Server spawning simulation...")
            (spawn worker payload 900.0)))) ; There shouldn't be a timeout for this.
            (islet/log/info "Simulation spawned.")
      (loop (duplet/block d)))))

(define (sim-server/boot)
  (islet/log/info "Running simulation server's boot function.")
  
  (define (sim/spawn) ; This function creates an islet that will receive spawn requests to run simulations.
    (let ([x (islet/new (this/island) 'server.simulation TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       service/spawn/simulation))) ; Executes service/spawn/simulation in the new islet.

  (sim/spawn)
  (islet/log/info "Simulation server boot completed."))

(define sim-server (example/island/new 'sim-server "sim_server_secret" sim-server/boot))

(island/log/level/set 'warning)
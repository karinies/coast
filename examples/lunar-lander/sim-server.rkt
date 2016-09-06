#lang racket

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./sim-server-env.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata curl/access]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../transport/gate.rkt"
  "./coastapp-utils.rkt")

(provide sim-server)

(define (service/spawn/simulation) ; A service to spawn a simulation
  (display "simulator: starting spawning service.\n")
  (let ([d  ; Create a duplet (a CURL/egress-point pair) for spawn requests.
         (islet/curl/known/new
          '(service spawn) 'ingress.service.spawn GATE/ALWAYS environ/null)])
      
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subislet/new 'simulation TRUST/LOWEST SIMULATOR/ENVIRON)])
            (display "simulator: spawning simulation ...\n")
            (spawn worker payload 900.0)))) ; Limit lifespan of simulation to 900 seconds.
            (display "simulator: simulation spawned\n")
      (loop (duplet/block d)))))

(define (sim-server/boot)
  (display "Running simulation server's boot function.\n")
  
  ; Create an islet that will receive spawn requests to run simulations.
  (define (sim/spawn)
    (let ([x  ; Create a new islet.
           (islet/new      
            (this/island)
            'server.simulation
            TRUST/MODERATE
            environ/null environ/null)])
      (islet/jumpstart
       x
       service/spawn/simulation))) ; Executes service/spawn/simulation in the new islet.

  (sim/spawn)
  (display "simulator: boot completed.\n"))

;fire up the sim-server island
(define sim-server
  (example/island/new 'sim-server "sim_server_secret" sim-server/boot))

(island/log/level/set 'warning)
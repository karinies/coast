#lang racket/base

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata]
  "../../transport/gate.rkt"
  "../../uuid.rkt"
  "../examples-base.rkt")

(provide pilot)

;(define SIM-SERVER/SECRET/PATH   (string-append CERTIFICATE/SECRET "sim_server_secret"))
;(define SIM-SERVER/CURVE/SECRET   (path-to-curve SIM-SERVER/SECRET/PATH))
;(display (curl-as-bytes SIM-SERVER/CURVE/SECRET '(service spawn) 'ingress.service.spawn #f))
;; Demonstrate how to generate an inline CURL for market-server:
;; Execute
;;   (display (curl-as-bytes SIM-SERVER/CURVE/SECRET '(service spawn) 'ingress.service.spawn #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline SIM-SERVER/CURL/SPAWN ...)
;; as shown below.
(define/curl/inline SIM-SERVER/CURL/SPAWN
  #<<!!
SIGNATURE = #"zVMpXbsnYES0IRooE3QI4a1FpwKws-CilmEexuawFCeCKXcBpZhBHzz9vzXYqpv6nf6hYJ6_qvInhnX2YrpECg"
CURL
    id = dda12627-9e96-4efe-8d8d-8dc8896cebf6
    origin = #"RaQDnsBmoxoaCe_rkNuPJB1Q7PgSaYm17jzafmYFPSc"
    path = (service spawn)
    access/id = ingress.service.spawn
    created = "2016-06-22T16:25:22Z"
    metadata = #f

!!
  )

(define SIMULATION
  ; This is Motile source.
  '(lambda (state/curl thruster/curl)
     (lambda ()     
       (letrec (
                [thruster-updates/d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTER)] ; create new curl for receiving pilot thruster updates
                [state/c state/curl]
                [thruster/c thruster/curl]
                [GRAVITY 2]
                ; Initial state is #(altitude fuel velocity).
                [INITIAL-STATE (vector 1000 500 70)]
                [altitude/get (lambda(v) (vector-ref v 0))]
                [fuel/get (lambda (v) (vector-ref v 1))]
                [velocity/get (lambda (v) (vector-ref v 2))]
                ; define is not available in motile
                [altitude/update (lambda (state)
                                   (let ([altitude (- (altitude/get state) (velocity/get state))])
                                     (if (<= altitude 0) 0 altitude)))]
                
                [state/calculate (lambda (state thruster)
                                   (let ([altitude (altitude/update state)]
                                         [fuel (- (fuel/get state) thruster)]
                                         [velocity (/ (- (* (+ (velocity/get state) GRAVITY) 10) (* thruster 2)) 10)]) ; ((velocity + GRAVITY)*10 - (thruster*2)) / 10
                                     (if (<= fuel 0)
                                         (vector altitude 0 velocity)
                                         (vector altitude fuel velocity))))])         
         (islet/log/info "Executing simulation")
         (islet/log/info "Sending pilot curl")   
         ; send pilot curl on which to receive thruster updates
         (send thruster/c (duplet/resolver thruster-updates/d))
         (islet/log/info "Sent pilot curl")
         (sleep 1.0) ; allow pilot time to recieve curl
         
         (let loop ([m (duplet/try thruster-updates/d)] [state INITIAL-STATE] [thruster 0]) 
           ; check for pilot thruster adjustment
           (cond                                                                                                                
             [m                                                                                                                  
              (let ([command (murmur/payload m)]) ; command is  ('THRUSTER . N) 
                (display "Got thruster update.\n")
                (islet/log/info command)
                (case (car command)                                                                                              
                  ((THRUSTER)                                                                                                    
                   (let ([new-thruster (cdr command)]) 
                     (cond 
                       [(= (fuel/get state) 0)
                        (display "Fuel depleted. Thruster not updated.\n")
                        (send state/c "FUEL DEPLETED!")
                        (loop (duplet/try thruster-updates/d) state 0)
                        ]
                       [else
                        (display "Setting thruster to ") (display new-thruster)(newline)
                        (loop (duplet/try thruster-updates/d) state new-thruster) 
                        ])                                                                     
                   (else   
                    (display "Invalid thruster update.\n")
                    (loop (duplet/try thruster-updates/d) state thruster))))))]
             
             [else ; No murmurs.                                                                                               
                (sleep 1.0)
                (let ([state/new (state/calculate state thruster)])
                  (display state/new)(newline)
                  (send state/c state/new)
                  (cond
                    [(= (altitude/get state/new) 0)
                     (cond 
                       [(<= (velocity/get state/new) 5)
                         (display "Successful landing.\n")
                         (send state/c "We've landed! You're a hero!\n")]
                       [else
                         (display "Unsuccessful landing.\n")
                         (send state/c "You crashed! Try again.\n")]
                       )]
                    [else
                      (cond 
                        [(= (fuel/get state/new) 0)
                         (send state/c "FUEL DEPLETED!")
                         (loop (duplet/try thruster-updates/d) state/new 0)]
                        [else
                           (loop (duplet/try thruster-updates/d) state/new thruster)])])       
                  )]))
         (display "Game Over.\n"))

       (display "Exiting Simulation...\n"))))


(define (service/spawn/pilot-input thruster/c) ; An input reader service for the pilot
  (islet/log/info "Running Pilot's input reader service.")
  (let loop ([value (read)]) ; 
    ; update on thruster curl
    (send thruster/c (cons 'THRUSTER value))
    (loop (read))))

;; Code for a pilot island.
;; server/u - CURL for spawn service on Sim Server.
(define (pilot/boot server/u)
  (islet/log/info "Pilot is booting...")
  
  (islet/log/info "Waiting for Simulation Server...")
  (island/enter/wait (curl/origin server/u))
  (islet/log/info "Simulation Server has been seen.")
  
  (let* ([state/d (islet/curl/new '(state notif) GATE/ALWAYS #f 'INTER)]  ; pilot will receive state reporting on this curl.
         ;[thruster/p (promise/new)] ; pilot will recieve curl on which to send thrust modifications on this promise.
         ; MICHAEL, COULD NOT GET WORKING WITH PROMISE, HAD TO USE DUPLET
         [thruster/d (islet/curl/new '(thrustercurl pass) GATE/ALWAYS #f 'INTER)] ; pilot will receive curl on which it can send thruster updates
         [THUNK/SIMULATION (motile/call (island/compile SIMULATION) environ/null (duplet/resolver state/d) (duplet/resolver thruster/d))])   
    (islet/log/info "Sending simulation thunk to Simulation Server...")
    (send server/u THUNK/SIMULATION)
    (islet/log/info "Simulation thunk sent") 
    
    ; listen for curl on which to send thrust updates to sim server
    (islet/log/info "Pilot waiting thruster curl")
    (let ([m (duplet/block thruster/d)])
      (let ([thruster/c (murmur/payload m)])
        (islet/log/info "Pilot has received thruster curl")
        
        ; fire up islet to get input events from the pilot
        (define (pilot/spawn) ; This function creates an islet that will receive spawn requests to run simulations.
          (let ([x (islet/new (this/island) 'pilot.input TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
            (islet/jumpstart
             x
             (lambda () (service/spawn/pilot-input thruster/c))))) ; Executes service/spawn/pilot-input in the new islet.
        
        (pilot/spawn)
        
        ; listen for multiple state change events
        (let loop ([m (duplet/block state/d)]) ; Listen for state change events
          (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
            (display payload)(newline))
          (loop (duplet/block state/d)))))))

; Construct an in-memory CURL instance of the predefined CURL for sim-server.
(define sim-server/curl/spawn (curl/zpl/safe-to-curl SIM-SERVER/CURL/SPAWN KEYSTORE))

(define pilot (example/island/new 'pilot  "pilot_secret"  (lambda () (pilot/boot sim-server/curl/spawn))))

(island/log/level/set 'warning)



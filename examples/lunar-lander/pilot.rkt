#lang racket/base

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata]
  "../../transport/gate.rkt"
  "../../uuid.rkt"
  "coastapp-utils.rkt")

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
  '(lambda (state/curl thrusterc/curl)
     (lambda () ; this will return a parameterless thunk with state/curl and thruster/curl embedded as variable values    
       (let* (
              ; on this curl, the executing simulation will send back a curl on which the pilot can communicate thruster updates
              [thrusterc/c thrusterc/curl] 
              ; on this curl, the executing simulation will send state updates
              [state/c state/curl]
              ; create new curl for receiving pilot thruster updates, we will send to pilot below
              [thruster-updates/d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTER)] 
              
              [GRAVITY 1.1]
              ; Initial state is #(ALTITUDE FUEL VELOCITY).
              [INITIAL-STATE (vector 1000.0 750.0 70.0)]
              
              ; define is not available in motile, use labmda functions instead
              [altitude/get (lambda(v) (vector-ref v 0))] ; get altitude from state vector
              [fuel/get (lambda (v) (vector-ref v 1))] ; get fuel from state vector
              [velocity/get (lambda (v) (vector-ref v 2))] ; get velocity from state vector   
              [state/calculate (lambda (state thruster) ; calculate new state, returns #(ALTITUDE FUEL VELOCITY)
                                 (let* (
                                        [calc-altitude (- (altitude/get state) (velocity/get state))]
                                        [altitude (if (<= calc-altitude 0) 0 calc-altitude)] ; don't allow altitute to go negative
                                        [calc-fuel (- (fuel/get state) thruster)]
                                        [fuel (if (<= calc-fuel 0) 0 calc-fuel)] ; don't allow fuel to go negative
                                        [calc-velocity (/ (- (* (+ (velocity/get state) GRAVITY) 10) (* thruster 2)) 10)] ; ((velocity + GRAVITY)*10 - (thruster*2)) / 10
                                        [velocity (if (<= calc-velocity 0) 0 calc-velocity)]) ; don't allow velocity to go negative
                                   (cond 
                                     [(and (= fuel 0) (> (fuel/get state) 0)) ; check if we just ran out of fuel   
                                      (display "Fuel depleted.\n")
                                      (send state/c "FUEL DEPLETED!")]) ; warn pilot]
                                   (vector altitude fuel velocity)))])
         
         (display "Executing simulation\n")
         (display "Sending pilot curl\n")   
         ; send pilot a curl on which this executing simulation will receive thruster updates
         (send thrusterc/c (duplet/resolver thruster-updates/d))
         (display "Sent pilot curl\n")
         (sleep 1.0) ; allow pilot time to recieve curl
         
         ; we check for murmurs from the pilot using duplet/try, this is non-blocking  
         ; at each iteration of loop we pass 1) possible murmur from the pilot, 2) the current state, and 3) the current thruster level
         (let loop ([m (duplet/try thruster-updates/d)] [state INITIAL-STATE] [thruster 0]) 
           (cond ; check for pilot thruster adjustment                                                                                                             
             [m ; got a murmur from the pilot                                                                                                                 
              (let ([command (murmur/payload m)]) ; command is  ('THRUSTER . N) 
                (display "Got thruster update.\n")
                (display command)(newline)
                (case (car command)                                                                                              
                  [(THRUSTER) ; checking that first element is 'THRUSTER                                                                                                   
                   (let ([new-thruster (cdr command)]) ; get thruster value
                     (cond 
                       [(= (fuel/get state) 0) ; if fuel is empty, we cannot change thruster, must remain at 0
                        (display "Fuel depleted. Thruster not updated.\n")
                        (send state/c "FUEL DEPLETED!") ; warn pilot
                        (loop (duplet/try thruster-updates/d) state 0)] ; check for another murmur
                       [else ; change thruster value
                        (display "Setting thruster to ") (display new-thruster)(newline)
                        (loop (duplet/try thruster-updates/d) state new-thruster)]))] ; check for another murmur                                                                    
                   [else   
                      (display "Invalid thruster update.\n")
                      (loop (duplet/try thruster-updates/d) state thruster)]))] ; check for another murmu
             
             [else ; no murmurs, go ahead and recalculate state                                                                                              
              (sleep 4.0) ; slow game to allow pilot time to respond to state changes
              (let ([state/new (state/calculate state thruster)]) ; calculate new state
                (display state/new)(newline)
                ; send new state to pilot
                (send state/c (format "Altitute = ~a, Fuel = ~a, Velocity = ~a" 
                                 (round (altitude/get state/new))
                                 (round (fuel/get state/new))
                                 (round (velocity/get state/new))))
                ;(send state/c state/new) 
                (cond
                  [(= (altitude/get state/new) 0) ; check if we have touched ground
                   (cond 
                     [(<= (velocity/get state/new) 5) ; if velocity is 5mph or less at touch down, pilot wins
                      (display "Successful landing.\n")
                      (send state/c "We've landed! You're a hero!\n")]
                     [else
                      (display "Unsuccessful landing.\n")
                      (send state/c "You crashed! Try again.\n")]
                     )]
                  [else ; still falling
                   (if (= (fuel/get state/new) 0) ; check if we are out of fuel
                       (loop (duplet/try thruster-updates/d) state/new 0) ; set thruster to 0, check for another murmur
                       (loop (duplet/try thruster-updates/d) state/new thruster))]))])); check for another murmur   
         
         (display "Game Over.\n")
         (display "Exiting Simulation...\n")))))


(define (service/listen/pilot-input thruster/c) ; An input reader service for the pilot
  (display "Running Pilot's input reader service.\n")
  (let loop ([value (read)]) ; promp pilot for thruster update
    (send thruster/c (cons 'THRUSTER value)) ; send new thrust value to executing simulation
    (loop (read))))

;; Code for a pilot island.
;; server/u - CURL for spawn service on Sim Server.
(define (pilot/boot server/u)
  (display "Pilot is booting...\n")
  
  (display "Waiting for Simulation Server...\n")
  (island/enter/wait (curl/origin server/u))
  (display "Simulation Server has been seen.\n")
  
  (let* ([state/d (islet/curl/new '(state notif) GATE/ALWAYS #f 'INTER)]  ; pilot will receive state updates on this curl.
         [thrusterc/p (promise/new)] ; pilot will recieve curl on which to send thrust modifications on this promise.
         [THUNK/SIMULATION (motile/call (island/compile SIMULATION) environ/null (duplet/resolver state/d) (duplet/resolver thrusterc/p))]) ; creates a compiled thunk to send to the simulation server
    (display "Sending simulation thunk to Simulation Server...\n")
    (send server/u THUNK/SIMULATION)
    (display "Simulation thunk sent\n") 
    
    ; listen for curl on which to send thrust updates to sim server
    (display "Pilot waiting for thruster curl\n")
    (let ([m (duplet/block thrusterc/p)]) ; listen for the curl
      (let ([thruster/c (murmur/payload m)]) ; extract curl from the payload
        (display "Pilot has received thruster curl\n")
        
        ; fire up islet to get input events from the pilot
        (define (pilot/input) ; This function creates an islet that will listen for thruster updates as keyboard input.
          (let ([x (islet/new (this/island) 'pilot.input TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
            (islet/jumpstart
             x
             (lambda () (service/listen/pilot-input thruster/c))))) ; Executes service/listen/pilot-input in the new islet.
        
        (pilot/input)
        
        ; listen for multiple state change events
        (let loop ([m (duplet/block state/d)]) ; Listen for state change events
          (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
            (display payload)(newline))
          (loop (duplet/block state/d)))))))

; Construct an in-memory CURL instance of the predefined CURL for sim-server.
(define sim-server/curl/spawn (curl/zpl/safe-to-curl SIM-SERVER/CURL/SPAWN KEYSTORE))

; fire up the pilot island
(define pilot (example/island/new 'pilot  "pilot_secret"  (lambda () (pilot/boot sim-server/curl/spawn))))

(island/log/level/set 'warning)



#lang racket/base

(require 2htdp/batch-io)
(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./market-server-env.rkt"
  "./market-server-register.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata curl/access]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../spawn.rkt"
  "../../transport/gate.rkt"
  "../examples-base.rkt"
  "../examples-env.rkt")

(provide market-server)

#|
This service will notify all CURLs registered in the market when there is an update and the clients 
are interested in it.
|#
(define (service/notification)
  (islet/log/info "Running Market Data Notification Service...")
  
  (define (market/notify/all)
    (if (zero? (market/subs/count))
        (islet/log/info "No clients to notify!")
        (market/subs/apply-all 
         (lambda (k v)
           (let ([thunk (format "Oh! Something happened with ~a!" k)])
             (when (not (send v thunk))
               (islet/log/info "Notification could not be sent")))))))
  
  (define (market/notify/event event)
    (if (zero? (market/subs/count))
        (islet/log/info "No clients to notify!")
        (market/subs/apply event
         (lambda (event v)
           (let ([notify-event event])
             (when (not (send v (struct->vector notify-event)))
               (islet/log/info "Notification could not be sent")))))))
  
  (define (process-stock-event event)
    (let* ([stock-name (vector-ref event 0)]
           [event-type (vector-ref event 1)]
           [price (vector-ref event 2)]  
           [quantity (vector-ref event 3)]
           [seller (vector-ref event 4)]
           [buyer (vector-ref event 5)]
           [new-market-event (market-event stock-name event-type price quantity seller buyer)])
      (islet/log/info new-market-event)
      (market/notify/event new-market-event)))
  
  ; read stock events from an external file
  (define event-file "events/market_events.txt")
    
  (for ([line (read-words/line event-file)])
    (define vline (list->vector line))
      (cond
        [(equal? "delay" (vector-ref vline 0))
         (sleep (/ (string->number (vector-ref vline 1)) 1000))] ;sleep for the specified amount
        [else (process-stock-event vline)])))
  

(define (service/spawn/registration) ; A Service to spawn computations that will register clients on the market.
  (islet/log/info "Running Market Data Server's spawning service.")
  (let* ([d (islet/curl/known/new '(service spawn) 'access:send.service.spawn GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST (environ/merge MARKET/SERVER/ENV EXAMPLES/ENVIRON) #f)]) ; Spawn the computation with a Binding Environment prepared (only) for registration.
            (spawn worker payload 900.0)))) ; There shouldn't be a timeout for this.
      (loop (duplet/block d)))))

(define (server/boot)
  (define (registration/spawn) ; This function creates an islet that will receive spawn requests to register for notifications.
    (let ([x (islet/new (this/island) 'server.registration TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/spawn/registration))))) ; Executes service/spawn/registration in the new islet.
  
  (define (notification/spawn) ; This function creates a new islet that will run the notification service.
    (let ([x (islet/new (this/island) 'updater TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/notification))))) ; Executes service/notification in the new islet.
  
  (islet/log/info "Running Market Data's boot function.")
  
  (registration/spawn)
  (notification/spawn))

(define market-server (example/island/new 'market-server "market_server_secret" server/boot))

(island/log/level/set 'warning)
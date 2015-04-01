#lang racket/base

(require racket/flonum)
(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./market-server-env.rkt"
  "./market-server-register.rkt"
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
(define MARKET-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "market_server_secret"))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define MARKET-SERVER/CURVE/SECRET (path-to-curve MARKET-SERVER/SECRET/PATH))

;; Returns <islet>@<island> where <islet> is the given name
;; and <island> is the island nickname.
;; For example (islent/name "service.foo") returns the symbol service.foo@market-server
;; when called on the island market-server.
(define (islet/name islet . rest)
  (string->symbol
   (format "~a@~a" islet (if (null? rest) (this/island/nickname) (car rest)))))

#|
This service will notify all CURLs registered in the market when there is an update and the clients 
are interested in it.
|#
(define (service/notification)
  (displayln "Running Notification Service...")
  
  (define (market/notify/all)
    (if (zero? (market/subs/count))
        (displayln "No clients to notify!")
        (market/subs/apply-all 
         (lambda (k v)
           (let ([thunk (format "Oh! Something happened with ~a!" k)])
             (when (not (send v thunk))
               (display "Notification could not be sent")))))))
  
  (define (market/notify/event symbol event-type price)
    (if (zero? (market/subs/count))
        (displayln "No clients to notify!")
        (market/subs/apply symbol event-type price
         (lambda (symb ev-type price v)
           (let ([thunk (format "~a ~a ~a" symb ev-type (/ (->fl price) 100))])
             (when (not (send v thunk))
               (display "Notification could not be sent")))))))
  
  (define delta-change 0)
  (define goog-price 55500) ;; store $ in cents to prevent percision error
  (define delay 0)
  (let loop ()
    (set! delay (random 10))
    (sleep delay)
    ;;(market/notify/all)
    (set! delta-change (-(random 2000) 1000)) ;; between -10 and 10
    (displayln (format "delta change: ~a" (/ (->fl delta-change) 100)))
    (set! goog-price (+ goog-price delta-change))
    (displayln (format "new GOOG price: ~a" (/ (->fl goog-price) 100)))

    (market/notify/event "GOOG" 'sell-event goog-price)
    (loop)))

(define (service/spawn/registration) ; A Service to spawn computations that will register clients on the market.
  (display "Running Server's spawning service.\n")
  (let* ([d (islet/curl/known/new '(service spawn) 'access:send.service.spawn GATE/ALWAYS environ/null)]) ; Create a CURL to listen for computations.
    
    (let loop ([m (duplet/block d)]) ; Wait for a spawn request.
      (let ([payload (murmur/payload m)]) ; Extract the murmur's payload.
        (when (procedure? payload) ; Check if the payload is a procedure.
          (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST MARKET/SERVER/ENV #f)]) ; Spawn the computation with a Binding Environment prepared (only) for registration.
            (spawn worker payload 90.0)))) ; There shouldn't be a timeout for this.
      (loop (duplet/block d)))))

(define (server/boot)
  (define (registration/spawn) ; This function creates an islet that will receive spawn requests to register for notifications.
    (let* ([server/name (islet/name "server.registration")] ; server.registration@market-server
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/spawn/registration))))) ; Executes service/spawn/registration in the new islet.
  
  (define (notification/spawn) ; This function creates a new islet that will run the notification service.
    (let* ([server/name (islet/name "server.notification")] ; server.notification@market-server
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/notification))))) ; Executes service/notification in the new islet.
  
  (display "Running server's boot function\n")
  
  (thread (lambda () (registration/spawn)))  ; Why (thread)??
  (thread (lambda () (notification/spawn))))  ; Why (thread)??

(define market-server (island/new 'market-server MARKET-SERVER/CURVE/SECRET server/boot))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set market-server KEYSTORE)
(island/log/level/set 'warning)

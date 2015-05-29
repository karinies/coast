#lang racket/base

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../transport/gate.rkt"
  "../../transport/gates/challenge.rkt"
  "../../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define TRADER/SECRET/PATH   (string-append CERTIFICATE/SECRET "trader_secret"))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define TRADER/CURVE/SECRET   (path-to-curve TRADER/SECRET/PATH))

;(define ROBOT-SERVER/SECRET/PATH   (string-append CERTIFICATE/SECRET "robot_serve_secret"))
;(define ROBOT-SERVER/CURVE/SECRET   (path-to-curve ROBOT-SERVER/SECRET/PATH))
;; Demonstrate how to generate an inline CURL for market-server:
;; Execute
;;   (display (curl-as-bytes ROBOT-SERVER/CURVE/SECRET '(service spawn) 'access:send.service.spawn #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline ROBOT-SERVER/CURL/chirp ...)
;; as shown below.

(define/curl/inline ROBOT-SERVER/CURL/SPAWN
  #<<!!
SIGNATURE = #"XbVHE3O5lhPAL-XreJ1z_q9QGftm21w5c9mOg48Fspe_KT0w5xKlVi9xprq8PcmZ7chKJK7yTgZMHW3UL4feBw"
CURL
    id = b927dace-e6b5-4ca7-9c22-b1d4e3ba4b9f
    origin = #"RaQDnsBmoxoaCe_rkNuPJB1Q7PgSaYm17jzafmYFPSc"
    path = (service spawn)
    access/id = access:send.service.spawn
    created = "2015-05-28T13:49:37Z"
    metadata = #f

!!
)

;; Return the petname of the island that transmitted murmur m.
(define (murmur/petname m)
  (keystore/petname/look (this/keystore) (murmur/origin m)))
(define (curl/petname u)
  (keystore/petname/look (this/keystore) (curl/origin u)))

;; Generate the spawn definition that trader sends to market notifications service.
(define THUNK/REGISTER-MARKET/NEW
  (island/compile
   ; name - trader's nickname the spawned computation.
   ; client/notif/u - The Traders's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (name client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          
          (let loop ([m (duplet/block d)])
            (let ([payload (murmur/payload m)])
              (send client/notif/u payload)
              (loop (duplet/block d))))))))) ; Wait again for an echo request.

;; Generate the spawn definition that trader sends to risk notifications service.
(define THUNK/REGISTER-RISK/NEW
  (island/compile
   ; name - trader's nickname the spawned computation.
   ; client/notif/u - The Traders's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (name client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          
          (let loop ([m (duplet/block d)])
            (let ([payload (murmur/payload m)])
              (send client/notif/u payload)
              (loop (duplet/block d))))))))) ; Wait again for an echo request.

;; Returns <islet>@<island> where <islet> is the given name
;; and <island> is the island nickname.
;; For example (islent/name "service.foo") returns the symbol service.foo@market-server
;; when called on the island market-server.
(define (islet/name islet . rest)
  (string->symbol
   (format "~a@~a" islet (if (null? rest) (this/island/nickname) (car rest)))))

; The client that trader will spawn to request chirps from market notification service.
; u is the CURL that market-server provides for its chirp service.
(define (trader/market-register market/register/curl client/notif/curl) ; Register a computation at the server side to be notified about market updates.
  (let* ([server/kp/base64 (curl/origin market/register/curl)] ; kp/base64 of market-server
         [spawn/name (islet/name "server.registration" (curl/petname market/register/curl))]) ; server.registration@market-server
    
    (island/enter/wait server/kp/base64) ; Wait for service provider to enter the network.
    (displayln "Server entered the network.")
    (let* ([thunk
            (motile/call THUNK/REGISTER-MARKET/NEW environ/null
                         spawn/name client/notif/curl)]) ; Invoke Motile code that creates a Motile thunk.
      (send market/register/curl thunk)))) ; Request the sevice provider to evaluate our thunk as a spawn.

; The client that trader will spawn to request chirps from risk notification service.
; u is the CURL that risk-server provides for its chirp service.
(define (trader/risk-register risk/register/curl client/notif/curl) ; Register a computation at the server side to be notified about market updates.
  (let* ([server/kp/base64 (curl/origin risk/register/curl)] ; kp/base64 of market-server
         [spawn/name (islet/name "server.registration" (curl/petname risk/register/curl))]) ; server.registration@market-server
    
    (island/enter/wait server/kp/base64) ; Wait for service provider to enter the network.
    (displayln "Server entered the network.")
    (let* ([thunk
            (motile/call THUNK/REGISTER-RISK/NEW environ/null
                         spawn/name client/notif/curl)]) ; Invoke Motile code that creates a Motile thunk.
      (send risk/register/curl thunk)))) ; Request the sevice provider to evaluate our thunk as a spawn.



(define (service/market-notifications u) ; Notification Service: It will print incoming messages into the console.
  (displayln "Starting Trader's Market Service Notification...")
  
  (let ([d (islet/curl/new '(service notifications) GATE/ALWAYS #f 'INTER)]) ; Creates the CURL that the Notification Service will use to receive messages.
    
    (send u (duplet/resolver d)) ; Send the CURL back so that computations can carry it to other Islands.
    
    (let loop ([m (duplet/block d)]) ; Wait for an incoming message.
      (let ([payload (murmur/payload m)]) ; Extract the message's payload.
        ;(when (string? payload) ; Check it's a string.
          (displayln payload));) ; Print it into the console.
      (loop (duplet/block d)))))

(define (service/risk-notifications u) ; Notification Service: It will print incoming messages into the console.
  (displayln "Starting Trader's Risk Service Notification...")
  
  (let ([d (islet/curl/new '(service notifications) GATE/ALWAYS #f 'INTER)]) ; Creates the CURL that the Notification Service will use to receive messages.
    
    (send u (duplet/resolver d)) ; Send the CURL back so that computations can carry it to other Islands.
    
    (let loop ([m (duplet/block d)]) ; Wait for an incoming message.
      (let ([payload (murmur/payload m)]) ; Extract the message's payload.
        ;(when (string? payload) ; Check it's a string.
          (displayln payload));) ; Print it into the console.
      (loop (duplet/block d)))))

;; p/notif/u A CURL where the Market Notification Service will put the CURL for its service.
(define (trader/setup/market-notifications p/notif/u) ; Creates a new islet that will use the given duplet to listen for marketnotifications.
  (let* ([server/name (islet/name "trader.market-notifications")] ; trader.market-notifications@trader
         [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
    (islet/jumpstart
     x
     (lambda () (service/market-notifications p/notif/u))))) ; Runs service/notifications in the new islet.

;; p/notif/u A CURL where the Risk Notification Service will put the CURL for its service.
(define (trader/setup/risk-notifications p/notif/u) ; Creates a new islet that will use the given duplet to listen for risk notifications.
  (let* ([server/name (islet/name "trader.risk-notifications")] ; trader.risk-notifications@trader
         [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
    (islet/jumpstart
     x
     (lambda () (service/risk-notifications p/notif/u))))) ; Runs service/notifications in the new islet.

;; Code for a trader island.
;; server/u - CURL for spawn service on remote island.
(define (trader/boot server/u)
  (displayln "Trader is booting...")
  
  (let ([p-market (promise/new)]
        [p-risk (promise/new)]) ; Creates the CURL that the Notification Service will use to receive messages.
    (trader/setup/market-notifications (promise/resolver p-market)) ; Setup the Notification Service for Market Notifications
    (trader/setup/risk-notifications (promise/resolver p-market))   ; Setup the Notification Service for Risk Notifications
    (let* ([m-market (promise/block p-market)]
           [m-risk (promise/block p-risk)]
           [service/market-notif/curl (murmur/payload m-market)]
           [service/risk-notif/curl (murmur/payload m-risk)])
      (trader/market-register server/u service/market-notif/curl)  ; Register a computation with the market server to receive market updates
      (trader/risk-register server/u service/risk-notif/curl))))   ; Register a computation with the risk server to receive risk updates.

; Construct an in-memory CURL instance of the predefined CURL for robot-server.
(define robot-server/curl/spawn (curl/zpl/safe-to-curl ROBOT-SERVER/CURL/SPAWN KEYSTORE))

(define trader (island/new 'trader  TRADER/CURVE/SECRET  (lambda () (trader/boot robot-server/curl/spawn))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set trader  KEYSTORE) 
(island/log/level/set 'warning)

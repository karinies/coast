#lang racket/base

(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "../../island.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../transport/gate.rkt"
  "../../transport/gates/challenge.rkt"
  "../../transport/gates/whitelist.rkt"
  "../../accounting/stomp-transport.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define MARKET-CLIENT/SECRET/PATH   (string-append CERTIFICATE/SECRET "market_client_secret"))

(define MARKET-CLIENT/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define MARKET-CLIENT/CURVE/SECRET   (path-to-curve MARKET-CLIENT/SECRET/PATH))

;; Demonstrate how to generate an inline CURL for market-server:
;; Execute
;;   (display (curl-as-bytes MARKET-SERVER/CURVE/SECRET '(service spawn) 'access:send.service.spawn #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline MARKET-SERVER/CURL/chirp ...)
;; as shown below.

(define/curl/inline MARKET-SERVER/CURL/SPAWN
  #<<!!
SIGNATURE = #"GNzBZNi6r6WTBdASzv_R0GJjAiwaBYtHkZhiMlyKTD8E-S-mL-A7SMFR7_9IKNl8_JJcfzOIBQh4YDnP3JoWBw"
CURL
    id = 0dd4f4f5-72ce-40fe-996f-f80700c322f0
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (service spawn)
    access/id = access:send.service.spawn
    created = "2014-05-30T14:47:58Z"
    metadata = #f

!!
  )

;; Return the petname of the island that transmitted murmur m.
(define (murmur/petname m)
  (keystore/petname/look (this/keystore) (murmur/origin m)))
(define (curl/petname u)
  (keystore/petname/look (this/keystore) (curl/origin u)))

;; Generate the spawn definition that client sends to market notifications service.
(define THUNK/REGISTER/NEW
  (island/compile
   ; name - client's nickname the spawned computation.
   ; client/notif/u - The Client's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (name client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          (register (list "GOOG" "YHOO") (duplet/resolver d))
          (register (list "GOOG") (duplet/resolver d))
          
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

; The client that clients  will spawn to request chirps from market notification service.
; u is the CURL that market-server provides for her chirp service.
(define (client/register market/register/curl client/notif/curl) ; Register a computation at the server side to be notified about market updates.
  (let* ([server/kp/base64 (curl/origin market/register/curl)] ; kp/base64 of market-server
         [spawn/name (islet/name "server.registration" (curl/petname market/register/curl))]) ; server.registration@market-server
    
    (island/enter/wait server/kp/base64) ; Wait for service provider to enter the network.
    (displayln "Server entered the network.")
    (let* ([thunk
            (motile/call THUNK/REGISTER/NEW environ/null
                         spawn/name client/notif/curl)]) ; Invoke Motile code that creates a Motile thunk.
      (send market/register/curl thunk)))) ; Request the sevice provider to evaluate our thunk as a spawn.

(define (service/notifications u) ; Notification Service: It will print incoming messages into the console.
  (displayln "Starting Client's Service Notification...")
  
  (let ([d (islet/curl/new '(service notifications) GATE/ALWAYS #f 'INTER)]) ; Creates the CURL that the Notification Service will use to receive messages.
    
    (send u (duplet/resolver d)) ; Send the CURL back so that computations can carry it to other Islands.
    
    (let loop ([m (duplet/block d)]) ; Wait for an incoming message.
      (let ([payload (murmur/payload m)]) ; Extract the message's payload.
        ;(when (string? payload) ; Check it's a string.
          (displayln payload));) ; Print it into the console.
      (loop (duplet/block d)))))

;; p/notif/u A CURL where the Notification Service will put the CURL for its service.
(define (client/setup/notifications p/notif/u) ; Creates a new islet that will use the given duplet to listen for notifications.
  (let* ([server/name (islet/name "client.notifications")] ; client.notifications@market-client
         [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
    (islet/jumpstart
     x
     (lambda () (service/notifications p/notif/u))))) ; Runs service/notifications in the new islet.

;; Code for a client island.
;; server/u - CURL for spawn service on remote island.
(define (client/boot server/u)
  (displayln "Client is booting...")
  
  (let ([p (promise/new)]) ; Creates the CURL that the Notification Service will use to receive messages.
    (client/setup/notifications (promise/resolver p)) ; Setup the Notification Service.
    (let* ([m (promise/block p)]
           [service/notif/curl (murmur/payload m)])
      (client/register server/u service/notif/curl)))) ; Register a computation at the server side to receive market updates.

; Construct an in-memory CURL instance of the predefined CURL for market-server.
(define market-server/curl/spawn (curl/zpl/safe-to-curl MARKET-SERVER/CURL/SPAWN KEYSTORE))

(define market-client   (island/new 'market-client   MARKET-CLIENT/CURVE/SECRET   (lambda () (client/boot market-server/curl/spawn))))

(let ([messenger (stomp-messenger-new #:host "peru.local"
                                       #:login "coastdev"
                                       #:pass "Hi123"
                                       #:destination "/queue/coast")])
  (island/monitoring/start messenger))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set market-client   KEYSTORE)
(island/log/level/set 'warning)

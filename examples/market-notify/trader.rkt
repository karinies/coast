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

;; This thunk will be executed on the Robot Server.
;; It will register for notifications coming from both the Market Data Server and the Risk Server
(define THUNK/REGISTER-ROBOT/NEW
  (island/compile
   '(lambda (motile/register/market motile/register/risk)
      (lambda ()
        (display "Executing trader's computation (market and risk registration) on the Robot Server\n")
        (let* ([robot/notif/u (islet/curl/new '(robot notif) GATE/ALWAYS #f 'INTER)] ; We create a CURL on the Robot Server to receive notifications from both the MD Server and Risk Server. 
               [market/curl (robot/get-curl/market-server)]  ; Get the Market Data Server Notification CURL.
               [risk/curl (robot/get-curl/risk-server)]  ; Get the Risk Server Notification CURL.
               [market-thunk (motile/call motile/register/market environ/null (duplet/resolver robot/notif/u))]
               [risk-thunk (motile/call motile/register/risk environ/null (duplet/resolver robot/notif/u))])
          (send market/curl market-thunk) ; Send the registration thunk to the Market Data Server.
          (send risk/curl risk-thunk) ; Send the registration thunk to the Risk Server.
          
          ; We now listen for notifications coming from the Market Data Server and the Risk Server through robot/notif/u.
          ; THIS BLOCKING LOOP WILL NOW RECIEVE BOTH KINDS OF MESSAGES
          ; MUST DISCERN BETWEEN THE TWO
          (let loop ([m (duplet/block robot/notif/u)]) ; Wait for an incoming message.
            (let ([payload (murmur/payload m)]) ; Extract the message's payload.
              ;(when (string? payload) ; Check it's a string.
              (display payload)
              (display "\n")) ; Print it into the console.
            (loop (duplet/block robot/notif/u))))))))


;; Generate the spawn definition that trader sends to market notifications service.
(define THUNK/REGISTER-MARKET/NEW
  (island/compile
   ; client/notif/u - The Traders's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          (display "Registered for market events\n")
          
          (let loop ([m (duplet/block d)])
            (let ([payload (murmur/payload m)])
              (send client/notif/u payload)
              (loop (duplet/block d))))))))) ; Wait again for an echo request.


;; Generate the spawn definition that trader sends to risk notifications service.
(define THUNK/REGISTER-RISK/NEW
  (island/compile
   ; client/notif/u - The Traders's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          (display "Registered for risk events\n")
          
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

;; Code for a trader island.
;; server/u - CURL for spawn service on Robot Server.
(define (trader/boot server/u)
  (displayln "Trader is booting...")
  (let ([thunk (motile/call THUNK/REGISTER-ROBOT/NEW environ/null THUNK/REGISTER-MARKET/NEW THUNK/REGISTER-RISK/NEW)])
    (displayln "Sending registrations thunk to Robot Server...")
    (send server/u thunk)))

; Construct an in-memory CURL instance of the predefined CURL for robot-server.
(define robot-server/curl/spawn (curl/zpl/safe-to-curl ROBOT-SERVER/CURL/SPAWN KEYSTORE))

(define trader (island/new 'trader  TRADER/CURVE/SECRET  (lambda () (trader/boot robot-server/curl/spawn))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set trader  KEYSTORE) 
;(island/log/level/set 'debug)
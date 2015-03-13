#lang racket/base


(require
 "../include/base.rkt"
 "../baseline.rkt"
 [only-in "../curl/base.rkt" curl/origin curl/path curl/metadata]
 "../promise.rkt"
 "../remote.rkt"
 "../transport/gate.rkt"
 "../transport/gates/challenge.rkt"
 "../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))

(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")
  
(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))

;; Demonstrate how to generate an inline CURL for alice:
;; Execute
;;   (display (curl-as-bytes ALICE/CURVE/SECRET '(service spawn) 'access:send.service.spawn #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline ALICE/CURL/chirp ...)
;; as shown below.

(define/curl/inline ALICE/CURL/SPAWN
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

;; Generate the spawn definition that bob sends to alice.
(define THUNK/REGISTER/NEW
  (island/compile
   ; u - CURL for client issuing the spawn
   ; name - client's nickname the spawned computation.
   ; Returns a thunk
   '(lambda (name client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        (display "Running computation\n")
        (display (format "# of entries: ~a~n" (market/subs/count)))
        (display (format "Name: ~a~n" name))
        ; This computation will be executed at the server side.
        ; Create a new CURL here (it cannot be passed because it has to be created on the server-side.
        (let ([d (spawn/exchange/duplet* '(service notif) #f)])
          (display (format "resolver: ~a\n" (duplet/resolver d)))
          (register (list "GOOG" "FB" "IBM") (duplet/resolver d))
          (display (format "# of entries: ~a~n" (market/subs/count)))
          
           (let loop ([m (duplet/block d)])
             (display "Notification received.\n")
             (let ([payload (murmur/payload m)])
                 (send client/notif/u payload)
             (loop (duplet/block client/notif/u))))))))) ; Wait again for an echo request.

;; Utility routines for printing
(define (display/curl/path nickname m)
  (display
   (format " ~a got curl ~a back from ~a\n"
           nickname (curl/path (murmur/payload m)) (murmur/petname m))))
(define (display/echo nickname m)
  (display
   (format "  ~a got echo ~s back from ~a\n"
           nickname (murmur/payload m) (murmur/petname m))))

;; Returns <islet>@<island> where <islet> is the given name
;; and <island> is the island nickname.
;; For example (islent/name "service.foo") returns the symbol service.foo@alice
;; when called on the island alice.
(define (islet/name islet . rest)
  (string->symbol
   (format "~a@~a" islet (if (null? rest) (this/island/nickname) (car rest)))))

; The client that bob and carol will spawn to request chirps from alice.
; u is the CURL that alice provides for her chirp service.
(define (client/register u notif/d)
  ; Create a new islet to act as a spawn client.
  (let* ([client/name (islet/name "client.spawn.echo")] ; client.spawn.echo@bob
         [server/kp/base64 (curl/origin u)] ; kp/base64 of alice
         [spawn/name (islet/name "server.registration" (curl/petname u))]) ; server.registration@alice
             ; Start the client islet
       (display "Running client's code in a new thread\n")
       (island/enter/wait server/kp/base64) ; Wait for service provider to enter the network.
       (display "Connected.\n")
       (let* ([thunk
               (motile/call THUNK/REGISTER/NEW environ/null
                            ;(promise/resolver p) ; CURL that spawn will use to contact client.
                            spawn/name (duplet/resolver notif/d))])
         ; Request the sevice provider to evaluate our thunk as a spawn.
         (displayln "About to send thunk.")
         (send u thunk)
         (displayln "Thunk sent."))))

(define (service/notifications d) ; Registration service (It spawns computations for registration).
  (display "Starting Client's Service Notification..\n")
           (let loop ([m (duplet/block d)])
             (display "Request received.\n")
             (let ([payload (murmur/payload m)])
               (when (string? payload)
                   (display (format "Payload: ~a~n" payload))))
             (loop (duplet/block d))))

(define (client/setup/notifications d)
  (let* ([server/name (islet/name "client.notifications")] ; client.notifications@bob
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; We create a new islet.
      (islet/jumpstart
       x
       (service/notifications d))))

;; Code for a client island.
;; server/u - CURL for spawn service on remote island.
(define (client/boot server/u)
  (display "Running client's boot function\n")
  
  (let ([d (islet/curl/new '(service notifications) GATE/ALWAYS #f 'INTER)])
    (thread (lambda () (client/setup/notifications d))) ; Why (thread)??
    (client/register server/u d)))

 ; Construct an in-memory CURL instance of the predefined CURL for alice.
(define alice/curl/spawn (curl/zpl/safe-to-curl ALICE/CURL/SPAWN KEYSTORE))

(define bob   (island/new 'bob   BOB/CURVE/SECRET   (lambda () (client/boot alice/curl/spawn))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set bob   KEYSTORE)
(island/log/level/set 'warning)

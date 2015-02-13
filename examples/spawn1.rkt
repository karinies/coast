#lang racket/base

(require
 "../include/base.rkt"
 "../baseline.rkt"
 [only-in "../curl/base.rkt" curl/origin curl/path curl/metadata]
 "../promise.rkt"
 "../remote.rkt"
 "../transport/gate.rkt"
 "../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))
;(define CAROL/SECRET/PATH (string-append CERTIFICATE/SECRET "carol_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")
(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")
(define CAROL/KP/BASE64 #"rqM_XCwrsziuhIEsG1d0yMA05mivoewXhUmzKUzhb0s")

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))
(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))
;(define CAROL/CURVE/SECRET (path-to-curve CAROL/SECRET/PATH))

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
    id = 0dd4f4(f5-72ce-40fe-996f-f80700c322f0
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
;; Return the petname of the island that created CURL u.
(define (curl/petname u)
  (keystore/petname/look (this/keystore) (curl/origin u)))

;; Generate the spawn definition that bob sends to alice.
(define THUNK/ECHO/NEW
  (island/compile
   ; u - CURL for client issuing the spawn
   ; name - client's nickname the spawned computation
   ; Returns a thunk
   '(lambda (u name)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Create a duplet that allows the client to transmit murmurs to the spawn.
        (let ([d (islet/curl/new
                  (list name) ; Something like (server.spawn.echo@alice).
                  (gate/whitelist/island (curl/origin u)) ; Access restricted to client
                  #f     ; No metadata
                  'INTER)]) ; Good for inter-island messaging only.

          ; Inform the client of the CURL that it must use to contact the spawn.
          (send u (duplet/resolver d))

           ; Loop reading and responding to echo requests from the client.
          (let loop ([m (duplet/block d)])
            (let ([payload (murmur/payload m)])
              (when (and (pair? payload) (curl? (car payload))) ; Verify content of payload.
                (send (car payload) (cdr payload)))) ; Echo back to the client.
            (loop (duplet/block d)))))))) ; Wait again for an echo request.

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


;; Code for a client island.
;; server/u - CURL for spawn service on remote island.
(define (client/boot server/u)
  ; The client that bob and carol will spawn to request chirps from alice.
  ; u is the CURL that alice provides for her chirp service.
  (define (client/spawn/echo u)
    ; Create a new islet to act as a spawn client.
    (let* ([client/name (islet/name "client.spawn.echo")] ; client.spawn.echo@bob
           [server/kp/base64 (curl/origin u)] ; kp/base64 of alice
           [spawn/name (islet/name "server.spawn.echo" (curl/petname u))] ; server.spawn.echo@alice
           [x (islet/new (this/island) client/name TRUST/LOW environ/null environ/null)])
      ; Start the client islet
      (islet/jumpstart
       x
       (lambda ()
         (island/enter/wait server/kp/base64) ; Wait for service provider to enter the network.
         (let* ([p (promise/new)] ; So spawn can reach back to the client.
                [thunk
                 (motile/call THUNK/ECHO/NEW environ/null
                              (promise/resolver p) ; CURL that spawn will use to contact client.
                              spawn/name)]) ;
           ; Request the sevice provider to evaluate our thunk as a spawn.
           (send server/u thunk)
           ; We expect to receive a CURL back from our personal server so that we can contact it.
           (let* ([m (promise/block p)] [server/echo/u (murmur/payload m)])
             (display/curl/path client/name m)
             ; Now create a CURL that we can send to our echo server repeatedly for the echo reply.
             ; CURL pathname is (reply server.spawn.echo@alice).
             ; Note that the CURL in the duplet has a use count of 4: per (gate/uses 4).
             (let* ([reply (islet/curl/new (list 'reply spawn/name) (gate/uses 4) environ/null INTER)])
               ; Send the odd numbers 1 3 5 7 9 to the echo server that we created.
               ; Since the CURL we send to our server server has a use count of 4 we will get back the first
               ; 4 odd numbers: 1 3 5 7. However, when the echo server replies with 9 the client island will
               ; reject the murmur since the use count of the reply CURL has been exceeded.
               (for ([i (in-range 1 10)] #:when (odd? i)) ; Send some odd numbers to the echo server.
                 (send server/echo/u (cons (duplet/resolver reply) i)) ; Ask the server to echo i back.
                 (display/echo client/name (duplet/block reply))       ; Show what came back.
                 (sleep 0.75)))))))))

  (client/spawn/echo server/u))

(define (server/boot)
  (define (server/spawn)
    (let* ([server/name (islet/name "server.spawn")] ; server.span@alice
           [x (islet/new (this/island) server/name TRUST/MODERATE BASELINE/SPAWN environ/null)])
      (islet/jumpstart
       x
       (lambda ()
         (let* ([gate/whitelist (gate/whitelist/island (list BOB/KP/BASE64 CAROL/KP/BASE64))] ; For bob and carol only.
                [d (islet/curl/known/new '(service spawn) 'access:send.service.spawn gate/whitelist environ/null)])
           ; Wait for a spawn request.
           (let loop ([m (duplet/block d)])
             (let ([payload (murmur/payload m)])
               (when (procedure? payload)
                 (let ([worker (subislet/new 'service.spawn.worker TRUST/LOWEST BASELINE/SPAWN)])
                   ; Task and start the worker with a 30.0 second deadline.
                   (spawn worker payload 30.0))))
             (loop (duplet/block d))))))))

  (server/spawn))

 ; Construct an in-memory CURL instance of the predefined CURL for alice.
(define alice/curl/spawn (curl/zpl/safe-to-curl ALICE/CURL/SPAWN KEYSTORE))

(define alice (island/new 'alice ALICE/CURVE/SECRET server/boot))
(define bob   (island/new 'bob   BOB/CURVE/SECRET   (lambda () (client/boot alice/curl/spawn))))
;(define carol (island/new 'carol CAROL/CURVE/SECRET (lambda () (client/boot alice/curl/spawn))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)
;(island/keystore/set carol KEYSTORE)
(island/log/level/set 'warning)

(define (test/start)
  (island/start alice)
  (island/start bob))

(define (test/halt)
  (island/destroy alice)
  (island/destroy bob))

#| Sample output
 client.spawn.echo@bob got curl (server.spawn.echo@alice) back from alice
  client.spawn.echo@bob got echo 1 back from alice
  client.spawn.echo@bob got echo 3 back from alice
  client.spawn.echo@bob got echo 5 back from alice
  client.spawn.echo@bob got echo 7 back from alice
|#

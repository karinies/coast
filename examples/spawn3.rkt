#lang racket/base

(require
 "../include/base.rkt"
 "../baseline.rkt"
 "../curl/base.rkt"
 "../promise.rkt"
 "../remote.rkt"
 "../spawn.rkt"
 "../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))
(define CAROL/SECRET/PATH (string-append CERTIFICATE/SECRET "carol_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")
(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")
(define CAROL/KP/BASE64 #"rqM_XCwrsziuhIEsG1d0yMA05mivoewXhUmzKUzhb0s")
  
(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))
(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))
(define CAROL/CURVE/SECRET (path-to-curve CAROL/SECRET/PATH))

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
(define THUNK/ECHO/NEW
  (island/compile
   ; u - CURL for client issuing the spawn
   ; Returns a thunk
   '(lambda (u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Create two duplets, one that allows the client to transmit murmurs to the spawn
        ; and a second that allows bob to request additional CURLs for the echo service.
        (let ([d/exchange (spawn/exchange/duplet* '(service echo) #f)]
              [d/control  (spawn/control/duplet*  '(control echo) #f)])
          ; Inform the client of the two CURLs that it must use to contact the spawn.
          (send u (cons (duplet/resolver d/exchange) (duplet/resolver d/control)))

           ; Loop responding to echo and control requests.
          (let loop ([x (sync d/exchange d/control)]) ;[start (time/now)])
            (cond
              [(eq? x d/exchange)
               (let* ([m (duplet/block x)] [payload (murmur/payload m)])
                 (when (and (pair? payload) (curl? (car payload)))
                       (send (car payload) (cdr payload)))) ; Echo back to the client.
               ;(display (format "server: ~a msecs\n" (duration-to-milliseconds (time- (time/now) start))))]
               ]

              [(eq? x d/control) ; Generate a service CURL for another island.
               (let* ([m (duplet/block x)] [payload (murmur/payload m)])
                 (when (list? payload)
                   (let ([reply     (car payload)]    ; Reply CURL for control request.
                         [kp/base64 (cadr payload)]   ; id of island for which CURL is intended.
                         [n         (caddr payload)]) ; uses limit for new CURL
                     (when (and (curl? reply) (kp/base64? kp/base64) (exact-positive-integer? n))
                       (let ([u
                              (spawn/exchange/curl
                               '(service echo) (gate/and (gate/uses n) (gate/whitelist/island kp/base64))
                               #f INTER)])
                         (send reply u))))))] ; Send the new CURL back to the requester.
              [else #f])

            (loop (sync d/exchange d/control))))))))

;; Utility routines for printing
(define (display/curl/path nickname m)
  (let* ([pair (murmur/payload m)]
         [exchange (car pair)]
         [control  (cdr pair)])
    (display
     (format
      "  ~a got curls ~a and ~a back from ~a\n"
      nickname (curl/path exchange) (curl/path control) (murmur/petname m)))))

(define (display/echo nickname m)
  (display
   (format
    "  ~a got echo back from ~a in ~a msecs\n"
    nickname (murmur/petname m) (- (epoch/milliseconds) (murmur/payload m)))))

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
  ; u is the CURL that alice provides for her spawn service.
  (define (client/spawn/echo u)
    ; Create a new islet to act as a spawn client.
    (let* ([client/name (islet/name "client.spawn.echo")] ; client.spawn.echo@bob for example.
           [server/kp/base64 (curl/origin u)] ; kp/base64 of alice
           [x (islet/new (this/island) client/name TRUST/LOW environ/null environ/null)])
      ; Start the client islet
      (islet/jumpstart
       x
       (lambda ()
         (island/enter/wait server/kp/base64) ; Wait for service provider to enter the network.
         (let* ([p (promise/new)] ; So spawn can reach back to the client.
                [thunk (motile/call THUNK/ECHO/NEW environ/null (promise/resolver p))])
           ; Request the sevice provider to evaluate our thunk as a spawn.
           (send server/u thunk)
           ; We expect to receive two CURLs back from our personal server so that we can contact it.
           (let* ([m (promise/block p)]
                  [payload          (murmur/payload m)]
                  [server/echo/u    (car payload)]
                  [server/control/u (cdr payload)])
             (display/curl/path client/name m)
             (let* ([reply (islet/curl/new '(reply echo) (gate/whitelist/island server/kp/base64) #f 'INTER)]
                    [reply/u (duplet/resolver reply)])
               (for ([i (in-range 1 50)])
                 ;(send server/echo/u (cons reply/u (epoch/milliseconds)))
                 (send server/echo/u (cons reply/u (epoch/milliseconds)))    ; Transmit a timestamp.
                 (display/echo client/name (duplet/block reply)))))))))) ; Show what came back.
                 ;(sleep 0.75)))))))))
;             (for ([i (in-range 1 10)])
;               (let ([reply (promise/new)])
;                 (send server/echo/u (cons (promise/resolver reply) (time/now))) ; Transmit a timestamp.
;                 (display/echo client/name (duplet/block reply)) ; Show what came back.
;                 (sleep 0.75)))))))))
  
  (client/spawn/echo server/u))

; (/ (+ 1006 820 1083 556 882 1156 1281 1418 955) 9) ; ~1017
; (/ (+ 923 1440 680 497 851 680 1181 964 1187) 9)   ; ~934

#| Timings where client uses a fresh promise for each request
  client.spawn.echo@bob got curls (service echo) and (control echo) back from alice
  client.spawn.echo@bob got echo back from alice in 1006.0 msecs
  client.spawn.echo@bob got echo back from alice in 820.0 msecs
  client.spawn.echo@bob got echo back from alice in 1083.0 msecs
  client.spawn.echo@bob got echo back from alice in 556.0 msecs
  client.spawn.echo@bob got echo back from alice in 882.0 msecs
  client.spawn.echo@bob got echo back from alice in 1156.0 msecs
  client.spawn.echo@bob got echo back from alice in 1281.0 msecs
  client.spawn.echo@bob got echo back from alice in 1418.0 msecs
  client.spawn.echo@bob got echo back from alice in 955.0 msecs

  Timings where client reuses the same CURL for each request.
  client.spawn.echo@bob got curls (service echo) and (control echo) back from alice
  client.spawn.echo@bob got echo back from alice in 923.0 msecs
  client.spawn.echo@bob got echo back from alice in 1440.0 msecs
  client.spawn.echo@bob got echo back from alice in 680.0 msecs
  client.spawn.echo@bob got echo back from alice in 497.0 msecs
  client.spawn.echo@bob got echo back from alice in 851.0 msecs
  client.spawn.echo@bob got echo back from alice in 680.0 msecs
  client.spawn.echo@bob got echo back from alice in 1181.0 msecs
  client.spawn.echo@bob got echo back from alice in 964.0 msecs
  client.spawn.echo@bob got echo back from alice in 1187.0 msecs
|#

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
                 (let ([worker (subspawn/new (murmur/origin m) TRUST/LOWEST BASELINE/SPAWN #f)]) ; (subspawn/new origin trust global policy) 
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
(island/log/level/set 'info)

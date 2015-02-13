#lang racket/base

(require
 "../include/base.rkt"
 "../promise.rkt")

(provide alice bob)

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))
(define CAROL/SECRET/PATH (string-append CERTIFICATE/SECRET "carol_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")

;; The CURL for alice's hello service.
(define/curl/inline ALICE/CURL/HELLO
#<<!!
SIGNATURE = #"-r3QQsXIHTWM_OlPtWtur2WvivNwiEUJFeNP4ktX1e0YjL1FKIGAFc6qURCF8w1wxCMMDE9bmKSoYyo7Fxh1Cg"
CURL
    id = 908791e6-5896-4fee-8676-326682de2ce6
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (hello)
    access/id = access:send:hello
    created = "2014-04-30T15:25:57Z"
    metadata = #f

!!
)

;; Code for a client island.
(define (client/boot u)
  ; Wait under the island with the given kp/base64 key enters the network.
  (define (island/enter/wait a kp/base64)
    (let loop ()
      (unless (archipelago/look a kp/base64)
        (sleep 2.0) ; Sleep for 2 seconds before looking again.
        (loop)))
    (sleep 1.0)) ; Make sure that the other island has seen us as well.

  ; The client that bob will spawn to request a hello from alice.
  ; u is the CURL that alice provides for her "hello" service.
  (define (client/hello u)
    ; Create a new islet on bob to act as a service client.
    (let* ([nickname (log/name/build (this/island/nickname) 'client.hello)]
           [x (islet/new (this/island) nickname TRUST/LOW environ/null environ/null)])
      (islet/jumpstart
       x
       (lambda ()
         (let* ([p (promise/new)]
                ;[p/receive (promise/receiver p)]
                [resolver  (promise/resolver p)]) ; The curl for  resolving the promise.
           ; Wait for alice to enter the network.
           (island/enter/wait (this/archipelago) ALICE/KP/BASE64)
           (displayln "bob: alice entered the network")
           ; Request alice to say hello.
           (send u resolver)
           (displayln "bob: waiting for alice to reply")
           (let ([m (promise/wait p 60.0)])
             (if m (displayln (murmur/payload m)) (displayln "bob: promise timed out"))))))))
           ;(display (format "hello server said ~s\n" (promise/wait p 60.0))))))))

  (client/hello u))

;; Code for alice who offers a "hello world" service.
(define (alice/boot)
  (define (service/hello)
    (let* ([t (transport:bankers/new)] ; Generic queuing transport.
           [a/send    (access:send/known/new t 'access:send:hello GATE/NONE EMBARGO/NO)]
           [a/receive (access:receive/new t 'access:receive:hello GATE/NONE)]
           [nickname (log/name/build (this/island/nickname) 'hello)]
           [keystore (this/keystore)])
      (accessor/add (this/accessors) a/send) ; So the island router knows where to find us.

      (displayln "alice starting")
      ; Wait until an island asks us to say hello.
      (let loop ([in (sync a/receive)]) ; block until something arrives on transport t.
        (let ([m (access/receive in #f)]) ; Now accept it.
          (when (murmur? m)
            (let ([payload (murmur/payload m)])
              (when (curl? payload)
                (send
                 payload ; The promise CURL sent by the client.
                 (format "hello world from ~a" (keystore/petname/look keystore (murmur/origin m)))))))
          (loop (sync a/receive))))))

  (service/hello))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

;; bob is given a CURL for alice at birth.
(define bob
  (let ([u (curl/zpl/safe-to-curl ALICE/CURL/HELLO KEYSTORE)])
    (island/new 'bob BOB/SECRET/PATH (lambda () (client/boot u)))))
(define alice (island/new 'alice ALICE/SECRET/PATH alice/boot))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)
;(island/log/level/set 'info)


(displayln "ready to go")

;; To run the demo do:
;; (island/start alice)
;; (island/start bob)
;; At the end of the demo you MUST do:
;; (island/destroy alice)
;; (island/destroy bob)

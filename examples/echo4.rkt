#lang racket/base

;; A sample echo server.
;; Alice will execute any thunk it receives and send its value via a curl included in the request.
;; A client island, bob, will send a single thunk to alice for evaluation.
;; The wrinkle in this example is that bob uses a promise to receive the
;; echo back from alice.

(require
 "../include/base.rkt"
 "../baseline.rkt"
 "../Motile/generate/baseline.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")
(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))
(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))

;; Demonstrate how to generate an inline CURL for alice:
;; Execute
;;   (display (curl-as-bytes ALICE/CURVE/SECRET '(echo) 'access:send:echo #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline ALICE/CURL/ECHO ...)
;; as shown below.

;; The CURL for alice's echo service.
(define/curl/inline ALICE/CURL/ECHO
#<<!!
SIGNATURE = #"Nxm6zGGiZDiao5vc8aYfdEeOIME104GEeOt4_K3ys2xDP673elLrwWa56SKAWp7gR2RI25QKZW0NvB2i23NJCg"
CURL
    id = 2eac24e1-f4fb-440d-a771-b3b60266a982
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (echo)
    access/id = access:send:echo
    created = "2014-05-17T16:17:17Z"
    metadata = #f

!!
)

;; Code for a client island.
;; u - CURL for echo service
;; snippet - datum sent to echo service by client
;; n - total number of times client uses echo service
(define (client/boot u)
  ; Wait under the island with the given kp/base64 key enters the network.
  (define (island/enter/wait a kp/base64)
    (let loop ()
      (unless (archipelago/look a kp/base64)
        (sleep 2.0) ; Sleep for 2 seconds before looking again.
        (loop)))
    (sleep 1.0)) ; Make sure that the other island has seen us as well.

  ; The client that bob will spawn to request an echo from alice.
  ; u is the CURL that alice provides for her echo service.
  (define (client/echo u)
    ; Create a new islet on bob to act as a service client.
    (let* ([nickname (log/name/build (this/island/nickname) 'client.echo)]
           [x (islet/new (this/island) nickname TRUST/LOW environ/null environ/null)])
      (islet/jumpstart
       x
       (lambda ()
         ; Wait for alice to enter the network.
         (island/enter/wait (this/archipelago) ALICE/KP/BASE64)
         ; Request a remote evaluation.
         (let ([p (promise/new)]
               [thunk (island/compile '(let ([n 15]) (lambda () (+ n 99))))]) ; Compile the mobile code.
           ; We send to the echo server the pair: (<promise CURL> . <thunk>)
           (send u (cons (promise/resolver p) thunk))
           (let ([m (promise/block p)]) ; Just block until the promise is resolved.
             (displayln
              (format
               "  ~a got ~s back from ~a"
               nickname (murmur/payload m)
               (keystore/petname/look (this/keystore) (murmur/origin m))))))))))
  (client/echo u))

;; Code for alice who offers an echo service.
(define (alice/boot)
  (define (service/echo)
    (let* ([t (transport:bankers/new)] ; Generic queuing transport.
           [a/send    (access:send/known/new t 'access:send:echo GATE/NONE EMBARGO/NO)]
           [a/receive (access:receive/new t 'access:receive:echo GATE/NONE)]
           [nickname  (log/name/build (this/island/nickname) 'server.echo)]
           [keystore  (this/keystore)])
      (accessor/add (this/accessors) a/send) ; So the island router knows where to find us.

      ; Wait for an echo request.
      (let loop ([m (access/receive a/receive #f)])
        ;(displayln m)
        (when (murmur? m)
          (let ([payload (murmur/payload m)])
            (when (and (pair? payload) (curl? (car payload)))
              (let ([snippet
                     (if (procedure? (cdr payload))
                         (motile/call (cdr payload) BASELINE) ; Evaluate the thunk.
                         (cdr payload))]) ; No thunk. So just return the value whatever it is.
                (displayln
                 (format
                  "  ~a echoing ~s to ~a"
                  nickname snippet
                  (keystore/petname/look keystore (murmur/origin m))))
                (send (car payload) snippet))))) ; Echo the snippet back to the sender.
        (loop (access/receive a/receive #f)))))

  (service/echo))

(define alice/curl/echo (curl/zpl/safe-to-curl ALICE/CURL/ECHO KEYSTORE)) ; In-memory CURL instance.
(define alice (island/new 'alice ALICE/CURVE/SECRET alice/boot))
(define bob (island/new 'bob BOB/CURVE/SECRET (lambda () (client/boot alice/curl/echo))))

;; Multiple islands in the same address space can share the exact same keystore
;; and any change in the keystore will be seen by all such islands in the
;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)

(island/log/level/set 'warning)

(define (test/start)
  (island/start alice)
  (island/start bob))

(define (test/halt)
  (island/destroy alice)
  (island/destroy bob))

#| Sample Output
  server.echo@alice echoing 114 to bob
  client.echo@bob got 114 back from alice
|#

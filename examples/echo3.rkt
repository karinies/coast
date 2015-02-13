#lang racket/base

;; A sample echo server.
;; Alice will echo the payload of any request sent to it.
;; Two client islands, bob and carol will each send multiple echo requests to alice.
;; The wrinkle in this example is that bob and carol use promises to receive the
;; echo back from alice.

(require
 "../include/base.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))
(define CAROL/SECRET/PATH (string-append CERTIFICATE/SECRET "carol_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")
(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))
(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))
(define CAROL/CURVE/SECRET (path-to-curve CAROL/SECRET/PATH))

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
(define (client/boot u snippet n)
  ; Wait under the island with the given kp/base64 key enters the network.
  (define (island/enter/wait a kp/base64)
    (let loop ()
      (unless (archipelago/look a kp/base64)
        (sleep 2.0) ; Sleep for 2 seconds before looking again.
        (loop)))
    (sleep 1.0)) ; Make sure that the other island has seen us as well.

  ; The client that bob will spawn to request an echo from alice.
  ; u is the CURL that alice provides for her echo service.
  (define (client/echo u snippet n)
    ; Create a new islet on bob to act as a service client.
    (let* ([nickname (log/name/build (this/island/nickname) 'client.echo)]
           [x (islet/new (this/island) nickname TRUST/LOW environ/null environ/null)])
      (islet/jumpstart
       x
       (lambda ()
         (let* ([nickname (log/name/build (this/island/nickname) 'client.echo)])
           ; Wait for alice to enter the network.
           (island/enter/wait (this/archipelago) ALICE/KP/BASE64)
           ; Request an echo a total of n times.
           (let loop ([i 1])
             (when (<= i n)
               (let ([p (promise/new)])
                 ; We send the echo server: (cons <promise CURL> <something-to-echo>)
                 (send u (cons (promise/resolver p) snippet))
                 (let ([m (promise/wait p 30.0)])
                   (display
                    (format
                     "  [~a] ~a got ~s back from ~a\n"
                     i nickname (murmur/payload m)
                     (keystore/petname/look (this/keystore) (murmur/origin m)))))
                 (sleep 3.0) ; Wait a bit before contacting the echo service again.
                 (loop (add1 i))))))))))

  (client/echo u snippet n))

;; Code for alice who offers an echo service.
(define (alice/boot)
  (define (service/echo)
    (let* ([t (transport:bankers/new)] ; Generic queuing transport.
           [a/send    (access:send/known/new t 'access:send:echo GATE/NONE EMBARGO/NO)]
           [a/receive (access:receive/new t 'access:receive:echo GATE/NONE)]
           [nickname (log/name/build (this/island/nickname) 'server.echo)]
           [keystore (this/keystore)])
      (accessor/add (this/accessors) a/send) ; So the island router knows where to find us.

      ; Wait for an echo request.
      (let loop ([m (access/receive a/receive #f)])
        (when (murmur? m)
          (let ([payload (murmur/payload m)])
            (when (and (pair? payload) (curl? (car payload)))
              (display
               (format
                "  ~a echoing ~s to ~a\n"
                nickname (cdr payload)
                (keystore/petname/look keystore (murmur/origin m))))
              (send (car payload) (cdr payload)))))
        (loop (access/receive a/receive #f)))))

  (service/echo))

(define alice/curl/echo (curl/zpl/safe-to-curl ALICE/CURL/ECHO KEYSTORE)) ; In-memory CURL instance.
(define alice (island/new 'alice ALICE/CURVE/SECRET alice/boot))
(define bob
  (island/new
   'bob BOB/CURVE/SECRET
   (lambda () (client/boot alice/curl/echo 9999 3))))
(define carol
  (island/new
   'carol CAROL/CURVE/SECRET
   (lambda () (client/boot alice/curl/echo "thanks" 2))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)
(island/keystore/set carol KEYSTORE)

(island/log/level/set 'warning)

;; Call (test/start) to begin the test.
;; You should see 3 echo request/response pairs for bob
;; and 2 echo request/response pairs for carol.
(define (test/start)
  (island/start alice)
  (island/start bob)
  (island/start carol))

;; Call (test/halt) to clean up after the test.
(define (test/halt)
  (island/destroy alice)
  (island/destroy bob)
  (island/destroy carol))

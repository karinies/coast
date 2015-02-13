#lang racket/base
 
;; A sample remote execution server.

(require
 "../include/base.rkt"
 "../baseline.rkt"
 [only-in "../curl/base.rkt" curl/origin]
 "../remote.rkt"
 "../transport/gate.rkt"
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

;(define (curl-as-bytes keys path access metadata)
;  (let ([core (curl/core/new (curve/kp/base64 keys) path access metadata)])
;    (curl/core-to-curl/zpl/safe core (curve/ks/sign keys))))

;; Demonstrate how to generate an inline CURL for alice:
;; Execute
;;   (display (curl-as-bytes ALICE/CURVE/SECRET '(chirp) 'access:send:chirp #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline ALICE/CURL/chirp ...)
;; as shown below.

;; The CURL for alice's chirp service.
(define/curl/inline ALICE/CURL/CHIRP
#<<!!
SIGNATURE = #"yXvgDz4oDXCmtnSe9uZhTq0K9GttG1PN0WmyfKBuhSuh_wfR5CHl4M_OFyRoI2K9CaRwW-g_SCxcNtUdDdeRCg"
CURL
    id = b5359936-5826-4ce5-b362-a28c7782e22f
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (remote chirp)
    access/id = access:send:chirp
    created = "2014-05-20T17:52:29Z"
    metadata = #f

!!
)

(define THUNK
  (island/compile
   '(lambda ()
      (map
       chirp
       (list "roses are red"      "violets are blue"
             "COAST is wonderful" "but remote are you")))))

;; Return the petname of the island that transmitted murmur m.
(define (origin/petname m) (keystore/petname/look (this/keystore) (murmur/origin m)))

;; Code for a client island.
;; u - CURL for chirp service
;; snippet - datum sent to chirp service by client
;; n - total number of times client uses chirp service
(define (client/boot u)
  ; The client that bob and carol will spawn to request chirps from alice.
  ; u is the CURL that alice provides for her chirp service.
  (define (client/chirp u)
    ; Create a new islet to act as a service client.
    (let* ([nickname (log/name/build (this/island/nickname) 'client.chirp)]
           [x (islet/new (this/island) nickname TRUST/LOW environ/null environ/null)])
      (islet/jumpstart
       x
       (lambda ()
         (island/enter/wait (curl/origin u)) ; Wait for island service provider to enter the network.
         (for ([i (in-range 1 4)])
           (let ([p (promise/new)])
             ; We send the chirp server: (cons <promise CURL> <thunk>)
             (send u (cons (promise/resolver p) THUNK)) ; Request a remote evaluation.
             (let ([m (promise/block p #f)])
               (display
                (format "  ~a got ~s back from ~a\n"
                        nickname (murmur/payload m) (origin/petname m))))
             (sleep 0.75)))))))
  (client/chirp u))

(define (chirp chirper s)
  (cond
    [(and (string? s) (< (string-length s) 70))
     (display (format "~a: ~a\n" chirper s)) #t]
    [else #f]))

(define BASELINE+chirp
  (pair-to-environ BASELINE (define/global/1 'chirp chirp)))


(define (chirp/n nickname m)
  (let ([count [box m]])
    (lambda (s)
      (cond
        [(positive? (unbox count))
         (begin0
           (chirp nickname s)
           (set-box! count (sub1 (unbox count))))]
        [else #f]))))

(define (BASELINE+chirp/n nickname m)
  (pair-to-environ BASELINE (define/global/1 'chirp (chirp/n nickname m))))

;; Code for alice who offers an chirp service.
(define (alice/boot)
  (define (service/chirp)
    (let* ([t (transport:bankers/new)] ; Generic queuing transport.
           [a/send
            (access:send/known/new
             t 'access:send:chirp
             ; Kari's CHANGE
             ;(gate/or (gate/and (gate/whitelist BOB/KP/BASE64)   (gate/uses 3))
                      ;(gate/and (gate/whitelist CAROL/KP/BASE64) (gate/uses 1)))
             (gate/or (gate/and (gate/whitelist/island BOB/KP/BASE64)   (gate/uses 3))
                      (gate/and (gate/whitelist/island CAROL/KP/BASE64) (gate/uses 1)))
             EMBARGO/NO)]
           [a/receive (access:receive/new t 'access:receive:chirp GATE/NONE)]
           [keystore  (this/keystore)])
      (accessor/add (this/accessors) a/send) ; So the island router knows where to find us.
      
      ; Wait for an chirp request.
      (let loop ([m (access/receive a/receive #f)])
        (when (murmur? m)
          (let ([payload (murmur/payload m)])
            (when (and (pair? payload) (curl? (car payload)) (procedure? (cdr payload)))
              ; Create a fresh subordinate islet to evaluate the thunk.
              (let* ([GLOBAL ; Global binding environment for subordinate.
                      (cond
                        [(bytes=? (murmur/origin m) BOB/KP/BASE64)   (BASELINE+chirp/n 'bob   1)]
                        [(bytes=? (murmur/origin m) CAROL/KP/BASE64) (BASELINE+chirp/n 'carol 4)]
                        [else BASELINE])]
                     [worker (subislet/new 'service.chirp.worker TRUST/LOWEST GLOBAL)])
                ; Task and start the worker with a 2.0 second deadline.
                (remote worker (cdr payload) (car payload) 2.0)))))
        (loop (access/receive a/receive #f)))))
  
  (service/chirp))

(define alice/curl/chirp (curl/zpl/safe-to-curl ALICE/CURL/CHIRP KEYSTORE)) ; In-memory CURL instance.
(define alice (island/new 'alice ALICE/CURVE/SECRET alice/boot))
(define bob (island/new 'bob BOB/CURVE/SECRET       (lambda () (client/boot alice/curl/chirp))))
(define carol (island/new 'carol CAROL/CURVE/SECRET (lambda () (client/boot alice/curl/chirp))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)
(island/keystore/set carol KEYSTORE)

(island/log/level/set 'warning)

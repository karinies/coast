#lang racket/base

(require
  "../include/base.rkt"
  "../baseline.rkt"
  "../promise.rkt"
  "../promise/base.rkt"
  "../transport/gate.rkt"
  "../accounting/stomp-transport.rkt"
  "../accounting/como-types.rkt"
  "../Island/island-como.rkt"
  "../accounting/como.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH (string-append CERTIFICATE/SECRET "bob_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")
(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))
(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))

;; CURLs used by alice/i, alice/j and bob/k to exchange information.
(define global/curl/ALICE/inter #f) ; INTER CURL for alice/i.
(define global/curl/ALICE/intra #f) ; INTRA CURL for alice/i.
(define global/curl/BOB/inter #f)   ; INTER CURL for bob/k

(define semaphore/ALICE/intra (make-semaphore)) ; Semaphore used to coordinate alice/i and alice/j.
(define semaphore/ALICE/inter (make-semaphore)) ; Semaphore used to coordinate alice and bob.

(define (boot/islet/ALICE/i)
  (displayln "Booting alice/i islet...")
  
  (let ([d/inter (islet/curl/new '(i) GATE/ALWAYS environ/null INTER)] ; Create a CURL for INTER communication.
        [d/intra (islet/curl/new '(j) GATE/ALWAYS environ/null INTRA)])  ; Create a CURL for INTRA communication.
    (displayln "alice/i created a CURL for INTER")
    (displayln "alice/i created a CURL for INTRA")
    
    (set! global/curl/ALICE/inter (duplet/resolver d/inter)) ; Set as global, other islets will use it.
    (set! global/curl/ALICE/intra (duplet/resolver d/intra)) ; Set as global, other islets will use it.
    
    (semaphore-post semaphore/ALICE/intra) ; Notify semaphore that alice/i is done.
    
    (let* ([murmur (duplet/block d/intra)] ; Wait until alice/j sends us a message.
           [curl/received (murmur/payload murmur)])
      (displayln "alice/i received CURL from alice/j")))
  
  (semaphore-wait semaphore/ALICE/inter)  ; Wait for bob/k.
  
  (let ([curl/packaged (islet/curl/new '(packaged) GATE/ALWAYS environ/null INTER)]) ; Create a CURL to be sent to bob.
    (displayln "alice/i created a CURL to be sent.")
    (send global/curl/BOB/inter (duplet/resolver curl/packaged))
    (displayln "alice/i sent a message to bob/k"))) ; Send the CURL to bob.


(define (boot/islet/ALICE/j)
  (displayln "Booting alice/j islet...")
  
  (let ([curl/packaged (islet/curl/new '(packaged) GATE/ALWAYS environ/null INTER)]) ; Create a CURL that will be transferred to alice/i.
    (displayln "alice/j created a CURL to be sent.")
    
    (semaphore-wait semaphore/ALICE/intra) ; Wait until alice/i is ready.
    
    (send global/curl/ALICE/intra (duplet/resolver curl/packaged)) ; Send the packaged curl to alice/i using its INTRA CURL.
    (displayln "alice/j sent a message to alice/i"))) 

(define (alice/boot)
  (let ([i (islet/new (this/island) 'i TRUST/MODERATE environ/null environ/null)]
        [j (islet/new (this/island) 'j TRUST/MODERATE environ/null environ/null)])
    (islet/jumpstart
     i
     (lambda () (boot/islet/ALICE/i))) ; Start alice/i.
    
    (islet/jumpstart
     j
     (lambda () (boot/islet/ALICE/j)))))  ; Start alice/j.

(define (boot/islet/BOB/k)
  (displayln "Booting bob/k islet...")
  
  (let ([d/inter (islet/curl/new '(packaged) GATE/ALWAYS environ/null INTER)])
    (displayln "bob/k created a CURL for INTER")
    
    (set! global/curl/BOB/inter (duplet/resolver d/inter))
    
    (semaphore-post semaphore/ALICE/inter) ; Let alice/i know that bob's CURL is ready.
    
    (let* ([murmur (duplet/block d/inter)] ; Wait for a message from alice/i.
           [curl/received (murmur/payload murmur)]) ; Unpack the message.
      (displayln "bob/k received a CURL from alice/i")))
  
  (print-filters)
  
  (island/monitoring/set/filter (this/island/nickname) como:filter/FALSE)
  
  (print-filters)
  
  (island/monitoring/set/filter (this/island/nickname) (lambda (event) (eq? (como:event-place event) INTER)))
  
  (print-filters)
  )

(define (print-filters)
  (displayln "Printing filter results")
  (displayln "=======================\n")
  
  (displayln (format "curl-new,INTER: ~a" (island/monitoring/pass? #:type COMO/CURL/NEW #:place INTER)))
  (displayln (format "curl-new,INTRA: ~a" (island/monitoring/pass? #:type COMO/CURL/NEW #:place INTRA)))
  (displayln (format "curl-send,INTER: ~a" (island/monitoring/pass? #:type COMO/CURL/SEND #:place INTER)))
  (displayln (format "curl-send,INTRA: ~a" (island/monitoring/pass? #:type COMO/CURL/SEND #:place INTRA)))
  (displayln (format "curl-transfer,INTER: ~a" (island/monitoring/pass? #:type COMO/CURL/TRANSFER #:place INTER)))
  (displayln (format "curl-transfer,INTRA: ~a" (island/monitoring/pass? #:type COMO/CURL/TRANSFER #:place INTRA)))
  (displayln (format "curl-receive,INTER: ~a" (island/monitoring/pass? #:type COMO/CURL/RECEIVE #:place INTER)))
  (displayln (format "curl-receive,INTRA: ~a" (island/monitoring/pass? #:type COMO/CURL/RECEIVE #:place INTRA))))

(define (bob/boot)
  (let ([k (islet/new (this/island) 'k TRUST/MODERATE environ/null environ/null)])
    (islet/jumpstart
     k
     (lambda () (boot/islet/BOB/k))))) ; Start bob/k.

(define alice (island/new 'alice ALICE/CURVE/SECRET alice/boot))
(define bob   (island/new 'bob   BOB/CURVE/SECRET bob/boot))

;; Enable COAST Monitoring on alice and bob.
(let ([messenger (stomp-messenger-new #:host "peru.local"
                                      #:login "coastdev"
                                      #:pass "Hi123"
                                      #:destination "/queue/coast")])
  (island/monitoring/start (island-nickname alice) messenger)
  (island/monitoring/start (island-nickname bob) messenger))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)
(island/log/level/set 'warning)

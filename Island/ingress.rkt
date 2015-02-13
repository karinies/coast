#lang racket/base

(require
 racket/contract/base
 ;racket/match
 "accessor.rkt"
 "archipelago.rkt"
 "base.rkt"
 "keystore.rkt"
 "logger.rkt"

 "../bindings/libzyre/zyre_event.rkt"
 "../bindings/libczmq/zframe.rkt"
 "../bindings/libczmq/zhash.rkt"
 "../bindings/libczmq/zmessage.rkt"
 "../curl/curl.rkt"
 [only-in "../curl/base.rkt" curl? curl curl/origin curl/verified]
 "../curve.rkt"
 "../islet.rkt"
 "../murmur.rkt"
 "../serialize.rkt")

(provide
 (contract-out
  [ingress/worker thunk/c])
 KP/HEADER
 KP/SIGN/HEADER)

#|
At the island level four distinct message forms are recognized:
ASK, SPAWN, REMOTE, and MURMUR.

ASK implements primitive service requests (predominantly to obtain bootstrap CURLs) that
are exposed to other islands as advertisments in ZyRE ENTER messages.

ASK <origin> <reply>
  <origin> - island kp/z85 for the island issuing the request
  <service> - the advertised service that is the topic of the GET
  <reply> - CURL for response
The responding island J issues a MURMUR in response:
MURMUR J <reply> <service-information>

SPAWN creates an free running islet on a neighboring island

SPAWN <origin> <target> <thunk>
  <origin> - island kp/z85 for the island issuing the request
  <target> - a CURL denoting the target to which the SPAWN is directed
  <thunk> - Motile thunk for execution by a spawned islet

REMOTE creates an subordinate islet on a neighboring island for remote evaluation

REMOTE <origin> <target> <thunk> <reply>
  <origin> - island kp/z85 for the island issuing the request
  <target> - a CURL denoting the target to which the REMOTE is directed
  <thunk> - Motile thunk for evaluation by <target>
  <reply> - a CURL denoting the destination of the return value of the remote evaluation
The return value is transmitted as a MURMUR J <reply> <value>

MURMUR directs an application-specific message to a target islet
residing on a neighboring island

MURMUR <origin> <target> <message>
  <origin> - island kp/z85 for the island issuing the message
  <target> - a CURL denoting the target to which the MURMUR is directed
  <message> - an arbitrary Motile value
The interpretation of the MURMUR is target-dependent
|#

(define KP/HEADER      "X-CURVE:KP")
(define KP/SIGN/HEADER "X-CURVE:KP/SIGN")

;; Returns byte string b or the first 32 bytes of b whichever is less.
(define (bytes/head b)
  (if (> (bytes-length b) 32) (subbytes b 0 31) b))

(define FAILURE (gensym))

(define (deserialize/safe x failure)
  (let ([in (open-input-bytes x)])
    (with-handlers
        ([exn:fail:contract? ; Fatal error in motile/deserialize.
          (lambda (e)
            (log/info (this/island/nickname) "motile/deserialize failed" (exn-message e))
            failure)]
         
         [exn:fail:read? ; Fatal error in reading motile/serial structure.
          (lambda (e)
            (close-input-port in)
            (log/info (this/island/nickname) "motile/serial structure ill-formed" (exn-message e))
            failure)])
      (let ([v (read in)])
        (close-input-port in)
        (motile/deserialize (motile/flat/reconstruct v))))))


;; Given a ZyRE UUID obtain the public key identifyig the neighboring island.
(define (sender-to-origin b sender)
  (let ([n (archipelago/look b sender)]) (and n (neighbor/kp/base64 n))))

;; Extract and return the transmitting island kp/base64 from the ENTER event headers.
;; Returns #f if the key was not present or if it was ill-formatted.
(define (kp/base64/extract event)
  (log/debug (this/island/nickname) "kp/base64/extract got"
             (cons (zyre/event/flavor event) (zyre/event/headers event)))
  (and
   (zyre/event/headers event)
   (let* ([kp/string (zyre/event/header event KP/HEADER)]
          [kp/bytes  (and kp/string (string->bytes/latin-1 kp/string))])
     (and (kp/base64? kp/bytes) kp/bytes))))
;; Extract and return the transmitting island kp/sign/base64 from the ENTER event headers.
;; Returns #f if the key was not present or if it was ill-formatted.
(define (kp/sign/base64/extract event)
  (log/debug (this/island/nickname) "kp/sign/base64/extract got"
             (cons (zyre/event/flavor event) (zyre/event/headers event)))
  (and
   (zyre/event/headers event)
   (let* ([kp/sign/string (zyre/event/header event KP/SIGN/HEADER)]
          [kp/sign/bytes  (and kp/sign/string (string->bytes/latin-1 kp/sign/string))])
     (and (kp/sign/base64? kp/sign/bytes) kp/sign/bytes))))

;; A raw message from one island to another contains two frames:
;; the target CURL and the payload.
;; On success returns (murmur <origin> <target> <payload>) and #f otherwise.
;; OBSOLETE !!!!
;(define (unpack m origin kp/base64 curl/bytes/max body/bytes/max)
;  (log/debug (this/island/nickname) (format "frames:~a size:~a" (zmessage/frames m) (zmessage/size m)) #t)
;  (if (and (= (zmessage/frames m) 2) (positive? (zmessage/size m)))
;      (let ([frame/curl (zmessage/pop m)]  ; curl
;            [frame/body (zmessage/pop m)]) ; body
;        (log/info (this/island/nickname) "frame/curl" (zframe/data frame/curl))
;        (log/debug (this/island/nickname) "frame/body" (zframe/data frame/body))
;        (begin0
;          (if (and (< 0 (zframe/size frame/curl) curl/bytes/max) (< 0 (zframe/size frame/body) body/bytes/max))
;              ; curl and body satisfy size restrictions.
;              (let ([u    (deserialize/safe (zframe/data frame/curl) #f)]
;                    [body (deserialize/safe (zframe/data frame/body) FAILURE)])
;                (if (and
;                     (and (curl? u) (bytes=? kp/base64 (curl/origin u)) (curl/verified u) u)
;                     (not (eq? body FAILURE)))
;                    (murmur origin u body) ; Success.
;                    #f)) ; u was not a CURL OR not a CURL for this island OR u was unverified
;              #f) ; curl or body did not satisfy size restrictions.
;          (zframe/destroy frame/curl)
;          (zframe/destroy frame/body)))
;      ; Message is ill-formed.
;      #f))


(define (unpack m origin kp/sign curl/bytes/max body/bytes/max)
  (log/debug (this/island/nickname) (format "frames:~a size:~a" (zmessage/frames m) (zmessage/size m)) #t)
  (if (and (= (zmessage/frames m) 2) (positive? (zmessage/size m)))
      (let ([frame/curl (zmessage/pop m)]  ; curl
            [frame/body (zmessage/pop m)]) ; body
        ;(log/info (this/island/nickname) "frame/curl" (zframe/data frame/curl))
        ;(log/debug (this/island/nickname) "frame/body" (zframe/data frame/body))
        (begin0
          (if (and (< 0 (zframe/size frame/curl) curl/bytes/max) (< 0 (zframe/size frame/body) body/bytes/max))
              ; curl and body satisfy size restrictions.
              (let* ([curl/zpl/signed (zframe/data frame/curl)]
                     [core (curl/zpl/signed-to-curl/core curl/zpl/signed kp/sign)]
                     [body (deserialize/safe (zframe/data frame/body) FAILURE)])
                (if (and core (not (eq? body FAILURE)))
                    (murmur origin (curl core kp/sign curl/zpl/signed) body) ; Success.
                    #f)) ; u was not a CURL for this island or ill-formed
              #f) ; curl or body did not satisfy size restrictions.
          (zframe/destroy frame/curl)
          (zframe/destroy frame/body)))
      ; Message is ill-formed.
      #f))
       
;


;; Extract the command contained in the message of the WHISPER event.
;(define (command/unpack frame command/bytes/max)
;  (if (< 0 (zframe/size frame) command/bytes/max)
;      (let ([buffer (zframe/data frame)])
;        (zframe/destroy frame)
;        buffer)
;      #f))

;; Extract the CURL used by the sending island.
;; frame - the zframe containing the serialized CURL
;; curl/bytes/max - the length in bytes of the largest CURL we will accept
;; kp/base64 - public key of the target island
;; On success returns the CURL used by the sending island
;; If the deserialization fails, the CURL u is for the wrong island, has been tampered with,
;; the frame is empty, or the frame was too large return #f.
;(define (curl/unpack frame curl/bytes/max kp/base64)
;  (if (< 0 (zframe/size frame) curl/bytes/max)
;      (let* ([buffer (zframe/data frame)]
;             [_      (zframe/destroy frame)] ; Guarantee that we deallocate the frame.
;             [u      (deserialize/safe buffer #f)])
;        ; u must be a signed CURL for this island. 
;        (and (curl? u) (bytes=? kp/base64 (curl/origin u)) (curl/verified u) u))
;     #f)) ; Frame is either empty or too large.

;; Recover the transmitted payload.
;; frame - the zframe containing the payload
;; payload/bytes/max - ceiling on the largest payload that we will accept
;; Returns ERROR/DESERIALZE if deserialization failed.
;; Returns ERROR/FRAME if frame is empty or too large.
;; Otherwise returns the deserialized value. 
;(define (payload/unpack frame payload/bytes/max)
;  (if (< 0 (zframe/size frame) payload/bytes/max)
;      (let ([buffer (zframe/data frame)])
;        (zframe/destroy frame)
;        (deserialize/safe buffer FAILURE))
;      FAILURE))
(define (payload? x) (not (eq? x FAILURE)))

;; i - island?
;; command/bytes/max
;; curl/bytes/max -  ceiling on size of serialized CURL
;; payload/bytes/max - ceiling on size of serialized payload
(define (ingress/worker)
  (let ([i (this/island)])
    (let ([nickname    (log/name/build (island/nickname i) 'ingress)]
          [router      (island/router i)]
          [archipelago (island/archipelago i)]
          [keystore    (island/keystore i)] ; For logging.
          [kp/sign     (curve/kp/sign (island/keys i))]
          [curl/bytes/max    (island/curl/bytes/max    i)]
          [payload/bytes/max (island/payload/bytes/max i)])
      (let loop ([x (thread-receive)])
        (cond
          ([vector? x]
           (cond
             [(eq? (vector-ref x 0) 'ENTER) ; #(ENTER <sender> <event>)
              (let ([sender (vector-ref x 1)]
                    [event  (vector-ref x 2)])
                (log/info nickname "saw ENTER" sender)
                (let* ([kp/base64       (kp/base64/extract      event)]
                       [kp/sign/base64  (kp/sign/base64/extract event)])
                  (if (and kp/base64 kp/sign/base64)
                      (archipelago/add archipelago sender kp/base64 kp/sign/base64)
                      (log/warning nickname "no CURVE keys" #f)))
                (zyre/event/destroy event)
                (loop (thread-receive)))]
             
             [(eq? (vector-ref x 0) 'WHISPER) ; #(WHISPER <sender> <event>)
              (let* ([sender (vector-ref x 1)]
                     [event  (vector-ref x 2)]
                     [m (zyre/event/message event)]
                     [origin (sender-to-origin archipelago sender)]) ; Discover kp/base64 of transmitting island.
                (if origin
                   (if m
                       (let ([unpacked (unpack m origin kp/sign curl/bytes/max payload/bytes/max)])
                         (if unpacked
                             (unless (thread-send router unpacked #f)
                               (log/fatal nickname "router is dead" #f))
                             ; unpack failed.
                             (log/error
                              nickname "message ill-formed" (keystore/petname/look keystore origin))))

                       ; Message empty or ill-formed.
                       (log/warning nickname "WHISPER ill-formed" (keystore/petname/look keystore origin)))

                    (log/info nickname "WHISPER of unknown origin" sender))

                (zyre/event/destroy event)
                (loop (thread-receive)))]

             [(eq? (vector-ref x 0) 'EXIT) ; #(EXIT <sender>)
              (let* ([sender (vector-ref x 1)]
                     [n (archipelago/look archipelago sender)])
                (when n
                  (archipelago/remove archipelago sender)
                  (log/info
                   nickname "saw EXIT" (keystore/petname/look keystore (neighbor/kp/base64 n)))))
              (loop (thread-receive))]))
          
          [else (loop (thread-receive))]))))) ; Ignore.

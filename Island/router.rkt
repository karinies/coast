#lang racket/base

(require
 racket/contract/base
 "base.rkt"
 "accessor.rkt"
 "keystore.rkt"
 "logger.rkt"
 "../curve.rkt"
 "../curl/base.rkt"
 "../islet.rkt"
 "../murmur.rkt"
 "../transport/access.rkt")

(provide
 (contract-out
  [router/worker thunk/c]))

;; Remark: we guarantee delivery order in the following sense:
;; For any specific transmitter x all murmurs delivered to a transport t
;; are delivered in transmission order.
;; That is, is x sends two murmurs m and m' via access:send points for t,
;; a and a' respectively in the order, first m and then m' then we guarantee
;; that m arrives at transport t before m'.
;; Even if the gates of a and a' are identical scheduling delays may arise
;; (if routing for a and a' are handled by different threads) that fail to
;; preserve message ordering.

;; We solve the ordering problem by relying on a single routing thread however
;; that may lead to delivery delays.
;; One solution is a pool of routing threads where a thread is selected from the
;; pool and assigned to handle all routing for a given transport.
;; This preserves the ordering constraint and may improve performance overall.
;; Alternatively we could weaken the ordering guarantee by requiring that murmurs
;; send via the SAME access:send point are delivered in order but, in the case above,
;; two murmurs send by the same transmitter, but via two distinct access points a and a'
;; (where both a and a' are for a single transport t), may not necessarily arrive in transmission
;; order. This solution would allow any thread in a pool of router threads to be assigned
;; to shepard a murmur through an access:send point for delivery.

;; accessors - box containing accessor map of island.
(define (router/worker)
  (let ([nickname  (log/name/build (this/island/nickname) 'router)]
        [accessors (this/accessors)]
        [kp/base64 (curve/kp/base64 (this/curve))]) ; Public key of this/island.

    ; Safely send murmur m via access:send a.
    (define (send/safe a m)
      (with-handlers
          ([exn:fail?
            (lambda (e)
              (let ([s (format "saw access:send ~a raise exeception" (access/nickname a))])
                (log/error nickname s e)
                #f))])  
        (access/send a m)))

    ; Just loop passing each murmur thru to the access:send point named in the CURL of the murmur.
    (let loop ([m (thread-receive)])
      (cond
        [(murmur? m)
         ;(log/info nickname "saw murmur" (keystore/petname/look (this/keystore) (murmur/origin m)))
          (if (murmur/target/local? m kp/base64)
              (let* ([access/id (curl/access (murmur/target m))])
                (if (symbol? access/id)
                    (cond
                      [(accessor/look accessors access/id) => (lambda (a) (send/safe a m))]
                      [else (log/info nickname "no such access:send" (vector access/id (curl/path (murmur/target m))))])
                    (log/warning nickname "saw local CURL" #f)))
              ; target CURL of murmur did not originate on this island.
              (log/warning nickname "saw remote murmur target" (curl/origin (murmur/target m))))]
          ; Router received something that wasn't a murmur?
        [else (log/warning nickname "saw something other than a murmur" m)])
      (loop (thread-receive)))))

    
    
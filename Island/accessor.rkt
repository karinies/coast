#lang racket/base

;; The accessor map is a hash map of "live" access:send points that are
;; referenced by an island's CURLs.

(require
 racket/contract/base
 "logger.rkt"
 "../persistent/hash.rkt"
 "../transport/access.rkt")

(provide
 accessor/c
 (contract-out
  [accessor/new    (-> accessor/c)]
  [accessor/add    (-> accessor/c access:send? void?)]
  [accessor/remove (-> accessor/c symbol? void?)]
  [accessor/remove* (-> accessor/c symbol? access:send? void?)]
  [accessor/look   (-> accessor/c symbol? (or/c access:send? #f))]))

(define accessor/c (flat-named-contract 'accessor (box/c hash/eq? #:flat? #t)))

(define (accessor/new) (box hash/eq/null))

;; Add the given access:send point to the access pond of island i.
(define (accessor/add b access)
  (let loop ([before (unbox b)])
    (when (not (box-cas! b before (hash/cons before (access/id access) access)))
      (loop (unbox b)))))

;; Remove the access:send point with the given id from the access pond of island i.
(define (accessor/remove b id)
  (let loop ([before (unbox b)])
    (unless (box-cas! b before (hash/remove before id))
      (loop (unbox b)))))

(define (accessor/look b id) (hash/ref (unbox b) id #f))

;; Similar to accessor/remove but checks to ensure that the
;; access:send point to be removed is actually the one referenced
;; by the given id.
;; This routine is used by the reaper to scavenge closed access:send points
;; and avoids removing an access:send point whose id is well-known
;; (for example, access:send:echo rather than a UUID) but whose corresponding
;; access:send point has changed.
;; This situation can arise if a well-known service has to restart or if the island
;; if forced to change the gates that restrict access to the service.
(define (accessor/remove* b id access)
  (let loop ([before (unbox b)])
    (when (eq? access (hash/ref before id #f))
      (unless (box-cas! b before (hash/remove before id))
        (loop (unbox b))))))




#lang racket/base

(require
 racket/contract/base
 "accessor.rkt"
 "base.rkt"
 "logger.rkt"
 "../islet.rkt"
 "../persistent/hash.rkt"
 "../transport/access.rkt"
)

(provide
 (contract-out
  [reaper/worker thunk/c]))

;; Generate a reaper closure to cull closed gates from an island's accessors map.
;; b - a box containing the accessors map.
;; The closure repeatedly scans the accessors map for closed access:send points accumulating the
;; id/access:send pairs.
;; At the end of each scan the closed access:send points are removed from the map.
;; Note: an access:send point is closed if it has been revoked or its gate is closed.
(define (reaper/worker)
  (let ([nickname (log/name/build (this/island/nickname) 'reaper)]
        [b (this/accessors)])
    (let loop ()
      (let ([purges
             (hash/fold
              (unbox b)
              (lambda (k a seed) (if (access/closed? a) (cons (cons k a) seed) seed)) null)])
        (when (not (null? purges))
          (log/info nickname "purges" (length purges))
          (for ([pair (in-list purges)]) (accessor/remove* b (car pair) (cdr pair))))
      (loop)))))  
    
#lang racket/base

(require
  racket/contract
  "islet.rkt"
  "promise.rkt"
  "send.rkt"
  "murmur.rkt"
  "baseline.rkt"
  "persistent/environ.rkt"
  "transport/gate.rkt"
  )

(provide 
 (contract-out
  [subislet/callback/new (-> symbol? environ? any/c pair?)])
 )

;; Creates a subislet that executes a given callback every time it receives a message via the returned CURL.
;; nickname - An alias for the worker.
;; environ - The binding environment available for the worker.
;; motile/callback - This callback will be executed every time the created worker receives a message via c.
;; Returns: A pair (w,c) where w is the created worker and c is a CURL that can be used to communicate with the worker.
(define (subislet/callback/new nickname environ motile/callback)
  (let* ([x (subislet/new nickname (this/islet/trust) environ)] ; The new worker.
         [parent (this/islet)] ; The islet invoking this function.
         [p/for/curl (promise/new)]) ; Used to receive the curl created by the worker.
    (islet/jumpstart
     x
     (lambda ()
       (let ([d (islet/curl/new (list nickname) GATE/ALWAYS #f INTER)]) ; Create a duplet that the worker will use to receive messages.
         (send (promise/resolver p/for/curl) (duplet/resolver d)) ; Send the new curl to the parent.
         (let loop ([m (duplet/block d)]) ; Listen for new messages.
           (when (murmur? m) ; When a murmur is received (it should happen every time)
             (let ([payload (murmur/payload m)]) ; Extract the payload.
               (motile/call motile/callback environ payload))) ; Execute the motile callback passing the received message.
           (loop (duplet/block d))))))
    (cons x (murmur/payload (promise/block p/for/curl))))) ; Return the pair (worker, curl)
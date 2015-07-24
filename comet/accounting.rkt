#lang racket/base

(require "comet.rkt"
         "stomp-transport.rkt")

#|
 | Application.
 |#

(let* ([messenger (stomp-messenger-new #:host "peru.local"
                                       #:login "coastdev"
                                       #:pass "Hi123"
                                       #:destination "/queue/coast")]
       [logger (comet:logger messenger)])
  (let loop ()
    (define anEvent (comet:event "argentina" "test-type" comet:protocol/LATEST "TestValue" (current-inexact-milliseconds)))
    (comet:log/event logger anEvent) 
    (sleep 1.0)
    (loop))
  (comet:transport-messenger/shutdown messenger)
  )
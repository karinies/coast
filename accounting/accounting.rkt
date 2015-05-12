#lang racket/base

(require "como.rkt"
         "stomp-transport.rkt")

#|
 | Application.
 |#

(let* ([messenger (stomp-messenger-new #:host "peru.local"
                                       #:login "coastdev"
                                       #:pass "Hi123"
                                       #:destination "/queue/coast")]
       [logger (como:logger messenger)])
  (let loop ()
    (define anEvent (como:event "argentina" "test-type" como:protocol/LATEST "TestValue" (current-inexact-milliseconds)))
    (como:log/event logger anEvent) 
    (sleep 1.0)
    (loop))
  (como:transport-messenger/shutdown messenger)
  )
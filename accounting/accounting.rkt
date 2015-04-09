#lang racket/base

(require "como.rkt"
         "stomp-transport.rkt")

#|
 | Application.
 |#

(let* ([messenger (stomp-messenger-new #:host "128.195.59.247"
                                       #:login "coastdev"
                                       #:pass "Hi123"
                                       #:destination "/exchange/coast")]
       [logger (como:logger messenger)])
  (let loop ()
    (define anEvent (como:event "argentina" "test-type" como:protocol/VERSION "TestValue"))
    (como:log logger anEvent) (loop))
  (stomp-messenger-shutdown messenger)
  )
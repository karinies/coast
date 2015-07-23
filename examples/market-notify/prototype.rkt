#lang racket/base

(require
  "../../include/base.rkt"
  "trader.rkt"
  "risk-server.rkt"
  "robot-server.rkt"
  "market-server.rkt"
  "order-router.rkt"
  "../examples-base.rkt"
  "../examples-env.rkt")

;; Silence Islands
;(islet/log/silence 'robot-server)
(islet/log/silence 'risk-server)
(islet/log/silence 'market-server)
(islet/log/silence 'order-router)
;(islet/log/silence 'trader)

(define (run)
  (island/start risk-server)
  (island/start robot-server)
  (island/start market-server)
  (island/start order-router)
  ;; Get the KP/BASE85 for these guys.
  (sleep 10)
  (island/start trader))

(define (kill)
  (island/destroy risk-server)
  (island/destroy robot-server)
  (island/destroy market-server)
  (island/destroy order-router)
  ;; Get the KP/BASE85 for these guys.
  (island/destroy trader))

;(island/log/level/set 'debug)
(island/log/level/set 'warning)
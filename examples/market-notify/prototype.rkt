#lang racket/base

(require
  "../../include/base.rkt"
  "trader.rkt"
  "risk-server.rkt"
  "robot-server.rkt"
  "market-server.rkt"
  "order-router.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")

(define RISK-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "risk_server_secret"))
(define TRADER/SECRET/PATH   (string-append CERTIFICATE/SECRET "trader_secret"))
(define ORDER-ROUTER/SECRET/PATH (string-append CERTIFICATE/SECRET "order_router_secret"))
(define MARKET-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "market_server_secret"))
(define ROBOT-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "robot_server_secret"))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ROBOT-SERVER/CURVE/SECRET (path-to-curve ROBOT-SERVER/SECRET/PATH))
(define RISK-SERVER/CURVE/SECRET (path-to-curve RISK-SERVER/SECRET/PATH))
(define MARKET-SERVER/CURVE/SECRET (path-to-curve MARKET-SERVER/SECRET/PATH))
(define ORDER-ROUTER/CURVE/SECRET (path-to-curve ORDER-ROUTER/SECRET/PATH))
(define TRADER/CURVE/SECRET   (path-to-curve TRADER/SECRET/PATH))

(define (run)
  (island/start risk-server)
  (island/start robot-server)
  (island/start market-server)
  (island/start order-router)
  ;; Get the KP/BASE85 for these guys.
  (sleep 5)
  (island/start trader))


(define (kill)
  (island/destroy risk-server)
  (island/destroy robot-server)
  (island/destroy market-server)
  (island/destroy order-router)
  ;; Get the KP/BASE85 for these guys.
  (island/destroy trader))
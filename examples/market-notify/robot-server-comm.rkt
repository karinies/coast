#lang racket/base

(require
  "../../include/base.rkt"
  )

(provide robot/get-curl/market-server
         robot/get-curl/risk-server
         robot/get-curl/order-router
         )

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")

(define MARKET-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "market_server_secret"))
(define MARKET-SERVER/CURVE/SECRET (path-to-curve MARKET-SERVER/SECRET/PATH))

(define RISK-SERVER/SECRET/PATH (string-append CERTIFICATE/SECRET "risk_server_secret"))
(define RISK-SERVER/CURVE/SECRET (path-to-curve RISK-SERVER/SECRET/PATH))

(define ORDER-ROUTER/SECRET/PATH (string-append CERTIFICATE/SECRET "order_router_secret"))
(define ORDER-ROUTER/CURVE/SECRET (path-to-curve ORDER-ROUTER/SECRET/PATH))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

;; Demonstrate how to generate an inline CURL for market-server:
;; Execute
;;   (display (curl-as-bytes MARKET-SERVER/CURVE/SECRET '(service spawn) 'access:send.service.spawn #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline MARKET-SERVER/CURL/chirp ...)
;; as shown below.

(define/curl/inline MARKET-SERVER/CURL/SPAWN
  #<<!!
SIGNATURE = #"GNzBZNi6r6WTBdASzv_R0GJjAiwaBYtHkZhiMlyKTD8E-S-mL-A7SMFR7_9IKNl8_JJcfzOIBQh4YDnP3JoWBw"
CURL
    id = 0dd4f4f5-72ce-40fe-996f-f80700c322f0
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (service spawn)
    access/id = access:send.service.spawn
    created = "2014-05-30T14:47:58Z"
    metadata = #f

!!
  )

(define/curl/inline RISK-SERVER/CURL/SPAWN
  #<<!!
SIGNATURE = #"Cm0bi_br3x1OqZiun6vvpMgsTMQuP5VOca-hVysyT_4xUCuOQUsx8SQUB8V7aQhHC5xilssNWCXayd5lhi31DA"
CURL
    id = 6264c494-b240-43dc-892f-10c7dfbea378
    origin = #"rqM_XCwrsziuhIEsG1d0yMA05mivoewXhUmzKUzhb0s"
    path = (service spawn)
    access/id = access:send.service.spawn
    created = "2015-05-26T17:15:50Z"
    metadata = #f

!!
  )

(define/curl/inline ORDER-ROUTER/CURL/SPAWN
  #<<!!
SIGNATURE = #"K-0c45TlB_tC5l5UkBJsub9TyRBazVMbUeh2HOCePEBudroFgHy_4kP8ZfE-uW_bytNv_Ge4eTqQEvOw7dASCw"
CURL
    id = e66966f1-1d86-4ae2-a4c5-721f2370faf5
    origin = #"IdXHE05WUxvB291oUXpQYdH2Q8DPi67D4cMgTxOSEiw"
    path = (service spawn)
    access/id = access:send.service.spawn
    created = "2015-05-26T17:17:28Z"
    metadata = #f

!!
  )


(define (robot/get-curl/market-server) ; Returns a curl for communicating capability with market server
  (curl/zpl/safe-to-curl MARKET-SERVER/CURL/SPAWN KEYSTORE))

(define (robot/get-curl/risk-server) ; Returns a curl for communicating capability with risk server
  (curl/zpl/safe-to-curl RISK-SERVER/CURL/SPAWN KEYSTORE))

(define (robot/get-curl/order-router) ; Returns a curl for communicating capability with order router
  (curl/zpl/safe-to-curl ORDER-ROUTER/CURL/SPAWN KEYSTORE))



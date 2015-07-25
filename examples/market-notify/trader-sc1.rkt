#lang racket/base

(require
 "../../include/base.rkt"
 "../../baseline.rkt"
 [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata]
 "../../islet-utils.rkt"
 "../../uuid.rkt"
 "../examples-base.rkt"
 "../examples-env.rkt")

(provide trader)

;(define ROBOT-SERVER/SECRET/PATH   (string-append CERTIFICATE/SECRET "robot_serve_secret"))
;(define ROBOT-SERVER/CURVE/SECRET   (path-to-curve ROBOT-SERVER/SECRET/PATH))
;; Demonstrate how to generate an inline CURL for market-server:
;; Execute
;;   (display (curl-as-bytes ROBOT-SERVER/CURVE/SECRET '(service spawn) 'access:send.service.spawn #f))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline ROBOT-SERVER/CURL/chirp ...)
;; as shown below.

(define/curl/inline ROBOT-SERVER/CURL/SPAWN
  #<<!!
SIGNATURE = #"XbVHE3O5lhPAL-XreJ1z_q9QGftm21w5c9mOg48Fspe_KT0w5xKlVi9xprq8PcmZ7chKJK7yTgZMHW3UL4feBw"
CURL
    id = b927dace-e6b5-4ca7-9c22-b1d4e3ba4b9f
    origin = #"RaQDnsBmoxoaCe_rkNuPJB1Q7PgSaYm17jzafmYFPSc"
    path = (service spawn)
    access/id = access:send.service.spawn
    created = "2015-05-28T13:49:37Z"
    metadata = #f

!!
  )

;; This thunk will be executed on the Robot Server.
;; It will register for notifications coming from both the Market Data Server and the Risk Server
;; For each stock symbol, it keeps track of the current price and risk values.
(define THUNK/REGISTER-ROBOT/NEW
  (island/compile
   '(lambda (motile/register/market motile/register/risk trader/notif/curl)
      (lambda ()
        (islet/log/info "Executing trader's computation (market and risk registration) on the Robot Server")
        
        (let* ([robot/notif/u (islet/curl/new '(robot notif) GATE/ALWAYS #f 'INTER)] ; We create a CURL on the Robot Server to receive notifications from both the MD Server and Risk Server. 
               [market/curl (robot/get-curl/market-server)]  ; Get the Market Data Server spawn CURL.
               [risk/curl (robot/get-curl/risk-server)]  ; Get the Risk Server spawn CURL.
               [order/curl (robot/get-curl/order-router)] ; Get the Order Router request CURL
               [market-thunk (motile/call motile/register/market environ/null (duplet/resolver robot/notif/u))]
               [risk-thunk (motile/call motile/register/risk environ/null (duplet/resolver robot/notif/u))]
               [stock/values (make-hash)] ; maps stock symbols to (price,risk) pairs, price is in cents
               [first-yhoo-sell (box #f)] ; first sell when yahoo stock first reaches 27 or below
               [second-yhoo-sell (box #f)]) ; second sell when yhoo stock firsst reaches 23 or below 
          
          (define (report-callback report) ;callback function  to handle order execution reports on this order
            (let ([c trader/notif/curl])
              (islet/log/info "Report received ~a: " report)
              (when (not (send c report)) 
                (islet/log/info "Could not notify trader of report."))))
          
          (send market/curl market-thunk) ; Send the registration thunk to the Market Data Server.
          (send risk/curl risk-thunk) ; Send the registration thunk to the Risk Server.
          
          ; We now listen for notifications coming from the Market Data Server and the Risk Server through robot/notif/u.
          (let loop ([m (duplet/block robot/notif/u)]) ; Wait for an incoming message.
            (let ([payload (murmur/payload m)]) ; Extract the message's payload.
              (islet/log/info payload) ; Print it into the console.
              (cond 
                ; handle market data event
                [(equal? (vector-ref payload 0) 'struct:market-event)
                 (let* ([m-event (vector->market-event payload)]
                        [stock-symbol (market-event/symbol m-event)]
                        [stock-price (market-event/price m-event)]
                        [quantity (market-event/quantity m-event)])   
                   (cond 
                     [(hash-has-key? stock/values stock-symbol) ; is there already a key for this symbol?
                      ; get the (price,risk,prev-price,prev-risk) vector, change the price, update hash
                      (let ([v (hash-ref stock/values stock-symbol)])
                        (vector-set! v 2 (vector-ref v 0)) ; remember last price  @ index 2
                        (vector-set! v 0 stock-price))] ; set new price @ index 0
                     [else 
                      (hash-set! stock/values stock-symbol (vector stock-price -1 -1 -1))])) ; -1 means no value seen
                 ;(islet/log/info stock/values) ; DEBUG  show hash
                 ]
                ; handle risk event
                [(equal? (vector-ref payload 0) 'struct:risk-event)
                 (let* ([r-event (vector->risk-event payload)]
                        [stock-symbol (risk-event/symbol r-event)]
                        [stock-risk (risk-event/risk r-event)])
                   (cond 
                     [(hash-has-key? stock/values stock-symbol) ; is there already a key for this symbol?
                      ; get the (price,risk,prev-price,prev-risk) vector, change the risk, update hash
                      (let ([v (hash-ref stock/values stock-symbol)])
                        (vector-set! v 3 (vector-ref v 1)) ; remember last risk value @ index 3
                        (vector-set! v 1 stock-risk))] ; set new risk value @ index 1
                     [else 
                      (hash-set! stock/values stock-symbol (vector -1 stock-risk -1 -1))])) ; -1 means no value seen
                 ;(islet/log/info stock/values) ; DEBUG  show hash
                 ]
                [else
                 (islet/log/info "UNKNOWN EVENT")])
              
              
              ;************** CHANGE THIS ***************************
              ; For now we are going to echo back each market notification as a request to the Order Router 
              ; just to generate some traffic....
              (when (equal? (vector-ref payload 0) 'struct:market-event)
                (let* ([p (subislet/callback/new (uuid/symbol) EXAMPLES/ENVIRON report-callback)] ; create a new islet to listen for order reports on this order request                             
                       [order-exec-curl (cdr p)]; curl to communicate order-exec-reports
                       [symbol (vector-ref payload 1)]
                       [price (string->number(vector-ref payload 3))]
                       [quantity (box (string->number(vector-ref payload 4)))]
                       [send-order (box #t)])
                  ; only echo FB and GOOG orders
                  (when (equal? symbol "YHOO") 
                    ;(islet/log/info "FOUND YAHOO MARKET EVENT.")
                    (cond 
                      [(and (<= price 2700) (not (unbox first-yhoo-sell))) 
                        (set-box! quantity 500) ; fixed amount representing first half of shares 
                        (set-box! first-yhoo-sell #t)] ; make sure we only do this once
                        ;(islet/log/info "TRIGGERING FIRST YAHOO SALE.")] 
                      [(and (<= price 2300) (not (unbox second-yhoo-sell))) ; YAHOO
                        (set-box! quantity 500) ; fixed amount representing second half of shares 
                        (set-box! second-yhoo-sell #t)] ; make sure we only do this once
                        ;(islet/log/info "TRIGGERING SECOND YAHOO SALE.")]
                        ; ADD CODE HERE TO MAKE GOOG AND FB PURCHASE USING ALL $ IN COMBINED YAHOO SALES
                        ; DISTRIBUTED PROPORTIONATELY TO RISK.
                      [else ; ignore all other yahoo events
                       (set-box! send-order #f)]))
                       ;(islet/log/info "IGNORING YAHOO MARKET EVENT.")]))
                  (when (unbox send-order)
                    (let ([new-order-request (order-request "trader" "broker" symbol price (unbox quantity) 0)])
                      (islet/log/info "Sending order: ~a" new-order-request)
                      ; send order to order router, adding curl to communicate order exec reports back
                      (when (not (send order/curl (vector-append (struct->vector new-order-request) (vector order-exec-curl))))
                        (islet/log/info "Order request could not be sent."))
                      ; notify trader of new order request
                      (when (not (send trader/notif/curl (struct->vector new-order-request)))
                        (islet/log/info "Order request notification could not be sent to trader."))))
              )))
            
            (loop (duplet/block robot/notif/u))))))))


;; Generate the spawn definition that trader sends to market notifications service.
(define THUNK/REGISTER-MARKET/NEW
  (island/compile
   ; client/notif/u - The Traders's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          (islet/log/info "Registered for market events")
          
          (let loop ([m (duplet/block d)])
            (let ([payload (murmur/payload m)])
              (send client/notif/u payload)
              (loop (duplet/block d))))))))) ; Wait again for an echo request.


;; Generate the spawn definition that trader sends to risk notifications service.
(define THUNK/REGISTER-RISK/NEW
  (island/compile
   ; client/notif/u - The Traders's Notification Service's CURL.
   ; Returns a thunk
   '(lambda (client/notif/u)
      ; This thunk will be executing as a spawn on a remote island.
      (lambda ()
        ; Creates a new CURL when it is evaluated (it cannot be passed because it has to be created on the server-side.
        (let ([d (islet/curl/new '(comp notif) GATE/ALWAYS #f 'INTRA)])
          (register (list "GOOG" "YHOO" "FB" "IBM") (duplet/resolver d))
          (islet/log/info "Registered for risk events")
          
          (let loop ([m (duplet/block d)])
            (let ([payload (murmur/payload m)])
              (send client/notif/u payload)
              (loop (duplet/block d))))))))) ; Wait again for an echo request.

;; Code for a trader island.
;; server/u - CURL for spawn service on Robot Server.
(define (trader/boot server/u)
  (islet/log/info "Trader is booting...")
  (let* ([pr (subislet/callback/new 'trader-notif EXAMPLES/ENVIRON ; create a new islet to listen notifications of order requests made on traders behalf
                                    (island/compile '(lambda (payload) ;callback function  to handle order notifications to the trader
                                                       (islet/log/info "Trader Notification received: ~a" payload)
                                                       )))]
         [trader/notif/curl (cdr pr)]
         [thunk (motile/call THUNK/REGISTER-ROBOT/NEW environ/null THUNK/REGISTER-MARKET/NEW THUNK/REGISTER-RISK/NEW trader/notif/curl)])
    (islet/log/info "Sending registrations thunk to Robot Server...")
    (send server/u thunk)))

; Construct an in-memory CURL instance of the predefined CURL for robot-server.
(define robot-server/curl/spawn (curl/zpl/safe-to-curl ROBOT-SERVER/CURL/SPAWN KEYSTORE))

(define trader (example/island/new 'trader  "trader_secret"  (lambda () (trader/boot robot-server/curl/spawn))))

(island/log/level/set 'warning)

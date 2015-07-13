#lang racket/base

(require  "../../getters.rkt")

(provide order-request
         order-request/sender
         order-request/target
         order-request/symbol
         order-request/unit-price
         order-request/quantity
         order-request/uid
         order-request/curl
         ; add a curl here?  no, create new struct that has an order request object and a curl
         )

(provide trader-request
         trader-request/order
         trader-request/curl
         )

(provide order-exec-report
         order-exec-report/uid
         order-exec-report/symbol
         order-exec-report/status  
         order-exec-report/unit-price 
         order-exec-report/qnty-requested
         order-exec-report/qnty-filled        
         )

(provide vector->order-request
         vector->order-exec-report)


(struct order-request
  (sender     ; The buyer making request
   target     ; The broker
   symbol     ; Stock symbol
   unit-price ; The price per share
   quantity   ; The amount of shares to be traded.
   uid        ; Unique id given to order
   curl)      ; Curl to used to transmit order-exect-reports back to the trader
  #:transparent)

(struct trader-request
  (order ; order-request struct
   curl) ; curl to used to transmit order-exect-reports back to the trader
  #:transparent)
   
  
(struct order-exec-report
  (uid            ; Unique id given to order
   symbol         ; Stock symbol
   status         ; Pending, Accepted, Rejected, Partial-Filled, or Filled
   unit-price     ; The price per share
   qnty-requested ; The total amount of shares to be traded
   qnty-filled)   ; The amount of shares that are traded so far
  #:transparent)

(struct/getters/define order-request sender target symbol unit-price quantity uid curl)
(struct/getters/define trader-request order curl)
(struct/getters/define order-exec-report uid symbol status unit-price qnty-requested qnty-filled)


(define (vector->order-request v)
  (cond 
    [(equal? (vector-ref v 0) 'struct:order-request)
     (let ([sender (vector-ref v 1)]
           [target (vector-ref v 2)]
           [symbol (vector-ref v 3)]
           [unit-price (vector-ref v 4)]
           [quantity (vector-ref v 5)]
           [uid (vector-ref v 6)]
           [curl (vector-ref v 7)])
       (order-request sender target symbol unit-price quantity uid curl))]
    [else
     (display "VECTOR IS NOT ORDER REQUEST")(display "\n")]))

(define (vector->order-exec-report v)
    (cond 
    [(equal? (vector-ref v 0) 'struct:order-exec-report)
     (let ([uid (vector-ref v 1)]
           [symbol (vector-ref v 2)]
           [status (vector-ref v 3)]
           [unit-price (vector-ref v 4)]
           [qnty-requested (vector-ref v 5)]
           [qnty-filled (vector-ref v 6)])
       (order-exec-report uid symbol status unit-price qnty-requested qnty-filled))]
    [else
     (display "VECTOR IS NOT ORDER EXECUTION REPORT")(display "\n")]))








#lang racket/base

(require  "../../getters.rkt")

(provide order-request
         order-request/sender
         order-request/target
         order-request/symbol
         order-request/unit-price
         order-request/quantity
         order-request/uid
         )

(provide order-exec-report
         order-exec-report/uid
         order-exec-report/symbol
         order-exec-report/status  
         order-exec-report/qnty-requested
         order-exec-report/qnty-filled        
         )

(struct order-request
  (sender     ; The buyer making request
   target     ; The broker
   symbol     ; Stock symbol
   unit-price ; The price per share
   quantity   ; The amount of shares to be traded.
   uid)       ; Unique id given to order
  #:transparent)

  
(struct order-exec-report
  (uid            ; Unique id given to order
   symbol         ; Stock symbol
   status         ; Pending, Accepted, Rejected, Partial-Filled, or Filled
   unit-price     ; The price per share
   qnty-requested ; The total amount of shares to be traded
   qnty-filled)   ; The amount of shares that are traded so far
  #:transparent)


(struct/getters/define order-request sender target symbol unit-price quantity uid)
(struct/getters/define order-exec-report uid symbol status unit-price qnty-requested qnty-filled)






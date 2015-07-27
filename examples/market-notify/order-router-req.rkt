#lang racket/base

(require  
  racket/contract/base
  "../../getters.rkt")

(provide order-request
         order-request/sender
         order-request/action
         order-request/symbol
         order-request/unit-price
         order-request/quantity
         order-request/uid
         ; add a curl here?  just tack the curl at the end of payload vector and reclaim on the other end
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
         vector->order-exec-report
         order-request/pretty
         order-exec-report/pretty)


(struct order-request
  (sender     ; The buyer making request
   action     ; "BUY" or "SELL"
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

(struct/getters/define order-request sender action symbol unit-price quantity uid)
(struct/getters/define order-exec-report uid symbol status unit-price qnty-requested qnty-filled)

(define (order-request/pretty order)
  (format "[~a order] Symbol: ~a Qty: ~a Price: ~a UUID: ~a" (order-request/action order) (order-request/symbol order) (order-request/quantity order) (order-request/unit-price order) (order-request/uid order)))

(define (order-exec-report/pretty report)
  (let ([report (if (vector? report) (vector->order-exec-report report) report)])
    (format "[~a Report] Symbol: ~a Status: ~a Price: ~a Requested: ~a Filled: ~a" (order-exec-report/uid report) (order-exec-report/symbol report) (order-exec-report/status report) (order-exec-report/unit-price report) (order-exec-report/qnty-requested report) (order-exec-report/qnty-filled report)))
  )

(define (vector->order-request v)
  (cond 
    [(equal? (vector-ref v 0) 'struct:order-request)
     (let ([sender (vector-ref v 1)]
           [action (vector-ref v 2)]
           [symbol (vector-ref v 3)]
           [unit-price (vector-ref v 4)]
           [quantity (vector-ref v 5)]
           [uid (vector-ref v 6)])
       (order-request sender action symbol unit-price quantity uid))]
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








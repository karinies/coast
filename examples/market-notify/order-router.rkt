#lang racket/base
(require 2htdp/batch-io)
(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "../../promise.rkt"
  "../../transport/gate.rkt"
  "./order-router-req.rkt"
  "../examples-base.rkt")

(provide order-router)

(define (service/order-request) ; A Service to listen for and handle order requests.
  
  ; order request handler, processes a request using the given response template
  (define (request/handler c request template) ; runs in own thread
    (islet/log/info "Handling request: ~a Using template: ~a" request template)
    ;(islet/log/info request)
    ;      (islet/log/info "Using template: ")
    ;    (islet/log/info template)
    ; send pending execution report
    (send/pending c request)
    ; accept or reject request
    (cond 
      [(equal? (vector-ref template 1) "Rejected") 
       (send/rejected c request)]
      [else 
       (send/accepted c request)
       ; fill accepted request
       (if (equal? (vector-ref template 2) "Filled")
           (send/fill c request (order-request/quantity request))  ; fill all at once
           (send/partial-fills c request template))])) ; process with multiple partial fills according to template
  
  ; general function for sending all order execution reports
  (define (send/report c request status fill-amt)
    (let* ([uid (order-request/uid request)]
           [symbol (order-request/symbol request)]
           [price (order-request/unit-price request)]
           [qnty (order-request/quantity request)]
           [report (order-exec-report uid symbol status price qnty fill-amt)])
      (islet/log/info "Sending ~a report: ~a" status report)
      ;(islet/log/info "Sending ")(islet/log/info status)(islet/log/info " report: ")(islet/log/info report)
      (when (not (send c (struct->vector report)))
        (islet/log/info "Order execution report could not be sent."))))   
  
  (define (send/pending c request)
    (send/report c request "pending" (order-request/quantity request)))
  
  (define (send/rejected c request)
    (send/report c request "rejected" (order-request/quantity request)))
  
  (define (send/accepted c request)
    (send/report c request "accepted" (order-request/quantity request)))
  
  (define (send/fill c request amt)
    (send/report c request "filled" amt))
  
  (define (send/partial-fill c request amt)
    (send/report c request "parital-fill" amt))
  
  ; fill an order request using partial fills
  (define (send/partial-fills c request template)
    ; idx is an index into template file, it picks up each percentation number
    ; to-fill is the amount of shares left to fill
    (let loop ([idx 3][to-fill (order-request/quantity request)])
      (let* ([percentage (string->number(vector-ref template idx))]
             [fill-amt (truncate (* (order-request/quantity request) (/ percentage 100)))]
             [remainder (- to-fill fill-amt)])
        (send/partial-fill c request fill-amt)
        (if (equal? (vector-ref template (+ idx 1)) "Partial")
            ; keep looping as long as the template has more partial fills
            (loop (+ idx 2) remainder)
            ; otherwise complete the fill with the remaining shares to be filled
            (send/fill c request remainder)))))
  
  
  (islet/log/info "Running order router's request service.")
  (let* ([d (islet/curl/known/new '(service request) 'access:send.service.request GATE/ALWAYS environ/null)] ; Create a CURL to listen for order requests.
         ; each line of this file is a template of how to respond to the next order request
         [templates-file "events/response-templates.txt"] 
         [templates (list->vector (read-words/line templates-file))] ; vector containing all response templates
         ; index into current template, 0 is index into default template (first line of templates file)
         [start-idx (if (> (vector-length templates) 1) 1 0)])
    ; in the case where template file only contains default template, set to 0 where it will remain, 
    ; otherwise it starts at 1
    (let loop ([m (duplet/block d)][idx start-idx]) ; Wait for an order request.
      (let* ([payload (murmur/payload m)] ; Extract the murmur's payload.
             [order-req (vector->order-request payload)] ; convert payload from vector back to order-request struct
             [notify/curl (vector-ref payload (- (vector-length payload) 1))]
             [templ (list->vector(vector-ref templates idx))]) 
        ; idx takes the values of 1 to length of templates vector - 1, 
        ; then remains 0 so that the default template will be applied to all subsequent requests
        (thread (lambda () (request/handler notify/curl order-req templ)))) ; run order request handler in separate thread
      (loop (duplet/block d) 
            (if (or (equal? idx 0)(equal? idx (- (vector-length templates) 1))) 0 (+ idx 1))))))


(define (order-router/boot)
  ;; Code for order-router request service.
  (define (order-router/listener) ; This function creates an islet that will receive and process order requests.
    (let* ([x (islet/new (this/island) 'order-router.request TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/order-request))))) ; Executes service/order-request in the new islet. 
  (islet/log/info "Running Order Router's boot function")
  (thread (lambda () (order-router/listener)))) 


(define order-router (example/island/new 'order-router "order_router_secret" order-router/boot))

(island/log/level/set 'warning)

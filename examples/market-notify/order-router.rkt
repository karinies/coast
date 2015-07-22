#lang racket/base
(require 2htdp/batch-io)
(require
  "../../include/base.rkt"
  "../../baseline.rkt"
  "./order-router-req.rkt"
  [only-in "../../curl/base.rkt" curl/origin curl/path curl/metadata curl/access]
  "../../promise.rkt"
  "../../remote.rkt"
  "../../spawn.rkt"
  "../../transport/access.rkt"
  "../../transport/gate.rkt"
  "../../transport/gates/challenge.rkt"
  "../../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ORDER-ROUTER/SECRET/PATH (string-append CERTIFICATE/SECRET "order_router_secret"))

(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ORDER-ROUTER/CURVE/SECRET (path-to-curve ORDER-ROUTER/SECRET/PATH))

;; Returns <islet>@<island> where <islet> is the given name
;; and <island> is the island nickname.
;; For example (islent/name "service.foo") returns the symbol service.foo@order-router
;; when called on the island order-router.
(define (islet/name islet . rest)
  (string->symbol
   (format "~a@~a" islet (if (null? rest) (this/island/nickname) (car rest)))))


(define (service/order-request) ; A Service to listen for and handle order requests.
  
  ; order request handler, processes a request using the given response template
  (define (request/handler c request template) ; runs in own thread
    (display "Handling request: ")
    (displayln request)
      (display "Using template: ")
    (displayln template)
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
      (display "Sending ")(display status)(display " report: ")(display report)(display"\n")
      (when (not (send c (struct->vector report)))
        (display "Order execution report could not be sent."))))   
  
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
    
  
  (display "Running order router's request service.\n")
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
    (let* ([server/name (islet/name "order-router.request")] ; order-router.request@market-server
           [x (islet/new (this/island) server/name TRUST/MODERATE environ/null environ/null)]) ; Creates a new islet.
      (islet/jumpstart
       x
       (lambda () (service/order-request))))) ; Executes service/order-request in the new islet. 
  (display "Running server's boot function\n")
  (thread (lambda () (order-router/listener)))) 


(define order-router (island/new 'order-router ORDER-ROUTER/CURVE/SECRET order-router/boot))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set order-router KEYSTORE)
(island/log/level/set 'debug)

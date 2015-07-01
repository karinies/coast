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
  #|(define (request/handler request template) ; runs in own thread and processes request using given response template
    ; Handle request here!
    (display "Handling request: ")
    (displayln request)
      (display "Using template: ")
    (displayln template))|#
  
  (display "Running order router's request service.\n")
  (let* ([d (islet/curl/known/new '(service request) 'access:send.service.request GATE/ALWAYS environ/null)] ; Create a CURL to listen for order requests.
        [templates-file "events/response-templates.txt"] ; each line of this file is a template of how to respond to the next request
        [templates (list->vector (read-words/line templates-file))] ; vector containing all response templates
        [idx (if (> (vector-length templates) 1) 1 0)]) ; index into current template, 0 is index into default template (first line of templates file)
              ; in the case where template file only contains default template, set to 0 where it will remain, otherwise it starts at 1
    (let loop ([m (duplet/block d)]) ; Wait for an order request.
      (let* ([payload (murmur/payload m)] ; Extract the murmur's payload.
             [order-req (vector->order-request payload)] ; convert payload from vector back to order-request struct
             [templ (vector-ref templates idx)] ; idx takes the values of 1 to length of templates vector-1, 
                                                ; then remains 0 so that the default template will be applied to all subsequent requests
             [idx (if (or (equal? idx 0)(equal? idx (- (vector-length templates) 1))) 0 (+ idx 1))]) ; set idx for next iteration
        (display "Received request: ")
        (displayln order-req)
        (display "Loaded template: ")
        (displayln templ)))))
        ;(thread (lambda () (request/handler order-req templ))))))) ; run handler in separate thread
               


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
;(island/log/level/set 'warning)
(island/log/level/set 'debug)

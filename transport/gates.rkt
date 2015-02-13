#lang racket/base

;; Contains a collection of gates for purposes of demonstration.

(require
 racket/contract/base
 [only-in "access.rkt" access/transport]
 [only-in "gate.rkt" gate?]
 [only-in "transport.rkt" transport/backlog]
 [only-in "../this.rkt" this/trust]
 [only-in "../persistent/environ.rkt" environ/cons environ/null])

(provide
 (contract-out
  [gate/backlog/new (-> exact-positive-integer? gate?)]
  [gate/trust/new (-> (integer-in TRUST/LOWEST TRUST/HIGHEST) gate?)]
  [gate/rate/new (-> exact-positive-integer? exact-positive-integer? gate?)]
  
  ))

(define (CLOSED/NEVER _access) #f)

;; Permits delivery of a message via an access:send only if the backlog in the transport
;; is less than the value of n.
;; In other words it constrains the high water mark of the transport relative to the
;; access:send point to which the gate is attached.
(define (gate/backlog/new n)
  (let ([pass? (lambda (access _message) (< (transport/backlog (access/transport access)) n))]
        [closed? (lambda (_access) #f)]
        [snap (lambda (access)
                (vector-immutable 'backlog (cons (transport/backlog (access/transport access)) n)))])
    (environ/cons
     environ/null
     'pass? pass?
     'closed? closed?
     'snap snap)))

;; Taken from Omer Ben-Shalom et. al. "Granular Trust Model Improves Enterprise Security," IT@Intel White Paper, November 2012.
;; Trust ratings.
(define TRUST/LOWEST   1)
(define TRUST/LOW      2)
(define TRUST/MODERATE 3)
(define TRUST/HIGH     4)
(define TRUST/HIGHEST  5)
;; Sensitivity ratings.
(define SENSITIVE/LOWEST   1) ; Very low consequence
(define SENSITIVE/LOW      2) ; Low consequence
(define SENSITIVE/MODERATE 3) ; Moderate consequence
(define SENSITIVE/HIGH     4) ; High consequence
(define SENSITIVE/HIGHEST  5) ; Very high consequence

(define (gate/trust/new t)
  (define (pass? access _message) (>= (this/trust) t))
  (define closed? CLOSED/NEVER)
  (define (snap _access) (vector-immutable 'gate/trust 'trust t))
  (environ/cons environ/null 'pass? pass? 'closed closed? 'snap snap))

(define-syntax-rule (box/set b v) (set-box! b v))
(define-syntax-rule (box/cas b before after) (box-cas! b before after))

;; Returns a rate limiter gate.
;; rate - number of messages per time span
;; milliseconds - time span in milliseconds.
;; For example (gate/rate/new 5 1000) creates a 5 Hz rate limiter gate.

;rate = 5.0; // unit: messages
;per  = 8.0; // unit: seconds
;allowance = rate; // unit: messages
;last_check = now(); // floating-point, e.g. usec accuracy. Unit: seconds
;
;when (message_received):
;  current = now();
;  time_passed = current - last_check;
;  last_check = current;
;  allowance += time_passed * (rate / per);
;  if (allowance > rate):
;    allowance = rate; // throttle
;  if (allowance < 1.0):
;    discard_message();
;  else:
;    forward_message();
;    allowance -= 1.0;

;; rate - positive integer
;; per - positive integer (milliseconds)
(define (gate/rate/new rate per)
  (let ([flow (/ rate per)]
        [allowance (box rate)] 
        [then (box (current-inexact-milliseconds))]
        [passed (box 0)]
        [denied (box 0)])
    (define (pass? _access _message)
        (>= (min rate (+ (unbox allowance) (* (- (current-inexact-milliseconds) (unbox then)) flow)))
            1))
    (define closed? CLOSED/NEVER)
    (define (act _access)
      (let* ([now (current-inexact-milliseconds)]
             [a (min rate (+ (unbox allowance) (* (- now (unbox then)) flow)))])
        (box/set then now)
        (cond
          [(>= a 1)
           (box/set allowance (sub1 (unbox allowance)))
           (box/set passed (add1 (unbox passed)))]
          [else
           (box/set allowance a)
           (box/set denied (add1 (box denied)))])))
    (define (snap _access)
      (vector-immutable
       'gate/rate
       'rate rate 'per per 'allowance (unbox allowance) 'passed (unbox passed) 'denied (unbox denied)))
    (define (reset _access) (box/set allowance rate))
    (environ/cons environ/null 'pass? pass? 'act act 'closed? closed? 'snap snap 'reset reset)))


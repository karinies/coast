#lang racket/base

(require
 racket/contract/base
 "../curve.rkt"
 "../getters.rkt"
 "../persistent/set.rkt"
 "../time.rkt")

(provide
 GATE/ALWAYS
 GATE/NEVER
 (contract-out
  [struct
   gate
   ([nickname symbol?]
    [_pass procedure?]
    [_act (or/c procedure? #f)]
    [_closed procedure?]
    [_snap  (or/c procedure? #f)]
    [_reset (or/c procedure? #f)])]
  [gate/pass?   (-> gate? any/c boolean?)]
  [gate/act     (-> gate? any/c any/c)]
  [gate/closed? (-> gate? boolean?)]
  [gate/snap    (-> gate? any/c)]
  [gate/reset   (-> gate? any/c)]
  [gate/pure? (-> gate? boolean?)]
  [gate/uses (-> exact-positive-integer? gate?)]
  [gate/once (-> gate?)]
  [gate/or  (->* () () #:rest (listof gate?) gate:or?)]
  [gate/and (->* () () #:rest (listof gate?) gate:and?)]
  [gate/xor (->* () () #:rest (listof gate?) gate:xor?)]
  [gate/not (-> gate/pure? gate?)]
  [gate/before (-> gate? date? gate:before?)]
  [gate/after  (-> gate? date? gate:after?)]
  [gate/during (-> gate? date? date? gate:during?)]
  [gate/lifespan (-> duration? gate:lifespan?)]
  [gate/rate (-> exact-positive-integer? exact-positive-integer? gate:rate?)]
  ;[gate/whitelist [-> (listof kp/base64/c) gate?]]
  )
 
 gate/nickname
 gate/_pass
 gate/_act
 gate/_closed
 gate/_snap
 gate/_reset
 
 gate:uses?
 gate:or?
 gate:and?
 gate:xor?
 gate:before?
 gate:after?
 gate:during?
 gate:always?
 gate:never?
 gate:rate?
 gate:lifespan?)

#|
Gates modulate the exploitation of transport access points.
Each access point may contain zero or more gates.
Before a message can be transmitted (received) via an access point each gate of the access
point must agree to the transmission (reception) as follows:
  * the access point evaluates the predicate of each gate
  * If each gate predicate returns #t then:
      - the action of each gate is executed
      - transmission (reception) via the transport then proceeds
  * If any access point returns #f then no transmission or reception is attempted
The "action" is a gate is typically a simple imperative procedure that modifies the
state of the gate in some manner.
A gate is said to be "closed" if the gate predicate will always return #f in the future,
in other words, the gate will NEVER, at any point in the future, permit the access point
to be used for transmission (reception).
The closed? function of a gate returns #t iff the gate predicate will always return #f
from that point forward and #f otherwise (that is, the gate predicate may return #t at
some point in the future).

For example, a "uses" gate enforces a limit on the number of time that an access point
may be used for transmission (reception). The initial limit (say 7) is set when the
gate is created. The gate predicate (p gate? access? <message>) returns #t iff the limit
is positive? and #f otherwise. The action procedure (a gate? access?) decrements the use count
maintained by the gate. The closed? predicate returns #t is the use count is zero? and #f otherwise.

All gates have a pass? and closed? predicates but not all gates require an imperative action.
|#

(struct gate (nickname _pass _act _closed _snap _reset))
(struct/getters/define gate nickname _pass _act _closed _snap _reset)
;; Invoke the pass? method of gate g.
(define (gate/pass? g m)
  ((gate/_pass g) m))
;; Invoike the act method of gate g.
(define (gate/act g m)
  (let ([f (gate/_act g)])
    (when f (f m))))
;; Invoke the closed method of gate g.
(define (gate/closed? g)
  (let ([f (gate/_closed g)])
    (f)))
;; Invoke the snap method of gate g.
(define (gate/snap g)
  (let ([f (gate/_snap g)])
    (when f (f))))
;; Invoke the reset method of gate g.
(define (gate/reset g)
  (let ([f (gate/_reset g)])
    (when f (f))))
;; A get is a pure predicate only if it has no action.
(define (gate/pure? g) (not (gate/_act g)))


(struct gate:uses gate ())
;; One simple example is a uses gate which counts the number of times an
;; transport access point is exploited.
;; Generate a gate that permits up to n > 0 uses.
(define (gate/uses n)
  (let ([b (box n)])
    (gate:uses
     'gate:uses
     (lambda (_m) (positive? (unbox b))) ; pass
     (lambda (_m) (set-box! b (sub1 (unbox b)))) ; act
     (lambda () (zero? (unbox b))) ; closed
     (lambda () (vector-immutable 'gate:uses n (unbox b))) ; snap
     (lambda () (set-box! b n))))) ; reset

;; A gate that permits exactly one use.
(define (gate/once) (gate/uses 1))

(struct gate:or gate ())
;; An or combinator for gates g_1 ... g_n.
;; Returns a gate that is a short-circuit (or g_1 ... g_n).
(define gate/or
  (lambda gates
    (let ([b (box #f)]
          [n (length gates)])
      (gate:or
       'gate:or
       ; Return #t if the pass? method of some g_i is #t and #f otherwise.
       (lambda (message)
         (set-box! b #f)
         ; Set b to the first gate g whose pass? method returns #t.
         (set-box! b (for/or ([g (in-list gates)]) (and (gate/pass? g message) g)))
         (and (unbox b) #t))
       
       ; Execute the act method of gate g_i whose pass? method returned #t.
       (lambda (message)
         (when (unbox b) (gate/act (unbox b) message))
         (set-box! b #f))
       
       ; Combinator is closed? if any gate g_i is closed?
       (lambda () (for/or ([g (in-list gates)]) (gate/closed? g)))
       
       ; The state snapshot is a vector of the state snapshots of gates g_1 ... g_n.
       (lambda ()
         (let ([v (vector (add1 n) #f)])
           (vector-set! v 0 'gate:or) 
           (for ([g (in-list gates)] [i (in-range 1 (add1 n))])
             (vector-set! v i (gate/snap g)))
           (vector->immutable-vector v)))
       
       ; Reset the combinator by resetting the state of each constituent gate g_i.
       (lambda () (for ([g (in-list gates)]) (gate/reset g)))))))

(struct gate:and gate ())
;; An and combinator for gates g_1 ... g_n.
;; Returns a gate that is a short-circuit (and g_1 ... g_n).
(define gate/and
  (lambda gates
    (let ([n (length gates)])
      (gate:and
       'gate:and
       ; Return #t if the pass? method of each g_i returns #t.
       (lambda (message) (for/and ([g (in-list gates)]) (gate/pass? g message)))
       ; Execute the act method of each gate g_i
       (lambda (message) (for ([g (in-list gates)]) (gate/act g message)))
       ; Combinator is closed? if all gates g_i are closed?
       (lambda () (for/and ([g (in-list gates)]) (gate/closed? g)))
       ; The state snapshot is a vector of the state snapshots of gates g_1 ... g_n.
       (lambda ()
         (let ([v (vector (add1 n) #f)])
           (vector-set! v 0 'gate:and) 
           (for ([g (in-list gates)] [i (in-range 1 (add1 n))])
             (vector-set! v i (gate/snap g)))
           (vector->immutable-vector v)))
       ; Reset the combinator by resetting the state of each constituent gate g_i.
       (lambda () (for ([g (in-list gates)]) (gate/reset g)))))))

(struct gate:not gate ())
;; Pass only if gate g does not pass.
;; Gate g must be a pure predicate gate (that is, a gate without an action)
;; otherwise the constructor returns #f rather than a new gate.
(define (gate/not g)
  (gate:not
   'gate'not
   (lambda (message) (not (gate/pass? g message)))
   #f ; act
   (lambda () (not (gate/closed? g)))
   (vector-immutable 'gate:not (gate/_snap g))
   (gate/_reset g)))

(struct gate:xor gate ())
(define gate/xor
  (lambda gates
    (let ([b (box #f)] [n (box 0)])
      (gate:xor
       'gate:xor
       ; To pass exactly one gate pass? method must evaluate to #t.
       (lambda (m)
         (set-box! b #f)
         (set-box! n 0)
         (for ([g (in-list gates)] #:break (> (unbox n) 1))
           (when (gate/pass? g m)
             (set-box! b g)
             (set-box! n (add1 (unbox n)))))
         (= (unbox n) 1))
       ; Evaluate the action of the one gate that passed.
       (lambda (m) (gate/act (unbox b) m))
       ; The gate is closed if every gate g_i is closed.
       (lambda () (for/and ([g (in-list gates)]) (gate/closed? g)))
       ; The state snapshot is a vector of the state snapshots of gates g_1 ... g_n.
       (lambda ()
         (let* ([n (length gates)]
                [v (vector (add1 n) #f)])
           (vector-set! v 0 'gate:xor) 
           (for ([g (in-list gates)] [i (in-range 1 (add1 n))])
             (vector-set! v i (gate/snap g)))
           (vector->immutable-vector v)))
       ; Reset each gate g_i.
       (lambda () (for ([g (in-list gates)]) (gate/reset g)))))))
             
(struct gate:before gate ())
(define (gate/before g d)
  (let ([before (date-to-time d)])
    (gate:before
     'gate:before
     (lambda (message)
      (and (time<? (time/now) before) (gate/pass? g message)))
     (gate/_act g)
     (lambda () (or (time>=? (time/now) before) (gate/closed? g)))
     (lambda () (vector-immutable 'gate:before (date-to-ISO8601 d) (gate/snap g)))
     (gate/_reset g))))

(struct gate:after gate ())
(define (gate/after g d)
  (let ([after (date-to-time d)])
    (gate:after
     'gate:after
     (lambda (message)
       (and (time>? (time/now) after) (gate/pass? g message)))
     (gate/_act g)
     (lambda () (and (time>? (time/now) after) (gate/closed? g)))
     (lambda () (vector-immutable 'gate:after (date-to-ISO8601 d) (gate/snap g)))
     (gate/_reset g))))

(struct gate:during gate ())
(define (gate/during g from to)
  (let ([t/from (date-to-time from)]
        [t/to   (date-to-time to)])
    (gate:during
     'gate:during
     (lambda (message) (and (<= t/from (time/now) t/to) (gate/pass? g message)))
     (gate/_act g)
     (lambda () (or (time>? (time/now) t/to) (gate/closed? g)))
     (lambda ()
       (vector-immutable 'gate:during (cons (date-to-ISO8601 from) (date-to-ISO8601 to)) (gate/snap g)))
     (gate/_reset g))))


(struct gate:lifespan gate ())
(define (gate/lifespan d)
  (let* ([t/from (time/now)]
         [t/to   (time+ t/from d)])
    (gate:lifespan
     'gate:lifespan
     (lambda (_) (<= t/from (time/now) t/to))
     #f
     (lambda () (time>? (time/now) t/to))
     (lambda () (vector-immutable 'gate:lifespan t/from t/to))
     #f)))
  

;; Note: build gate/during as:
;;(gate/and (gate/not (gate/before GATE/NEVER)) (gate/not (gate/after GATE/NEVER)))

(struct gate:always gate ())
;; The trivial gate that always passes and is never closed.
(define GATE/ALWAYS
  (gate:always
   'gate:always
   (lambda (_message) #t)
   #f
   (lambda () #f)
   #f
   #f))

(struct gate:never gate ())
;; The trivial gate that never passes and is always closed.
(define GATE/NEVER
  (gate:never
   'gate:never
   (lambda (_message) #f)
   #f
   (lambda () #t)
   #f
   #f))

(struct gate:rate gate ())
;; Returns a rate limiter gate.
;;   rate - number of messages per time span
;;   per - time span in milliseconds.
;; For example (gate/rate/new 5 1000) creates a 5 Hz rate limiter gate.
(define (gate/rate rate per)
  (let ([flow (/ rate per)]
        [allowance (box rate)] 
        [then (box (current-inexact-milliseconds))]
        [passed (box 0)]
        [denied (box 0)])
    (gate:rate
     'gate:rate
     (lambda (_message)
       (>= (min rate (+ (unbox allowance) (* (- (current-inexact-milliseconds) (unbox then)) flow)))
           1))

     (lambda (_message)
       (let* ([now (current-inexact-milliseconds)]
              [a (min rate (+ (unbox allowance) (* (- now (unbox then)) flow)))])
         (set-box! then now)
         (cond
           [(>= a 1)
            (set-box! allowance (sub1 (unbox allowance)))
            (set-box! passed (add1 (unbox passed)))]
           [else
            (set-box! allowance a)
            (set-box! denied (add1 (box denied)))])))

     (lambda () #f)
     
     (lambda ()
       (vector-immutable 'gate:rate (unbox allowance) (unbox passed) (unbox denied)))
     
     (lambda () (set-box! allowance rate)))))

#|
Note: Draconian versus Lenient gates policy.
The gates of transport access points prevent an actor
from exploiting an access point if the gate conditions are not met.
These conditions of entry are pure predicates, that is, they may depend on
evolving state but do not themselves modify state. In other words, the
gate predicates are functional.
An optional imperative procedure (the action) may be paired with a gate predicate.
For example, the "uses" gate restricts the number of times that an access point can
be used to send or receive. The "uses" gate is #t iff the uses count > 0 and
the accompanying action procedure decrements the use count consulted by the gate.

A "draconian" policy would apply the action procedures of gates irrespective of whether
the gate permits the access or not. In other words, actions accumulate at EVERY attempt
to exploit an access point whether that exploitation judged legitimate (that is, all
of the gates for the access point returned #t) or illegitimate (one or more gates
for the access point returned #f).

However, a draconian policy is prey to a denial of service attack in which a
malevolent computation repeatedly attempts to exploit an access point in an effort
to rack up enough actions that other computations will be denied (at least in part)
as many legitimate exploitations as they might have enjoyed otherwise absent the
denial of service attack.

A "lenient" policy invokes the action procedures of gates g_1, ..., g_n only if the predicate
of each individual gate g_i returns #t, that is, we require (and g_1 ... g_n) to hold.
This policy blocks ill-behaved computations from exploiting the access point (assuming that said
computation fails one or more gates) while preserving full access (to the extent permitted by
actions) for well-behaved computations.

Note that just because a computation passes the muster of a set of access point gates
there is no guarantee (necessarily) that the underlying transport referenced by the access
point will supply the service requested by the computation. gates grant a computation
the opportunity to exploit an access point but they do not guarantee the outcome.
|#


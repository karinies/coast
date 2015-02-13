#lang racket/base

(provide
 RATE/LIMIT/UPDATE
 RATE/LIMIT/TEST
 RATE/LIMIT/HZ
 rate/limit
 rate/combine)

(define RATE/LIMIT/UPDATE   0)
(define RATE/LIMIT/TEST     1)
(define RATE/LIMIT/HZ       2)
(define RATE/LIMIT/STATISTICS 3)

(define (rate/limit rate milliseconds)
  (let ((bucket 0)
        (flow (/ rate milliseconds))
        (prior (current-inexact-milliseconds)) ; When did we last check?
        (go   0)  ; The total number of times we permitted passage.
        (stop 0)) ; The total number of times we prevented passage.
    ; Total number of tries = go + stop.
    
    (lambda (action)
      (cond
        ((= action RATE/LIMIT/UPDATE) ; Update the token bucket.
          (let* ((now (current-inexact-milliseconds))
                 (duration (- now prior)) ; How much time has passed (in milliseconds) since we last checked?
                 (b (min rate (+ bucket (* duration flow)))))
            (set! prior now)
            (cond
              ((>= b 1.0)
               (set! bucket (sub1 b))
               (set! go (add1 go))
               #t)
              (else
               (set! bucket b)
               (set! stop (add1 stop))
               #f))))

        ((= action RATE/LIMIT/TEST) ; Test the token bucket.
          (let* ((now (current-inexact-milliseconds))
                 (duration (- now prior))) ; How much time has passed (in milliseconds) since we last checked?
            (>=  (min rate (+ bucket (* duration flow))) 1.0)))

        ((= action RATE/LIMIT/HZ) (* flow 1000.0))

        ((= action RATE/LIMIT/STATISTICS) (cons go stop))))))

;; Combinator for a superior and derived rate.
(define (rate/combine superior derived)
  (lambda (action)
    (cond
      ((or (= action RATE/LIMIT/TEST) (= action RATE/LIMIT/UPDATE))
       (and (derived action) (superior action)))
      ((= action RATE/LIMIT/TEST)
       (min (derived action) (superior action)))
      ((= action RATE/LIMIT/STATISTICS)
       (list (derived action) (superior action)))
      (else (lambda () #f)))))

(define (test)
  (let ((g (rate/limit 3 5000))
        (start (current-inexact-milliseconds)))
    
    (define (lifespan)
      (/ (- (current-inexact-milliseconds) start) 1000.0))
    
    (define (work)
      (let loop ((tries 0) (wins 0))
        (sleep 1.5)
        (cond
          ((g RATE/LIMIT/UPDATE)
           (display (format "tries: ~s rate: ~s\n" (add1 tries) (/ (add1 wins) (lifespan))))
           (loop (add1 tries) (add1 wins)))
          (else
           (display (format "tries: ~s rate: ~s\n" (add1 tries) (/ wins        (lifespan))))
           (loop (add1 tries) wins)))))

    (thread work)))
       
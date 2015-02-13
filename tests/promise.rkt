#lang racket/base

(require
 "../send.rkt"
 "../actor.rkt"
 "../address.rkt"
 "../locative.rkt"
 "../promise.rkt"
 "../this.rkt"
 "../uuid.rkt")

(this/island (island/address #"foobarpublickey" "www.example.com" 5050))


;; Test promissary/initialize and the basic mechanism of PROMISSARY/PROFILE.
;; The output should be:
;;   #(struct:promise/profile 0 0 0)
;;   #(struct:promise/profile 0 1 0)
(define (test01)
  (let* ((self (actor/root/new)))
    (promissary/initialize)
    (motile/send PROMISSARY/PROFILE self)
    (display (motile/receive))
    (newline)
    (motile/send PROMISSARY/PROFILE self)
    (display (motile/receive))
    (newline)))   

;; Creates an actor a that increments a counter in response to messages.
;; Returns a locative for actor a.
(define (actor/counter/new patience)
  (let* ((a (actor/new 'counter))
         (x (locative/any/new a #(counter))))
    (actor/jumpstart
     a
     (lambda ()
       (let loop ((counter 100) (m (motile/receive/wait patience)))
         (when m
           (let ((reply (jot/payload m)))
             (motile/send reply counter)
             (loop (add1 counter) (motile/receive/wait patience)))))))
    x))

;; Test promise/block. The output should be:
;;   100
;;   101
;;   102
;;   #(struct:promise/profile 3 0 0)
(define (test02)
  (let* ((self (actor/root/new))
         (x (actor/counter/new 0.2)))
    
    (promissary/initialize)
    (let loop ((i 0))
      (cond
        ((< i 3)
         (let-values ([(p resolver) (promise/new)])
           (motile/send x resolver)
           (display (promise/block p)) (newline)
           (sleep 0.15)
           (loop (add1 i))))
        (else (display (promissary/profile)) (newline))))))

;; Test of promise/try. The output should be:
;;    promise/try failed
;;    100
;;    #(struct:promise/profile 1 0 0)
(define (test03)
  (let* ((self (actor/root/new))
         (x (actor/counter/new 0.1)))
    
    (promissary/initialize)
    (let-values ([(p resolver) (promise/new)])
      (motile/send x resolver)
      (let loop ((v (promise/try p #f)))
        (cond
          (v
            (display v) (newline)
            (display (promissary/profile)) (newline))
          (else
           (display "promise/try failed\n")
           (sleep 0.1)
           (loop (promise/try p #f))))))))

;; Test of promise/wait. The output should be:
;;   100
;;   #(struct:promise/profile 1 0 0)
(define (test04)
  (let* ((self (actor/root/new))
         (x (actor/counter/new 0.3)))
    
    (promissary/initialize)
    (let-values ([(p resolver) (promise/new)])
      (motile/send x resolver)
      (let loop ((v (promise/wait p 0.1 #f)))
        (cond
          (v
            (display v) (newline)
            (display (promissary/profile)) (newline))
          (else
           (display "promise/wait failed\n")
           (sleep 0.1)
           (loop (promise/wait p 0.1 #f))))))))
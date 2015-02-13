#lang racket/base

;; Tests for send.rkt.

(require
 "../send.rkt"
 "../actor.rkt"
 "../address.rkt"
 "../locative.rkt"
 "../this.rkt"
 "../uuid.rkt")

(this/island (island/address #"foobarpublickey" "www.example.com" 5050))

;; Prints
;; hello a
(define (test01)
  (let ((a (actor/new 'a)))
    (actor/jumpstart
     a
     (lambda ()
       (display (actor/receive))
       (newline)))
    (actor/send a "hello a")))
  
;; Prints
;; #f
(define (test02a)
  (let ((a (actor/new 'a)))
    (actor/jumpstart
     a
     (lambda ()
       (let loop ((m (actor/receive/try)))
         (display m)
         (newline)
         (when m (loop (actor/receive/try))))))))

;; Prints
;; hello a
;; #f
(define (test02b)
  (let ((a (actor/new 'a)))
    (actor/jumpstart
     a
     (lambda ()
       (let loop ((m (actor/receive/try)))
         (display m)
         (newline)
         (when m (loop (actor/receive/try))))))
    (actor/send a "hello a")))
    
;; Immediately prints
;; hello a
;; pauses for three seconds and then prints
;; #f
(define (test03a)
  (let ((a (actor/new 'a)))
    (actor/jumpstart
     a
     (lambda ()
       (let loop ((m (actor/receive/wait 3)))
         (display m)
         (newline)
         (when m (loop (actor/receive/wait 3))))))
    (actor/send a "hello a")))

;; Test that (actor/receive/wait 0) is equivalent to (actor/receive/try).
;; Immediately prints:
;;   hello a
;;   #f
(define (test03b)
    (let ((a (actor/new 'a)))
    (actor/jumpstart
     a
     (lambda ()
       (let loop ((m (actor/receive/wait 0)))
         (display m)
         (newline)
         (when m (loop (actor/receive/wait 0))))))
    (actor/send a "hello a")))

;; A message is sent from actor b to actor a via a locative for a.
(define (test04a)
  (let* ((a (actor/new 'a))
         (b (actor/new 'b))
         (x (locative/any/new (uuid/symbol) a #(silly) LOCATIVE/GUARD/EMPTY LOCATIVE/METADATA/EMPTY)))
    (actor/jumpstart
     a
     (lambda ()
       (let ((j (motile/receive)))
         (display (format "from: ~a\n"    (jot/from j)))
         (display (format "via: ~a\n"     (locative/pretty (jot/via j))))
         (display (format "payload: ~a\n" (jot/payload j))))))
    (actor/jumpstart
     b
     (lambda ()
       (motile/send x "hello a")))))

;; A reformulation of test04a using actor/root/new.
(define (test04b)
  (let* ((a (actor/new 'a))
         (x (locative/any/new (uuid/symbol) a #(silly) LOCATIVE/GUARD/EMPTY LOCATIVE/METADATA/EMPTY))
         (self (actor/root/new))) ; Convert the Racket thread executing this test into a actor.
    (actor/jumpstart
     a
     (lambda ()
       (let ((j (motile/receive)))
         (display (format "from: ~a\n"    (jot/from j)))
         (display (format "via: ~a\n"     (locative/pretty (jot/via j))))
         (display (format "payload: ~a\n" (jot/payload j))))))
    (motile/send x "hello a")))

;; Use a guard to filter out unwanted messages.
;; Prints only the debug information associated with the "hello a"
;; message as the guard forces motile/receive to discard the
;; "goodbye a" message.
(define (test05)
  ;; Returns #t if "hello" is a prefix of s and #f otherwise.
  (define (hello? s)
    (let* ((hello "hello")
           (n (string-length hello)))
      (and
       (>= (string-length s) n)
       (string=? (substring s 0 n) hello))))

  ;; Returns #t if the payload of jot j is a string
  ;; with prefix "hello" and #f otherwise.
  (define (g j)
    (and
     (string? (jot/payload j))
     (hello?  (jot/payload j))))
       
  (let* ((a (actor/new 'a))
         (x (locative/any/new (uuid/symbol) a #(silly) g LOCATIVE/METADATA/EMPTY))
         (self (actor/root/new))) ; Convert the Racket thread executing test05 into a actor.
    (actor/jumpstart
     a
     (lambda ()
       (let loop ((j (motile/receive)))
         (display (format "from: ~a\n"    (jot/from j)))
         (display (format "via: ~a\n"     (locative/pretty (jot/via j))))
         (display (format "payload: ~a\n" (jot/payload j)))
         (loop (motile/receive)))))
    (motile/send x "goodbye a")
    (motile/send x "hello a")))    

;; Identical to test04b except that the target of motile/send is a CURL.
(define (test06a)
  (let* ((a (actor/new 'a))
         (x (curl/any/new a CURL/PATH/EMPTY CURL/METADATA/EMPTY)) ; Actor a is the receiver of CURL x.
         (self (actor/root/new))) ; Convert the Racket thread executing this test into a actor.
    (actor/jumpstart
     a
     (lambda ()
       (let ((j (motile/receive)))
         (display (format "from: ~a\n"    (jot/from j)))
         (display (format "via: ~a\n"     (curl/pretty (jot/via j))))
         (display (format "payload: ~a\n" (jot/payload j))))))
    (motile/send x "hello a")))

;; Identical to test06a except that the receiver in the CURL is a locative
;; rather than an actor.
(define (test06b)
  (let* ((a (actor/new 'a))
         (x (locative/any/new (uuid/symbol) a LOCATIVE/PATH/EMPTY LOCATIVE/GUARD/EMPTY LOCATIVE/METADATA/EMPTY))
         (u (curl/any/new x CURL/PATH/EMPTY CURL/METADATA/EMPTY))
         (self (actor/root/new))) ; Convert the Racket thread executing this test into a actor.
    (actor/jumpstart
     a
     (lambda ()
       (let ((j (motile/receive)))
         (display (format "from: ~a\n"    (jot/from j)))
         (display (format "via: ~a\n"     (curl/pretty (jot/via j))))
         (display (format "payload: ~a\n" (jot/payload j))))))
    (motile/send u "hello a")))

;; Test motile/receive/wait.
(define (test07a)
  (define (g j)
    (and (jot? j) (number? (jot/payload j)) (even? (jot/payload j))))
  (let* ((a (actor/new 'a))
         (x (locative/any/new  (uuid/symbol) a #(even) g LOCATIVE/METADATA/EMPTY))
         (self (actor/root/new)))
    ;; Locative x allows only even numbers to pass through to a.
    (actor/jumpstart
     a
     (lambda ()
       (let ((m (motile/receive/wait 1)))
         (when m
           (display (format "~a\n" (jot/payload m)))))))
    (motile/send x 33)
    (sleep 0.9)
    (motile/send x 99)
    (motile/send x 44)))

;; Test motile/receive/wait.
(define (test07b)
  (define (g j)
    (and (jot? j) (number? (jot/payload j)) (even? (jot/payload j))))
  (let* ((a (actor/new 'a))
         (x (locative/any/new  (uuid/symbol) a #(even) g LOCATIVE/METADATA/EMPTY))
         (self (actor/root/new)))
    ;; Locative x allows only even numbers to pass through to a.
    (actor/jumpstart
     a
     (lambda ()
       (let ((m (motile/receive/wait 1)))
         (when m
           (display (format "~a\n" (jot/payload m)))))))
    (motile/send x 33)
    (sleep 0.9)
    (motile/send x 99)
    (sleep 0.11)
    (motile/send x 44)))    
        
;; Test motile/receive/wait using a CURL as the target.
(define (test07c)
  (define (g j)
    (and (jot? j) (number? (jot/payload j)) (even? (jot/payload j))))
  (let* ((a (actor/new 'a))
         (x (locative/any/new  (uuid/symbol) a #(even) g LOCATIVE/METADATA/EMPTY))
         (u (curl/any/new x CURL/PATH/EMPTY CURL/METADATA/EMPTY))
         (self (actor/root/new)))
    ;; Locative x allows only even numbers to pass through to a.
    (actor/jumpstart
     a
     (lambda ()
       (let ((m (motile/receive/wait 1)))
         (when m
           (display (format "~a\n" (jot/payload m)))))))
    (motile/send u 33)
    (sleep 0.9)
    (motile/send u 99)
    (motile/send u 44)))

;; Test motile/receive/wait using a CURL as the target.
(define (test07d)
  (define (g j)
    (and (jot? j) (number? (jot/payload j)) (even? (jot/payload j))))
  (let* ((a (actor/new 'a))
         (x (locative/any/new  (uuid/symbol) a #(even) g LOCATIVE/METADATA/EMPTY))
         (u (curl/any/new x CURL/PATH/EMPTY CURL/METADATA/EMPTY))
         (self (actor/root/new)))
    ;; Locative x allows only even numbers to pass through to a.
    (actor/jumpstart
     a
     (lambda ()
       (let ((m (motile/receive/wait 1)))
         (when m
           (display (format "~a\n" (jot/payload m)))))))
    (motile/send x 33)
    (sleep 0.9)
    (motile/send x 99)
    (sleep 0.11)
    (motile/send x 44)))
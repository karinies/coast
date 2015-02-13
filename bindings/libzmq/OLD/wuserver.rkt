#lang racket/base

(require
 racket/match
 "zmq.rkt")

(provide server )

;; Prepad string s to length n with character c.
(define (prepad s n c)
  (let ((m (string-length s)))
    (if (< m n)
        (string-append (make-string (- n m) c) s)
        s)))
  

(define (server)
  (let* ((context (zmq/context/new))
         (publisher (zmq/socket/new context 'PUBLISH)))
    (zmq/socket/bind publisher "tcp://*:5556") ; Socket for publishiing weather updates.
    (let loop ()
      (let ((zipcode (random 100000))
            (temperature (- (random 215) 80))
            (humidity (+ (random 50) 10)))
      (zmq/socket/send
       publisher
       (string->bytes/utf-8 (format "~a ~a ~a" zipcode temperature humidity)))
      (loop))
    (zmq/socket/close publisher)
    (zmq/context/close context))))


;(define (client zipcode)
;  
;
;
;
;
;
;  (match-define
;    (regexp #rx"([0-9]+) (-?[0-9]+) ([0-9]+)" (list _ zipcode temp humid))
;    (zmq/socket/receive sock))
;  (+ tot (string->number (bytes->string/utf-8 temp))))


#| To launch the server on the command line:
      racket -l racket/base -t wuserver.rkt -e '(server)' &
   To launch the client on the command line:
      racket -l racket/base -t wuserver.rkt -e '(client)' &
|#
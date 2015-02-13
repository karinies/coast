#lang racket/base

(require
 ffi/unsafe
 "../../libczmq/czmq.rkt"
; (only-in
;  "../../libczmq/czmq.rkt"
;  zcontext/new zcontext/destroy zmessage/new zmessage/append*
;  zmessage/debug zmessage/frames zmessage/pop zframe/size zframe/payload zmessage/destroy zhash=>list
;  zhash/unpack)
 "../zyre.rkt")

(provide
 zyre/test01
 alice/01/run
 peer/02/run
 bob/01/run
 bob/02/run)

(define (ENTER/dump prefix message)
  (printf "~a\n" prefix)
  (printf "~a frames\n" (zmessage/frames message))
  (let ((frame1 (zmessage/pop message))   ; ENTER
        (frame2 (zmessage/pop message))   ; 32 character hex representation of UUID 
        (frame3 (zmessage/pop message)))  ; zhash of headers
    (printf "~a ~a\n" (zframe/size frame1) (zframe/payload frame1))
    (printf "~a ~a\n" (zframe/size frame2) (zframe/payload frame2))
    (let ((h (zhash/unpack frame3)))
      (printf "~a\n" (zhash=>list h))
      (zhash/destroy h))
    (zframe/destroy frame1)
    (zframe/destroy frame2)
    (zframe/destroy frame3)
    (zmessage/destroy message)))

(define (test/dump prefix message)
  (printf "~a\n" prefix)
  (printf "frames: ~a\n" (zmessage/frames message))
  (zmessage/debug message (current-output-port))
  (zmessage/destroy message))

(define (zyre/test01)
  (printf "\n\n")
  (let* ((context (zcontext/new))
         (alice (zyre/new context))
         (bob (zyre/new context)))
    (zyre/start bob)
    (zyre/header/set alice "X-FILEMQ"  "tcp://128.0.0.1:6777")
    (zyre/header/set alice "X-IGNORED" "Hello world")
    (zyre/start alice)
    (zyre/join alice "GLOBAL")
    (zyre/join bob   "GLOBAL")
    
    ;; Give the two nodes time to find one another.
    (sleep 0.250)
    
    ;; alice and bob should be aware of the presence of the other.
    (let ((alice/arrives (zyre/receive bob))
          (bob/arrives   (zyre/receive alice)))
      (ENTER/dump "alice says HELLO to bob" alice/arrives)
      (ENTER/dump "bob says HELLO to alice" bob/arrives))
    
    ;; alice should receive JOIN of bob.
    (let ((join (zyre/receive alice)))
      (test/dump "bob says JOIN to alice" join))
    
    ;; alice SHOUTs to group GLOBAL
    (let ((m (zmessage/new)))
      (zmessage/append* m #"GLOBAL")
      (zmessage/append* m "Hello everyone!")
      (zyre/shout alice m))
    
    ;; bob should receive JOIN and SHOUT of alice.
    (let ((join  (zyre/receive bob))
          (shout (zyre/receive bob)))
      (test/dump "alice says JOIN to bob"  join)
      (test/dump "alice says SHOUT to bob" shout))
    
    (zyre/destroy alice)
    (zyre/destroy bob)
    (zcontext/destroy context)
    (printf "\nOK\n")))

;; Issue this shell command in the zyre/tests directory:
;;    racket -l racket/base -t zyre.rkt -e '(zyre/test01)' &

;; Replicate test01 above but run alice and bob in two separate processes.

(define (alice/01/run)
    (printf "\n\n")
  (let* ((context (zcontext/new))
         (alice (zyre/new context)))
    (zyre/header/set alice "X-FILEMQ"  "tcp://128.0.0.1:6777")
    (zyre/header/set alice "X-IGNORED" "Hello world")
    (zyre/start alice)
    (zyre/join alice "GLOBAL")
    (sleep 0.250) ; Give alice and bob some time to find one another.
    
    ; alice receives notice of presence of bob.
    (let ((bob/arrives (zyre/receive alice)))
      (ENTER/dump "bob says HELLO to alice" bob/arrives))
    
    ;; alice SHOUTs to group GLOBAL
    (let ((m (zmessage/new)))
      (zmessage/append* m #"GLOBAL")
      (zmessage/append* m "Hello world")
      ; NOTE: We are not responsible for the memory of message me after the shout.
      (zyre/shout alice m))
    
    (zyre/destroy alice)
    (zcontext/destroy context)
    (printf "\nOK\n")))

(define (bob/01/run)
    (printf "\n\n")
  (let* ((context (zcontext/new))
         (bob (zyre/new context)))
    (zyre/start bob)
    (zyre/join bob "GLOBAL")
    
    ;; Give alice and bob time to find one another.
    (sleep 0.250)
    
    ;; Bob receives notice of the presence of alice.
    (let ((alice/arrives (zyre/receive bob)))
      (ENTER/dump "alice says HELLO to bob" alice/arrives))
    
    ;; bob should receive JOIN and SHOUT of alice.
    (let ((join (zyre/receive bob))
          (shout (zyre/receive bob)))
      (test/dump "alice says JOIN to bob"  join)
      (test/dump "alice says SHOUT to bob" shout))

    (zyre/destroy bob)
    (zcontext/destroy context)
    (printf "\nOK\n")))

;; racket -l racket/base -t zyre.rkt -e '(alice/01/run)' &
;; racket -l racket/base -t zyre.rkt -e '(bob/01/run)' &

;; Alice and Bob exchange a few polite greetings.

(define (peer/02/run)
  (let* ((arguments (current-command-line-arguments))
         (nickname (string->bytes/latin-1 (if (> (vector-length arguments) 0) (vector-ref arguments 0) "malvern")))
         (context (zcontext/new))
         (self (zyre/new context)))
    (zyre/header/set self "NICKNAME" nickname)
    (zyre/start self)
    (sleep 0.250) ; Give alice and bob some time to find one another.
    
    ; alice receives notice of presence of bob.
    (let loop ((m (zyre/message/unpack (zyre/receive self))))
      ;(printf "~a: ~a\n" nickname m)
      (cond
        ((ENTER? m)
         (printf "~a got: " nickname) (write m) (newline)
         (let* ((greeting (zmessage/new))
                (other (string->bytes/latin-1 (zyre/headers/find (ENTER-headers m) "NICKNAME" "???")))
                (sentiment (bytes-append #"Hello " other #" it's " nickname #"!")))
           (printf "~a about to send: ~a ~a\n" nickname (bytes-length sentiment) sentiment)
           (WHISPER/send self (ENTER-peer m) sentiment)))

        ((WHISPER? m)
         (printf "~a got: " nickname) (write (WHISPER=>vector m)) (newline))

        (else
         (printf "~a saw: " nickname) (write m) (newline)))
      (loop (zyre/message/unpack (zyre/receive self))))
    
    (zyre/destroy self)
    (zcontext/destroy context)
    (printf "\nOK\n")))

(define (bob/02/run)
    (printf "\n\n")
  (let* ((context (zcontext/new))
         (bob (zyre/new context)))
    (zyre/header/set bob "NICKNAME" "bob")
    (zyre/start bob)
    (sleep 0.250) ; Give alice and bob some time to find one another.
    
    ; bob receives notice of presence of alice.
    (let loop ((m (zyre/message/unpack (zyre/receive bob))))
      (printf "bob: ~a\n" m)
      (cond
        ((ENTER? m)
         (let ((greeting (zmessage/new))
               (nickname (zyre/headers/find (ENTER-headers m) #"NICKNAME" #"<anonymous>")))
           (zmessage/append* greeting (ENTER-peer m))
           (zmessage/append* greeting (format "Hello there ~a!" nickname))
           (zyre/whisper bob greeting)))
        ((WHISPER? m)
         (printf "~s\n" (WHISPER-payload m)))
        (else
         (printf "Bob saw: ~a\n" m))))
    
    (zyre/destroy bob)
    (zcontext/destroy context)
    (printf "\nOK\n")))

;; racket -l racket/base -t zyre.rkt -e '(peer/02/run)' &
;; racket -l racket/base -t zyre.rkt -e '(bob/02/run)' &
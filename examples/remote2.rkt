#lang racket/base
 
;; A sample remote execution server.

(require
 "../include/base.rkt"
 "../baseline.rkt"
 [only-in "../curl/base.rkt" curl/origin curl/metadata]
 "../remote.rkt"
 "../transport/gate.rkt"
 "../transport/gates/whitelist.rkt")

(define CERTIFICATE/PUBLIC "./certificates/public/")
(define CERTIFICATE/SECRET "./certificates/secret/")
(define ALICE/SECRET/PATH (string-append CERTIFICATE/SECRET "alice_secret"))
(define BOB/SECRET/PATH   (string-append CERTIFICATE/SECRET "bob_secret"))
(define CAROL/SECRET/PATH (string-append CERTIFICATE/SECRET "carol_secret"))

(define ALICE/KP/BASE64 #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc")
(define BOB/KP/BASE64   #"49u_B0VEdFFS3WCPMMX5T5MFQ3SaSHjM8fM63I4L338")
(define CAROL/KP/BASE64 #"rqM_XCwrsziuhIEsG1d0yMA05mivoewXhUmzKUzhb0s")
  
(define KEYSTORE (keystore/new))
;; Download all of the predefined public certificates.
(keystore/load KEYSTORE CERTIFICATE/PUBLIC)

(define ALICE/CURVE/SECRET (path-to-curve ALICE/SECRET/PATH))
(define BOB/CURVE/SECRET   (path-to-curve BOB/SECRET/PATH))
(define CAROL/CURVE/SECRET (path-to-curve CAROL/SECRET/PATH))

;; Demonstrate how to generate an inline CURL for alice:
;; Execute
;;   (display (curl-as-bytes ALICE/CURVE/SECRET '(remote chirp) 'access:send:chirp ALICE/CURL/METADATA))
;; and then copy and paste the text as the body of a
;;  (define/curl/inline ALICE/CURL/chirp ...)
;; as shown below.

;; The CURL for alice's chirp service.
;; It is 2907 bytes long.
(define/curl/inline ALICE/CURL/CHIRP
#<<!!
SIGNATURE = #"EluzePQ3yTfqZaY-zS0cl3EOEMSYUxYH0IFvcBKvqDIUtDWCWREuT4zuxZOWvcj-uVqUtS4uueTW8ak2mvACDg"
CURL
    id = 3693ca03-68ee-410a-b719-cefede58fcf9
    origin = #"wdvbN1svfhEAewhM76oSVPKj-4kzfbDhaiTFW61VdUc"
    path = (remote chirp)
    access/id = access:send:chirp
    created = "2014-05-24T14:58:53Z"
    metadata = #(struct:motile/flat #(1 0 0) 0 #() () #(struct:environ #(struct:hash eq #(GLOBAL (M v . #(lambda/inner 4 #f (v . #(combination 2 (v . #(lambda/outer 2 (v . #(sequence 3 (v! . #((v . #(combination 1 (v . #(letrec*/set 1)) #(c (v . #(closure/outer 2 1 (v! . #(#(c 1 2))) (v . #(sequence 2 (v! . #((v . #(combination 1 (v . #(variable/get 3)) #(c (v . #(combination 3 (v . #(global/get format)) #(c (v . #(constant "~a: ~a\n")) #(c (v . #(variable/get 1)) #(c (v . #(variable/get 2)) ()))))) ()))) (v . #(constant #t)))))))) ()))) (v . #(combination 1 (v . #(letrec*/set 2)) #(c (v . #(lambda/outer 2 (v . #(combination 1 (v . #(lambda/outer 1 (v . #(closure/outer 1 2 (v! . #(#(c 1 1) #(c 0 1))) (v . #(if (v . #(combination 1 (v . #(global/get positive?)) #(c (v . #(combination 1 (v . #(global/get unbox)) #(c (v . #(variable/get 3)) ()))) ()))) (v . #(sequence 2 (v! . #((v . #(combination 2 (v . #(global/get box/set)) #(c (v . #(variable/get 3)) #(c (v . #(combination 1 (v . #(global/get sub1)) #(c (v . #(combination 1 (v . #(global/get unbox)) #(c (v . #(variable/get 3)) ()))) ()))) ())))) (v . #(combination 1 (v . #(variable/get 2)) #(c (v . #(variable/get 1)) ()))))))) (v . #(constant #f)))))))) #(c (v . #(combination 1 (v . #(global/get box)) #(c (v . #(variable/get 2)) ()))) ()))))) ()))) (v . #(combination 0 (v . #(closure/outer 0 4 (v! . #(#(c 1 3) #(c 1 4) #(c 0 2) #(c 1 1))) (v . #(combination 2 (v . #(lambda/outer 2 (v . #(environ/cons 1 (v . #(variable/get 1)) (v! . #(chirp)) (v! . #((v . #(variable/get 2)))))))) #(c (v . #(combination 0 (v . #(global/get environ/capture)) ())) #(c (v . #(if (v . #(combination 2 (v . #(global/get bytes=?)) #(c (v . #(combination 1 (v . #(global/get murmur/origin)) #(c (v . #(variable/get 4)) ()))) #(c (v . #(variable/get 1)) ())))) (v . #(combination 2 (v . #(variable/get 3)) #(c (v . #(closure/outer 1 1 (v! . #(#(c 1 1))) (v . #(combination 2 (v . #(variable/get 2)) #(c (v . #(constant bob)) #(c (v . #(variable/get 1)) ())))))) #(c (v . #(constant 2)) ())))) (v . #(if (v . #(combination 2 (v . #(global/get bytes=?)) #(c (v . #(combination 1 (v . #(global/get murmur/origin)) #(c (v . #(variable/get 4)) ()))) #(c (v . #(variable/get 2)) ())))) (v . #(combination 2 (v . #(variable/get 3)) #(c (v . #(closure/outer 1 1 (v! . #(#(c 1 1))) (v . #(combination 2 (v . #(variable/get 2)) #(c (v . #(constant carol)) #(c (v . #(variable/get 1)) ())))))) #(c (v . #(constant 4)) ())))) (v . #(lambda/outer 1 (v . #(constant #f)))))))) ())))))) ())))))))) #(c (v . #(constant #f)) #(c (v . #(constant #f)) ()))))))))))

!!
)

(define THUNK
  (island/compile
   '(lambda ()
      (map
       chirp
       (list "roses are red"      "violets are blue"
             "COAST is wonderful" "but remote are you")))))

;; Return the petname of the island that transmitted murmur m.
(define (origin/petname m) (keystore/petname/look (this/keystore) (murmur/origin m)))

;; Code for a client island.
;; u - CURL for chirp service
;; n - total number of times client uses chirp service
(define (client/boot u)
  ; The client that bob and carol will spawn to request chirps from alice.
  ; u is the CURL that alice provides for her chirp service.
  (define (client/chirp u)
    ; Create a new islet to act as a service client.
    (let* ([nickname (log/name/build (this/island/nickname) 'client.chirp)]
           [x (islet/new (this/island) nickname TRUST/LOW environ/null environ/null)])
      (islet/jumpstart
       x
       (lambda ()
         (island/enter/wait (curl/origin u)) ; Wait for island service provider to enter the network.
         (for ([i (in-range 1 4)])
           (let ([p (promise/new)])
             ; We send the chirp server: (cons <promise CURL> <thunk>)~
             (send u (cons (promise/resolver p) THUNK)) ; Request a remote evaluation.
             (let ([m (promise/block p)])
               (display
                (format "  ~a got ~s back from ~a\n"
                        nickname (murmur/payload m) (origin/petname m))))
             (sleep 0.75)))))))
  (client/chirp u))

(define CHIRP
  (island/compile
   '(lambda (m display BOB/KP/BASE64 CAROL/KP/BASE64)
      ; Print a message string that identifies the sender.
      (define (chirp/base chirper s)
        (display (format "~a: ~a\n" chirper s))
        #t)

      ; Wrap a single argument function f so that it can be called
      ; no more than limit times.
      (define (limit/wrap f limit)
        (let ([uses (box limit)])
          (lambda (s)
            (cond
              [(positive? (unbox uses))
               (box/set uses (sub1 (unbox uses)))
               (f s)]
              [else #f]))))

      (let ([BASELINE (environ/capture)]
            [chirp
             (cond
               [(bytes=? (murmur/origin m) BOB/KP/BASE64)
                ; Custom version of chirp for bob.
                (limit/wrap (lambda (s) (chirp/base 'bob s)) 2)]
               [(bytes=? (murmur/origin m) CAROL/KP/BASE64)
                ; Custom version of chirp for carol.
                (limit/wrap (lambda (s) (chirp/base 'carol s)) 4)]
               [else
                ; We should never get here.
                (lambda (s) #f)])])
        (environ/cons BASELINE chirp)))))

(define ALICE/CURL/METADATA (environ/cons environ/null 'GLOBAL CHIRP))
(define DISPLAY/1 (motile/global/1 'display display)) ; A Motile-compatible display function.

;; Code for alice who offers an chirp service.
(define (alice/boot)
  (define (service/chirp)
    (let* ([t (transport:bankers/new)] ; Generic queuing transport.
           [a/send
            (access:send/known/new
             t 'access:send:chirp
             (gate/or (gate/and (gate/whitelist/island BOB/KP/BASE64)   (gate/uses 3))
                      (gate/and (gate/whitelist/island CAROL/KP/BASE64) (gate/uses 1)))
             EMBARGO/NO)]
           [a/receive (access:receive/new t 'access:receive:chirp GATE/NONE)]
           [keystore  (this/keystore)])
      (accessor/add (this/accessors) a/send) ; So the island router knows where to find us.
      
      ; Wait for an chirp request.
      (let loop ([m (access/receive a/receive #f)])
        (when (murmur? m)
          (let ([payload (murmur/payload m)])
            (when (and (pair? payload) (curl? (car payload)) (procedure? (cdr payload)))
              (let* ([generator ; Extract the binding environ generator from the murmur CURL.
                      (environ/ref (curl/metadata (murmur/target m)) 'GLOBAL #f)]
                     [GLOBAL ; Invoke the generator for a client-specific global binding environ
                      (motile/call generator BASELINE m DISPLAY/1 BOB/KP/BASE64 CAROL/KP/BASE64)]
                     ; Create a subordinate islet, whose global binding environ is client-specific,
                     ; to evaluate the thunk.
                     [worker (subislet/new 'service.chirp.worker TRUST/LOWEST GLOBAL)])
                ; Task and start the worker with a 2.0 second deadline.
                (remote worker (cdr payload) (car payload) 2.0))))
        (loop (access/receive a/receive #f))))))
  
  (service/chirp))

 ; Construct an in-memory CURL instance of the predefined CURL for alice.
(define alice/curl/chirp (curl/zpl/safe-to-curl ALICE/CURL/CHIRP KEYSTORE))

(define alice (island/new 'alice ALICE/CURVE/SECRET alice/boot))
(define bob   (island/new 'bob   BOB/CURVE/SECRET   (lambda () (client/boot alice/curl/chirp))))
(define carol (island/new 'carol CAROL/CURVE/SECRET (lambda () (client/boot alice/curl/chirp))))

;;; Multiple islands in the same address space can share the exact same keystore
;;; and any change in the keystore will be seen by all such islands in the
;;; address space.
(island/keystore/set alice KEYSTORE)
(island/keystore/set bob   KEYSTORE)
(island/keystore/set carol KEYSTORE)
(island/log/level/set 'info)

#|
> (island/start carol) (island/start alice) (island/start bob)
#(info "bridge@alice: saw ENTER" 0 bridge@alice)
#(info "bridge@alice: saw ENTER" 0 bridge@alice)
#(info "ingress@alice: saw ENTER" "6A9B65C25BA34537B6154EB9433B476E" ingress@alice)
#(info "bridge@carol: saw ENTER" 0 bridge@carol)
#(info "ingress@carol: saw ENTER" "DECE49E44822486A8F2F4E5640C1C131" ingress@carol)
#(info "bridge@bob: saw ENTER" 0 bridge@bob)
#(info "bridge@bob: saw ENTER" 1 bridge@bob)
#(info "ingress@bob: saw ENTER" "6A9B65C25BA34537B6154EB9433B476E" ingress@bob)
#(info "ingress@bob: saw ENTER" "DECE49E44822486A8F2F4E5640C1C131" ingress@bob)
#(info "bridge@carol: saw ENTER" 1 bridge@carol)
#(info "ingress@carol: saw ENTER" "B0D1D7C8E00E46EC92E63AC441EBCF0D" ingress@carol)
#(info "bridge@alice: saw ENTER" 1 bridge@alice)
#(info "ingress@alice: saw ENTER" "B0D1D7C8E00E46EC92E63AC441EBCF0D" ingress@alice)
#(info "bridge@alice: saw WHISPER" 2 bob: roses are red
bob: violets are blue
bridge@alice)
#(info "router@alice: saw murmur" bob router@alice)
#(info "bridge@alice: saw WHISPER" 3 bridge@alice)
#(info "bridge@bob: saw WHISPER" 2 bridge@bob)
#(info carol: roses are red
carol: violets are blue
  client.chirp@bob got (#t #t #f #f) back from alice
carol: COAST is wonderful
carol: but remote are you
"router@alice: saw murmur" carol router@alice)
#(info "router@bob: saw murmur" alice router@bob)
#(info "reaper@bob: purges" 1 reaper@bob)
#(info "bridge@carol: saw WHISPER" 2 bridge@carol)
#(info "bridge@alice: saw WHISPER" 4 bridge@alice)
  client.chirp@carol got (#t #t #t #t) back from alice
#(info "router@carol: saw murmur" alice router@carol)
#(info "reaper@carol: purges" 1 reaper@carol)
#(info "router@alice: saw murmur" bob router@alice)
bob: roses are red
bob: violets are blue
#(info "bridge@bob: saw WHISPER" 3 bridge@bob)
  client.chirp@bob got (#t #t #f #f) back from alice
#(info "router@bob: saw murmur" alice router@bob)
#(info "reaper@bob: purges" 1 reaper@bob)
#(info "bridge@alice: saw WHISPER" 5 bridge@alice)
#(info "router@alice: saw murmur" carol router@alice)
#(info "bridge@alice: saw WHISPER" 6 bridge@alice)
#(info "router@alice: saw murmur" bob router@alice)
bob: roses are red
bob: violets are blue
#(info "bridge@bob: saw WHISPER" 4 bridge@bob)
  client.chirp@bob got (#t #t #f #f) back from alice
#(info "router@bob: saw murmur" alice router@bob)
#(info "reaper@bob: purges" 1 reaper@bob)

> (island/destroy alice)
#(info "bridge@bob: saw EXIT" 5 bridge@bob)
#(info "ingress@bob: saw EXIT" alice ingress@bob)
#(info "bridge@carol: saw EXIT" 3 bridge@carol)
#(info "ingress@carol: saw EXIT" alice ingress@carol)
> (island/destroy carol)
#(info "bridge@bob: saw EXIT" 6 bridge@bob)
#(info "ingress@bob: saw EXIT" carol ingress@bob)
> (island/destroy bob)
> 
|#

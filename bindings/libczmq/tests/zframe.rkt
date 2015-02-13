#lang racket/base

(require
 (only-in typed/racket/base assert)
 "../zcontext.rkt"
 "../zframe.rkt"
 "../zsocket.rkt")

(define (zframe/test)
  (printf " * zframe: ")
  (let* ((context (assert (zcontext/new)))
         (output (assert (zsocket/new context 'PAIR)))
         (input  (assert (zsocket/new context 'PAIR))))
    (zsocket/bind output   "inproc://zframe.test")
    (zsocket/connect input "inproc://zframe.test")

    ; Send five distinct frames to test frame MORE.
    (let loop ((n 0) (f (zframe/new #"Hello")))
      (when (< n 5)
        (assert (zframe/send f output #:more #t))
        (loop (add1 n) (zframe/new #"Hello"))))

    ; Send same frame five times to rest REUSE.
    (let ((f (assert (zframe/new #"Hello"))))
      (let loop ((n 0))
        (when (< n 5)
          (assert (zframe/send f output #:more #t #:reuse #t))
          (loop (add1 n)))))
    ; Note at this point we have transmitted 10 frames.

    ; Test zframe/duplicate and zframe=?
    (let* ((f (assert (zframe/new #"Hello")))
           (copy (assert (zframe/duplicate f))))
      (assert (zframe=? f copy))
      (zframe/destroy f) ; Once we destroy a frame we can NEVER touch it again!
      (assert (= (zframe/size copy) 5))
      (zframe/destroy copy))
    
    ; Test frame/empty/new.
    (let ((f (assert (zframe/empty/new))))
      (assert (= (zframe/size f) 0))
      (zframe/destroy f))
    
    ; Send END frame.
    (let ((f (assert (zframe/new #"NOT"))))
      (zframe/reset f #"END")
      ;(assert (bytes=? (zframe/payload f) #"END"))
      (assert (bytes=? (zframe/data f) #"END"))
      (assert (zframe/send f output)))
    ; Note at this point we have transmitted 11 frames.
    
    ; Read and count until we receive END.
    (let loop ((i 0) (f (zframe/receive input)))
      (cond
        ((bytes=? (zframe/data f) #"END");(bytes=? (zframe/payload f) #"END")
         (zframe/destroy f)
         (assert (= i 10))) ; 11 frames total since we count from 0.
        (else
         (assert (zframe/more f))
         (zframe/more f #f)
         (assert (not (zframe/more f)))
         (zframe/destroy f)
         (loop (add1 i) (zframe/receive input)))))
    
    ; The inproc pipe should be empty.
    (let ((f (zframe/receive* input)))
      (assert (not f)))
    
    (zcontext/destroy context)
    (printf "OK\n")
    #t))
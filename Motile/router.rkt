#lang racket/base

(require
 racket/pretty ; Testing only.
 (only-in "persistent/hash.rkt" hash/equal/null hash/cons hash/empty? hash/ref list/hash)
 (only-in "persistent/tuple.rkt" tuple tuple? tuple/length tuple/take/left))


;; Island-level router.

;; bases is a vector of path/thunk pairs organized as #(p_1 t_1 p_2 f_2 ...)
;; where the paths are organized from longest path to shortest.

;; Wrap thunk t in an exception handler that will restart t in event of failure.
(define (forwarder/wrap router prefix thunk)
  ;; This is the wrapper for thunk t which handles path routing for the given path prefix.
  (lambda ()
    (with-handlers
        ([exn:fail
          (lambda (e)
            ; Add logging for thunk t failure here!
            ; Send a restart request to the router nanny asking it to restart thunk for given path prefix.
            ; at router position i-1.
            (thread-send (router/nanny router) (message/restart prefix thunk)))])
      (thunk))))

;; Message to nanny to restart thunk t in router.
(define (message/restart prefix thunk) #('restart prefix thunk))
(define-syntax-rule (restart/prefix x) (vector-ref x 1))
(define-syntax-rule (restart/thunk x)  (vector-ref x 2))


(define (message/restart? x)
  (and
   (vector? x)
   (eq? (vector-ref x 0) 'restart)
   (= (vector-length x) 3)
   (tuple?     (restart/prefix x)
   (procedure? (restart/thunk x)))))

;; Generate a thunk for the nanny that will regenerate forwarding threads for the given router.
(define (nanny/new router)
  (lambda ()
    (let loop ((alert (thread-receive)))
      (when (message/restart? alert)
        (let* ((p (restart/prefix alert))
               (t (restart/thunk alert))
               (hops (router/hops router)))
          ; Restart the thunk that died and reinsert it into the forwarding table indexed by prefix p.
          (router/hops!
           router
           (hash/cons hops p (thread (forwarder/wrap router p t)))))
        ; Wait for the next restart alert.
        (loop (thread-receive))))))


;; A router is a 3-slot vector #(<nanny> <max prefix length> <hops map>) where
;; <nanny> - nanny thread responsible for restarting forwarder threads in hops map when they fail
;; <max prefix length> - length (in components) of longest prefix (tuple) in hops map
;; <hops map> - a map of p/f pairs where p is a path prefix and f is a destination thread for any path with prefix p.
(define-syntax-rule (router/nanny r) (vector-ref r 0))
(define-syntax-rule (router/max   r) (vector-ref r 1))
(define-syntax-rule (router/hops r)  (vector-ref r 2))

(define-syntax-rule (router/nanny! r x) (vector-set! r 0 x))
(define-syntax-rule (router/max!   r x) (vector-set! r 1 x))
(define-syntax-rule (router/hops!  r x) (vector-set! r 2 x))

(define (router/new)
  (let ((r (make-vector 3 #f)))
    (router/nanny! r (thread (nanny/new r)))
    (router/max!   r 0)
    (router/hops!  r hash/equal/null)
    r))

(define (forwarding/prefix/add hops prefix thunk)
  (hash/cons hops prefix (thread (forwarder/wrap hops prefix thunk))))

(define (forwarding/add router forwardings)
  (let loop ((i 0)
             (n (vector-length forwardings))
             (longest (router/max router))
             (hops     (router/hops router)))
    (cond 
      ((< i n)
       (let ((prefix (vector-ref forwardings i))
             (thunk  (vector-ref forwardings (add1 i))))
         (router/max! router (max longest (tuple/length prefix)))
         (loop (+ i 2) n (max (tuple/length prefix) longest) (forwarding/prefix/add hops prefix thunk))))

      (else
       (router/max! router longest)
       (router/hops! router hops)
       router))))


(define TUPLE/EMPTY (tuple))


(define (forward/new prefix path thread)
  (vector 'forward prefix path thread))
(define (forward? x)
  (and
   (vector? x)
   (eq? (vector-ref x 0) 'forward)
   (= (vector-length x) 4)))
(define-syntax-rule (forward/prefix f) (vector-ref f 1))
(define-syntax-rule (forward/path   f) (vector-ref f 2))
(define-syntax-rule (forward/target f) (vector-ref f 3))
(define-syntax-rule (forward/payload f) (vector-ref f 3))

;; Copy forward f replacing the target of f with the given payload.
(define (forward/copy f payload)
  (forward/new (forward/prefix f) (forward/path f) payload))
  

;; Find a routing for the given path in the given router.
;; Returns a forward message if successful and #f otherwise.
(define (forwarding/find router path)
  (let ((hops (router/hops router)))
    (let loop ((n (min (router/max router) (tuple/length path)))) ; Starting length of prefix.
    (cond
      ((zero? n)
       (let ((t (hash/ref hops TUPLE/EMPTY #f)))
         (if t
             (forward/new TUPLE/EMPTY path t)
             #f)))
      (else
       (let* ((prefix (tuple/take/left path n))
              (t      (hash/ref hops prefix #f)))
         (if t
             (forward/new prefix path t)
             (loop (sub1 n)))))))))

(define (router/forward router path payload)
  (let ((forward (forwarding/find router path)))
    (when forward
      (thread-send (forward/target forward) (forward/copy forward payload)))))
    
(define PATHS
  (vector
   (tuple)           (lambda () (let loop ((x (thread-receive))) (display "root: ")    (display x) (newline) (loop (thread-receive))))
   (tuple 'foo 'bar) (lambda () (let loop ((x (thread-receive))) (display "foo/bar: ") (display x) (newline) (loop (thread-receive))))
   (tuple 'foo)      (lambda () (let loop ((x (thread-receive))) (display "foo: ") (display x) (newline) (loop (thread-receive))))
   (tuple 'bar)      (lambda () (let loop ((x (thread-receive))) (display "bar: ") (display x) (newline) (loop (thread-receive))))
   (tuple 'a 'b 'c)  (lambda () (let loop ((x (thread-receive))) (display "a/b/c: ") (display x) (newline) (loop (thread-receive))))))

(define XPATHS
  (list
   (tuple)           1
   (tuple 'foo 'bar) 2
   (tuple 'foo)      3
   (tuple 'bar)      4))

(define (test/router/1)
  (let* ((router (router/new)))
    (pretty-display (forwarding/add router PATHS)) (newline) (newline)))

(define (test/router/2)
  (let ((router (router/new)))
    (forwarding/add router PATHS)
    (pretty-display router)
    (pretty-display (router/hops router))
    (router/forward router (tuple 'foo 'bar 'baz) 100)
    (router/forward router (tuple 'foo 'baz)      200)))
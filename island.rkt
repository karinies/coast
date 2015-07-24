#lang racket/base

(require
 racket/contract/base
 "Island/base.rkt"
 "Island/accessor.rkt"
 "Island/archipelago.rkt"
 "Island/bridge.rkt"
 "Island/egress.rkt"
 "Island/ingress.rkt"
 "Island/keystore.rkt"
 "Island/reaper.rkt"
 "Island/router.rkt"
 "Island/logger.rkt"

 "Motile/compile/compile.rkt"

 "bindings/libczmq/zcontext.rkt"
 "bindings/libczmq/zconfig.rkt"
  "bindings/libzyre/libzyre.rkt"
 "bindings/libzyre/peer.rkt"
 "persistent/environ.rkt"
 "curve.rkt"
 "islet.rkt"
 [only-in "Island/island-comet.rkt" island/monitoring/log]
 "comet/comet-types.rkt")

(provide 
 (contract-out
  [path-to-curve (-> string? curve?)]
  [path-to-curve/public (-> string? curve/public?)]
  [island/new (-> symbol? (or/c string? curve?) procedure? island?)]
  [island/advertise (-> island? (or/c string? symbol?) (or/c string? symbol? number?) void?)]
  [island/keys/advertise (-> zyre? kp/base64/c kp/sign/base64/c void?)]
  [island/enter/wait (-> kp/base64/c void?)]
  [islands/enter/wait (-> (listof kp/base64/c) void?)]
  [island/start (-> island? void?)]
  [island/destroy (-> island? void?)]
  [island/compile (-> any/c procedure?)]))

(define (jumpstart)
  (let loop ((x (thread-receive)))
    (if (procedure? x) (x) (loop (thread-receive)))))

;; Jumpstart a Racket thread into a faux islet.
(define (jumpstart-to-islet i nickname trust)
  (lambda ()
    (let ([a (islet/faux i nickname trust)])
      (parameterize ([this/islet a]) ; Self reference.
        (jumpstart)))))

;; Construct a new curve? instance by downloading the island secret certificate.
;; path - path name of island secret certificate.
(define (path-to-curve path)
  (let* ([certificate (zconfig/load path)]
         [curve  (certificate-to-curve certificate)])
    (zconfig/destroy certificate)
    curve))

(define (path-to-curve/public path)
  (let* ([certificate (zconfig/load path)]
         [curve/public (certificate-to-curve/public certificate)])
    (zconfig/destroy certificate)
    curve/public))

;; Construct a new island? 
;; nickname - island nickname (symbol?) for debugging and logging.
;; x - either a curve? instance or the file path (string?) for the island's secret certificate
;; f - thunk defining island services and clients
(define (island/new nickname x f)
  (let* ([curve (if (string? x) (path-to-curve x) x)]
         [treasure
         (island
          nickname ; For logging and debugging.
          curve ; CURVE keys.
          (thread jumpstart) ; bridge
          (thread jumpstart) ; ingress
          (thread jumpstart) ; router
          (thread jumpstart) ; egress
          (thread jumpstart) ; reaper (cleans out closed access:send points)
          (thread jumpstart) ; peer (executes service and client functions)
          (zyre/new THIS/CONTEXT);(zyre/new (THIS/CONTEXT)) ; Underlying ZyRE node.
          f                         ; Service and client function.
          (archipelago/new) ; Achipelago map.
          (accessor/new)    ; Accessors map for access:send points named by CURLs.
          (keystore/new)    ; Keystore map for all island public key pairs and island petnames.
          CURL/BYTES/MAX
          PAYLOAD/BYTES/MAX)])
    (island/keys/advertise treasure (curve/kp/base64 curve) (curve/kp/sign/base64 curve))
    (thread-send (island/bridge  treasure) (jumpstart-to-islet treasure 'bridge  TRUST/HIGHEST))
    (thread-send (island/ingress treasure) (jumpstart-to-islet treasure 'ingress TRUST/HIGHEST))
    (thread-send (island/router  treasure) (jumpstart-to-islet treasure 'router  TRUST/HIGHEST))
    (thread-send (island/egress  treasure) (jumpstart-to-islet treasure 'egress  TRUST/HIGHEST))
    (thread-send (island/reaper  treasure) (jumpstart-to-islet treasure 'reaper  TRUST/HIGHEST))
    (thread-send (island/peer    treasure) (jumpstart-to-islet treasure 'peer    TRUST/HIGHEST))
    
    treasure))

;; Set a key/value pair in the headers advertised by island i.
;; Any headers must be set BEFORE the island is started.
;; i -island
;; key - header key as ASCII string
;; value - header value as ASCII string
(define (island/advertise i key value)
  (zyre/header/set (island/underpeer i) key value))

(define (island/keys/advertise i kp/base64 kp/sign/base64)
  (island/advertise i KP/HEADER kp/base64)
  (island/advertise i KP/SIGN/HEADER kp/sign/base64))

;; Load the keystore whose root directory is path into island i.
(define (island/keystore/load i path)
  (keystore/load (island/keystore i) path))
;; Merge hash/equal? h into the keystore of island i.
(define (island/keystore/merge i h)
  (keystore/merge (island/keystore i) h))
;; Add an island's public keys to the keystore of i.
(define (island/keystore/add i kp kp/sign)
  (keystore/add (island/keystore i) kp kp/sign))
;; Remove an island's  public keys from the keystore of i.
(define (island/keystore/remove i kp)
  (keystore/remove (island/keystore i) kp))

;; Start the island after setting all headers.
;; Once started it will undertake discovery and connection.
(define (island/start i)
  ;(log/info (island/nickname i) "starting underpeer" #t)
  ; Start the ZyRE node.
  (zyre/start (island/underpeer i))
  ; Jumpstart the ingress, egress, reaper and router threads.
  ;(log/info (island/nickname i) "starting ingress, egress, and router" #t)
  (thread-send (island/ingress i) ingress/worker)
  (thread-send (island/egress i)  egress/worker)
  (thread-send (island/reaper i)  reaper/worker)
  (thread-send (island/router i)  router/worker)
  ; Jumpstart the bridge thread.
  ;(log/info (island/nickname i) "starting bridge" #t)
  (thread-send (island/bridge i) bridge/worker)
  ; Jumpstart the peer thread.
  ;(log/info (island/nickname i) "starting peer" #t)
  (thread-send (island/peer i) (island/boot i)))
  

;; Kill the island.
;; Any pending messages in the zyre peer will be lost.
;(define (island/stop i)
;  (thread-send (island/bridge  i) 'TERMINATE)
;  (thread-send (island/ingress i) 'TERMINATE)
;  (thread-send (island/router  i) 'TERMINATE)
;  (thread-send (island/egress  i) 'TERMINATE)
;  (thread-send (island/reaper  i) 'TERMINATE)
;  (zyre/stop (island/underpeer i)))

(define (island/destroy i)
  (kill-thread (island/bridge  i))
  (kill-thread (island/ingress i))
  (kill-thread (island/router  i))
  (kill-thread (island/egress  i))
  (kill-thread (island/reaper  i))
  (kill-thread (island/peer    i))
  (zyre/destroy (island/underpeer i))
  ; Reset all of the island fields that hold threads to snip
  ; their circular references to the island.
  (island/bridge/set  i #f)
  (island/ingress/set i #f)
  (island/router/set  i #f)
  (island/egress/set  i #f)
  (island/reaper/set  i #f)
  (island/peer/set    i #f))

;; Wait under the island with the given kp/base64 key enters the network.
(define (island/enter/wait kp/base64)
  (let loop ([a (this/archipelago)])
    (unless (archipelago/look a kp/base64)
      (sleep 2.0) ; Sleep for 2 seconds before looking again.
      (loop a)))
  (sleep 1.0)) ; Make sure that the other island has seen us as well.

(define (islands/enter/wait origins)
  (let loop ([a (this/archipelago)])
    (unless (for/and ([kp/base64 (in-list origins)]) (archipelago/look a kp/base64))
      (sleep 2.0) ; Sleep for 2 seconds before looking again.
      (loop a)))
  (sleep 1.0)) ; Make sure that the other islands have seen us as well.

;; Island-level compilation of expressions for transmission to other islands.
(define (island/compile e)
  (let ([f (motile/compile e)])
    (f
     (lambda (x) x) ; Continuation
     #f             ; Starting lexical scope
     environ/null))) ; Global binding environ.

;; One single zcontext is shared by every island created in this Racket process.
;(define THIS/CONTEXT (make-parameter (zcontext/new)))
(define THIS/CONTEXT (zcontext/new))

(define CURL/BYTES/MAX    (* 128 1024))
(define PAYLOAD/BYTES/MAX (* 256 1024))
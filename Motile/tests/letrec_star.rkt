#lang racket/base

(require
 (only-in "../generate/baseline.rkt" motile/call motile/decompile)
 (only-in "../compile/compile.rkt" motile/compile))

(motile/compile
 '(letrec 
      ([f (lambda ()
            ; curl used to allow decoders to look up this address upon their arrival on island.
            (define me/lookup@ (curl/new/any (this/locative) null #f))
            
            (define g (new-video-gui (curl/new/any (this/locative) null #f)))
            (define (spawn-gui-endpoint requester@)
              ;; !!! NO SERIALIZATION WARNING !!!
              ;; (we lifted `g' and `add-video!' out of the lambda)
              ;; a grown-up implementation would check that this
              ;; curl `requester@' is a local curl.
              (let* ([av! video-gui-add-video!]
                     [buffer-maker! (lambda (w h)
                                      (av! g w h requester@))]
                     [cnvs (canvas-endpoint)]
                     [cve (lambda (rpy@) ((cnvs) rpy@ buffer-maker!))]
                     [linker (linker-bang)])
                ;; launch the linker to start up our wrapped endpoint
                (curl/send PUBLIC/CURL (spawn/new (lambda () 
                                                    ((linker) PUBLIC/CURL cve (make-metadata) requester@))
                                                  (make-metadata '(nick . linker)) #f))))
            
            (set-current-gui-curl! me/lookup@)
            
            (let loop ([m (mailbox-get-message)]
                       [decoders set/equal/null])
              (define body (:remote/body (delivered/contents-sent m)))
              (cond 
                ;; sent from new decoders.
                [(AddCURL? body)
                 (spawn-gui-endpoint (:AddCURL/curl body))
                 (loop (mailbox-get-message) (set/cons decoders (:AddCURL/curl body)))]

                ;; forwarded from the actual GUI.
                [(RemoveCURL? body)
                 (curl/send (:RemoveCURL/curl body) (remote/new (Quit/new) (make-metadata) #f))
                 (loop (mailbox-get-message) (set/remove decoders (:RemoveCURL/curl body)))]

                ;[(PIPOn? v)
                ; (let ([the-pipλ ((unwrap (make-pip-decoder)) 
                ;                  (list (PIPOn.major v) (PIPOn.minor v)))])
                ;   (ask/send* "SPAWN" PUBLIC/CURL the-pipλ (make-metadata accepts/webm) PUBLIC/CURL))
                ; (loop (mailbox-get-message) decoders)]
                ;; copy all children then copy self.
                [(CopyActor? body)
                 (curl/send (curl/get-public (:CopyActor/host v) (:CopyActor/port v))
                            (spawn/new f (make-metadata is/gui '(nick . gui-controller)) #f))
                 (set/map decoders (lambda (decoder@)
                                     (curl/send decoder@ (delivered/contents-sent m))))
                 (loop (mailbox-get-message) decoders)]

                ;; only copy one child.
                [(CopyChild? body)
                 (curl/send (:CopyChild/curl body) 
                            (remote/new (CopyActor/new (:CopyChild/host body) (:CopyChild/port body)) 
                                        (make-metadata) #f))
                 (loop (mailbox-get-message) decoders)]

                ;; just pass backwards.
                [(InitiateBehavior? body)
                 (curl/send (InitiateBehavior.ref body) (delivered/contents-sent m))
                 (loop (mailbox-get-message) decoders)]

                ;; probably something sent back beyond the decoder.
                [(FwdBackward? body)
                 (curl/send (FwdBackward.ref body) (delivered/contents-sent m))
                 (loop (mailbox-get-message) decoders)] 

                ;; move decoders, then move self.
                [(Quit/MV? body)
                 (curl/send (curl/get-public (:Quit/MV/host body) (:Quit/MV/port body))
                            (spawn/new f (make-metadata is/gui '(nick . gui-controller)) #f))
                 (set/map decoders (lambda (decoder@) (curl/send decoder@ (delivered/contents-sent m))))]

                ;; pass on to decoders.
                [(Quit? body)
                 (set/map decoders (lambda (decoder@) (curl/send decoder@ (delivered/contents-sent m))))]
                [else
                 (printf "Not a valid request to GUI: ~a~n" v)
                 (loop (mailbox-get-message) decoders)])))])
    (f)))

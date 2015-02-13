#! /usr/bin/env racket
#lang racket/base

(require "../libcurl/libcurl.rkt"
         racket/list
         ffi/unsafe)

(define HTTP/version #"^[A-Z|a-z]+/[0|1]\\.[0|1|9]")
(define HTTP/code #"[0-9]{3}")
(define HTTP/message #"([A-Z|a-z]+[ ])*[A-Z|a-z]+$")
(define HTTP/line (bytes-append HTTP/version #" " HTTP/code #" " HTTP/message))
(define HTTP/version-rx (byte-pregexp HTTP/version))
(define HTTP/code-rx (byte-pregexp HTTP/code))
(define HTTP/message-rx (byte-pregexp HTTP/message))
(define HTTP/line-rx (byte-pregexp HTTP/line))

(define HTTP/header/key #"^[-|A-Z|a-z|0-9]+:")
(define HTTP/header/value #".+$")
(define HTTP/header/line (bytes-append HTTP/header/key HTTP/header/value))
(define HTTP/header/line-rx (byte-pregexp HTTP/header/line))
(define HTTP/header/key-rx (byte-pregexp HTTP/header/key))

(struct header (key value) #:transparent)
(struct response-line-values (protocol/version code message) #:transparent)

(define curl (curl-easy-init))

; defines a callback function that takes some output port
; and writes the bytes received to that output port for later processing
; always process exactly (* size nmemb) bits (where size = units of char)
; and return that count or libcurl will report an error
(define-syntax-rule (define-write-to-outport id port)
  (define (id bytes size nmemb *data)
    (printf "data size: ~a*~a~n" size nmemb)
    (write-bytes bytes port 0 (* size nmemb))
    (* nmemb size)))

(define (GET url body headers)
  ; set up the callbacks to use for processing request body
  ; and request headers as they are received
  (define-write-to-outport write-body body)
  (define-write-to-outport write-header headers)
  (curl-easy-setopt-writefunction curl CURLOPT_WRITEFUNCTION write-body)
  (curl-easy-setopt-writefunction curl CURLOPT_HEADERFUNCTION write-header)
  
  ; set the URL and send the request
  (curl-easy-setopt curl CURLOPT_URL url)
  (curl-easy-perform curl))

(define (my-program)
  ; do the request
  (define body-port (open-output-bytes))
  (define headers-port (open-output-bytes))
  (GET "http://www.google.com" body-port headers-port)
  
  ;;; ... process data from the output ports ...
  (let ([headers (get-output-bytes headers-port)]
        [body (get-output-bytes body-port)])
    (printf "Headers size: ~a~n" (bytes-length headers))
    (printf "Body size: ~a~n" (bytes-length body))
    
    ;; let's do some header reading
    (filter-map (λ (line)
                  (cond
                    [(regexp-match-exact? HTTP/line-rx line)
                     (let ([version (first (regexp-match HTTP/version-rx line))]
                           [code (string->number (bytes->string/utf-8 (first (regexp-match HTTP/code-rx line))))]
                           [message (first (regexp-match HTTP/message-rx line))])
                       (response-line-values version code message))]
                    
                    [(regexp-match-exact? HTTP/header/line-rx line)
                     (let* ([key (first (regexp-match HTTP/header/key-rx line))])
                       (header (subbytes key 0 (- (bytes-length key) 1))
                               (subbytes line (+ 1 (bytes-length key)))))]
                    
                    [else (printf "Unparseable line: ~a~n" line) #f]))
                
                (regexp-split #"\r\n" headers))))

(my-program)
(curl-easy-cleanup curl)

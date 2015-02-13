#lang racket/base

(require ffi/unsafe
         racket/list
         "libcurl-types.rkt")

(define libcurl (ffi-lib "libcurl" "4"))

(provide (except-out (all-defined-out)
                     libcurl
                     defcurl-lookup
                     defcurl
                     defcurl*)
         (all-from-out "libcurl-types.rkt"))

(define-syntax-rule (defcurl-lookup obj typ)
  (get-ffi-obj obj libcurl typ))
(define-syntax-rule (defcurl obj typ)
  (define obj (defcurl-lookup (regexp-replaces 'obj '((#rx"-" "_"))) typ)))
(define-syntax-rule (defcurl* typ obj ...)
  (begin (defcurl obj typ)
         ...))

;; general api
(defcurl curl-formfree (_fun _CURL-HTTPPost-pointer -> _void))
(defcurl curl-formget (_fun _CURL-HTTPPost-pointer _pointer _CURL-formget-callback -> _void))
(defcurl curl-free (_fun _string -> _void))
(defcurl curl-getdate (_fun _string ((_ptr i _time_t) = #f) -> _time_t))
(defcurl curl-global-cleanup (_fun -> _void))
(defcurl curl-global-init (_fun _long -> _CURLCode))
(defcurl curl-slist-append (_fun (_or-null _CURL-SList-pointer) _string -> _CURL-SList-pointer))
(defcurl curl-slist-free-all (_fun _CURL-SList-pointer -> _void))
(defcurl curl-unescape (_fun _string _int -> _string))
(defcurl curl-version (_fun -> _string))

;; "easy" interface
(defcurl curl-easy-init (_fun -> _CURL-pointer))
(defcurl curl-easy-escape (_fun _CURL-pointer _string _int -> _string))
(defcurl* (_fun _CURL-pointer -> _void)
  curl-easy-cleanup
  curl-easy-reset)
(defcurl curl-easy-duphandle (_fun _CURL-pointer -> _CURL-pointer))
(defcurl curl-easy-getinfo (_fun _CURL-pointer _int _pointer -> _CURLCode))
(defcurl curl-easy-pause (_fun _CURL-pointer _int -> _CURLCode))
(defcurl curl-easy-perform (_fun _CURL-pointer -> _CURLCode))
(defcurl curl-easy-strerror (_fun _CURLCode -> _string))
(defcurl curl-easy-unescape (_fun _CURL-pointer _string _int (o : (_ptr o _int))
                                  -> (s : _string)
                                  -> (values s o)))
; setopt versions
(define curl-easy-setopt-long 
  (defcurl-lookup 'curl_easy_setopt (_fun _CURL-pointer _CURLOpt _long -> _CURLCode)))
(define curl-easy-setopt-string
  (defcurl-lookup 'curl_easy_setopt (_fun _CURL-pointer _CURLOpt _string -> _CURLCode)))
(define curl-easy-setopt-ptr
  (defcurl-lookup 'curl_easy_setopt (_fun _CURL-pointer _CURLOpt _pointer -> _CURLCode)))
(define curl-easy-setopt-writefunction
  (defcurl-lookup 'curl_easy_setopt (_fun _CURL-pointer _CURLOpt
                                          (_fun _bytes _int _int _pointer -> _int)
                                          -> _CURLCode)))

(define (curl-easy-setopt handler option parameter)
  (cond
    [(integer? parameter) (curl-easy-setopt-long handler option parameter)]
    [(string? parameter) (curl-easy-setopt-string handler option parameter)]
    [else (curl-easy-setopt-ptr handler option parameter)]))

; getinfo versions

(define curl-easy-getinfo-string
  (defcurl-lookup 'curl_easy_getinfo (_fun _CURL-pointer _CURLInfo (out : (_ptr o _string))
                                           -> (res : _CURLCode) -> (values out res))))
(define curl-curl-easy-getinfo-long
  (defcurl-lookup 'curl_easy_getinfo (_fun _CURL-pointer _CURLInfo (out : (_ptr o _long))
                                           -> (res : _CURLCode) -> (values out res))))

(define curl-easy-getinfo-double
  (defcurl-lookup 'curl_easy_getinfo (_fun _CURL-pointer _CURLInfo (out : (_ptr o _double))
                                           -> (res : _CURLCode) -> (values out res))))

; formadd versions

(define curl-formadd-first
  (defcurl-lookup 'curl_formadd (_fun (first : (_ptr io (_or-null _CURL-HTTPPost-pointer)))
                                      (last : (_ptr io (_or-null _CURL-HTTPPost-pointer)))
                                      (_CURLformoption = 'copyname)
                                      _string
                                      (_CURLformoption = 'copycontents)
                                      _string
                                      (_CURLformoption = 'end)
                                      -> (code : _CURLformcode)
                                      -> (values code first last))))

(define curl-formadd-copyname
  (defcurl-lookup 'curl_formadd (_fun (first : (_ptr io _CURL-HTTPPost-pointer))
                                      (last : (_ptr io _CURL-HTTPPost-pointer))
                                      (_CURLformoption = 'copyname)
                                      _string
                                      (_CURLformoption = 'copycontents)
                                      _string
                                      (_CURLformoption = 'end)
                                      -> _CURLformcode)))

(define (curl-slist-new s . ss)
  (let ([head (curl-slist-append #f s)])
    (if (not (empty? ss))
        (let loop ([to-append ss] [current-head head])
          (cond [(= (length ss) 1) (curl-slist-append current-head (first to-append))]
                [else (loop (rest to-append) (curl-slist-append current-head (first to-append)))]))
        head)))

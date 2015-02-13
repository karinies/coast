#! /usr/bin/env racket
#lang racket/base

(require "../libcurl/libcurl.rkt")

(define curl (curl-easy-init))

(define (GET url)
  (curl-easy-setopt curl CURLOPT_URL url)
  (curl-easy-perform curl)
  (curl-easy-getinfo-string curl Effective-URL)
  (curl-easy-getinfo-string curl Content-Type))
(GET "http://www.google.com")
(curl-easy-cleanup curl)
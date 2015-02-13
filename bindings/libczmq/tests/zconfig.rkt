#lang racket/base

(require
 "../zconfig.rkt")

(define (zconfig/test)
  (let* ((root (zconfig/new "root" #f))
         (section (zconfig/new "metadata" root))
         (email (zconfig/new "email" section))
         (name  (zconfig/new "name"  section)))
    (zconfig/value/set email "some@random.com")
    (zconfig/value/set name  "Justin Kayce")
    
    (zconfig/put root "curve/secret-key" "TOP SECRET")
    (zconfig/put root "curve/passkey" "#99241")
    
    (zconfig/comment/set root "   CURVE certificate")
    (zconfig/comment/set root "   -----------------")
    (zconfig/file/save root "foobar.cfg")
    (zconfig/save root (current-output-port))))
  

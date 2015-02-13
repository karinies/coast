#lang racket/base

(require
 ffi/unsafe
 racket/format
 "libczmq.rkt"
 "zlist.rkt")

(provide
 zconfig/new
 zconfig/duplicate
 zconfig/destroy
 zconfig/name
 zconfig/value
 zconfig/put
 zconfig/name/set
 zconfig/value/set
 zconfig/child
 zconfig/next
 zconfig/locate
 zconfig/resolve
 zconfig/depth/at
 zconfig/load
 zconfig/save
 zconfig/file/save
 zconfig/display
 zconfig/comment/set
 zconfig/comments
 zconfig/comments/save)

;; Constructor.
(define-czmq-function zconfig/new "zconfig_new" (_fun (name : _string) (parent : _zconfig/null) -> _zconfig))
;; Destructor.
(define-czmq-function zconfig/destroy "zconfig_destroy" (_fun (_ptr i _zconfig) -> _void))

;; Access.
(define-czmq-function zconfig/name  "zconfig_name"  (_fun _zconfig -> _string))
(define-czmq-function zconfig/value "zconfig_value" (_fun _zconfig -> _string))

(define-czmq-function zconfig/put "zconfig_put" (_fun _zconfig (path : _string) (value : _string) -> _void))
(define-czmq-function zconfig/name/set "zconfig_set_name" (_fun _zconfig (name : _string) -> _void))
(define-czmq-function zconfig/value/set "zconfig_set_value" (_fun _zconfig (value : _string) -> _void))

;; First child (if any).
(define-czmq-function zconfig/child "zconfig_child" (_fun _zconfig -> _zconfig/null))
;; Next sibling (if any).
(define-czmq-function zconfig/next "zconfig_next" (_fun _zconfig -> _zconfig/null))
;; Find a zconfig along a path.
(define-czmq-function zconfig/locate  "zconfig_locate"  (_fun _zconfig (path : _string) -> _zconfig/null))
;; Find the value at the end of a path.
(define-czmq-function zconfig/resolve "zconfig_resolve" (_fun _zconfig (path : _string) (default : _string) -> _string))

(define-czmq-function zconfig/depth/at "zconfig_at_depth" (_fun _zconfig (depth : _int) -> _zconfig/null))

(define-czmq-function zconfig/load "zconfig_load" (_fun (filename : _string) -> _zconfig/null))
(define-czmq-function zconfig/file/save "zconfig_save" (_fun _zconfig (filename : _string) -> (r : _int = (zero? r))))
;; Write to standard output.
(define-czmq-function zconfig/display "zconfig_print" (_fun _zconfig -> _void))

;; Add a comment to a node.
(define-czmq-function zconfig/comment/set "zconfig_set_comment" (_fun _zconfig (comment : _string) -> _void))
;; Return all of the comments attached to a node as a zlist.
(define-czmq-function zconfig/comments "zconfig_comments" (_fun _zconfig -> _zlist/null))

;; Walk a zconfig depth-first invoking (f z level) on each zconfig z as you go.
(define (walk z f level)
  (let loop ([go? (f z level)]
             [child (zconfig/child z)])
    (when (and go? child)
      (loop (walk child f (add1 level)) (zconfig/next child)))
      #t))
(define (zconfig/walk z f) (walk z f 0))

(define (indent output level)
  (let ((n (if (> level 1) (* (sub1 level) 4) 0)))
    (when (positive? n)
      (display (~a "" #:width n) output))))

;; Return #t if string s contains a hash character and #f otherwise.
(define octothorpe?
  (let ((octothorpe (regexp "#")))
    (lambda (s) (regexp-match octothorpe s))))

(define (zconfig/comment/save comment output level)
  (indent output level)
  (display (format "#~a\n" comment) output))

(define (zconfig/comments/save z output level)
  ;(display (format "z:~a output:~a level:~a\n" z output level))
  (let ([comments (zconfig/comments z)])
    (when comments
      (let loop ([comment (zlist/first comments)])
        (when comment
          (zconfig/comment/save comment output level)
          (loop (zlist/next comments))))
      (newline output))))

(define (save z output level)
  (zconfig/comments/save z output level)
  (when (positive? level)
    (let* ([v (zconfig/value z)]
           [n (or (zconfig/name z) "(Unnamed)")]
           [form
            (if (or (not v) (string=? v ""))
                (format "~a\n" n)
                (format (if (octothorpe? v) "~a = ~s\n" "~a = ~a\n") n v))])
      (indent output level)
      (display form output))))

(define (zconfig/save z output)
  (let ((f (lambda (z level) (save z output level))))
    (zconfig/walk z f)))

(define (zconfig/duplicate z parent)
  (and
   z
   (let ([name     (zconfig/name z)]
         [value    (zconfig/value z)]
         [comments (zconfig/comments z)])
     (let ([root (zconfig/new name parent)])
       (zconfig/value/set root value)
       ; Copy all of the comments (if any).
       (when comments
         (let loop ([comment (zlist/first comments)])
           (when comment
             (zconfig/comment/set root comment)
             (loop (zlist/next comments)))))
       ; Duplicate the children of z.
       (let loop ([child (zconfig/child z)])
         (when child
           (zconfig/duplicate child root)
           (loop (zconfig/next child))))
       root))))       
    

;(define z (zconfig/new "example" #f))
;    
;(define sample (zconfig/load "../../bindings/libczmq/zpl_sample.cfg"))
      
  
    
#lang racket/base

(require
 ffi/unsafe
 [only-in "zlist.rkt" zlist/first zlist/next zlist=>list zlist/destroy]
 [only-in "../../persistent/hash.rkt" hash/equal/null vector/hash]
 "libczmq.rkt")

(provide
 zhash/new
 zhash/destroy
 zhash/insert
 zhash/update
 zhash/delete
 zhash/lookup
 zhash/size
 zhash/keys
 zhash/unpack
 zhash-to-list
 zhash-to-vector
 zhash-to-hash/persist)


(define-czmq-function zhash/new "zhash_new" (_fun -> _zhash))
(define-czmq-function zhash/destroy "zhash_destroy" (_fun (_ptr i _zhash) -> _void))
;; Insert key/value pair into hash table. If key is already present original value is
;; left untouched.
(define-czmq-function
  zhash/insert "zhash_insert"
  (_fun _zhash (key : _string) (value : _string) -> (r : _int) -> (zero? r)))
;; Insert and overwrite any prior key/value pair.
(define-czmq-function
  zhash/update "zhash_update"
  (_fun _zhash (key : _string) (value : _string) -> _void))
;; Delete key/value pair from hash table.
(define-czmq-function zhash/delete "zhash_delete" (_fun _zhash _string -> _void))

;; Note: zhash/lookup returns #f if key is not in zhash table.
(define-czmq-function zhash/lookup "zhash_lookup" (_fun _zhash (key : _string) -> (value : _string)))
(define-czmq-function zhash/size "zhash_size" (_fun _zhash -> _size_t))

(define-czmq-function zhash-keys "zhash_keys" (_fun _zhash -> (keys : _zlist)))
;; Return the keys of zhash x as a Racket list.
(define (zhash/keys x)
  (let* ((k (zhash-keys x))
         (keys (zlist=>list k)))
    (zlist/destroy k)
    keys))

(define-czmq-function zhash/unpack "zhash_unpack" (_fun _zframe -> _zhash))

;; Generate the association list equivalent ((k_1 . v_1) ... (k_n . v_n)) of a zhash table.
(define (zhash-to-list x)
  (if (zero? (zhash/size x))
      null
      (let ((keys (zhash-keys x)))
        (let loop ((key (zlist/first keys)) (outcome null))
          (if key
              (loop (zlist/next keys) (cons (cons key (zhash/lookup x key)) outcome))
              outcome)))))

;; Generate the vector equivalent #(k_1 v_1 ... k_n v_n) of a zhash table.
(define (zhash-to-vector x)
  (let ((n (zhash/size x)))
    (if (zero? n)
        #()
        (let ((keys (zhash-keys x))
              (v (make-vector (* 2 n) #f)))
          (let loop ((i 0) (key (zlist/first keys)))
            (when key
              (vector-set! v i key)
              (vector-set! v (add1 i) (zhash/lookup x key))
              (loop (+ i 2) (zlist/next keys))))
          v))))

;; Generate the persistent hash table equivalent of a zhash table.
(define (zhash-to-hash/persist x)
  (if (zero? (zhash/size x))
      hash/equal/null
      (vector/hash hash/equal/null (zhash-to-vector x))))

(define (zhash/test01)
  (let* ((h (zhash/new))
         (a (zhash-to-list h)))
    (zhash/destroy h)
    a))

(define (zhash/test02)
  (let ((h (zhash/new)))
    (zhash/insert h "foo" "FOO")
    (zhash/insert h "bar" "BAR")
    (zhash/insert h "nix" "NIX")
    (let ((a (zhash-to-list h)))
      (zhash/destroy h)
      a)))

(define (zhash/test03)
  (let* ((h (zhash/new))
         (a (zhash-to-vector h)))
    (zhash/destroy h)
    a))

(define (zhash/test04)
  (let ((h (zhash/new)))
    (zhash/insert h "foo" "FOO")
    (zhash/insert h "bar" "BAR")
    (zhash/insert h "nix" "NIX")
    (let ((a (zhash-to-vector h)))
      (zhash/destroy h)
      a)))

(define (zhash/test05)
  (let* ((h (zhash/new))
         (a (zhash-to-hash/persist h)))
    (zhash/destroy h)
    a))

(define (zhash/test06)
  (let ((h (zhash/new)))
    (zhash/insert h "foo" "FOO")
    (zhash/insert h "bar" "BAR")
    (zhash/insert h "nix" "NIX")
    (let ((a (zhash-to-hash/persist h)))
      (zhash/destroy h)
      a)))
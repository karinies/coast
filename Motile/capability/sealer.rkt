#lang racket/base

(define-syntax-rule (cell/new x) (box x))
(define-syntax-rule (cell! c x) (set-box! c x))
(define-syntax-rule (uncell c)  (unbox c))

(define-syntax-rule (slot/clear! slot) (cell! slot #f))


;; Create a (sealer . unsealer) pair.
;(define (sealer/new)
;  (let* ((slot (cell/new #f))
;
;         (sealer
;          (lambda (capability)
;            (lambda () (cell! slot capability)))) ; Return the box of the sealer/unsealer
;
;         (unsealer
;          (lambda (b) ; b is some box.
;            (slot/clear! slot)       ; Clear the slot.
;            (b)                      ; Invoke the box.
;            ; Return the capability c that was in the slot after clearing the slot.
;            (begin0
;              (uncell slot)          ; Grab the capability in the slot shared with the box.
;              (slot/clear! slot))))) ; Clear the slot.
;
;
;    (cons sealer unsealer)))

;; Create a (sealer . unsealer) pair.
(define (sealer/new)
  (letrec
      ((slot      (cell/new #f))
       (semaphore (make-semaphore 1))
       
       (sealer
        (lambda (capability)
          ; Return a box of the sealer/unsealer
          (lambda (u)
            (when (eq? u unsealer)
              (semaphore-wait semaphore)
              (cell! slot capability)))))
       
       (unsealer
        (lambda (b) ; b is some box.
          (slot/clear! slot) ; Clear the slot.
          (b unsealer)       ; Invoke the box.
          ; Return the capability c that was in the slot after clearing the slot.
          (begin0
            (uncell slot)                  ; Grab the capability in the slot shared with the box.
            (slot/clear! slot)             ; Clear the slot.
            (semaphore-post semaphore))))) ; Reset the semaphore allowing the box/unsealer pair to be used elsewhere.

    (cons sealer unsealer)))

(define (motile/serialize/bytes capability)
  (let ((out (open-output-bytes capability)))
    (write (motile/serialize capability) out)
    (get-output-bytes out)))

(define (motile/deserialize/bytes bytes)
  (motile/deserialize (read (open-input-bytes bytes))))
        

;; Returns #t if x is a lockbox and #f otherwise.
(define (lockbox? x)
  (and
   (vector? x)
   (= (vector-length x) 4)
   (eq? (vector-ref x 0) '<lockbox>)
   (string? (vector-ref x 1)
   (bytes? (vector-ref x 2))
   (positive? (bytes-length (vector-ref x 2)))
   (bytes? (vector-ref x 3))
   (bytes-length (vector-ref x 3) 64))))


;; The function (sealer/new) returns a function seal.
;; The function seal can not be serialized and therefore can never be exported off-island.
;; Given any Motile value x (seal x) returns (u v (K- (SHA bytes-append u v)))
;; where u and v are two 16-byte random numbers and the third element is a signature over u and v.
;; the "unsealer" and "lockbox" of x respectively.
;; of x. Repeated calls of (seal x) will always return distinct pairs of 16-byte random numbers.
;; That is, for any two calls of seal returning (u_i v_i signing_i)  and (u_j v_j signing_j) we have:
;;   * u_i ~= v_i
;;   * u_i ~= u_j
;;   * u_i ~= v_j
;;   * v_i ~= u_j
;;   * v_i ~= v_j
;; in other words all random numbers are pair-wise disjoint.
;;
;; A zero-argument call (seal) always returns (actor locative)
;; The owner of the seal uses the three values actor A, locative and the lockbox as follows:
;;
;; (1) With the capability granted by the possession of both actor A and a locative for A, the seal owner
;; constructs a CURL c for A whose PATH is the byte string u. This CURL is the "unsealer."
;; CURL c may assert temporal, use-count, and location constraints and may contain arbitrary metadata.
;; Note that c contains the public key K+ of the actor.
;;
;; (2) The seal owner constructs the pair lockbox = (v . (K- (SHA (bytes-append u v)))
;; where K- is the private key of the actor.
;; The second element of the lockbox is a signature that only the holder of the seal 
;; 
;; To obtain value x sealed with (u . v) an actor B most possess both CURL c and the latch key.
;; With these two items in hand B sends the message tuple("UNSEAL" <latch key>) to CURL c.
;; If the receiving island I grants B the right to communicate (that is CURL c is correct and meets the
;; temporal, use-count, and location constraints) then the seal actor A on I:
;; (1) Decrypts the latch key using its public key K+ to obtain sha = (SHA (bytes-append u v))
;;     Recall that u is contained as metadata in c. Assume that sha is correct, that is, the latch key is legitimate.
;; (2) Uses u as a key to a hash table obtains the secret value x
;; (3) Returns the secret value x to actor B

;; The structure of CURL c and latchkey (v .



single-argument function seal.
;; Given a "treasure" x (seal x) returns a triple (<actor> <locative> <lockbox>)
;; where <actor> is a Motile actor a, <locative> is a Motile locative for a and <lockbox> 



;(define (motile/sealer/new moniker)
;  (let* ((pair (crypto/key-pair))
;         (K+   (car pair)) ; Public key.
;         (K-   (cdr pair)) ; Private key.
;
;         (sha   (crypto/sha/512 nonce))
;         (signature (crypto/public/sign K- sha)))
;    
;    (let ((sealer
;           (lambda (capability)
;             (let* ((b (motile/serialize/bytes capability))
;                    (sha (crypto/sha/512 b)))
;               (lambda () (vector (crypto/public/sign K- b) (crypto/public/sign K- sha))))))
;
;          (unsealer
;           (lambda (lockbox)
;               (when 
;                 (let ((cleartext (crypto/public/verify K+ (vector-ref v 0)))
;                   (when (crypto/public/verify K+ (vector-ref v 1) (crypto/sha/512 cleartext))
;                     (motile/deserialize/bytes cleartext))))))))
;      (cons sealer unsealer))))
;
;(define (lockbox/new K- capability)
;  (let* ((b (motile/serialize/bytes capability))
;         (hash (crypto/sha/512 b)))
;    (lockbox (cons (crypto/public/sign K- b) (crypto/pubic/sign K- hash)))))
;
;(define (lockbox/ok? x)
;  (and
;   (pair? x)
;   (bytes? (car x))
;   (bytes? (cdr x))
;   (positive? (bytes-length (car x)))
;   (= (bytes-length (cdr x) 64))))
;  
;(define (sealer/new)
;  (let* ((pair (crypto/keys/pair))
;         (K+ (car pair))  ; Public key.
;         (K- (cdr pair))) ; Private key.
;    (let (sealer
;          (lambda (capability)
;            (let* ((b (motile/serialize/bytes capability))
;                   (n (bytes-length b))
;                   (hash (crypto/sha/512 b))
;                   (unsealer
;                    (lambda (lockbox)
;                      (when (lockbox/ok? lockbox)
;                        (let ((original (crypto/public/verify K- (car lockbox))))
;                          (when (crypto/sign/verify K- (cdr lockbox) (crypto/sha/512 original))
;                            (motile/deserialize/bytes original)))))))
;              (cons
;                              
;                        
;        
;         
;
;
;
;
;    (let ((sealer
;           (lambda (K- capability)
;
;               (lambda (K+)
;                 (let ((b (crypto/public/verify K+ (car lockbox)
;               
;
;
;
;
;
;             ; This closure is the lockbox.
;             (lambda (K+ n)
;               (cond
;                 (and (not K+) (not n))
;                   sha ; Prove to the unsealer that you are the right lockbox.
;                   (if (bytes= (crypto/public/verify K+ sha) (crypto/sha/512/ n)) capability #f)))))
;
;
;          (unsealer
;           (lambda (lockbox)
;             (if (bytes= sha (lockbox #f #f)) ; Test to make sure that this is the matching lockbox for this unsealer.
;                 (lockbox K+ nonce)
;                 #f))))
;      (cons sealer unsealer))))
;
;    
;      
;
;
;
;
;(define (test x)
;  (let* ((pair (sealer/new))
;         (seal (car pair))
;         (unseal (cdr pair)))
;    (let ((box (seal x)))
;      (list (unseal box) (unseal box)))))
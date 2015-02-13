#lang racket/base

(require
 racket/contract/base
 [only-in racket/format ~a]
 ffi/unsafe/atomic
 "getters.rkt"
 "persistent/hash.rkt"
 )

(provide
  zpl/name zpl/value
  [rename-out (zpl/value/set zpl/value/slam)]
  (contract-out
   [struct
    zpl
    ((name symbol?)
     (value any/c)
     (children hash/eq?)
     (count exact-nonnegative-integer?))]          
   [zpl/root/new (-> zpl?)]
   [zpl/new (symbol? any/c (or/c zpl? #f) . -> . zpl?)]
   [zpl/unset/new (symbol? zpl? . -> . (or/c zpl? #f))]
   [zpl/children/list (zpl? . -> . (or/c null (listof zpl?)))]
   [zpl/load (input-port? . -> . zpl?)]
   [zpl/locate (zpl? (or/c symbol? (listof symbol?)) . -> . (or/c zpl? #f))]
   [zpl/blaze  (-> zpl? (listof symbol?) zpl?)]
   [zpl/duplicate (zpl? . -> . zpl?)]
   [zpl/attach (-> zpl? zpl? zpl?)]
   ;[zpl/skeleton  (zpl? . -> . zpl?)]
   [zpl/save (zpl? output-port? . -> . void?)]
   ;[zpl/at (zpl? exact-nonnegative-integer? . -> . (or/c zpl? #f))]
   [zpl/set? (zpl? . -> . boolean?)]
   [zpl/unset  (zpl? . -> . void?)]
   [zpl/value/set (zpl? any/c . -> . boolean?)]
   [zpl/get (-> zpl? (listof symbol?) any/c any/c)]
   [zpl/put (zpl? (listof symbol?) (not/c zpl?) . -> . void?)])

 zpl/duplicate/test
 zpl/fold/test
; zpl/list/test
; zpl/load/test
; zpl/locate/test
; curl/template/test
 )

;(define WLC #rx"^ *#(.*)$") ; Whole Line Comment.
;(define NAME #rx"([a-zA-z0-9$_@.@+/-])+")
;;<space>=<space><double-quote><
;(define QUOTED-VALUE #rx" *= *(\\\"[^\\\"]*\\\") *$")
;(define APOSTROPHE-VALUE #rx" *= *(\\\'[^\\\"]*\\\') *$")
;
;(define s1 " =  \"foobar\"   ")
;(define s2 " =  \"foo17   $bar\"   ")
;(define s3 " =  'foo17   $bar'    ")
;
;(define (m p s) (regexp-match-positions p s))
    
;; Calculate the level of indentation by counting the number of blanks at the head of the line.
(define (level/collect s line#)
  (let* ([outcome (regexp-match-positions #rx"^( *)" s)]
         [span (car outcome)]
         [level (quotient (cdr span) 4)])
    (if (= (* level 4) (cdr span))
        level
        (begin
          (display
           (format
            "error zpl.rkt:level/collect: (line# ~a) indentation must be a multiple of 4 spaces\n"
            line#))
          #f))))

(define (line/parse/error message line#)
  (format "error zpl.rkt: line/parse (line# ~a): ~a\n" line# message))

;;Returns:
;;   - #f on fatal error in parse of line buffer b
;;   - ((<name> . <value>) . <level>) when it sees <name> = <value>
;;   - (<name> . <level>) whene it sees <name> alone
;;   - null when it sees an empty line
(define (line/parse b line#)
  (with-handlers
      ([exn:fail (lambda (e) (line/parse/error "reader failure" line#) #f)])
    (let* ([level (level/collect b line#)]
           [p (open-input-bytes b (format "line:~a" line#))]
           [name   (read p)]
           [equals (read p)]
           [value  (read p)]
           [stop   (read p)])
      (cond
        [(eof-object? name) null] ; Empty line.
        [(symbol? name)
         (cond
           [(and (eq? '= equals) (not (eof-object? value))) (vector name value level)] ; <name> = <value>
           [(and (eq? '= equals) (eof-object? value)) ; <name> = <eof>
            (display (line/parse/error "missing value" line#))
            #f]
           [(eof-object? equals) (vector name (void) level)] ; <name> alone.
           [else ; <name> <something-other-than-equals-sign>
            (display (line/parse/error "expected = (equals sign)" line#))
            #f])]
        [else
         (display (line/parse/error "expected name" line#))
         #f]))))
(struct
 zpl
 (name                 ; name (as symbol) assigned to this node.
  (value #:mutable)    ; value assigned to this node.
  (children #:mutable) ; hash table of name/node pairs.
  (count #:mutable))   ; Total number of children.
 #:transparent)
(struct/getters/define zpl name value children count)
(struct/setters/define zpl children count)
(struct/++/define zpl count)

;; Return a fresh child node of parent with the given name and value.
(define (zpl/new name value parent)
  (let ([z (zpl name value hash/eq/null 0)]) ; The fresh child node.
    ; If a parent was provided then attach the child z to the given parent.
    (when parent
      (zpl/children/set parent (hash/new (zpl/children parent) name z (zpl/count parent) z))
      (zpl/count++ parent))
    z))

;; Constructs an unset child of the parent only if no child of that name exists.
;; Returns the fresh child node on success and #f if a child of that name
;; is already present.
(define (zpl/unset/new name parent)
  (let ([z (hash/ref (zpl/children parent) name #f)])
    (and (not z) (zpl/new name (void) parent))))
         
;; A fresh root node for a zpl.
(define (zpl/root/new) (zpl/new 'root (void) #f))

(define (zpl/latest z)
  (and
   (positive? (zpl/count z))
   (hash/ref (zpl/children z) (sub1 (zpl/count z)) #f)))

(define (zpl/children/list z)
  (let ([children (zpl/children z)])
    (for/list ([i (in-range 0 (zpl/count z))]) (hash/ref children i #f))))

;; Find the most recent node at the given depth below self.
;; Returns the node if such exists or #f otherwise.
(define (zpl/at z depth)
  (let loop ([n depth] [z z])
    (if (and (positive? n) z)
        (loop (sub1 n) (zpl/latest z))
        z)))

;; Returns #t if the value of z is set and #f otherwise.
(define (zpl/set? z) (not (void? (zpl/value z))))
;; Erase the value of of z.
(define (zpl/unset z) (zpl/value/set z (void)))

;; Set the value of zpl z iff it is still unset.
;; where setting the value is an atomic action.
;; Returns #t if the value was set and #f if z already holds a value.
(define (zpl/value/set z v)
  (start-atomic)
  (begin0
    (if (zpl/set? z) #f (begin (set-zpl-value! z v) #t))
    (end-atomic)))

(define (walk z f level)
  ;(and (f z level) (hash/and (zpl/children z) (lambda (_ child) (walk child f (add1 level))))))
  (and
   (f z level)
   (for ([child (zpl/children/list z)])
     (walk child f (add1 level)))))

;; Walk beginning at root z applying f at each zpl instance
(define (zpl/walk z f) (walk z f 0))

;; Generate indentation consistent with the given level.
(define (indent output level)
  (let ([n (if (> level 1) (* (sub1 level) 4) 0)])
    (when (positive? n)
      (display (~a "" #:width n) output))))

;; Write the single zpl instance z onto the given output stream
;; with the indentation required for the level.
(define (save z output level)
  (when (positive? level)
    (let* ([v (zpl/value z)]
           [n (zpl/name z)]
           [form
            (if (void? v)
                (format "~a\n" n)
                (format "~a = ~s\n" n v))])
      (indent output level)
      (display form output))))

;; Save the zpl tree rooted at z on the given output port.
(define (zpl/save z output)
  (let ((f (lambda (z level) (save z output level))))
    (zpl/walk z f)))

; Load a zpl from the given input stream.
(define (zpl/load in)
  (let ([root (zpl/root/new)])
    (let loop ([b (read-bytes-line in)] [line# 1])
      (if (eof-object? b)
          (and (zpl/latest root) root) 
          ; Parse the line
          (let ([outcome (line/parse b line#)])
            (cond
              [(not outcome) #f] ; Ill-structured input so indicate failure.
              [(null? outcome)
               ; Empty line so just move on to the next one.
               (loop (read-bytes-line in) (add1 line#))]
              [else
               ; Line is nonempty.
               (let* ([name  (vector-ref outcome 0)]
                      [value (vector-ref outcome 1)]
                      [level (vector-ref outcome 2)]
                      [parent (zpl/at root level)]) ; Navigate to parent for this element.
                 (cond
                   [parent
                    (zpl/new name value parent)
                    (loop (read-bytes-line in) (add1 line#))]
                   [else
                    (display (format "zpl.rkt:zpl/load: (line# ~a) indentation error\n" line#))
                    #f]))]))))))
    
(define (zpl/fold z f seed)
  (let ([children (zpl/children z)])
    (for/fold ([seed (f z seed)]) ([i (in-range 0 (zpl/count z))])
      (zpl/fold (hash/ref children i #f) f seed))))

;; Return the zpl reachable from z via the path.
;; If no such path exists return #f.
(define (zpl/locate z path)
  (if (symbol? path)
      (hash/ref (zpl/children z) path #f)
      (for/fold ([seed z]) ([p (in-list path)] #:break (not seed))
        (hash/ref (zpl/children seed) p #f))))

;; An alternative to zpl/locate that constructs the missing suffix (if any) of the path.
(define (zpl/blaze z path)
  (if (symbol? path)
      (or (hash/ref (zpl/children z) path #f) (zpl/new path (void) z))
      (for/fold ([seed z]) ([p (in-list path)])
        (zpl/blaze seed p))))



;(define (zpl/blaze self path)
;  (let loop ([path path] [z self])
;    (cond
;      [(null? path) z]
;      [(hash/ref (zpl/children z) (car path) #f) => (lambda (child) (loop (cdr path) child))]
;      [else (loop (cdr path) (zpl/unset/new (car path) z))])))

(define (zpl/put self path value)
  (let ([z (call-as-atomic (lambda () (zpl/blaze self path)))])
    (zpl/value/set z value)))

(define (zpl/get self path failure)
  (let ([z (zpl/locate self path)])
    (if (and z (zpl/set? z)) (zpl/value z) failure)))

(define (zpl/children/duplicate z parent)
  (let ([clone (zpl/new (zpl/name z) (zpl/value z) parent)])
    (for ([child (zpl/children/list z)])
      (zpl/children/duplicate child clone))
    clone))

;; Construct an exact duplicate of zpl tree z.
(define (zpl/duplicate z) (zpl/children/duplicate z #f))

;; Attach zpl tree b as a child of zpl node a.
(define (zpl/attach a b)
  (zpl/children/set a (hash/new (zpl/children a) (zpl/name b) b (zpl/count a) b))
  (zpl/count++ a)
  a)
  
;; Interface to Motile serializer.
(define (zpl/flatten z loop)
  (vector
   'struct:zpl
   (zpl/name z)
   (loop (zpl/value z))
   (loop (zpl/children z))
   (loop (zpl/count z))))

;; Rough sanity test on a flattened struct:zpl.
(define (zpl/flatten? x)
  (and
   (vector? x)
   (= (vector-length x) 5)
   (eq? 'struct:zpl (vector-ref x 0))
   (let ([name (vector-ref x 1)])
     (or (symbol? name) (exact-nonnegative-integer? name)))
   (exact-nonnegative-integer? (vector-ref x 4))))
;
;(define (chained? first last)
;  (cond
;    [(not first) (not last)]
;    [(and (zpl? first) (zpl? last))
;     (and
;       ; The last node in the chain of children REALLY is the last.
;      (not (zpl/sibling last))
;      ; The chain must either be a singleton or well-constructed.
;      (or
;       (eq? first last)
;       (chain/intact? first last)))]
;    [else #f])) ; The chain of children is ill-constructed.
;      
;(define (chain/intact? first last)
;  (let loop ([z first] [seen set/eq/null])
;    (cond
;      [(not z) #t] ; End of chain.
;      [(zpl? z) ; Guarantee that each element of the chain is a struct:zpl and the chain is cycle free.
;       (and (not (set/contains? seen z)) (loop (zpl/sibling z) (set/cons seen z)))]
;      [else #f])))
;
(define (zpl/unflatten v f)
  (let ([name  (f (vector-ref v 1))]
        [value (f (vector-ref v 2))]
        [children (f (vector-ref v 3))]
        [count (f (vector-ref v 4))])
     (zpl name value children count)))

  ;
;(define (zpl/acyclic? z members)
;  (let loop ([child (zpl/child/first z)] [members set/eq/null] [acyclic #f])
;    (if child
;        (loop (zpl/sibling child) (zpl/fold child f seed))
;        (and (set/contains? members z) #f)
;)))

;(define (flavor x)
;  (cond
;    [(symbol? x) 'symbol]
;    [(string? x) 'string]
;    [(bytes? x) 'bytes]
;    [(char? x) 'char]
;    [(number? x) 'number]
;    [else 'unknown]))

;(define (zpl/locate/test)
;  (let ([z (zpl/load (open-input-file "./curl+.zpl" #:mode 'text))])
;    (zpl/save z (current-output-port)) (newline)
;    (pretty-write (zpl/tree z)) (newline)
;    (let* ([created (zpl/locate z '(metadata created))]
;           [sweetness (zpl/locate z '(CURL bindings sweetness))])
;      (display (format "~a = ~s\n" (zpl/name created) (zpl/value created)))
;      (display (flavor (zpl/value created))) (newline)
;      (display (format "~a = ~s\n" (zpl/name sweetness) (zpl/value sweetness)))
;      (display (flavor (zpl/value sweetness))) (newline))))
;  
(define (zpl/duplicate/test)
  (let ([z (zpl/load (open-input-file "examples/curl+.zpl" #:mode 'text))])
    (zpl/save z (current-output-port)) (newline)
    (zpl/save (zpl/duplicate z) (current-output-port)) (newline)))
;
;(define (zpl/list/test)
;  (let ([z (zpl/load (open-input-file "./curl+.zpl" #:mode 'text))])
;    (zpl/save z (current-output-port)) (newline)
;    (display (map (lambda (x) (zpl/name x)) (zpl/list z)) (current-output-port)) (newline)))
;
(define (zpl/fold/test)
  (let ([z (zpl/load (open-input-file "./curl+.zpl" #:mode 'text))])
    (zpl/save z (current-output-port)) (newline)
    (display (map (lambda (x) (zpl/name x)) (zpl/fold z cons null)) (current-output-port)) (newline)))

(define Z/TEST
  (let ([root (zpl/root/new)])
    (zpl/put root '(metadata created)        "2014-01-26T23:03:09Z")
    (zpl/put root '(metadata sign-algorithm) 'CURVE-ed25519)
    (zpl/put root '(metadata public-sign-key) #")YI31D3#Y@2yWUmzLu&v8or#)[JavRb!7<pzcDWE")
    (zpl/put root '(metadata signed-on )      "2014-01-26T23:03:09Z")
    (zpl/put root '(metadata signature)       #"Ev)&KJDVlIDl/Vf5dijJa&RmMdq6PY!/ieT&m9HCQeBk)+miJHgjB@!>UwaBDN#s^cd7*+g]Ahl@lxu(")
    (zpl/put root '(CURL island) #".q0Z6tq7Bq0da#B[2v11[$V8sgKVH^y9Z=5TYoHl")
    (zpl/put root '(CURL path)  '(hello kitty cartoon foo bar baz seventy-three 99))
    (zpl/put root '(CURL bindings soda) 'thirsty)
    (zpl/put root '(CURL bindings sweetness) 77)
    (zpl/put root '(CURL access id)  #"2*=hY%biPJoItzBhVH4g=4u9x")
    (zpl/blaze root '(CURL access bindings))
    root))

(define Z1/TEST (zpl/duplicate Z/TEST))


;; racket -l racket/base -t ../zpl-v2.rkt -e '(zpl/load/test)' &
        
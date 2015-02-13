#lang racket/base

;; Tests for (match <expression> <clause> ...+).

(require
 racket/vector
 racket/function
 rackunit
 rackunit/text-ui
 (only-in "../compile/compile.rkt" motile/compile)
 "../baseline.rkt"
 (only-in "../generate/baseline.rkt" motile/call motile/decompile)
 (only-in "../persistent/tuple.rkt"  tuple)
 (only-in "../persistent/vector.rkt" list/vector vector/cons vector/null))

(define compile/start (make-parameter (lambda (e) (motile/call (motile/compile e) ENVIRON/TEST))))

(define-test-suite match/literal
  (test-case
   "Null"
   (check-equal?
    ((compile/start)
     '(let ()
        (match null
          [33      'no]
          ['()     'ok]
          ["silly" 'nack])))
    'ok))
  
  (test-case
   "True"
   (check-equal?
    ((compile/start)
     '(let ()
        (match #t
          ['symbol  'no]
          [#f       'nack]
          [#t       'ok])))
    'ok))
  
  (test-case
   "False"
   (check-equal?
    ((compile/start)
     '(let ()
        (match (> 0 3)
          [#f      'yes]
          ['symbol 'nack]
          [#t      'no])))
    'yes))

  (test-case
   "String"
   (check-equal?
    ((compile/start)
     '(let ((x (string-append "Apple " "or pear")))
        (match x
          ["Apple"         0]
          ["Apple or"      0]
          ["Apple or pear" 1])))
    1))

  (test-case
   "0"
   (check-equal?
    ((compile/start)
     '(let ((x 0))
        (match x
          [33 33]
          [0  12]
          [17 99])))
    12)))

(define-test-suite match/quotation
  (test-case
   "List"
   (check-equal?
    ((compile/start)
     '(let ((x (list 1 2 3)))
        (match x
          ['foo     0]
          ['(1 2 3) 1]
          [_        3])))
    1))
  
  (test-case
   "Vector"
   (check-equal?
    ((compile/start)
     '(let ((x (vector 1 2 3)))
        (match x
          ['foo      0]
          ['#(1 2 3) 1]
          [_         3])))
    1))

  (test-case
   "Symbol"
   (check-equal?
    ((compile/start)
     '(let ((x 'foobar))
        (match x
          ['foo      0]
          ['foobar   1]
          [_         3])))
    1)))

(define-test-suite match/compound
  (test-case
   "Cons/1"
   (check-equal?
    ((compile/start)
     '(let ((x (cons 3 7)))
        (match x
          [(cons 1 5)  6]
          ['symbolic   9]
          [(cons 3 7) 10]
          [_          99])))
    10))
  
  (test-case
   "Cons/2"
   (check-equal?
    ((compile/start)
     '(let ((x (cons 3 7)))
        (match x
          [(cons 3 5)  8]
          ['symbolic   9]
          [(cons 3 b)  b]
          [_          99])))
    7))  

  (test-case
   "List/0"
   (check-equal?
    ((compile/start)
     '(let ((x null))
        (match x
          [(list 3 5) 8]
          [(list)     9]
          [(list b)   b]
          [_          99])))
    9))

  (test-case
   "List/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 17)))
        (match x
          [(list 3 5) 8]
          [(list)     9]
          [(list b)   b]
          [_          99])))
    17))

  (test-case
   "List/2"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 7)))
        (match x
          [(list 3 5)  8]
          ['symbolic   9]
          [(list 3 b)  b]
          [_          99])))
    7))  

  (test-case
   "List/3"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 7 11)))
        (match x
          [(list 3 7)    8]
          ['symbolic     9]
          [(list 3 b 11) b]
          [_            99])))
    7))
  
  (test-case
   "list with replicated variable"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 7 3)))
        (match x
          [(list 3 7)    8]
          ['symbolic     9]
          [(list a b a)  a]
          [_            99])))
    3))        

  (test-case
   "List-rest/1"
   (check-equal?
    ((compile/start)
     '(let ((x (cons 3 7)))
        (match x
          [(cons 3 5)       8]
          ['symbolic        9]
          [(list-rest 3 7) 10]
          [_               99])))
    10))

  (test-case
   "List-rest/3"
   (check-equal?
    ((compile/start)
     '(let ((x (cons 3 (cons 7 11))))
        (match x
          [(cons 3 7)       8]
          ['symbolic        9]
          [(list-rest 3 a)  a]
          [_               99])))
    '(7 . 11)))
  
 (test-case
   "List-rest/4"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 7 11 13 17)))
        (match x
          [(cons 3 7)       8]
          ['symbolic        9]
          [(list-rest 3 7 a)  a]
          [_               99])))
    '(11 13 17)))
  
  (test-case
   "Tuple/0"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple)))
        (match x
          ['()        'nope]
          [(list _)   'wrong]
          [(tuple)   'right]
          [_          'bad])))
    'right))
                      
  (test-case
   "Tuple/1"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 3)))
        (match x
          ['()        'nope]
          [(tuple 7) 'wrong]
          [(tuple a) (* 3 a)]
          [_          'bad])))
    9)) 
  
  (test-case
   "Tuple/3"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 1 2 3)))
        (match x
          ['()           'nope]
          [(tuple _)     'wrong]
          [(tuple a 2 c) (+ a 2 c)]
          [_             'bad])))
    6))  
  
  (test-case
   "Tuple/7"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 1 2 3 4 5 6 7)))
        (match x
          ['()                   'nope]
          [(tuple _)             'wrong]
          [(tuple a 2 b _ _ _ c) (list a b c)]
          [_                     'bad])))
    '(1 3 7)))

  (test-case
   "Term/0"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 'empty)))
        (match x
          ['()                 'nope]
          [(list _)            'wrong]
          [(term (head empty)) 'right]
          [_                   'bad])))
    'right))
  
  (test-case
   "Term/1"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 'foo 3)))
        (match x
          ['()        'nope]
          [(term (head bar) 3) 'wrong]
          [(term (head foo) a) (* 3 a)]
          [_                   'bad])))
    9)) 
  
  (test-case
   "Term/3"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 'sequence 1 2 3)))
        (match x
          ['()                          'nope]
          [(term (head sequence) _)     'wrong]
          [(term (head sequence) a 2 c) (+ a 2 c)]
          [_                            'bad])))
    6)) 
  
  (test-case
   "Term/7"
   (check-equal?
    ((compile/start)
     '(let ((x (tuple 'silly 1 2 3 4 5 6 7)))
        (match x
          ['()                               'nope]
          [(term (head silly) _)             'wrong]
          [(term (head silly) a 2 b _ _ _ c) (list a b c)]
          [_                                 'bad])))
    '(1 3 7)))

  (test-case
   "Vector/0"
   (check-equal?
    ((compile/start)
     '(let ((x vector/null))
        (match x
          ['()        'nope]
          [(list _)   'wrong]
          [(vector)   'right]
          [_          'bad])))
    'right))
                      
                      
  (test-case
   "Vector/1"
   (check-equal?
    ((compile/start)
     '(let ((x (vector/cons vector/null 3)))
        (match x
          ['()        'nope]
          [(vector 7) 'wrong]
          [(vector a) (* 3 a)]
          [_          'bad])))
    9))
  
  (test-case
   "Vector/3"
   (check-equal?
    ((compile/start)
     '(let ((x (list/vector vector/null (list 1 2 3))))
        (match x
          ['()            'nope]
          [(vector _)     'wrong]
          [(vector a 2 c) (+ a 2 c)]
          [_              'bad])))
    6)) 
  
  (test-case
   "Vector/7"
   (check-equal?
    ((compile/start)
     '(let ((x (list/vector vector/null (list 1 2 3 4 5 6 7))))
        (match x
          ['()            'nope]
          [(vector _)     'wrong]
          [(vector a 2 b _ _ _ c) (list a b c)]
          [_              'bad])))
    '(1 3 7)))
  
  (test-case
   "app/1"
   (check-equal?
    ((compile/start)
     '(let ((x (vector 1 2 3)))
        (match x
          ['()            'nope]
          [(vector _)     'wrong]
          [(app
            (lambda (v) (apply + (vector->list v)))
            6)            99]
          [_              'bad])))
    99))
  
  (test-case
   "record/1"
   (check-equal?
    ((compile/start)
     '(let ((x (record sample a 1 b 2 c 3)))
        (match x
          ['() 'nope]
          [(vector _ _ _) 'wrong]
          [(record sample a _ b N) N]
          [_ 'bad])))
    2))

  (test-case
   "record/2"
   (check-equal?
    ((compile/start)
     '(let ((x (record sample a 1 b 2 c 3)))
        (match x
          [(record sample a (and A (? even? A))) 'even]
          [(record sample a (and A (? odd?  A))) 'odd]
          [_ 'bad])))
    'odd))
)


(define-test-suite match/logical
  (test-case
   "And/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17) 'no]
          [(and)         'yes]
          [_             'bad])))
    'yes))
  
  (test-case
   "And/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17) 'no]
          [(and)         'yes]
          [_             'bad])))
    'yes))
  
  (test-case
   "And/2"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17)      'no]
          [(and (list a _ b)) (+ a b)]
          [_                  'bad])))
    10))
  
  (test-case
   "And/3"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17)                      'no]
          [(and (list a _ b) (list _ 'bar _)) 'wrong]
          [(and (list a _ b) (list _ c    _)) (list (+ a b) c)]
          [_                                  'bad])))
    '(10 foo)))
  
  
  (test-case
   "Or/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17) 'no]
          [(or)          'bad]
          [_             'yes])))
    'yes)) 
  
  (test-case
   "Or/2"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17)     'no]
          [(or (list a b c)) (list (+ a c) b)]
          [_                 'wrong])))
    '(10 foo)))
  
  (test-case
   "Or/3"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo 7)))
        (match x
          [(list a _ 17)                    'no]
          [(or (vector a b c) (list a b c)) (list (+ a c) b)]
          [_                                'wrong])))
    '(10 foo)))
  
  (test-case
   "Not/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo #f)))
        (match x
          [(list a _ #t)      'no]
          [(list a b (not #t)) (list b a)]
          [_                  'wrong])))
    '(foo 3)))
  
  (test-case
   "?/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo #t)))
        (match x
          [(list a b (not c)) 'no]
          [(list (? integer? a) (? symbol? b) (? boolean? c)) (list c b a)]
          [_                  'wrong])))
    '(#t foo 3))))

(define-test-suite match/guard
  (test-case
   "Guard/1"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo #f)))
        (match x
          [(list a _ #t)      'no]
          [(list a b (not #t)) (where (> a 3)) (list b a)]
          [_                  'ok]))) ; The guard worked.
    'ok))

  (test-case
   "Guard/2"
   (check-equal?
    ((compile/start)
     '(let ((x (list 3 'foo #f)))
        (match x
          [(list a _ #t)      'no]
          [(list a b (not #t)) (where (> a 2)) (list b a)]
          [_                  'bad])))
    '(foo 3))))

(define (tests/match/all)
  (run-tests match/literal)
  (run-tests match/quotation)
  (run-tests match/compound)
  (run-tests match/logical)
  (run-tests match/guard))
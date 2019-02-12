#lang racket

;; figures out which method should be used to evaluate this
(define m-what-type
  (lambda (exp s)
    (exp #|TODO: UPDATE THIS!!!|#)))

;; Code a function that can take in expression of numbers and operators and return the value
;; e.g. (3 + (4 / 2))
;;      (1 + 2)
;; The operators are +, -, *, /, %, and division is integer division
(define mvalue
  (lambda (exp)
    (cond
      [(null? exp)             (error 'undefined "undefined expression")]
      [(number? exp)           exp]
      [(eq? (operator exp) '+) (+         (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '-) (-         (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '*) (*         (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '/) (quotient  (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '%) (remainder (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [else                    (error 'undefined "undefined expression")])))

;; Code a function that can take in an expression such as (5 < 2) and return true/false
;; Supports ==, !=, <, >, <=, >=, &&, ||, !
(define mcondition
  (lambda (exp s) ; exp = expression, s = state
    (cond
      ; null checking
      [(null? exp)                    (error 'undefined "undefined expression")]
      [(not (pair? exp))              exp]
      [(null? (operator exp))    (mvalue exp s)]
      [(null? (operator exp))     (mvalue exp s)]
      [(null? (operator exp))    (mvalue exp s)]

      ; condition checking (&&, ||, !)
      [(eq? (operator exp) '||)  (or  (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (operator exp) '&&)  (and (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (car exp) '!)             (not (mcondition (cdr exp) s))]

      ; equality/inequality operator checking (==, !=, <, >, <=, >=)
      [(eq? (operator exp) '==)  (eq? (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (operator exp) '!=)  (not (eq? (mcondition (left-operand exp) s) (mcondition (right-operand exp) s)))]
      [(eq? (operator exp) '<)   (< (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (operator exp) '>)   (> (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (operator exp) '<=)  (<= (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (operator exp) '>=)  (>= (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]

      ; oh no
      [else                           (error 'undefined "undefined expression")])))

;; implementing if statement
(define mifstatement
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(mcondition (loop-condition exp) s) (m-what-type (loop-body exp) s)]
      [(not (null? (else-statement exp)))  (m-what-type (else-statement exp) s)])))

;; implementing while loop
(define whileloop
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(mcondition (loop-condition exp) s) (whileloop exp s #|TODO: SOMETHING TO UPDATE THE STATE IN HERE!!!|#)])))

;; Abstration
;; for if statements
(define loop-type-id car) ; e.g. if, while, etc.
(define else-statement cadddr) ; else statement, if it exists
(define loop-condition cadr)
(define loop-body caddr)

;; for value operations
(define operator car)
(define left-operand cadr)
(define right-operand caddr)

; (5 + 2 <= 7)
; ((5 + 2) <= 7)


;;;;**********TESTING**********

;; Performs a quick test to see if the test passed or failed and prints info about test if failure
(define pass?
  (lambda (actual expected)
    (if (equal? actual expected)
        (display 'Pass)
        (donothing (display "Fail {actual} ") (display actual)      ; displays info about the failed test
                   (display " != {expected} ") (display expected)))
    (newline)))

;; This function does nothing but allows for all the displays of a failed test to occure without an error :)
(define donothing ; there is probably a better way to do this but as this isn't part of the grade, ̄\_(ツ)_/̄
  (lambda (a b c d)
    '())) ; literly does nothing (like what I wish I was doing xD)

;; Performs all the tests needed to prove the validity of the functions, I love this function
(define (test)

  ;Example:
  ;(diplay "Test #{test number} {test name}") (newline)
  ;(pass? {actual} {expected})
  ;(newline)

  (display "Test #1 mvalue") (newline)                                                ;Test mvalue
  (pass? (mvalue '(3 + (4 / 2))) 5)                                                       ; 1/2
  (pass? (mvalue '((3 * 2) + (4 / (2 % 3)))) 8)                                           ; 2/2
  (newline)

  (display "Test 2 exp") (newline)                                                    ;Test start
  (pass? (start "Test1.txt") '((var x) (= x 10) (var y (+ (* 3 x) 5))                     ; 1/1
                                   (while (!= (% y x) 3) (= y (+ y 1)))
                                   (if (> x y) (return x)
                                       (if (> (* x x) y) (return (* x x))
                                           (if (> (* x (+ x x)) y)
                                               (return (* x (+ x x)))
                                               (return (- y 1)))))))
  (newline)


  ) ;left hanging for easy test addition

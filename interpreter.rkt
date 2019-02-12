#lang racket

(require "simpleParser.rkt") ; loads simpleParser.rkt, which itself loads lex.rkt

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define start
  (lambda (filename)
    (parser filename)))
    

;; Code a function that can take in expression of numbers and operators and return the value
;; e.g. (3 + (4 / 2))
;;      (1 + 2)
;; The operators are +, -, *, /, %, and division is integer division
(define mvalue
  (lambda (exp)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(number? exp) exp]
      [(eq? (operator exp) '+) (+         (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '-) (-         (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '*) (*         (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '/) (quotient  (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '%) (remainder (mvalue (left-operand exp)) (mvalue (right-operand exp)))])))

(define operator caddr)
(define left-operand car)
(define right-operand cadr)




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
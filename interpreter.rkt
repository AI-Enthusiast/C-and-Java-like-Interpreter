#lang racket

; Code a function that can take in expression of numbers and operators and return the value
; e.g. (3 + (4 / 2))
;      (1 + 2)
; The operators are +, -, *, /, %, and division is integer division
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
  (display "Test #1 exp") (newline)                                                ;Test exp
  (pass? (exp ((3 * 2) + (4 / (2 % 3)))) 8)                                              ; 1/1
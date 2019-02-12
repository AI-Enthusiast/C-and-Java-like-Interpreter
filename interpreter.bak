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
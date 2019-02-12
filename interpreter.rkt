#lang racket

; Code a function that can take in expression of numbers and operators and return the value
; e.g. (3 + (4 / 2))
;      (1 + 2)
; The operators are +, -, *, /, %, and division is integer division
(define mvalue
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(number? exp) exp]
      [(eq? (operator exp) '+) (+         (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '-) (-         (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '*) (*         (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '/) (quotient  (mvalue (left-operand exp)) (mvalue (right-operand exp)))]
      [(eq? (operator exp) '%) (remainder (mvalue (left-operand exp)) (mvalue (right-operand exp)))])))

; Code a function that can take in an expression such as (5 < 2) and return true/false
; ==, !=, <, >, <=, >=
; &&, ||, !
(define mcondition
  (lambda (exp s) ; exp = expression, s = state
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(eq? (bool_operator exp) '||) (or  (mcondition (left-operand) s) (mcondition (right-operand) s))]
      [(eq? (bool_operator exp) '&&) (and (mcondition (left-operand) s) (mcondition (right-operand) s))]
      [(eq? (car exp) '!)            (not (mcondition (cdr exp) s))]
      []

; for mcondition
(define bool_operator caddr)

; for mvalue
(define operator caddr)
(define left-operand car)
(define right-operand cadr)
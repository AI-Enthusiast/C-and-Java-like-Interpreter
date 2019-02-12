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
      [(eq? (operator exp) '+) (+         (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '-) (-         (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '*) (*         (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '/) (quotient  (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))]
      [(eq? (operator exp) '%) (remainder (mvalue (left-operand exp) s) (mvalue (right-operand exp) s))])))

; Code a function that can take in an expression such as (5 < 2) and return true/false
; ==, !=, <, >, <=, >=
; &&, ||, !
(define mcondition
  (lambda (exp s) ; exp = expression, s = state
    (cond
      [(null? exp)                    (error 'undefined "undefined expression")]
      [(not (pair? exp))              exp]
      [(null? (bool_operator exp))    (mvalue exp s)]
      [(null? (left-operand exp))     (mvalue exp s)]
      [(null? (right-operand exp))    (mvalue exp s)]

      [(eq? (bool_operator exp) '||)  (or  (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (bool_operator exp) '&&)  (and (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (car exp) '!)             (not (mcondition (cdr exp) s))]

      [(eq? (bool_operator exp) '==)  (eq? (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (bool_operator exp) '!=)  (not (eq? (mcondition (left-operand exp) s) (mcondition (right-operand exp) s)))]
      [(eq? (bool_operator exp) '<)   (< (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (bool_operator exp) '>)   (> (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (bool_operator exp) '<=)  (<= (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]
      [(eq? (bool_operator exp) '>=)  (>= (mcondition (left-operand exp) s) (mcondition (right-operand exp) s))]

      [else                           (mvalue exp s)])))

; for mcondition
(define bool_operator cadr)

; for mvalue
(define operator cadr)
(define left-operand car)
<<<<<<< HEAD
(define right-operand caddr)

; (5 + 2 <= 7)
; ((5 + 2) <= 7)

(define right-operand cadr)
(define vars car)
(define vals cadr)

;;define state with abstration as
;((x, y, ...) (4, 6, ...))
;state is s
;methods to implement
;look up binding
;remove binding
;update existing binding

(define m-update
  (lambda (var update-val s)
    (cond
      [(base case)]
      [(eq? 

(define var-assign
  (lambda (val location s)
    (cons ((car s) (assign val location (vals lis))))))

(define assign
  (lambda (val location vals)
    (cond
      [(null? vals) "error"]
      [(eq? val (car
                                 



                     



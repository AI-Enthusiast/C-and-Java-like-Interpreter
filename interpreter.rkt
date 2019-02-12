#lang racket

(require racket/trace)

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
(define right-operand caddr)

; (5 + 2 <= 7)
; ((5 + 2) <= 7)

(define vars car)
(define vals cadr)
(define nextvar caar)
(define nextval caadr)

;;define state with abstration as
;((x, y, ...) (4, 6, ...))
;state is s
;methods to implement
;look up binding
;remove binding
;update existing binding


(define m-lookup
  (lambda (var s)
    (cond
      [(null? (vars s)) "error, does not exist"]
      [(equal? var (nextvar s)) (nextval s)] 
      [else (m-lookup var (cons (cdr (vars s)) (cons (cdr (vals s)) '())))])))


 
(define m-update
  (lambda (var update-val s)
    (cond
      [(not (number? (var m-lookup))) "error"]
      [else (update var update-val s)])))

#|(define update
  (lambda (|#


#|(define var-assign
  (lambda (val location s)
    (cons ((car s) (assign val location (vals lis))))))|#

#|(define assign
  (lambda (val location vals)
    (cond
      [(null? vals) "error"]
      [(eq? val (car|#
                                 



                     



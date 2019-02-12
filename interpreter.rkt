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
  (lambda (exp s)
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


                     



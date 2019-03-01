#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Will run a txt file containing code by using the run function (run "Filename.txt")

(provide (all-defined-out))         ; allows for testing to be done in interpreter-testing.rkt
(require "simpleParser.rkt")        ; loads simpleParser.rkt, which itself loads lex.rkt
(require racket/trace)              ; for debugging

;; Runs the filename, should be provided in quotes
;; e.g. (run "Tests/Test1.txt")
(define run
  (lambda (filename)
    (m-state (parse-t filename) empty-state)))

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define parse-t
  (lambda (filename)
    (parser filename)))

;; Executes code, returns updated state
(define m-state
  (lambda (exp s)
    (cond
      [(null? exp)                         s]
      [(null? (rest-of-body exp))          (m-what-type (first-statement exp) s)]
      [(not (list? (first-statement exp))) (m-what-type (first-statement exp) s)]
      [else                                (m-state (rest-of-body exp)
                                                    (m-what-type (first-statement exp) s))])))

;; Returns state with most recent state popped off
(define m-pop
  (lambda (s)
    (popped-state s)))

;; Returns state with new empty state pushed on
(define m-push
  (lambda (s)
    (cons empty-state s)))

;; Figures out which method should be used to evaluate this, and evaluates this
;; Returns updated state
(define m-what-type
  (lambda (exp s)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(or (null? exp) (not (pair? exp)))    s]

      ; conditional statement checking (if/while/etc.)
      [(eq? (statement-type-id exp) 'if)     (m-if-statement exp s)]
      [(eq? (statement-type-id exp) 'while)  (m-while-loop exp s)]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)    (m-var-dec exp s)]

      ; is it an assignment
      [(eq? (statement-type-id exp) '=)      (m-assign exp s)]

      ; is it a return statement
      [(eq? (statement-type-id exp) 'return) (m-return (statement-body exp) s)]

      ; oh no
      [else                                  (error 'undefined "undefined expression")])))

;; Code a function that can take in expression of numbers and operators and return the value
;; e.g. (+ 3 (/ 4 2))
;;      (+ 1 2)
;; The operators are +, -, *, /, %, and division is integer division
(define m-value
  (lambda (exp s)
    (cond
      ; null checking
      [(null? exp)                            (error 'undefined "undefined expression")]
      [(number? exp)                          exp] ; if it's a number, return that number
      [(and (not (pair? exp)) (boolean? exp)) exp] ; if it's a boolean, return the boolean

      ; boolean checking
      [(eq? exp 'true)                        #t] ; true
      [(eq? exp 'false)                       #f] ; false
      
      ; more complex boolean expression (e.g. 10 >= 20 || 10 == a)
      [(and (pair? exp) (am-i-boolean exp))   (m-condition exp s)] 

      ; variable checking
      [(not (pair? exp))                      (m-lookup exp s)]

      ;operators
      [(eq? (operator exp) '+) (+         (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(and (eq? (operator exp) '-) (null? (cddr exp))) ; handle negitive numbers
                               (* -1 (m-value (left-operand exp) s))]
      [(eq? (operator exp) '-) (-         (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '*) (*         (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '/) (quotient  (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '%) (remainder (m-value (left-operand exp) s) (m-value (right-operand exp) s))]

      ; oh no
      [else                    (error 'undefined "undefined expression")])))

;; Code a function that can take in an expression such as (< 5 2) and return true/false
;; Supports ==, !=, <, >, <=, >=, &&, ||, !
(define m-condition
  (lambda (exp s) ; exp = expression, s = state
    (cond
      ; null checking
      [(null? exp)               (error 'undefined "undefined expression")]
      [(not (pair? exp))         (m-value exp s)]
      [(null? (operator exp))    (m-value exp s)]

      ; condition checking (&&, ||, !)
      [(eq? (operator exp) '||)  (or  (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '&&)  (and (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '!)   (not (m-condition (left-operand exp) s))]

      ; equality/inequality operator checking (==, !=, <, >, <=, >=)
      [(eq? (operator exp) '==)  (eq? (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '!=)  (not (eq? (m-condition (left-operand exp) s)
                                           (m-condition (right-operand exp) s)))]
      [(eq? (operator exp) '<)   (<   (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '>)   (>   (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '<=)  (<=  (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '>=)  (>=  (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]

      ; oh no
      [else                      (m-value exp s)])))

;; Implementing if statement
(define m-if-statement
  (lambda (exp s)
    (cond
      ; invalid expression
      [(null? exp)         (error 'undefined "undefined expression")]

      ; run the loop of the body (body is multiple statements)
      [(and (m-condition (loop-condition exp) s) (pair? (first-statement (loop-body exp))))
                           (m-state (loop-body exp) s)]

      ; run the loop of the body (body is single statement)
      [(m-condition (loop-condition exp) s)
                           (m-what-type (loop-body exp) s)]
      
      ; if there's no else statement, return the state
      [(null? (cdddr exp)) s] 

      ; run the else of the body (body is multiple statements)
      [(and (not (null? (else-statement exp))) (pair? (first-statement (else-statement exp))))
                           (m-state (else-statement exp) s)]

      ; run the else of the body (body is single statement)
      [(not (null? (else-statement exp)))
                           (m-what-type (else-statement exp) s)])))

;; Implementing while loop
(define m-while-loop
  (lambda (exp s)
    (cond
      ; invalid expression
      [(null? exp)
           (error 'undefined "undefined expression")]

      ; runs the while loop (body is multiple statements)
      [(and (m-condition (loop-condition exp) s) (pair? (first-statement (loop-body exp))))
           (m-while-loop exp (m-state (loop-body exp) s))]

      ; runs the while loop (body is single statement)
      [(m-condition (loop-condition exp) s)
           (m-while-loop exp (m-what-type (loop-body exp) s))]

      ; otherwise, returns initial state
      [else s])))

;; Takes an assinment and a state
;; Returns the updated state
(define m-assign
  (lambda (assign s)
      (if (not (number? (locate (variable assign) 0 s)))
                           (error "use before declaration")
          (m-update (variable assign) (m-value (expression assign) s) s))))

;; Takes a variable declaration and a state
;; Returns the updated state
(define m-var-dec
  (lambda (dec s)
    (cond
      ; check variable not already declared
      [(number? (locate (variable dec) 0 s)) (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))              (m-add (variable dec) s)]
      ; need to add value as well
      [else                                  (m-update (variable dec)
                                                       (m-value (expression dec) s)
                                                       (m-add (variable dec) s))])))

#|
define state with abstration as
((x, y, ...) (4, 6, ...))
state is s
methods for state
m-lookup - looks up variable's value, returns value
m-update - updates variable's value, returns updated state
m-add - adds uninitilized variable to state, returns updated state
m-remove - removes a variable and it's value from state, returns updated state
|#

;; Takes a variable and a state
;; Returns the value of the variable, or error message if it does not exist
;; Will return "init" if not yet initilized
(define m-lookup
  (lambda (var s)
    (cond
      [(or (null? s) (null? (vars s)))                         (error "use before declared")]
      [(and (equal? var (nextvar s)) (eq? "init" (nextval s))) (error "use before assignment")]
      [(equal? var (nextvar s))                                (nextval s)]
      [else                                                    (m-lookup var
                                                                         (list (rest-of-vals (vars s))
                                                                               (rest-of-vals (vals s))))])))

;; Takes a variable, the value it is to be updated to, and the state, returns the updated state
(define m-update
  (lambda (var update-val s)
    (cond
      [(or (null? s) (null? (vars s)))  "error"]
      [(not (number? (locate var 0 s))) "error"]
      [else                             (list (vars s) (update var update-val s))])))

;; Takes the value to be updated, the location of the value and the
;; Updates the variable at the location with the new value, returns the updated state
(define update
  (lambda (var update-val s)
    (cond
      [(eq? var (nextvar s))  (cons update-val (rest-of-vals (vals s)))]
      [else                   (cons (nextval s) (update var update-val (list (rest-of-vals (vars s))
                                                                             (rest-of-vals (vals s)))))])))

;; Finds the location of the variable's value in the state
;; Takes the variable it is locating, a counter and a state
(define locate
  (lambda (var counter s)
    (cond
      [(or (null? s)(null? (vars s)))
       "error"]
      [(eq? var (nextvar s))
       counter]
      [else
       (locate var (+ counter 1) (cons (rest-of-vals (vars s)) (cons (rest-of-vals (vals s)) '())))])))

;; Takes a varaiable and a state, adds it to a state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
;; Returns the updated state, if used before assigned, should result in error
;; Will accept an empty state '(), a state formated '(()()) or a state formated '((var1 ...)(val1 ...))
(define m-add
  (lambda (var s)
      (cond
        [(or (null? s) (null? (vars s)))   (list (list var) (list "init"))]
        [(eq? (locate var 0 s) "init")     (m-update var "init" s)]
        [else                              (list (cons  var (vars s)) (cons "init" (vals s)))])))

;; Takes a variable and a state
;; Returns the updated state with the variable and assosiated value removed
(define m-remove
  (lambda (var s)
    (if (not (number? (locate var 0 s)))
        "error"
        (list (remove var (vars s)) (remove-val var s)))))

;; Takes a variable and a state
;; Returns the value list with the value attached to the variable removed
(define remove-val
  (lambda (var s)
    (if (eq? var (nextvar s))
        (rest-of-vals (vals s))
        (cons (nextval s) (remove-val var (list (rest-of-vals (vars s)) (rest-of-vals (vals s))))))))

;; Takes an atom and a list
;; Returns the list with the first instance of the atom removed
(define remove
  (lambda (a lis)
    (cond
      [(null? lis)                '()]
      [(eq? a (first-val lis))    (rest-of-vals lis)]
      [else                       (cons (first-val lis) (remove a (rest-of-vals lis)))])))

;; Determines if an expression is boolean
(define am-i-boolean
  (lambda (exp)
    (cond
      [(eq? (operator exp) '||)  #t]
      [(eq? (operator exp) '&&)  #t]
      [(eq? (operator exp) '!)   #t]
      [(eq? (operator exp) '==)  #t]
      [(eq? (operator exp) '!=)  #t]
      [(eq? (operator exp) '<)   #t]
      [(eq? (operator exp) '>)   #t]
      [(eq? (operator exp) '<=)  #t]
      [(eq? (operator exp) '>=)  #t]
      [else #f])))

;; Takes an expression and a state
;; Returns it as if it where in C/Java
(define m-return
  (lambda (exp s)
    (cond
      [(eq?   exp #t) "true"]
      [(eq?   exp #f) "false"]
      [(and (pair? exp) (am-i-boolean exp)) (m-return (m-condition exp s) s)]
      [(pair? exp)    (m-value exp s)]
      [(eq? (m-value exp s) #t) "true"]
      [(eq? (m-value exp s) #f) "false"]
      [else           (m-value exp s)])))

;;;;**********ABSTRACTION**********
(define statement-type-id car) ; e.g. if, while, var, etc.
(define statement-body cadr)   ; e.g. the body of a return statement

; for if statements
(define else-statement cadddr) ; else statement, if it exists
(define loop-condition cadr)
(define loop-body caddr)

; for value operations
(define left-operand cadr)

; for m-value
(define operator car)
(define right-operand caddr)

;for m-var-dec
(define assignment cddr)
(define variable cadr)
(define expression caddr)

; for remove
(define first-val car)

; for state computation
(define vars car)
(define vals cadr)
(define nextvar caar)
(define rest-of-vals cdr)
(define nextval caadr)

; for running/state
(define empty-state '(() ()))
(define first-statement car)
(define rest-of-body cdr)
(define popped-state cdr)

;; Thank you, sleep well :)
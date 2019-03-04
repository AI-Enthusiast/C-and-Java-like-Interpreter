#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #21: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Will run a txt file containing code by using the run function (run "Filename.txt")
;;;; Order of inputs for ALL m-state and m-state like things
;;;; m-state(exp, s, return, break, continue, try, catch, finally)

(provide (all-defined-out))         ; allows for testing to be done in interpreter-testing.rkt
(require "simpleParser.rkt")        ; loads simpleParser.rkt, which itself loads lex.rkt
(require racket/trace)              ; for debugging

;; Runs the filename, should be provided in quotes
;; e.g. (run "Tests/Test1.txt")
(define run
  (lambda (filename)
    (m-state (parse-t filename) empty-state
             (lambda (v) v) ;; CPS
             (lambda (v) v)
             (lambda (v) v)
             (lambda (v) v)
             (lambda (v) v)
             (lambda (v) v))))

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define parse-t
  (lambda (filename)
    (parser filename)))

;; Executes code, returns updated state
(define m-state
  (lambda (exp s return break continue try catch finally)
    (cond
      [(null? exp)                         s]
      [(not (list? (first-statement exp))) (m-what-type                  exp  s return break continue try catch finally)]
      [(null? (rest-of-body exp))          (m-what-type (first-statement exp) s return break continue try catch finally)]
      [else                                (m-state (rest-of-body exp)
                                                    (m-what-type (first-statement exp) s return break continue try catch finally) return break continue try catch finally)])))

;; Returns state with most recent state popped off
(define m-pop
  (lambda (s)
    (nextlayer s))) ; TODO popped state does not exist

;; Returns state with new empty state pushed on
(define m-push
  (lambda (s)
    (cons new-layer s)))


;; Figures out which method should be used to evaluate this, and evaluates this
;; Returns updated state
;(define m-what-type
;  (lambda (exp s)
;    (call/cc
;     (lambda (return break continue try catch finally)
;       (m-what-type-cc exp s return break continue try catch finally)))))

;TODO add break and continue
(define m-what-type
  (lambda (exp s return break continue try catch finally)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(or (null? exp) (not (pair? exp)))    s]

      ; is it a new block
      [(eq? (first-statement exp) 'begin)    (m-pop (m-state (rest-of-body exp) (m-push s) return break continue try catch finally))]

      ; conditional statement checking (if/while/etc.)
      [(eq? (statement-type-id exp) 'if)     (m-if-statement exp s return break continue try catch finally)]
      [(eq? (statement-type-id exp) 'while)  (m-while-loop   exp s return break continue try catch finally)]

      ; is it a break
      [(eq? (statement-type-id exp) 'break)  (break #| DO SOMETHING |#)]

      ;is it a continue
      [(eq? (statement-type-id exp) 'continue) (continue #| DO NOTHING |#)]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)    (m-var-dec exp s)]

      ; is it an assignment
      [(eq? (statement-type-id exp) '=)      (m-assign exp s)]

      ; is it a return statement
      [(eq? (statement-type-id exp) 'return) (m-return (statement-body exp) s return finally)]

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
  (lambda (exp s return break continue try catch finally)
    (cond
      ; invalid expression
      [(null? exp)         (error 'undefined "undefined expression")]

      ; run the loop of the body
      [(m-condition (loop-condition exp) s) (m-state (loop-body exp) s return break continue try catch finally)]

      ; if there's no else statement, return the state
      [(null? (cdddr exp)) s]

      ; run the else of the body
      [else (m-state (else-statement exp) s return break continue try catch finally)])))

;; Implementing while loop
(define m-while-loop
  (lambda (exp s return break continue try catch finally)
    (cond
      ; invalid expression
      [(null? exp)
           (error 'undefined "undefined expression")]

      ; runs the while loop (body is multiple statements)
      [(and (m-condition (loop-condition exp) s) (pair? (first-statement (loop-body exp))))
           (m-while-loop exp (m-state (loop-body exp) s return break continue try catch finally) return break continue try catch finally)]

      ; runs the while loop (body is single statement)
      [(m-condition (loop-condition exp) s)
           (m-while-loop exp (m-what-type (loop-body exp) s return break continue try catch finally) return break continue try catch finally)]

      ; otherwise, returns initial state
      [else s])))

;; Takes an assinment and a state
;; Returns the updated state
(define m-assign
  (lambda (assign s)
      (if (not (locate (variable assign) s))
                           (error "use before declaration")
          (m-update (variable assign) (m-value (expression assign) s) s))))

;; Takes a variable declaration and a state
;; Returns the updated state
(define m-var-dec
  (lambda (dec s)
    (cond
      ; check variable not already declared
      [(locate (variable dec) s) (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))              (m-add (variable dec) s)]
      ; need to add value as well
      [else                                  (m-update (variable dec)
                                                       (m-value (expression dec) s)
                                                       (m-add (variable dec) s))])))

#|
define state with abstration as one of the following
'()
'((()()))
'(((x, y, ...) (4, 6, ...)))
'(((x, y, ...) (4, 6, ...))((a, b, ...)(1, 2, ...))((...)(...)))
state is s
methods for state
m-lookup - looks up variable's value, returns value found at highest layer
m-update - updates variable's value on the first location the variable is found, returns updated state
m-add - adds uninitilized variable to state (on topmost layer), returns updated state
m-remove - removes a variable and it's value from the first layer it is found at, returns updated state
|#

;; Takes a variable and a state
;; Returns the value of the variable at the first instance it is found, or error message if it does not exist
;; Will return "init" if not yet initilized
(define m-lookup
  (lambda (var s)
    (cond
      [(null? s) (error "use before declared")]
      [(null? (vars s)) (m-lookup var (nextlayer s))]
      [(and (equal? var (nextvar s)) (eq? "init" (nextval s))) (error "use before assignment")]
      [(equal? var (nextvar s))                                (nextval s)]
      [else                                                    (m-lookup var (next-part s))])))


;; Takes a variable, the value it is to be updated to, and the state, returns the updated state
(define m-update
  (lambda (var update-val s)
    (cond
      [(null? s) "error"]
      [(not (locate var s)) "error"]
      [else (update var update-val s)])))

;;takes a variable, the value it is to be updated to and a state
;;returns the state with the variable's value updated at the first instance of the variable
(define update
  (lambda (var update-val s)
    (cond
      [(null? s) '()]
      [(null? (vars s)) (cons new-layer (update var update-val (nextlayer s)))]
      [(local-locate var s) (cons (list (vars s) (local-update var update-val s)) (nextlayer s))]
      [else (cons (layer s) (update var update-val (nextlayer s)))])))

;;takes a variable and a state
;; returns true if the variable exists on the top layer of the state, false otherwise
(define local-locate
  (lambda (var s)
    (cond
      [(or (null? s) (null? (vars s)))  #f]
      [(eq? var (nextvar s)) #t]
      [else (local-locate var (next-part s))])))

;;takes a variable, the value to be updated and the state with the top layer the layer to be updated
;;returns the state with the layer updated with the new value for the variable
(define local-update
  (lambda (var update-val s)
    (cond
      [(eq? var (nextvar s))  (cons update-val (rest-of (vals s)))]
      [else                   (cons (nextval s) (local-update var update-val (next-part s)))])))


;; returns #t if the value is found in the state, #f otherwise
;; Takes the variable it is locating, a counter and a state
(define locate
  (lambda (var s )
    (cond
      [(null? s) #f]
      [(null? (vars s)) (locate var (nextlayer s))]
      [(eq? var (nextvar s)) #t]
      [else (locate var (next-part s))])))


;; Takes a varaiable and a state, adds it to a state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
;; Returns the updated state, if used before assigned, should result in error
;; Will accept an empty state '(), a state formated '((()())) or a state formated '(((var1 ...)(val1 ...))((varx ...) (valx ...)))
(define m-add
  (lambda (var s)
      (cond
        [(null? s)    (list (list (list var) (list "init")))]
        [(null? (vars s)) (cons (list (list var) (list "init")) (nextlayer s))]
        [else                              (cons (list (cons  var (vars s)) (cons "init" (vals s))) (nextlayer s))])))


;;takes a variable and a state
;;returns the state with the variable and it's value removed
(define m-remove
  (lambda (var s)
    (cond
      [(null? s) "error"]
      [(not (locate var s)) "error"]
      [else (remove var s)])))

;;takes a variable and a state, removes the variable and it's value from the first layer it is found in
;;returns the updated state with the variable and it's value removed
(define remove
  (lambda (var s)
    (cond
      [(null? s) '()]
      [(null? (vars s)) (remove var (nextlayer s))]
      [(local-locate var s) (cons (list (remove-var var (vars s)) (remove-val var  s)) (nextlayer s))]
      [else (cons (layer s) (remove var (nextlayer s)))])))


;; Takes a variable and a state
;; Returns the value list with the value attached to the variable removed
(define remove-val
  (lambda (var s)
    (if (eq? var (nextvar s))
        (rest-of (vals s))
        (cons (nextval s) (remove-val var (next-part s))))))

;; Takes an variable and a variable list
;; Returns the variable list with the first instance of the variable removed
(define remove-var
  (lambda (a lis)
    (cond
      [(null? lis)                '()]
      [(eq? a (first-val lis))    (rest-of lis)]
      [else                       (cons (first-val lis) (remove-var a (rest-of lis)))])))

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
  (lambda (exp s return finally)
    (cond
      [(eq?   exp #t)                       (return 'true)]
      [(eq?   exp #f)                       (return 'false)]
      [(and (pair? exp) (am-i-boolean exp)) (m-return (m-condition exp s) s return finally)]
      [(pair? exp)                          (return (m-value exp s))]
      [(eq? (m-value exp s) #t)             (return 'true)]
      [(eq? (m-value exp s) #f)             (return 'false)]
      [else                                 (return (m-value exp s))])))


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
(define vars caar)
(define vals cadar)
(define nextvar caaar)
(define rest-of cdr)
(define nextval caadar)
(define layer car)
(define nextlayer cdr)
(define next-part
  (lambda (s)
    (cons (list (cdr (vars s)) (cdr (vals s))) (nextlayer s))))

; for running/state
(define new-layer '(()()))
(define empty-state '((() ())))
(define first-statement car)
(define rest-of-body cdr)

;; Thank you, sleep well :)

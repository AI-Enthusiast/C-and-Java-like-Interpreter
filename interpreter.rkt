#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin ...., Cormac Dacker

;;TODO: Var decleration, assignment (after decleration, error otherwise)

(require "simpleParser.rkt") ; loads simpleParser.rkt, which itself loads lex.rkt


;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define start
  (lambda (filename)
    (parser filename)))

(require racket/trace)

;; executes code, returns updated state
(define m-state
  (lambda (exp s)
    (if (null? exp)
        s
        (m-state (cdr exp) (m-what-type (car exp) s)))))

;; figures out which method should be used to evaluate this, and evaluates this
;; returns updated state
(define m-what-type
  (lambda (exp s)
    (cond
      ;; null checking
      [(null? exp) s]
      [(not (pair? exp)) s] ; if exp is not a list, then it's either just a variable or a number, which wouldn't change the state

      ;; conditional statement checking (if/while/etc.)
      [(eq? (statement-type-id exp) 'if)    (m-if-statement exp s)]
      [(eq? (statement-type-id exp) 'while) (m-while-loop exp s)]

      ;; is it a declaration
      [(eq? (statement-type-id exp) 'var) (m-var-dec exp s)]

      ;; is it an assignment
      [(eq? (statement-type-id exp) '=) (m-assign exp s)]

      ;; is it a return statement
      [(eq? (statement-type-id exp) 'return) (m-return exp s)])))

;; Code a function that can take in expression of numbers and operators and return the value
;; e.g. (+ 3 (/ 4 2))
;;      (+ 1 2)
;; The operators are +, -, *, /, %, and division is integer division
(define m-value
  (lambda (exp s)
    (cond
      [(null? exp)             (error 'undefined "undefined expression")]
      [(number? exp)           exp]
      [(not (pair? exp))       (m-lookup exp s)] ; if it's not a number, and it's not a list, it's a variable
      [(eq? (operator exp) '+) (+         (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '-) (-         (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '*) (*         (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '/) (quotient  (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [(eq? (operator exp) '%) (remainder (m-value (left-operand exp) s) (m-value (right-operand exp) s))]
      [else                    (error 'undefined "undefined expression")])))

;; Code a function that can take in an expression such as (< 5 2) and return true/false
;; Supports ==, !=, <, >, <=, >=, &&, ||, !
(define m-condition
  (lambda (exp s) ; exp = expression, s = state
    (cond
      ; null checking
      [(null? exp)                    (error 'undefined "undefined expression")]
      [(not (pair? exp))              (m-value exp s)]
      [(null? (operator exp))         (m-value exp s)]

      ; condition checking (&&, ||, !)
      [(eq? (operator exp) '||)  (or  (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '&&)  (and (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (car exp) '!)        (not (m-condition (left-operand exp) s))]

      ; equality/inequality operator checking (==, !=, <, >, <=, >=)
      [(eq? (operator exp) '==)  (eq? (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '!=)  (not (eq? (m-condition (left-operand exp) s) (m-condition (right-operand exp) s)))]
      [(eq? (operator exp) '<)   (< (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '>)   (> (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '<=)  (<= (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '>=)  (>= (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]

      ; oh no
      [else                      (m-value exp s)])))

;; implementing if statement
(define m-if-statement
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(m-condition (loop-condition exp) s) (m-state (loop-body exp) s)] ; run the loop of the body
      [(not (null? (else-statement exp)))   (m-state (else-statement exp) s)]))) ; run the else of the body

;; implementing while loop
;; NEEDS 'm-state' TO USE!!!
(define m-while-loop
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(m-condition (loop-condition exp) s) (m-while-loop exp (m-state loop-body s))])))

;; Abstration
(define statement-type-id car) ; e.g. if, while, var, etc.
(define statement-body cadr) ; e.g. the body of a return statement

;; for if statements
(define else-statement cadddr) ; else statement, if it exists
(define loop-condition cadr)
(define loop-body caddr)

;; for value operations
(define left-operand cadr)
; for m-value
(define operator car)
(define right-operand caddr)

; (5 + 2 <= 7) ;can we remove these? or test for them?
; ((5 + 2) <= 7)

(define vars car)
(define vals cadr)
(define nextvar caar)
(define nextval caadr)


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


;;takes a variable and a state
;;returns the value of the variable, or error message if it does not exist
;;will return "init" if not yet initilized
(define m-lookup
  (lambda (var s)
    (cond
      [(or (null? s) (null? (vars s))) "error, does not exist"]
      [(and (equal? var (nextvar s)) (nextval s))]
      [else (m-lookup var (list (cdr (vars s)) (cdr (vals s))))])))


;;takes a variable, the value it is to be updated to, and the state, returns the updated state
(define m-update
  (lambda (var update-val s)
    (cond
      [(or (null? s) (null? (vars s))) "error"]
      [(not (number? (locate var 0 s))) "error"]
      [else (list (vars s) (update var update-val s))])))


;;takes the value to be updated, the location of the value and the      
;;updates the variable at the location with the new value, returns the updated state
(define update
  (lambda (var update-val s)
    (cond
      [(eq? var (nextvar s)) (cons update-val (cdr (vals s)))]
      [else (cons (nextval s) (update var update-val (list (cdr (vars s)) (cdr (vals s)))))])))
                                                                 
;(m-update 'v '4 '((f s a v x)(5 6 7 1 8)))

;;finds the location of the variable's value in the state
;;takes the variable it is locating, a counter and a state
(define locate
  (lambda (var counter s)
    (cond
      [(or (null? s)(null? (vars s)))
       "error"]
      [(eq? var (nextvar s))
       counter]
      [else
       (locate var (+ counter 1) (cons (cdr (vars s)) (cons (cdr (vals s)) '())))])))

;;takes a varaiable and a state, adds it to a state with non number uninitilized value "init"
;;(does not take value, to update value, use m-update)
;;returns the updated state, if used before assigned, should result in error
;;will accept an empty state '(), a state formated '(()()) or a state formated '((var1 var2 ...)(val1 val2 ...))
(define m-add
  (lambda (var s)
     (if (null? s)
         (list (list var) (list "init"))
         (list (cons  var (vars s)) (cons "init" (vals s))))))

;;takes a variable and a state
;;returns the updated state with the variable and assosiated value removed
(define m-remove
  (lambda (var s)
    (if (not (number? (locate var 0 s)))
        "error"
        (list (remove var (vars s)) (remove-val var s)))))

;;takes a variable and a state
;;returns the value list with the value attached to the variable removed
(define remove-val
  (lambda (var s)
    (if (eq? var (nextvar s))
        (cdr (vals s))
        (cons (nextval s) (remove-val var (list (cdr (vars s)) (cdr (vals s))))))))
                              
;;takes an atom and a list
;;returns the list with the first instance of the atom removed
(define remove
  (lambda (a lis)
    (cond
      [(null? lis) '()]
      [(eq? a (car lis)) (cdr lis)]
      [else (cons (car lis) (remove a (cdr lis)))])))

;; takes an expression
;; returns it as if it where in C/Java
(define m-return
  (lambda (exp s)
    (cond
      [(eq?   (statement-body exp) #t) "True"]
      [(eq?   (statement-body exp) #f) "False"]
      [(pair? (statement-body exp))    (m-return (m-value (statement-body exp) s) s)]
      [else                            (statement-body exp)])))


;;;;**********TESTING**********

;; Performs a quick test to see if the test passed or failed and prints info about test if failure
(define pass?
  (lambda (actual expected)
    (if (equal? actual expected)
        (display 'Pass)
        (donothing (display "Fail {actual} ") (display actual)      ; displays info about the failed test
                   (display " != {expected} ") (display expected)))
    (newline)))

;; This function does nothing but allows for all the displays of a failed test to occure without an error
(define donothing ; there is probably a better way to do this but  ̄\_(ツ)_/̄
  (lambda (a b c d)
    '())) ; literly does nothing (like what I wish I was doing)


; TODO: Test (m-what-type, m-if-statment, whileloop
;; Performs all the tests needed to prove the validity of the functions, I love this function
(define (test)

  ;Example:
  ;(diplay "Test #{test number} {test name}") (newline)
  ;(pass? {actual} {expected})
  ;(newline)

  ;checks the code is parsed into a tree as exprected
  (display "Test #1 Start") (newline)                                                 ;Test start
  (pass? (start "Test1.txt") '((var x) (= x 10) (var y (+ (* 3 x) 5))                           ; 1/1
                                   (while (!= (% y x) 3) (= y (+ y 1)))
                                   (if (> x y) (return x)
                                       (if (> (* x x) y) (return (* x x))
                                           (if (> (* x (+ x x)) y)
                                               (return (* x (+ x x)))
                                               (return (- y 1)))))))
  (newline)
  
  ;checks math opperations perform correctly
  ;TODO: replace 'temp with an s state
  (display "Test #2 m-value") (newline)                                               ;Test m-value
  (pass? (m-value '(+ 3 (/ 4 2)) 'temp) 5)                                                      ; 1/2
  (pass? (m-value '(+ (* 3 2) (/ 4 (% 2 3))) 'temp) 8)                                          ; 2/2
  (newline)

  ;boolean ooporators for var assignments and conditions in if & while statements
  ;TODO: replace 'temp with an s state
  (display "Test #3 m-condition") (newline)                                           ;Test m-condition
  (pass? (m-condition '(== 1 1) 'temp) #t)                                                      ; 1/23
  (pass? (m-condition '(== 1 0) 'temp) #f)                                                      ; 2/23
  (pass? (m-condition '(!= 1 1) 'temp) #f)                                                      ; 3/23
  (pass? (m-condition '(!= 1 0) 'temp) #t)                                                      ; 4/23
  (pass? (m-condition '(> 1 1) 'temp) #f)                                                       ; 5/23
  (pass? (m-condition '(> 1 0) 'temp) #t)                                                       ; 6/23
  (pass? (m-condition '(> 0 1) 'temp) #f)                                                       ; 7/23
  (pass? (m-condition '(< 1 1) 'temp) #f)                                                       ; 8/23
  (pass? (m-condition '(< 1 0) 'temp) #f)                                                       ; 9/23
  (pass? (m-condition '(< 0 1) 'temp) #t)                                                       ; 10/23
  (pass? (m-condition '(>= 1 1) 'temp) #t)                                                      ; 11/23
  (pass? (m-condition '(>= 1 0) 'temp) #t)                                                      ; 12/23
  (pass? (m-condition '(>= 0 1) 'temp) #f)                                                      ; 13/23
  (pass? (m-condition '(<= 1 1) 'temp) #t)                                                      ; 14/23
  (pass? (m-condition '(<= 1 0) 'temp) #f)                                                      ; 15/23
  (pass? (m-condition '(<= 0 1) 'temp) #t)                                                      ; 16/23
  (pass? (m-condition '(&& #t #t) 'temp) #t)                                                    ; 17/23
  (pass? (m-condition '(&& #t #f) 'temp) #f)                                                    ; 18/23
  (pass? (m-condition '(|| #t #t) 'temp) #t)                                                    ; 19/23
  (pass? (m-condition '(|| #t #f) 'temp) #t)                                                    ; 20/23
  (pass? (m-condition '(|| #f #f) 'temp) #f)                                                    ; 21/23
  (pass? (m-condition '(! #t) 'temp) #f)                                                        ; 22/23
  (pass? (m-condition '(! #f) 'temp) #t)                                                        ; 23/23
  (newline)
  
  ;lookup variable's value in the state
  (display "Test #4 m-lookup") (newline)                                              ;Test m-lookup
  (pass? (m-lookup 'a '((a b c d)(2 5 6 7))) 2)                                                 ; 1/5
  (pass? (m-lookup 'c '((a b c d)(2 5 6 7))) 6)                                                 ; 2/5
  (pass? (m-lookup 'd '((a b c d)(2 5 6 7))) 7)                                                 ; 3/5
  (pass? (m-lookup 'd '()) "error, does not exist")                                             ; 4/5
  (pass? (m-lookup 's '(()())) "error, does not exist")                                         ; 5/5
  (newline)

  ;update variable's value in the state
  (display "Test #5 m-update") (newline)                                              ;Test m-update
  (pass? (m-update 's 3 '((a b c d)(2 5 6 7))) "error")                                         ; 1/6
  (pass? (m-update 'a 3 '((a b c d)(2 5 6 7))) '((a b c d)(3 5 6 7)))                           ; 2/6
  (pass? (m-update 'b 21 '((a b c d)(2 5 6 7))) '((a b c d)(2 21 6 7)))                         ; 3/6
  (pass? (m-update 'd 1 '((a b c d)(2 5 6 7)))  '((a b c d)(2 5 6 1)))                          ; 4/6
  (pass? (m-update 'a 0 '()) "error")                                                           ; 5/6
  (pass? (m-update 'a 0 '(()())) "error")                                                       ; 6/6
  (newline)

  ;add a variable to the state
  (display "Test #6 m-add") (newline)                                                 ;Test m-add
  (pass? (m-add 's '()) '((s)("init")))                                                         ; 1/4
  (pass? (m-add 's '(()())) '((s)("init")))                                                     ; 2/4
  (pass? (m-add 's '((a)(2))) '((s a)("init" 2)))                                               ; 3/4
  (pass? (m-add 's '((a b c)(3 4 5))) '((s a b c)("init" 3 4 5)))                               ; 4/4
  (newline)

  ;remove a variable from a state
  (display "Test #7 m-remove") (newline)                                              ;Test m-remove
  (pass? (m-remove 'a '((a b c d)(2 5 6 7))) '((b c d)(5 6 7)))                                 ; 1/6
  (pass? (m-remove 'b '((a b c d)(2 5 6 7))) '((a c d)(2 6 7)))                                 ; 2/6
  (pass? (m-remove 'd '((a b c d)(2 5 6 7))) '((a b c)(2 5 6)))                                 ; 3/6
  (pass? (m-remove 'a '((b c d)(5 6 7))) "error")                                               ; 4/6
  (pass? (m-remove 'a '(()())) "error")                                                         ; 5/6
  (pass? (m-remove 'a '()) "error")                                                             ; 6/6
  (newline)

  ) ;left hanging for easy test addition

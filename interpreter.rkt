#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Will run a txt file containing code by using the run function (run "Filename.txt")
;;;; Run tests by using the test function (test)

(require "simpleParser.rkt") ; loads simpleParser.rkt, which itself loads lex.rkt
(require racket/trace)       ; for debugging

;; Runs the filename, should be provided in quotes
;; e.g. (run "Tests/Test1.txt")
(define run
  (lambda (filename)
    (m-state (parse-tree filename) empty-state)))

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define parse-tree
  (lambda (filename)
    (parser filename)))

;; Executes code, returns updated state
(define m-state
  (lambda (exp s)
    (cond
      [(null? exp)              s]
      [(null? (cdr exp))       (m-what-type (car exp) s)]
      [(not (list? (car exp))) (m-what-type (car exp) s)]
      [else                    (m-state (cdr exp) (m-what-type (car exp) s))])))

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
      [(number? exp)                          exp] ; if it's a number, return a number
      [(and (not (pair? exp)) (boolean? exp)) exp]

      ; boolean checking
      [(eq? exp 'true)  #t] ; true
      [(eq? exp 'false) #f] ; false
      [(and (pair? exp) (am-i-boolean exp)) (m-condition exp s)]

      ; variable
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
      [(eq? (car exp) '!)        (not (m-condition (left-operand exp) s))]

      ; equality/inequality operator checking (==, !=, <, >, <=, >=)
      [(eq? (operator exp) '==)  (eq? (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '!=)  (not (eq? (m-condition (left-operand exp) s)
                                           (m-condition (right-operand exp) s)))]
      [(eq? (operator exp) '<)   (< (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '>)   (> (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '<=)  (<= (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]
      [(eq? (operator exp) '>=)  (>= (m-condition (left-operand exp) s) (m-condition (right-operand exp) s))]

      ; oh no
      [else                      (m-value exp s)])))

;; Implementing if statement
(define m-if-statement
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      ; run the loop of the body (body is multiple statements)
      [(and (m-condition (loop-condition exp) s) (pair? (car (loop-body exp))))
              (m-state (loop-body exp) s)]
      ; run the loop of the body (body is single statement)
      [(m-condition (loop-condition exp) s)
              (m-what-type (loop-body exp) s)]
      [(null? (cdddr exp)) s] ; if there's no else statement, return the state
      [(and (not (null? (else-statement exp))) (pair? (car (loop-body exp))))
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
      [(and (m-condition (loop-condition exp) s) (pair? (car (loop-body exp))))
       ; runs the while loop (body is multiple statements)
           (m-while-loop exp (m-state (loop-body exp) s))]
      [(m-condition (loop-condition exp) s)
           (m-while-loop exp (m-what-type (loop-body exp) s))]
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
      [(or (null? s) (null? (vars s))) (error "use before assignment")]
      [(and (equal? var (nextvar s))   (eq? "init" (nextval s))) (error "use before assignment")]
      [(equal? var (nextvar s))        (nextval s)]
      [else                            (m-lookup var (list (cdr (vars s)) (cdr (vals s))))])))

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
      [(eq? var (nextvar s))  (cons update-val (cdr (vals s)))]
      [else                   (cons (nextval s) (update var update-val (list (cdr (vars s))
                                                                             (cdr (vals s)))))])))

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
       (locate var (+ counter 1) (cons (cdr (vars s)) (cons (cdr (vals s)) '())))])))

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
        (cdr (vals s))
        (cons (nextval s) (remove-val var (list (cdr (vars s)) (cdr (vals s))))))))

;; Takes an atom and a list
;; Returns the list with the first instance of the atom removed
(define remove
  (lambda (a lis)
    (cond
      [(null? lis)          '()]
      [(eq? a (car lis))    (cdr lis)]
      [else (cons (car lis) (remove a (cdr lis)))])))

;; determines if an expression is boolean
(define am-i-boolean
  (lambda (exp)
    (cond
      [(eq? (operator exp) '||)  #t]
      [(eq? (operator exp) '&&)  #t]
      [(eq? (car exp) '!)        #t]
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

; for state computation
(define vars car)
(define vals cadr)
(define nextvar caar)
(define nextval caadr)

(define empty-state '(() ()))

;;;;**********TESTING**********

;; Performs a quick test to see if the test passed or failed and prints info about test if failure
(define pass?
  (lambda (actual expected)
    (if (equal? actual expected)
        (display 'Pass)
        (do-nothing (display "Fail {actual} ") (display actual)      ; displays info about the failed test
                   (display " != {expected} ") (display expected)))
    (newline)))

;; This function does nothing but allows for all the displays of a failed test to occure without an error
(define do-nothing ; there is probably a better way to do this but ̄\_(ツ)_/̄
  (lambda (a b c d)
    '())) ; literly does nothing (like what I wish I was doing)

;; Performs all the tests needed to prove the validity of the functions, I love this function
(define (test)

  ;Example:
  ; (diplay "Test #{test number} {test name}") (newline)
  ; (pass? {actual} {expected})
  ; (newline)

  ; checks the code is parsed into a tree as exprected
  (display "Test #1 parse-tree") (newline)                                            ;Test parse-tree
  (pass? (parse-tree "Tests/Test1.txt") '((var x) (= x 10) (var y (+ (* 3 x) 5))                ; 1/1
                                   (while (!= (% y x) 3) (= y (+ y 1)))
                                   (if (> x y) (return x)
                                       (if (> (* x x) y) (return (* x x))
                                           (if (> (* x (+ x x)) y)
                                               (return (* x (+ x x)))
                                               (return (- y 1)))))))
  (newline)

  ; checks math opperations perform correctly
  (display "Test #2 m-value") (newline)                                               ;Test m-value
  (pass? (m-value '(+ 3 (/ 4 2)) 's) 5)                                                         ; 1/2
  (pass? (m-value '(+ (* 3 2) (/ 4 (% 2 3))) 's) 8)                                             ; 2/2
  (newline)

  ; boolean ooporators for var assignments and conditions in if & while statements
  (display "Test #3 m-condition") (newline)                                           ;Test m-condition
  (pass? (m-condition '(== 1 1) 's) #t)                                                         ; 1/23
  (pass? (m-condition '(== 1 0) 's) #f)                                                         ; 2/23
  (pass? (m-condition '(!= 1 1) 's) #f)                                                         ; 3/23
  (pass? (m-condition '(!= 1 0) 's) #t)                                                         ; 4/23
  (pass? (m-condition '(> 1 1) 's) #f)                                                          ; 5/23
  (pass? (m-condition '(> 1 0) 's) #t)                                                          ; 6/23
  (pass? (m-condition '(> 0 1) 's) #f)                                                          ; 7/23
  (pass? (m-condition '(< 1 1) 's) #f)                                                          ; 8/23
  (pass? (m-condition '(< 1 0) 's) #f)                                                          ; 9/23
  (pass? (m-condition '(< 0 1) 's) #t)                                                          ; 10/23
  (pass? (m-condition '(>= 1 1) 's) #t)                                                         ; 11/23
  (pass? (m-condition '(>= 1 0) 's) #t)                                                         ; 12/23
  (pass? (m-condition '(>= 0 1) 's) #f)                                                         ; 13/23
  (pass? (m-condition '(<= 1 1) 's) #t)                                                         ; 14/23
  (pass? (m-condition '(<= 1 0) 's) #f)                                                         ; 15/23
  (pass? (m-condition '(<= 0 1) 's) #t)                                                         ; 16/23
  (pass? (m-condition '(&& #t #t) 's) #t)                                                       ; 17/23
  (pass? (m-condition '(&& #t #f) 's) #f)                                                       ; 18/23
  (pass? (m-condition '(|| #t #t) 's) #t)                                                       ; 19/23
  (pass? (m-condition '(|| #t #f) 's) #t)                                                       ; 20/23
  (pass? (m-condition '(|| #f #f) 's) #f)                                                       ; 21/23
  (pass? (m-condition '(! #t) 's) #f)                                                           ; 22/23
  (pass? (m-condition '(! #f) 's) #t)                                                           ; 23/23
  (newline)

  ; lookup variable's value in the state
  (display "Test #4 m-lookup") (newline)                                              ;Test m-lookup
  (pass? (m-lookup 'a '((a b c d)(2 5 6 7))) 2)                                                 ; 1/5
  (pass? (m-lookup 'c '((a b c d)(2 5 6 7))) 6)                                                 ; 2/5
  (pass? (m-lookup 'd '((a b c d)(2 5 6 7))) 7)                                                 ; 3/5
  ;(pass? (m-lookup 'd '()) "error) ;should error                                               ; 4/5
  ;(pass? (m-lookup 's '(()())) "error) ;should error                                           ; 5/5
  (newline)

  ; update variable's value in the state
  (display "Test #5 m-update") (newline)                                              ;Test m-update
  (pass? (m-update 's 3 '((a b c d)(2 5 6 7))) "error")                                         ; 1/6
  (pass? (m-update 'a 3 '((a b c d)(2 5 6 7))) '((a b c d)(3 5 6 7)))                           ; 2/6
  (pass? (m-update 'b 21 '((a b c d)(2 5 6 7))) '((a b c d)(2 21 6 7)))                         ; 3/6
  (pass? (m-update 'd 1 '((a b c d)(2 5 6 7)))  '((a b c d)(2 5 6 1)))                          ; 4/6
  (pass? (m-update 'a 0 '()) "error")                                                           ; 5/6
  (pass? (m-update 'a 0 '(()())) "error")                                                       ; 6/6
  (newline)

  ; add a variable to the state
  (display "Test #6 m-add") (newline)                                                 ;Test m-add
  (pass? (m-add 's '()) '((s)("init")))                                                         ; 1/4
  (pass? (m-add 's '(()())) '((s)("init")))                                                     ; 2/4
  (pass? (m-add 's '((a)(2))) '((s a)("init" 2)))                                               ; 3/4
  (pass? (m-add 's '((a b c)(3 4 5))) '((s a b c)("init" 3 4 5)))                               ; 4/4
  (newline)

  ; remove a variable from a state
  (display "Test #7 m-remove") (newline)                                              ;Test m-remove
  (pass? (m-remove 'a '((a b c d)(2 5 6 7))) '((b c d)(5 6 7)))                                 ; 1/6
  (pass? (m-remove 'b '((a b c d)(2 5 6 7))) '((a c d)(2 6 7)))                                 ; 2/6
  (pass? (m-remove 'd '((a b c d)(2 5 6 7))) '((a b c)(2 5 6)))                                 ; 3/6
  (pass? (m-remove 'a '((b c d)(5 6 7))) "error")                                               ; 4/6
  (pass? (m-remove 'a '(()())) "error")                                                         ; 5/6
  (pass? (m-remove 'a '()) "error")                                                             ; 6/6
  (newline)

  ; assign a variable
  (display "Test #8 m-assign") (newline)                                              ;Test m-assign
  ;(pass? (m-assign '(var 'a 2) '(()()) ;should error                                           ; 1/7
  (pass? (m-assign '(var a 2) '((a)(1))) '((a)(2)))                                             ; 2/7
  (pass? (m-assign '(var d 2) '((x y d z)(1 1 1 1))) '((x y d z)(1 1 2 1)))                     ; 3/7
  (pass? (m-assign '(var d 2) '((x y d z)(1 1 "init" 1))) '((x y d z)(1 1 2 1)))                ; 4/7
  (pass? (m-assign '(var d (+ 2 4)) '((x y d z)(1 1 1 1))) '((x y d z)(1 1 6 1)))               ; 5/7
  (pass? (m-assign '(var d (+ x 4)) '((x y d z)(2 3 7 1))) '((x y d z)(2 3 6 1)))               ; 6/7
  (pass? (m-assign '(var d (+ x (* y 2))) '((x y d z)(2 3 7 1))) '((x y d z)(2 3 8 1)))         ; 7/7
  (newline)

  ; declares a variable
  (display "Test #9 m-var-dec") (newline)                                             ;Test m-var-dec
  (pass? (m-var-dec '(var a) '((q)(1))) '((a q) ("init" 1)))                                    ; 1/9
  ;(pass? (m-var-dec '(var a) '((d a s)(1 2 3))) "error") ;should error                         ; 2/9
  (pass? (m-var-dec '(var a) '(()())) '((a)("init")))                                           ; 3/9
  ;(pass? (m-var-dec '(var a 1) '((d a s)(1 2 3))) "error") ;should error                       ; 4/9
  (pass? (m-var-dec '(var a 1) '((d s)(2 3))) '((a d s)(1 2 3)))                                ; 5/9
  (pass? (m-var-dec '(var a (+ x 1)) '((c s x)(2 3 4))) '((a c s x)(5 2 3 4)))                  ; 6/9
  (pass? (m-var-dec '(var a (+ x (* c 3))) '((c s x)(2 3 4))) '((a c s x)(10 2 3 4)))           ; 7/9
  ;(pass? (m-var-dec '(var a (+ x 1)) '((c s a x)(2 3 5 7))) "error") ;should error             ; 8/9
  ;(pass? (m-var-dec '(var a (+ a 1)) '((c s a x)(2 3 5 4))) "error") ;should error             ; 9/9
  (newline)

  ; tests interpreter functionality
  (display "Test #10 run") (newline)                                                  ;Test run
  (pass? (run "Tests/Test1.txt") 100)                                                           ; 1/23
  (pass? (run "Tests/Test2.txt") 21)                                                            ; 2/23
  (pass? (run "Tests/Test3.txt") 4)                                                             ; 3/23
  (pass? (run "Tests/p1.Test1.txt") 150)                                                        ; 4/23
  (pass? (run "Tests/p1.Test2.txt") -4)                                                         ; 5/23
  (pass? (run "Tests/p1.Test3.txt") 10)                                                         ; 6/23
  (pass? (run "Tests/p1.Test4.txt") 16)                                                         ; 7/23
  (pass? (run "Tests/p1.Test5.txt") 220)                                                        ; 8/23
  (pass? (run "Tests/p1.Test6.txt") 5)                                                          ; 9/23
  (pass? (run "Tests/p1.Test7.txt") 6)                                                          ; 10/23
  (pass? (run "Tests/p1.Test8.txt") 10)                                                         ; 11/23
  (pass? (run "Tests/p1.Test9.txt") 5)                                                          ; 12/23
  (pass? (run "Tests/p1.Test10.txt") -39)                                                       ; 13/23
  ;;(pass? (run "Tests/p1.Test11.txt") "error" ) ;should error                                    ; 14/23
  ;;(pass? (run "Tests/p1.Test12.txt") "error") ;should error                                     ; 15/23
  ;;(pass? (run "Tests/p1.Test13.txt") "error") ;should error                                     ; 16/23
  ;;(pass? (run "Tests/p1.Test14.txt") "error") ;should error                                     ; 17/23
  (pass? (run "Tests/p1.Test15.txt") "true")                                                    ; 18/23
  (pass? (run "Tests/p1.Test16.txt") 100)                                                       ; 19/23
  (pass? (run "Tests/p1.Test17.txt") "false")                                                   ; 20/23
  (pass? (run "Tests/p1.Test18.txt") "true")                                                    ; 21/23
  (pass? (run "Tests/p1.Test19.txt") 128)                                                       ; 22/23
  (pass? (run "Tests/p1.Test20.txt") 12)                                                        ; 23/23
  (newline)

  ) ;left hanging for easy test addition

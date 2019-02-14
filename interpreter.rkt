#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti ..., Catlin ...., Cormac Dacker


; Code a function that can take in expression of numbers and operators and return the value
; e.g. (3 + (4 / 2))
;      (1 + 2)
; The operators are +, -, *, /, %, and division is integer division
(require "simpleParser.rkt") ; loads simpleParser.rkt, which itself loads lex.rkt

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define start
  (lambda (filename)
    (parser filename)))

;; figures out which method should be used to evaluate this
(define m-what-type
  (lambda (exp s)
    (exp #|TODO: UPDATE THIS!!!|#)))
(require racket/trace)



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

#|;; implementing while loop
(define whileloop
  (lambda (exp s)
    (cond
      [(null? exp) (error 'undefined "undefined expression")]
      [(mcondition (loop-condition exp) s) (whileloop exp s #|TODO: SOMETHING TO UPDATE THE STATE IN HERE!!!|#)])))|#

;; Abstration
;; for if statements
(define loop-type-id car) ; e.g. if, while, etc.
(define else-statement cadddr) ; else statement, if it exists
(define loop-condition cadr)
(define loop-body caddr)

;; for value operations
(define left-operand cadr)
; for mvalue
(define operator cadr)
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


;;takes a variable and a state
;;returns the value of the variable, or error message if it does not exist
;;will return "init" if not yet initilized
(define m-lookup
  (lambda (var s)
    (cond
      [(null? s) "error, does not exist"]
      [(null? (vars s)) "error, does not exist"]
      [(and (equal? var (nextvar s)) (nextval s))]
      [else (m-lookup var (list (cdr (vars s)) (cdr (vals s))))])))


;;takes a variable, the value it is to be updated to, and the state, returns the updated state
(define m-update
  (lambda (var update-val s)
    (cond
      [(null? s) "error"]
      [(not (number? (locate var 0 s))) "error"]
      [else (cons (vars s) (list (update update-val (locate var 0 s) (vals s))))])))


;;takes the value to be updated, the location of the value and the      
;;updates the variable at the location with the new value, returns the updated state
(define update
  (lambda (update-val loc values)
    (cond
      [(null? values)
       "error"]
      [(eq? loc 0) 
       (cons update-val (cdr values))]
      [else
       (cons (car values) (update update-val (- loc 1) (cdr values)))])))
                                                                 

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

  ;Example:
  ;(diplay "Test #{test number} {test name}") (newline)
  ;(pass? {actual} {expected})
  ;(newline)

  (display "Test #1 mvalue") (newline)                                                ;Test mvalue
  ;(pass? (mvalue '(3 + (4 / 2))) 5)                                                       ; 1/2
  ;(pass? (mvalue '((3 * 2) + (4 / (2 % 3)))) 8)                                           ; 2/2
  (newline)

  (display "Test #2 exp") (newline)                                                   ;Test start
  (pass? (start "Test1.txt") '((var x) (= x 10) (var y (+ (* 3 x) 5))                     ; 1/1
                                   (while (!= (% y x) 3) (= y (+ y 1)))
                                   (if (> x y) (return x)
                                       (if (> (* x x) y) (return (* x x))
                                           (if (> (* x (+ x x)) y)
                                               (return (* x (+ x x)))
                                               (return (- y 1)))))))
  (newline)


  ;lookup variable's value in the state
  (display "Test #3 m-lookup") (newline)
  (pass? (m-lookup 'a '((a b c d)(2 5 6 7))) 2)
  (pass? (m-lookup 'c '((a b c d)(2 5 6 7))) 6)
  (pass? (m-lookup 'd '((a b c d)(2 5 6 7))) 7)
  (pass? (m-lookup 'd '()) "error, does not exist")
  (pass? (m-lookup 's '(()())) "error, does not exist")
  (newline)

  ;update variable's value in the state
  (display "Test #4 m-update") (newline)
  (pass? (m-update 's 3 '((a b c d)(2 5 6 7))) "error")
  (pass? (m-update 'a 3 '((a b c d)(2 5 6 7))) '((a b c d)(3 5 6 7)))
  (pass? (m-update 'b 21 '((a b c d)(2 5 6 7))) '((a b c d)(2 21 6 7)))
  (pass? (m-update 'd 1 '((a b c d)(2 5 6 7)))  '((a b c d)(2 5 6 1)))
  (pass? (m-update 'a 0 '()) "error")
  (pass? (m-update 'a 0 '(()())) "error")
  (newline)

  ;add a variable to the state
  (display "Test #5 m-add") (newline)
  (pass? (m-add 's '()) '((s)("init")))
  (pass? (m-add 's '(()())) '((s)("init")))
  (pass? (m-add 's '((a)(2))) '((s a)("init" 2)))
  (pass? (m-add 's '((a b c)(3 4 5))) '((s a b c)("init" 3 4 5)))
  (newline)

  ;remove a variable from a state
  (display "Test #6 m-remove") (newline)
  (pass? (m-remove 'a '((a b c d)(2 5 6 7))) '((b c d)(5 6 7)))
  (pass? (m-remove 'b '((a b c d)(2 5 6 7))) '((a c d)(2 6 7)))
  (pass? (m-remove 'd '((a b c d)(2 5 6 7))) '((a b c)(2 5 6)))
  (pass? (m-remove 'a '((b c d)(5 6 7))) "error")
  (pass? (m-remove 'a '(()())) "error")
  (pass? (m-remove 'a '()) "error")
           
  ) ;left hanging for easy test addition

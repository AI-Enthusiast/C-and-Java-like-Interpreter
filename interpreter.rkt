#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Will run a txt file containing code by using the run function (run "Filename.txt" "class w/ main")
;;;; Order of inputs for ALL m-state and m-state like things

(provide (all-defined-out))         ; allows for testing to be done in interpreter-testing.rkt
(require "classParser.rkt")         ; loads simpleParser.rkt, which itself loads lex.rkt
(require racket/trace)              ; for debugging

;; Runs the filename, should be provided in quotes
;; e.g. (run "Tests/Test1.txt")
(define run
  (lambda (filename classmain)
    (call/cc
     (lambda (k)
       (runner filename k (string->symbol classmain))))))

(define runner
  (lambda (filename callcc classmain)
    (let* [(s (m-base-layer (parse-t filename) empty-list empty-list
                                                                  callcc ;; return
                                                                  (lambda (v) v) ;; break
                                                                  (lambda (v) v) ;; continue
                                                                  (lambda (v) v) ;; try
                                                                  (lambda (v) v) ;; catch
                                                                  (lambda (v) v)))]
       (m-funcall 'main no-params (lambda (v) v)
                  (m-lookup-class classmain s) s)))) ;; finally

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define parse-t
  (lambda (filename)
    (parser filename)))

;; Executes code, returns updated state
(define m-state
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; null checking
      [(null? exp)                         s]

      ; checking for single statement
      [(not (list? (first-statement exp))) (m-what-type exp closure s return break continue try catch finally)]
      [(null? (rest-of-body exp))          (m-what-type (first-statement exp) closure s
                                                        return break continue try catch finally)]

      ; checking for block
      [(eq? (first-statement exp) 'begin)  (m-pop (lambda (k) (m-state (rest-of-body exp)
                                                                       closure (m-push s) return k continue
                                                                       try catch finally)))]
      ; else: process one statement at a time
      [else                                (m-state (rest-of-body exp) closure
                                                    (m-what-type (first-statement exp) closure s return break
                                                                 continue try catch finally)
                                                    return break continue try catch finally)])))
;; takes a closure
;; Returns state (within closure) with most recent layer popped off
(define m-pop
  (lambda (closure)
    (list (closure-class-name closure) (closure-super closure) (nextlayer (closure-body closure)))))


;; takes a closure
;; Returns state (within closure) with new empty layer pushed on
(define m-push
  (lambda (closure)
    (list (closure-class-name closure) (closure-super closure) (list (cons new-layer (local closure)) (global closure)))))

;; Works through the top layer of the code then
(define m-base-layer
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(null? exp)                 s]
      [(null? (rest-of-body exp))  (m-base-layer (first-statement exp) closure s
                                                         return break continue try catch finally)]
      ;is it the main
      ;[(and (eq? (statement-body exp) 'main) (eq? (statement-type-id exp) 'function))
      ;                             (m-state (main-body exp) (m-push s)
      ;                                      return break continue try catch finally)]

      ; is it a class
      [(eq? (statement-type-id exp) 'class) (m-add-class exp s)]

      ;is it a function
      [(eq? (statement-type-id exp) 'function)
                                   (m-add-global-func (full-func exp) (list (append (list (func-name exp))
                                                                                    (list (func-body exp)))) closure)]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)       (m-var-dec exp closure s)]

      ; otherwise, process the first statement, and then the rest of it
      ; (the program shouldn't actually reach this point, because all things in the
      ; main base level of the program will be either functions or variable declarations.
      [else                                     (m-base-layer (rest-of-body exp) closure
                                                         (m-base-layer (first-statement exp) closure s return break
                                                                       continue try catch finally)
                                                         return break continue try catch finally)])))

;; Figures out which method should be used to evaluate this, and evaluates this
;; Returns updated state
;; in this m-what-type, "function" should return a number
(define m-what-type
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(or (null? exp) (not (pair? exp)))      s]

      ;is  it a function
      [(eq? (statement-type-id exp) 'function) (m-add-local-func (full-func exp)
                                                                  ; function closure
                                                                  (list (append (list (func-name exp))
                                                                              (list (func-body exp))))
                                                                  ; class closure
                                                                  closure
                                                          s)]

      ;is it a function call w/dot and no params
      [(and (eq? (statement-type-id exp) 'funcall)
       (and (list? (funcall-name exp))
            (null? (func-params exp))))
                   (m-dot (dot-var-name exp) (dot-func-name exp) no-params closure s (lambda (v) s))]

      ;is it a function call w/dot
      [(and (eq? (statement-type-id exp) 'funcall)
            (list? (funcall-name exp)))
                   (m-dot (dot-var-name exp) (dot-func-name exp) (func-params exp) closure s (lambda (v) s))]

      ;is it a function call w/o parameters
      [(and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp)))
                                               (m-funcall (funcall-name exp) no-params (lambda (v) s) s)]

      ;is it a function call
      [(eq? (statement-type-id exp) 'funcall)
                                               (m-funcall (funcall-name exp) (func-params exp) (lambda (v) s) s)]

      ; is it a new block
      [(eq? (first-statement exp) 'begin)      (m-pop (m-state (rest-of-body exp) (m-push s)
                                                               return break continue try catch finally))]

      ; conditional statement checking (if/while/etc.)
      [(eq? (statement-type-id exp) 'if)       (m-if-statement exp closure s return break continue try catch finally)]
      [(eq? (statement-type-id exp) 'while)    (call/cc (lambda (k) (m-while-loop exp closure s return k continue
                                                                                  try catch finally)))]

      ; is it a break
      [(eq? (statement-type-id exp) 'break)    (break (m-pop s))]

      ; is it a continue
      [(eq? (statement-type-id exp) 'continue) (continue s)]

      ; is it a try/catch statement
      [(eq? (statement-type-id exp) 'try)      (call/cc (λ (k) (m-try-catch-finally exp closure s return break
                                                                                    continue k catch
                                                                                    finally)))]

      ; is it a throw
      [(eq? (statement-type-id exp) 'throw)    (try (m-pop (catch (statement-body exp))))]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)      (m-var-dec exp closure s)]

      ; is it an assignment
      [(eq? (statement-type-id exp) '=)        (m-assign exp s)]

      ; is it a return statement
      [(eq? (statement-type-id exp) 'return)   (m-return (statement-body exp) closure s return finally)]

      ; oh no
      [else                                    (error 'undefined "undefined expression")])))


;; m-funcall returns a number
(define m-funcall
  ;; name is name of the function, actual = input parameters
  (lambda (name actual return closure s)
    ;gets the body and the formal parameters of the function
    (let* [(all (m-lookup-func name closure s))
           (formal (func-formal-params all))
           (body (func-call-body all))]
        (if (eq? (num-in-list actual 0) (num-in-list formal 0))
            ;runs the body
            ;(call/cc (lambda (k)
                       ;(m-pop
            (m-state body (lists-to-assign actual formal (m-push closure) s) s; THERE'S AN ISSUE HERE!!!! IT'S NOT LETTING TEST 6 WORK!!!!!!
                                                                     ; We tried to fix it but it broke more things :( 
                 return
                 (lambda (v) v) ;; break
                 (lambda (v) v) ;; continue
                 (lambda (v) v) ;; try
                 (lambda (v) v) ;; catch
                 (lambda (v) v)) ;; finally
            (error 'undefined "Paramater mismatch")))))

;; Takes two lists (l1 actual values)  (l2 formal values)
;; Returns an updated state
;; eg: (lists-to-assign '(1 2 3) '(a b c) s)
(define lists-to-assign
  (lambda (l1 l2 closure s)
    (if (null? l1)
            closure
            (if (and (not (number? (car l1))) (> (num-in-list l1 0) 1))
                    (lists-to-assign (list-from-state l1 closure) l2 closure) ;if l1 null assign this to closure
                    (lists-to-assign (cdr l1) (cdr l2)
                                     (m-var-dec (cons 'var (cons (car l2) (list (car l1)))) closure s))))))

(define list-from-state
  (lambda (lis s)
    (cond
      [(null? lis) '()]
      [(not (number? (car lis))) (cons (m-lookup-var (car lis) s) (list-from-state (cdr lis) s))]
      [else (cons (car lis) (list-from-state (cdr lis) s))])))


;; Sums the number of attoms in a list
;; helper for m-funcall
(define num-in-list
  (lambda (lis acc)
    (if (null? lis)
        acc
        (num-in-list (cdr lis) (+ acc 1)))))

(define m-try-catch-finally
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; oh no
      [(and (not (pair? (third-statement exp))) (not (pair? (catch-statement exp))))
       (error 'undefined "try statement missing catch or finally")]

      ; check if it has catch (and no finally)
      [(and (not (pair? (third-statement exp))) (eq? (second-identifier exp) 'catch))
       (call/cc (lambda (k) (m-state (try-body exp) closure s return break continue k
                                     ;; CATCH STATEMENT
                                     (lambda (exception) (m-state (catch-body (second-body exp))
                                                                  closure
                                                                  ;; MODIFYING THE STATE
                                                                  (m-var-dec (list 'var (catch-var-name
                                                                                         (second-body exp))
                                                                                   exception) (m-push s))
                                                                  return break continue k catch finally))
                                     finally)))]

      ; check if has finally first (no catch)
      [(and (eq? (third-identifier exp) 'finally) (not (pair? (catch-statement exp))))
       (m-state (third-body exp) (m-state (try-body exp) closure s return break continue
                                          (lambda (v) s) (lambda (v) s) finally)
                return break continue try catch finally)]



      ; check for a catch AND a finally
      [(and (eq? (second-identifier exp) 'catch) (eq? (third-identifier exp) 'finally))
       (m-state (third-body exp)
                (call/cc (lambda (k) (m-state (try-body exp) closure s return break continue k
                                              ;; CATCH STATEMENT
                                              (lambda (exception) (m-state (catch-body (second-body exp))
                                                                           closure
                                                                           ;; MODIFYING THE STATE
                                                                           (m-var-dec
                                                                            (list 'var
                                                                                  (catch-var-name
                                                                                   (second-body exp))
                                                                                  exception) (m-push s))
                                                                           return k continue
                                                                           try catch finally)) finally)))
                return break continue try catch finally)]
      [else
       (error 'undefined "try statement missing catch or finally")])))

;; Code a function that can take in expression of numbers and operators and return the value
;; e.g. (+ 3 (/ 4 2))
;;      (+ 1 2)
;; The operators are +, -, *, /, %, and division is integer division
(define m-value
  (lambda (exp closure s)
    (cond
      ; null checking
      [(null? exp)                            (error 'undefined "undefined expression")]
      [(number? exp)                          exp] ; if it's a number, return that number
      [(and (not (pair? exp)) (boolean? exp)) exp] ; if it's a boolean, return that boolean

      ; boolean checking
      [(eq? exp 'true)                        #t] ; true
      [(eq? exp 'false)                       #f] ; false

      ; more complex boolean expression (e.g. 10 >= 20 || 10 == a)
      [(and (pair? exp) (am-i-boolean exp))   (m-condition exp s)]

      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                              (call/cc (lambda (k) (m-funcall (funcall-name exp) '() k closure s)))]

      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                              (call/cc (lambda (k) (m-funcall (funcall-name exp) (func-params exp) k s)))]


      ; variable checking
      [(not (pair? exp))                      (m-lookup-var exp s)]


      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                              (m-value (m-funcall (funcall-name exp) '() (λ(v) v) s) s)]

      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                              (m-value (m-funcall (funcall-name exp) (func-params exp) (λ(v) v) s) s)]


      ;operators
      [(eq? (operator exp) '+) (+         (m-value (left-operand exp) closure s) (m-value (right-operand exp) closure s))]
      [(and (eq? (operator exp) '-) (null? (right-operand-exists exp))) ; handle negitive numbers
                               (* -1      (m-value (left-operand exp) s))]
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
      [(null? exp)                          (error 'undefined "undefined expression")]

      ; run the loop of the body
      [(m-condition (loop-condition exp) s) (m-state (loop-body exp) s
                                                     return break continue try catch finally)]

      ; if there's no else statement, return the state
      [(null? (cdddr exp)) s]

      ; run the else of the body
      [else                                 (m-state (else-statement exp) s
                                                     return break continue try catch finally)])))

;; Implementing while loop
(define m-while-loop
  (lambda (exp s return break continue try catch finally)
    (cond
      ; invalid expression
      [(null? exp)
       (error 'undefined "undefined expression")]

      ; runs the while loop (body is multiple statements)
      [(m-condition (loop-condition exp) s)
       (m-while-loop exp (call/cc (lambda (k) (m-state (loop-body exp) s
                                                       return break k try catch finally)))
                     return break continue try catch finally)]

      ; otherwise, returns initial state
      [else s])))

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
  (lambda (exp closure s return finally)
    (cond
      [(eq?   exp #t)                       (return 'true)]
      [(eq?   exp #f)                       (return 'false)]
      [(and (pair? exp) (am-i-boolean exp)) (finally (m-return (m-condition exp s) s return finally))]
      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                            (return (m-value (m-funcall (funcall-name exp) '() return s)))]

      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (return (m-funcall (funcall-name exp) (func-params exp) return s))]

      [(pair? exp)                          (return (m-value exp closure s))]
      [(eq? (m-value exp closure s) #t)     (return 'true)]
      [(eq? (m-value exp closure s) #f)     (return 'false)]
      [else                                 (return (m-value exp closure s))])))

;; Takes an assinment and a state
;; Returns the updated state
(define m-assign
  (lambda (assign s)
    (if (not (locate-var (variable assign) s))
        (error "use before declaration")
        (m-update (variable assign) (m-value (expression assign) s) s))))


;; Takes a variable declaration and a state
;; Returns the updated state
(define m-var-dec
  (lambda (dec closure s)
    (cond
      ; check variable not already declared
      [(local-locate (variable dec) (closure-body closure)) (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))         (m-add (variable dec) s)]
      ; need to add a value, and that value is a class
      [(eq? (is-new-instance dec) 'new) (m-instance-dec dec closure s)]
      ; need to add value as well
      [else                             (m-update (variable dec)
                                                 (m-value (expression dec) closure s)
                                                 (m-add (variable dec) closure s) s)])))

; declares a variable that's an instance
(define m-instance-dec
  (lambda (dec closure s)
    (m-update (variable dec) (m-lookup-class (instance-class-name dec) s) closure)))

(define m-global-var-dec
  (lambda (dec closure s)
    (cond
      ; check variable not already declared
      [(locate-global-var (variable dec) (closure-body closure))  (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))              (m-add-global-var (variable dec) closure s)]
      ; need to add value as well
      [else                                  (m-update (variable dec)
                                                       (m-value (expression dec) closure s)
                                                       (m-add-global-var (variable dec) closure s))])))


#|
define an internal var and func closure for the class with abstration with the format:
'(((((var1, var2 ..)(val1, val2 ..))((local-func1, localfunc2 ...)(closure1, closure2 ... )))(Inner local layer1)(Inner local layer2))
   (((glob-var1, glob-var2 ...)(val1, val2 ..))((glob-func1, glob-func2 ..)(closure1, closure2 ..)))))
methods for state
(Note, when searching for values/closures, they will be searched for first in the local layer
then globaly)
m-lookup-var - looks up variable's value, returns value found at highest layer
m-lookup-func - lookes up a function's closure, returns closure found at highest level
m-update - updates variable's value on the first location the variable is found, returns updated state
m-add-local-var - adds uninitilized variable to local layer of state (on topmost layer), returns updated state
m-add-global-var - adds uninitilized variable to global layer of state , returns updated state
m-add-local-func - adds function and function closure to local layer of state
m-add-global-func - adds function and function closure to the global layer of state
|#

;; Takes a variable and a state
;; Returns the value of the variable at the first instance it is found, or error message if it does not exist
;; Will error if not yet initilized or if it does not exist


;;takes a state and strips off everything except the top layer and global
#| class closure needs to be passed in. later on, state will need to be passed in
need to handle objectes function calls on object, and
 superclasses (with inherited vars and functions),
update lookup for funcs vars to continue looking if it has a superclass still
in that case need to pass in entire closure along with the specific body,
just pass along and continue if have super class
|#
;;all of these functions now take a class closure instead of the full state.
;;They will only operate on the class closure
(define m-strip
  (lambda (s)
   (list (list (toplayer s)) (global s))))

;; takes a variable and a state
;; returns the value or an error

(define m-lookup-var
  (lambda (var closure s)
    [(m-lookup-var-nested var (closure-body closure) closure s)]))


(define m-lookup-var-nested
  (lambda (var closure-s closure state)
    (cond
      [(null? closure-s)                       (error "use before declared")]
      [(null? (local closure-s))               (lookup-global-var var closure-s closure state)]
      [(null? (vars closure-s))                (m-lookup-var var (nextlayer closure-s) closure state)]
      [(and (equal? var (nextvar closure-s)) (eq? "init" (unbox (nextval closure-s))))
        (error "use before assignment")]
      [(equal? var (nextvar closure-s))        (unbox (nextval closure-s))]
      [else                            (m-lookup-var var (next-part-vars closure-s) closure state)])))


;; takes a global variable and a state
;; returns the value or an error
(define lookup-global-var
  (lambda (var closure-s closure state)
    (cond
     [(and (empty-check closure-s) (not (null? (closure-super closure)))) (lookup-global-var var (m-lookup-class (car (closure-super closure)) state))]
     [(empty-check closure-s)                    (error "use before declared")]
     [(and (eq? var (global-nextvar closure-s)) (eq? "init" (unbox (global-nextval closure-s))))
                                      (error "use before assignment")]
     [(equal? var (global-nextvar closure-s)) (unbox (global-nextval closure-s))]
     [else                            (lookup-global-var var (global-nextpart-vars closure-s) closure state)])))

(define empty-check
  (lambda (closure-s)
    (if (or (or (null? closure-s) (null? (global closure-s))) (null? (global-vars closure-s)))
        #t
        #f)))


;; takes a function and a state
;; returns the function closure
(define m-lookup-func
  (lambda (var closure s)
    (m-lookup-func-nested var (closure-body closure) s)))


(define m-lookup-func-nested
  (lambda (func closure-s s)
    (cond
      [(null?  closure-s)                      (error "function not found")]
      [(null? (local  closure-s))              (lookup-global-func func  closure-s)]
      [(null? (funcs  closure-s))              (m-lookup-func-nested func (nextlayer  closure-s) s)]
      [(equal? func (nextfunc  closure-s))     (unbox (nextfunc-def  closure-s))]
      [else                           (m-lookup-func-nested func (next-part-funcs  closure-s))])))


;; takes a global function and a state
;; returns the function closure
(define lookup-global-func
  (lambda (func s)
    (cond
     [(or (or (null? s)(null? (global s))) (null? (global-funcs s)))
                                        (error "function not found")]
     [(equal? func (global-nextfunc s)) (unbox (global-nextfunc-def s))]
     [else                              (lookup-global-func func (global-nextpart-funcs s))])))


;; takes a variable, the value to be updated, and the state
;; returns the updated state
(define m-update
  (lambda (var update-val closure)
    (list (closure-class-name closure) (closure-super closure) (m-update-nested var update-val (closure-body closure)))))


(define m-update-nested
  (lambda (var update-val s)
    (cond
      [(null? s)                "error"]
      [(not (locate-var var s)) "error"]
      [(local-locate-var var s) (list (local-update var update-val (local s)) (global s))]
      [else                     (list (local s) (global-update var update-val (global s)))])))

;; takes a variable, the value to be updated, and the local layer of the state
;; returns the updated local layer
(define local-update
  (lambda (var update-val s)
    (cond
      [(null? s)      "error"]
      [(local-layer-locate var (top-layer s))
                      (cons (local-toplayer-update var update-val (top-layer s)
                                                   (lambda (v1 v2) (list (list v1 v2) (local-funcs s))))
                            (rest-of s))]
      [else           (cons (top-layer s) (local-update var update-val (cdr s)))])))


;; takes a variable, the value to be updated, and the global layer of the state
;; returns the updated global layer
(define global-update
  (lambda (var update-val s)
    (cond
      [(null? s)                         "error"]
      [(not (local-layer-locate var s))  "error"]
      [else                              (local-toplayer-update var update-val s
                                                                (lambda (v1 v2) (list (list v1 v2)
                                                                                      (s-funcs s))))])))

;;takes a variable, the value to be updated and the layer to be updated
;;returns the variables and updated values of the layer in two lists, to be combined by the calling function
(define local-toplayer-update
  (lambda (var update-val s return)
    (if (equal? var (s-nextvar s))
        (return (s-vars s) (begin  (set-box! (s-nextval s) update-val)
                                   (cons (s-nextval s) (rest-of (s-vals s)))))
        (local-toplayer-update var update-val  (s-next-part-vars s)
                               (lambda (v1 v2) (return (cons (s-nextvar s) v1)
                                                       (cons (s-nextval s) v2)))))))



;; Takes a local variable and a state, adds it to the topmost local section of the state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
(define m-add
  (lambda (var closure s)
    [(list (closure-class-name closure) (closure-super closure) (m-add-nested var (closure-body closure)))]))

(define m-add-nested
  (lambda (var s)
     (list (cons (list (list (cons  var (vars s))
                             (cons (box "init") (vals s))) (func-layer s))
                 (cdr (local s)))
           (global s))))

;; Takes a local function and it's closure, adds the function and it's closure to the topmost local section of the state
(define m-add-local-func
  (lambda (func func-closure class-closure s)
    (list (closure-class-name class-closure) (closure-super class-closure) (m-add-local-func-nested func func-closure class-closure s))))



(define m-add-local-func-nested
  (lambda (func func-closure class-closure s)
    (list (cons (list (var-layer s) (list (cons func (funcs s))
                                          (cons (box func-closure) (func-defs s))))
                (cdr (local s)))
          (global s))))

;; Takes a global variable and a state, adds it to the global section of the state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
(define m-add-global-var
  (lambda (var closure s)
    (list (closure-class-name closure) (closure-super closure) (m-add-global-var-nested var (closure-body closure)))))

(define m-add-global-var-nested
  (lambda (var s)
    (list (local s) (list (list (cons var (global-vars s))
                                (cons (box "init") (global-vals s)))
                          (global-func-layer s)))))

;; Takes a global function and it's closure, adds the function and it's closure to the global section of the state
(define m-add-global-func
  (lambda (func func-closure class-closure)
    (list (closure-class-name class-closure) (closure-super class-closure) (m-add-global-func-nested  func func-closure (closure-body class-closure)))))

(define m-add-global-func-nested
  (lambda (func closure s)
    (list (local s) (list (global-var-layer s)
                          (list (cons func (global-funcs s))
                                (cons (box closure) (global-func-defs s)))))))

;;; the following are helper methods for state functions

;;takes a variable and a state
;; returns true if the variable exists on the top layer of the state, false otherwise
;can no longer use i think
(define local-locate
  (lambda (var s)
    (cond
      [(null? s)             #f]
      [(null? (vars s))      #f]
      [(eq? var (nextvar s)) #t]
      [else                  (local-locate var (next-part-vars s))])))


;;returns #t if the variable exists in the topmost layer
(define local-layer-locate
  (lambda (var s)
    (cond
      [(null? s)               #f]
      [(null? (s-vars s))      #f]
      [(eq? var (s-nextvar s)) #t]
      [else                    (local-layer-locate var (s-next-part-vars s))])))


;; returns #t if the var is found in the state, #f otherwise
;; Takes the variable it is locating and a state
(define locate-var
  (lambda (var s)
    (cond
      [(null? s)             #f]
      [(eq? (local s) '())   (locate-global-var var s)]
      [(null? (vars s))      (locate-var var (nextlayer s))]
      [(eq? var (nextvar s)) #t]
      [else                  (locate-var var (next-part-vars s))])))


;; returns #t if the given variable exists in the global layer
(define locate-global-var
  (lambda (var s)
    (cond
      [(null? s)                    #f]
      [(null? (global s))           #f]
      [(null? (global-vars s))      #f]
      [(eq? var (global-nextvar s)) #t]
      [else                         (locate-global-var var (global-nextpart-vars s))])))


;; returns #t if the given variable exists in the local layer
(define local-locate-var
   (lambda (var s)
    (cond
      [(null? s)             #f]
      [(null? (local s))     #f]
      [(null? (vars s))      (local-locate-var var (nextlayer s))]
      [(eq? var (nextvar s)) #t]
      [else                  (local-locate-var var (next-part-vars s))])))


;; returns #t if the given function exists in any part of the state
(define locate-func
  (lambda (func s)
    (cond
      [(null? s)               #f]
      [(eq? (local s) '())     (locate-global-func func s)]
      [(null? (funcs s))       (locate-func func (nextlayer s))]
      [(eq? func (nextfunc s)) #t]
      [else                    (locate-func func (next-part-funcs s))])))

;; returns #t if the given global function exists
(define locate-global-func
  (lambda (func s)
    (cond
      [(null? s)                      #f]
      [(null? (global s))             #f]
      [(null? (global-funcs s))       #f]
      [(eq? func (global-nextfunc s)) #t]
      [else                           (locate-global-func func (global-nextpart-funcs s))])))

;; will return the instance's closure
(define get-instance
  (lambda (name closure s)
    (m-lookup-var name closure s)))

;; will return a value
(define m-dot
  (lambda (var-name func-name params closure s return)
    (m-funcall func-name params return (get-instance var-name closure s) s)))

;new state format
;starting state is empty list
;(class with closure, class with closure, class with closure)
;class with closure contains: (name (superclass) (traditional state))
(define next-class caar)
(define next-extends cadar)
(define next-closure caddar)
(define next-part-classes cdr)
(define c1 '(class B (extends A)  body))
(define c2 '(class A () body))

;returns values for the input class information
;format (class A (super) (body))
(define class-name cadr)
(define class-extends
  (lambda (class-closure)
    (if (null? (caddr class-closure))
        '()
      (cdaddr class-closure))))
(define class-body cadddr)

;returns values for a class closure
;format (A (super) (body)) where body is a complete state of vars and funcs
(define closure-super cadr)


(define closure-class-name car)
(define closure-body caddr)
(define next car)
(define first car)


;;iterate along next part of state, classnames, and closures
(define s-test '((A (B) (stateA))(B (C) (stateB))(C () (stateC))))
(define noI '((C () (stateC))))
(define yesI '((A (B) (stateC))))
(define simplebody '(A (B) (stateC)))
(define simpleemptybody '(A () (stateC)))

;returns the class closure for the given class name
(define m-lookup-class-closure
  (lambda (class-name s)
    (cond
      [(null? s) (error "class does not exist")]
      [(equal? class-name (next-class s)) (next-closure s)]
      [else (m-lookup-class-closure class-name (next-part-classes s))])))

(define m-lookup-class
  (lambda (class-name s)
    (cond
      [(null? s) (error "class does not exist")]
      [(equal? class-name (next-class s)) (next s)]
      [else (m-lookup-class class-name (next-part-classes s))])))

#|(define m-update-class-closure
  (lambda (closure update s)
    (cond
      [(null? s) '()]
      [(equal? class-name (next-class s)) (cons (m-new-closure class-name (closure-super (next-class s)) closure-to-update) s)]
      [else (cons (next-closure s) (m-update-class-closure class-name closure-to-update (next-part-classes s)))])))
|#
(define m-new-closure
  (lambda (class-name class-super closure)
    (list (class-name class-super closure))))

;takes a state and class name and returns the class the class extends
(define m-lookup-super-class
  (lambda (class-name s)
    (cond
      [(null? ( s)) (error "class does not exist")]
      [(equal? class-name (next-class s)) (next-extends s)]
      [else (m-lookup-super-class class-name (next-part-classes s))])))

;add a class to a state
(define m-add-class
 (lambda (class-dec s)
   (cons (generate-closure (class-body class-dec) (list (class-name class-dec) (class-extends class-dec) empty-state) s) s)))


;This needs to be filled in. Given a class, the closure or code for the class should be filled in
;all of the functions and global variables must be searched and filled in
(define generate-closure
  (lambda (body closure s)
    (cond
      [(null? body) closure]
      [(equal? 'var (first (next body))) (generate-closure (cdr body) (m-global-var-dec (next body) closure s) s)]
      [(equal? 'function (first (next body)))
       (generate-closure (cdr body) (m-add-global-func  (full-func (next body)) (list (append (list (func-name (next body)))(list (func-body (next body))))) closure) s)]
      [(equal? 'static-function (first (next body)))
       (generate-closure (cdr body) (m-add-global-func  (full-func (next body)) (list (append (list (func-name (next body)))(list (func-body (next body))))) closure) s)]
      [else (generate-closure (cdr body) closure s)])))



(define test-class '((var x 100)
     (var y 10)
     (function add (g h) ((return (+ g h))))
     (static-function main () ((return (funcall (dot (new A) add) (dot (new A) x) (dot (new A) y)))))))
(define empty-closure '(dd () ((((() ()) (() ()))) ((() ()) (() ())))))
;(generate-closure test-class empty-closure empty-state)

;;;;**********ABSTRACTION**********
(define statement-type-id car) ; e.g. if, while, var, etc.
(define statement-body cadr)   ; e.g. the body of a return statement

; for if statements
(define else-statement cadddr) ; else statement, if it exists
(define loop-condition cadr)
(define loop-body caddr)

; for value operations
(define left-operand cadr)
(define operator car)
(define right-operand caddr)
(define right-operand-exists cddr)

;for m-var-dec
(define assignment cddr)
(define is-new-instance caaddr)
(define instance-class-name (lambda (v) (cadar (assignment v))))
(define variable cadr)
(define expression caddr)

; for try/catch/finally
(define try-body cadr)
(define catch-statement caddr)
(define second-identifier caaddr) ;; will say "catch" if it's a catch
(define second-body cdaddr) ;; body of the catch statement
(define third-statement cadddr)
(define third-identifier (lambda (s) (car (third-statement s))))
(define third-body (lambda (s) (cadr (third-statement s))))
(define catch-body cadr)
(define catch-var-name caar)

; dot product stuff
(define dot-var-name  cadadr)
(define dot-func-name (lambda (s) (car (cddadr s))))

; for remove
(define first-val car)

; for function definition/calling
(define func-name caddr)
(define func-body cdddr)
(define full-func cadr)
(define main-body cadddr)
(define func-params cddr)
(define funcall-name cadr)
(define no-params '())

(define func-formal-params caar)
(define func-call-body caadar)

; for state computation
(define vars caaaar) ;local vars
(define vals (lambda (s) (car (cdaaar s)))) ;local vals
(define nextvar (lambda (s) (car (vars s)))) ;local var
(define nextval (lambda (s) (car (vals s)))) ;local val
(define local-layer caar) ;returns single local layer
(define var-layer  caaar)
(define func-layer cadaar)
(define func-layer2 cadar)
(define global cadr) ;returns global state
(define local car) ;returns entire local state
(define toplayer caar)



(define next-local-layer ;returns entire state minus a local layer
  (lambda (s)
        (list (cdr (local s)) (global s))))

(define nextlayer next-local-layer);;TURN TO NEXTLOCALLAYER

(define layer car) ;;;;REMOVE FROM ALL

(define next-part-vars
  (lambda (s)
    (list (cons (list (list (cdr (vars s)) (cdr (vals s))) (func-layer s)) (cdr (local s))) (global s)))) ;has extra parens when removing layer, probobly for best

(define next-part-funcs
  (lambda (s)
    (list (cons (list (var-layer s) (list (cdr (funcs s)) (cdr (func-defs s)))) (cdr (local s))) (global s))))

(define nextfunc (lambda (s) (caar (func-layer s))))
(define nextfunc-def (lambda (s) (caadr (func-layer s))))
(define funcs (lambda (s) (car (func-layer s)))) ; local funcs
(define func-defs (lambda (s) (cadr (func-layer s)))) ;local func defs
(define global-var-layer (lambda (s) (car (global s))))
(define global-func-layer (lambda (s) (cadr (global s))))
(define global-vars (lambda (s) (car (global-var-layer s))))
(define global-vals (lambda (s) (cadr (global-var-layer s))))
(define global-funcs (lambda (s) (car (global-func-layer s))))
(define global-func-defs (lambda (s) (cadr (global-func-layer s))))
(define global-nextvar (lambda (s) (car (global-vars s))))
(define global-nextval (lambda (s) (car (global-vals s))))
(define global-nextfunc (lambda (s) (car (global-funcs s))))
(define global-nextfunc-def (lambda (s) (car (global-func-defs s))))
(define global-nextpart-vars
  (lambda (s)
    (list (local s) (cons (list (cdr (global-vars s)) (cdr (global-vals s))) (cdr (global s))))))
(define global-nextpart-funcs
  (lambda (s)
    (list (local s) (list (global-var-layer s) (list (cdr (global-funcs s)) (cdr (global-func-defs s)))))))

;(define global-vals (lambda cadaadr)
(define rest-of cdr)
(define b-global-vars (lambda (s) (caar (global s))))
(define b-global-nextval (lambda (s) (cadar (global s))))

;single local or global layer abstraction, includes only one set of vars, one set of functions
(define s-nextvar caaar)
(define s-nextval caadar)
(define s-varlayer car)
(define s-vars caar)
(define s-vals cadar)
(define s-funcs cadr)
(define s-next-part-vars
  (lambda (s)
    (list (list (cdr (s-vars s)) (cdr (s-vals s))) (cadr s))))
;;top layer when dealing with just the local state
(define top-layer car)

(define local-funcs cadar)
(define next-part-local cdr)

; for running/state
(define new-layer '((()())(()())))
(define empty-state '((((()())(()())))((()())(()()))))
(define empty-list '())
(define class-adding-state '(()(()())))
(define b '((((()())(()())))((()())(()()))))
(define first-statement car)
(define rest-of-body cdr)

 ;;FOR TESTING PURPOSES!!!:
(define a-global '(((a b) (1 2))((f1 f2)((stuff1) (stuff2)))))
(define a-local '((((c d) (3 4))((f3 f4)((s3) (s4))))(((g h) (5 6))((f5 f6)((s5) (s6))))))
(define a  '(((((c d) (3 4))((f3 f4)((s3) (s4))))(((g h) (5 6))((f5 f6)((s5) (s6)))))(((a b) (1 2))((f1 f2)((stuff1) (stuff2))))))
(define c '((((() ()) (() ()))) (((a) (#&"init")) (() ()))))
(define d '((((() ()) (() ()))) (((a) (#&2)) (() ()))))
(define e '(((((c d) (#&1 #&34)) ((f1 f2) (#&(stufffff) #&(stuff2))))(((q)(#&0))((f3 f4)(#&(dd) #&(qqq))))) (((a) (#&2)) ((f5 f6) (#&(s5) #&(s6))))))
(define q '((((() ())((f3 f4)((s3) (s4))))(((g h) (5 6))((f5 f6)((s5) (s6)))))(((a b) (1 2))((f1 f2)((stuff1) (stuff2))))))
(define state2 '(((a b c d)(#&2 #&5 #&6 #&7))((s d e w)(#&1 #&8 #&9 #&0))))
(define w '(((a b) (1 2)) ((f1 f2) ((stuff1) (stuff2)))))
(define p '(((a b) (#&1 #&2)) ((f1 f2) (#&(s1) #&5(s2)))))
;;(define z '((((c d) (#&1 #&34)) ((f1 f2) (#&(stufffff) #&(stuff2))))(((q)(#&0))((f3 f4)(#&(dd) #&(qqq)))) (((a f)(#&2 #&1))((f8 f9)(#&(yyd) #&(uuu)))))) ;local test
(define qqq  '(((((x) (#&"init")) (() ())) ((() ()) (() ()))) ((() ()) (() ()))))
(define test1 '(((((z y x) (#&30 #&20 #&10)) (() ())) ((() ()) (() ()))) ((() ()) (() ()))))

(define c1-closure (cons '(super-a) e))
(define c2-closure (cons '(super-b) q))
(define c3-closure (cons '(super-c) qqq))
;;'(q
 ;; ((((c d) (#&1 #&34)) ((f1 f2) (#&(stufffff) #&(stuff2)))) (((q) (#&0)) ((f3 f4) (#&(dd) #&(qqq)))))
 ;; (((a) (#&2)) ((f5 f6) (#&(s5) #&(s6)))))
;;new state format
(define a1 '((c1 c2 c3 c4) (close1 close2 close3 close4)))
(define a2 (cons '(c1 c2 c3) (list (list c1-closure c2-closure c3-closure))))
#|
'((c1 c2 c3)
  ((super-a
    ((((c d) (#&1 #&34)) ((f1 f2) (#&(stufffff) #&(stuff2)))) (((q) (#&0)) ((f3 f4) (#&(dd) #&(qqq)))))
    (((a) (#&2)) ((f5 f6) (#&(s5) #&(s6)))))
   (super-b
    (((() ()) ((f3 f4) ((s3) (s4)))) (((g h) (5 6)) ((f5 f6) ((s5) (s6)))))
    (((a b) (1 2)) ((f1 f2) ((stuff1) (stuff2)))))
   (super-c ((((x) (#&"init")) (() ())) ((() ()) (() ()))) ((() ()) (() ()))))) |#

#|
(trace generate-closure)
(trace m-global-var-dec)
(trace m-update)
(trace m-add-global-var)
(trace m-update-nested)
(trace m-lookup-func)
(trace m-lookup-func-nested)
(trace m-var-dec)
(trace m-state)
(trace m-update)
(trace m-value)
|#


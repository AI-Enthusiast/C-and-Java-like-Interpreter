#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Will run a txt file containing code by using the run function (run "Filename.txt" "class w/ main")
;;;; Order of inputs for ALL m-state and m-state like things matters

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
       (m-funcall 'main no-params callcc
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
                                                                       (m-push closure) s return k continue
                                                                       try catch finally)))]
      ; else: process one statement at a time
      [else                                (m-state (rest-of-body exp)
                                                    (m-what-type (first-statement exp) closure s return break
                                                                 continue try catch finally) s
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
    (list (closure-class-name closure) (closure-super closure) (list (cons new-layer (local (closure-body closure))) (global (closure-body closure))))))

;; Works through the top layer of the code then
(define m-base-layer
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(null? exp)                          s]
      [(null? (rest-of-body exp))           (m-base-layer (first-statement exp) closure s
                                                         return break continue try catch finally)]

      ; is it a class
      [(eq? (statement-type-id exp) 'class) (m-add-class exp s)]

      ;is it a function
      [(eq? (statement-type-id exp) 'function)
                                            (m-add-global-func (full-func exp) (list (append (list (func-name exp))
                                                                                    (list (func-body exp)))) closure)]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)   (m-var-dec exp closure s)]

      ; otherwise, process the first statement, and then the rest of it
      ; (the program shouldn't actually reach this point, because all things in the
      ; main base level of the program will be either functions or variable declarations.
      [else                                 (m-base-layer (rest-of-body exp) closure
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
                   (m-update (dot-var-name exp) (m-dot-func (dot-var-name exp)  (dot-var-name exp) (dot-func-name exp) no-params closure s (lambda (v) s)) closure s)]

      ;is it a function call w/dot
      [(and (eq? (statement-type-id exp) 'funcall)
            (list? (funcall-name exp)))
                   (m-update (dot-var-name exp) (m-dot-func (dot-var-name exp) (dot-func-name exp) (func-params exp) closure s (lambda (v) s)) closure s)]

      ;is it a function call w/o parameters
      [(and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp)))
                                               (m-funcall (funcall-name exp) no-params (lambda (v) closure) s)]

      ;is it a function call
      [(eq? (statement-type-id exp) 'funcall)
                                               (m-funcall (funcall-name exp) (func-params exp) (lambda (v) closure) closure s)]

      ; is it a new block
      [(eq? (first-statement exp) 'begin)      (m-pop (m-state (rest-of-body exp) (m-push closure) s
                                                               return break continue try catch finally))]

      ; conditional statement checking (if/while/etc.)
      [(eq? (statement-type-id exp) 'if)       (m-if-statement exp closure s return break continue try catch finally)]
      [(eq? (statement-type-id exp) 'while)    (call/cc (lambda (k) (m-while-loop exp closure s return k continue
                                                                                  try catch finally)))]

      ; is it a break
      [(eq? (statement-type-id exp) 'break)    (break (m-pop closure))]

      ; is it a continue
      [(eq? (statement-type-id exp) 'continue) (continue closure)]

      ; is it a try/catch statement
      [(eq? (statement-type-id exp) 'try)      (call/cc (λ (k) (m-try-catch-finally exp closure s return break
                                                                                    continue k catch
                                                                                    finally)))]

      ; is it a throw
      [(eq? (statement-type-id exp) 'throw)    (try (m-pop (catch (statement-body exp))))]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)      (m-var-dec exp closure s)]

      ; is it an assignment
      [(eq? (statement-type-id exp) '=)        (m-assign exp closure s)]

      ; is it a return statement
      [(eq? (statement-type-id exp) 'return)   (m-return (statement-body exp) closure s return finally)]

      ; oh no
      [else                                    (error 'undefined "undefined expression")])))


;; ABSTRACTION M-FUNCALL
(define funcall-function-name caddr)
(define funcall-instance-name cadr)


;; m-funcall returns a number
(define m-funcall
  ;; name is name of the function, actual = input parameters
  (lambda (name actual return closure s)
    (cond
      ; is it a dot this funcall (dot this a)
      [(and (list? name) (eq? (cadr name) 'this))  (m-update (funcall-function-name name) (m-funcall (funcall-function-name name) actual return closure s) closure s)]
      ; is it a dot funcall (dot a setX) () => a.setX()
      [(list? name)                               (m-update (funcall-instance-name name)
                                                            (m-dot-func (funcall-instance-name name) (funcall-function-name name) actual  closure s return)
                                                            closure s)]
      [else
        ;gets the body and the formal parameters of the function
        (let* [(all (m-lookup-func name closure s))
               (formal (func-formal-params all))
               (body (func-call-body all))]
          (if (eq? (num-in-list actual 0) (num-in-list formal 0))
              (m-state body (lists-to-assign actual formal (m-push closure) s) s
                       return
                       (lambda (v) v) ;; break
                       (lambda (v) v) ;; continue
                       (lambda (v) v) ;; try
                       (lambda (v) v) ;; catch
                       (lambda (v) v)) ;; finally
              (error 'undefined "Paramater mismatch")))])))

;; Takes two lists l1: (actual values)  l2: (formal values)
;; Returns an updated state
;; eg: (lists-to-assign '(1 2 3) '(a b c) closure s)
(define lists-to-assign
  (lambda (l1 l2 closure s) ;; l1 = actual parameters, l2 = formal parameters
    (cond
      [(null? l1)            closure]
      ;[(and (not (number? (car l1))) (not (boolean? (car l1)))) (lists-to-assign (cons (m-value (car l1) closure s) (cdr l1)) l2 closure s)]
      [(and (not (number? (car l1))) (> (num-in-list l1 0) 1))
                    (lists-to-assign (list-from-state l1 closure s) l2 closure s)] ;if l1 null assign this to closure
      

      [else (lists-to-assign (cdr l1) (cdr l2)
                                     (m-var-dec (cons 'var (cons (car l2) (list (car l1)))) closure s) s)])))

(define list-from-state
  (lambda (lis closure s)
    (cond
      [(null? lis) '()]
      [(not (number? (car lis))) (cons (m-lookup-var (car lis) closure s) (list-from-state (cdr lis) closure s))]
      [else (cons (car lis) (list-from-state (cdr lis) closure s))])))


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

                                                                  ;; MODIFYING THE STATE
                                                                  (m-var-dec (list 'var (catch-var-name
                                                                                         (second-body exp))
                                                                                   exception) (m-push closure) s)
                                                                  s
                                                                  return break continue k catch finally))
                                     finally)))]

      ; check if has finally first (no catch)
      [(and (eq? (third-identifier exp) 'finally) (not (pair? (catch-statement exp))))
       (m-state (third-body exp) (m-state (try-body exp) closure s return break continue
                                          (lambda (v) closure) (lambda (v) closure) finally)
                s
                return break continue try catch finally)]

      ; check for a catch AND a finally
      [(and (eq? (second-identifier exp) 'catch) (eq? (third-identifier exp) 'finally))
       (m-state (third-body exp)
                (call/cc (lambda (k) (m-state (try-body exp) closure s return break continue k
                                              ;; CATCH STATEMENT
                                              (lambda (exception) (m-state (catch-body (second-body exp))
                                                                           ;; MODIFYING THE CLOSURE
                                                                           (m-var-dec
                                                                            (list 'var
                                                                                  (catch-var-name
                                                                                   (second-body exp))
                                                                                  exception) (m-push closure) s)
                                                                           s
                                                                           return k continue
                                                                           try catch finally)) finally)))
                s
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
      ; is it a this?
      [(and (and (list? exp) (eq? (car exp) 'dot)) (eq? (cadr exp) 'this))
                                              (lookup-global-var (caddr exp) (closure-body closure) closure s)]
      [(and (not (pair? exp)) (boolean? exp)) exp] ; if it's a boolean, return that boolean

      ; boolean checking
      [(eq? exp 'true)                        #t] ; true
      [(eq? exp 'false)                       #f] ; false

      ; more complex boolean expression (e.g. 10 >= 20 || 10 == a)
      [(and (pair? exp) (am-i-boolean exp))   (m-condition exp closure s)]

      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                              (call/cc (lambda (k) (m-funcall (funcall-name exp) '() k closure s)))]

      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                              (call/cc (lambda (k) (m-funcall (funcall-name exp) (func-params exp) k closure s)))]


      ; variable checking
      [(not (pair? exp))                      (m-lookup-var exp closure s)]

      ; is it looking up a variable in another function
      [(and (pair? exp) (eq? (statement-type-id exp) 'dot)) (m-dot-value (dot-instance-name exp) (dot-variable-name exp) closure s)]
;instance variable closure

      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                              (m-value (m-funcall (funcall-name exp) '() (λ(v) v) s) s)]

      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                              (m-value (m-funcall (funcall-name exp) (func-params exp) (λ(v) v) s) s)]



      ;operators
      [(eq? (operator exp) '+) (+         (m-value (left-operand exp) closure s) (m-value (right-operand exp) closure s))]
      [(and (eq? (operator exp) '-) (null? (right-operand-exists exp))) ; handle negitive numbers
                               (* -1      (m-value (left-operand exp) closure s))]
      [(eq? (operator exp) '-) (-         (m-value (left-operand exp) closure s) (m-value (right-operand exp) closure s))]
      [(eq? (operator exp) '*) (*         (m-value (left-operand exp) closure s) (m-value (right-operand exp) closure s))]
      [(eq? (operator exp) '/) (quotient  (m-value (left-operand exp) closure s) (m-value (right-operand exp) closure s))]
      [(eq? (operator exp) '%) (remainder (m-value (left-operand exp) closure s) (m-value (right-operand exp) closure s))]

      ; oh no
      [else                    (error 'undefined "undefined expression")])))

;; Code a function that can take in an expression such as (< 5 2) and return true/false
;; Supports ==, !=, <, >, <=, >=, &&, ||, !
(define m-condition
  (lambda (exp closure s) ; exp = expression, s = state
    (cond
      ; null checking
      [(null? exp)               (error 'undefined "undefined expression")]
      [(not (pair? exp))         (m-value exp closure s)]
      [(null? (operator exp))    (m-value exp closure s)]

      

      ; condition checking (&&, ||, !)
      [(eq? (operator exp) '||)  (or  (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]
      [(eq? (operator exp) '&&)  (and (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]
      [(eq? (operator exp) '!)   (not (m-condition (left-operand exp) closure s))]

      ; equality/inequality operator checking (==, !=, <, >, <=, >=)
      [(eq? (operator exp) '==)  (eq? (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]
      [(eq? (operator exp) '!=)  (not (eq? (m-condition (left-operand exp) closure s)
                                           (m-condition (right-operand exp) closure s)))]
      [(eq? (operator exp) '<)   (<   (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]
      [(eq? (operator exp) '>)   (>   (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]
      [(eq? (operator exp) '<=)  (<=  (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]
      [(eq? (operator exp) '>=)  (>=  (m-condition (left-operand exp) closure s) (m-condition (right-operand exp) closure s))]

      ; oh no
      [else                      (m-value exp closure s)])))

;; Implementing if statement
(define m-if-statement
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; invalid expression
      [(null? exp)                          (error 'undefined "undefined expression")]

      ; run the loop of the body
      [(m-condition (loop-condition exp) closure s) (m-state (loop-body exp) closure s
                                                     return break continue try catch finally)]

      ; if there's no else statement, return the closure
      [(null? (cdddr exp)) closure]

      ; run the else of the body
      [else                                 (m-state (else-statement exp) closure s
                                                     return break continue try catch finally)])))

;; Implementing while loop
(define m-while-loop
  (lambda (exp closure s return break continue try catch finally)
    (cond
      ; invalid expression
      [(null? exp)
       (error 'undefined "undefined expression")]

      ; runs the while loop (body is multiple statements)
      [(m-condition (loop-condition exp) closure s)
       (m-while-loop exp
                     ; CLOSURE:
                     (call/cc (lambda (k) (m-state (loop-body exp) closure s
                                                       return break k try catch finally)))
                     s
                     return break continue try catch finally)]

      ; otherwise, returns initial state
      [else closure])))

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
      [(and (pair? exp) (am-i-boolean exp)) (finally (m-return (m-condition exp closure s) closure s return finally))]
      ; is it a function call that involves dot
      [(and (pair? exp)
       (and (eq? (statement-type-id exp) 'funcall)
       (and (pair? (funcall-name exp))
            (eq? (car (funcall-name exp)) 'dot))))
                                            (return (m-dot-func (cadr (funcall-name exp)) (caddr (funcall-name exp)) (cddr exp) closure s return))]
      ; (var-name func-name params closure s return)
      
      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (func-params exp))))
                                            (return (m-value (m-funcall (funcall-name exp) (cddr exp) return closure s) closure s))]

      
      ; is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (return (m-funcall (funcall-name exp) (func-params exp) return closure s))]

      [(pair? exp)                          (return (m-value exp closure s))]
      [(eq? (m-value exp closure s) #t)     (return 'true)]
      [(eq? (m-value exp closure s) #f)     (return 'false)]
      [else                                 (return (m-value exp closure s))])))

;; Takes an assinment and a state
;; Returns the updated state
(define m-assign
  (lambda (assign closure s)
    (if (and (not (list (variable assign))) (not (locate-var (variable assign) (closure-body closure) closure s)))
        (error "use before declaration")
        (m-update (variable assign) (m-value (expression assign) closure s) closure s))))


;; Takes a variable declaration and a state
;; Returns the updated state
(define m-var-dec
  (lambda (dec closure s)
    (cond
      ; check variable not already declared
      [(local-locate (variable dec) (closure-body closure)) (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))        (m-add (variable dec) closure s)]
      ; need to add a value, and that value is a class
      [(eq? (is-new-instance dec) 'new) (m-instance-dec dec closure s)]
      [(and (pair? (expression dec)) (am-i-a-class-name (car (expression dec)) s))
                                       (m-update (variable dec)
                                                  (expression dec)
                                                  (m-add (variable dec) closure s) s)]

      
      ; need to add value as well
      [else                             (m-update (variable dec)
                                                 (m-value (expression dec) closure s)
                                                 (m-add (variable dec) closure s) s)])))

(define am-i-a-class-name
  (lambda (class-name s)
    (cond
      [(null? s) #f]
      [(equal? class-name (next-class s)) #t]
      [else (am-i-a-class-name class-name (next-part-classes s))])))

; declares a variable that's an instance
(define m-instance-dec
  (lambda (dec closure s)
    (m-update (variable dec) (m-lookup-class (instance-class-name dec) s) (m-add-global-var (variable dec) closure s) s)))


(define m-global-var-dec
  (lambda (dec closure s)
    (cond
      ; check variable not already declared
      [(locate-global-var-simple (variable dec) (closure-body closure))  (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))              (m-add-global-var (variable dec) closure s)]
      ; need to add value as well
      [else                                  (m-update (variable dec)
                                                       (m-value (expression dec) closure s)
                                                       (m-add-global-var (variable dec) closure s) s)])))


#|
The main state has the format
(class1, class2, class3 ...)

where a class closure has the format
(Class-name (super-class-name) (inner-class-closure))

where an inner class closure has the format
((local)(global))

where local has the format
((layer1)(layer2)(layer3) ...) and global has the format (layer)

An internal layer has the format
(((vars)(vals))((funcs)(func-definitions)))

A closer look, looks like this:
(((var1, var2 ..)(val1, val2 ..))((func1, func2 ...)(closure1, closure2 ... )))
|#


;; takes a variable and a state
;; returns the value or an error
(define m-lookup-var
  (lambda (var closure s)
    (m-lookup-var-nested var (closure-body closure) closure s)))


(define m-lookup-var-nested
  (lambda (var closure-s closure state)
    (cond
      [(null? closure-s)                       (error "use before declared")]
      [(null? (local closure-s))               (lookup-global-var var closure-s closure state)]
      [(null? (vars closure-s))                (m-lookup-var-nested var (nextlayer closure-s) closure state)]
      [(and (equal? var (nextvar closure-s)) (eq? "init" (unbox (nextval closure-s))))
                                               (error "use before assignment")]
      [(equal? var (nextvar closure-s))        (unbox (nextval closure-s))]
      [else                                    (m-lookup-var-nested var (next-part-vars closure-s) closure state)])))

 
;; takes a global variable and a state
;; returns the value or an error
(define lookup-global-var
  (lambda (var closure-s closure state)
    (cond
     [(and (empty-check-vars closure-s) (not (null? (closure-super closure))))
                                      (m-lookup-var var (m-lookup-class (car (closure-super closure)) state) state)]
     ; it's not there! oh no!
     [(empty-check-vars closure-s)    (error "use before declared")]

     ; it's not assigned! oh no!
     [(and (eq? var (global-nextvar closure-s)) (eq? "init" (unbox (global-nextval closure-s))))
                                      (error "use before assignment")]
     [(equal? var (global-nextvar closure-s)) (unbox (global-nextval closure-s))]
     [else                            (lookup-global-var var (global-nextpart-vars closure-s) closure state)])))

;;check if state is done with recursion with respect to variables
(define empty-check-vars
  (lambda (closure-s)
    (if (or (or (null? closure-s) (null? (global closure-s))) (null? (global-vars closure-s)))
        #t
        #f)))


;; takes a function and a state
;; returns the function closure
(define m-lookup-func
  (lambda (var closure s)
    (m-lookup-func-nested var (closure-body closure) closure s)))


(define m-lookup-func-nested
  (lambda (func closure-s closure state)
    (cond
      [(null?  closure-s)                      (error "function not found")]
      [(null? (local  closure-s))              (lookup-global-func func closure-s closure state)]
      [(null? (funcs  closure-s))              (m-lookup-func-nested func (nextlayer  closure-s) closure state)]
      [(equal? func (nextfunc  closure-s))     (unbox (nextfunc-def  closure-s))]
      [else                                    (m-lookup-func-nested func (next-part-funcs  closure-s) closure state)])))


;; takes a global function and a state
;; returns the function closure
(define lookup-global-func
  (lambda (func closure-s closure state)
    (cond
     [(and (empty-check-funcs closure-s) (not (null? (closure-super closure))))
                                        (m-lookup-func func (m-lookup-class (car (closure-super closure)) state) state)]
     [(empty-check-funcs closure-s)     (error "function not found")]
     [(equal? func (global-nextfunc closure-s))
                                        (unbox (global-nextfunc-def closure-s))]
     [else                              (lookup-global-func func (global-nextpart-funcs closure-s) closure state)])))

;; check if state is done with recursion in respect to functions
(define empty-check-funcs
  (lambda (closure-s)
    (if (or (or (null? closure-s) (null? (global closure-s))) (null? (global-funcs closure-s)))
        #t
        #f)))

;; takes a variable, the value to be updated, and the state
;; returns the updated state
(define m-update
  (lambda (var update-val closure s)
    (list (closure-class-name closure) (closure-super closure) (m-update-nested var update-val (closure-body closure) closure s))))


(define m-update-nested
  (lambda (var update-val closure-s closure s)
    (cond
      [(null? closure-s)                          "error"]
      ;is it a this?
      [(and (and (list? var) (eq? (car var) 'dot)) (eq? (cadr var) 'this))
                                                  (list (local closure-s) (global-update (caddr var) update-val (global closure-s)))]
      [(not (locate-var var closure-s closure s)) "error"]
      [(local-locate-var var closure-s)           (list (local-update var update-val (local closure-s)) (global closure-s))]
      [else                                       (list (local closure-s) (global-update var update-val (global closure-s)))])))

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
    (list (closure-class-name closure) (closure-super closure) (m-add-nested var (closure-body closure)))))

(define m-add-nested
  (lambda (var s)
     (list (cons (list (list (cons  var (vars s))
                             (cons (box "init") (vals s))) (func-layer s))
                 (cdr (local s)))
           (global s))))

;; Takes a local function and it's closure, adds the function and it's closure to the topmost local section of the state
(define m-add-local-func
  (lambda (func func-closure class-closure s)
    (list (closure-class-name class-closure) (closure-super class-closure) (m-add-local-func-nested func func-closure (closure-body class-closure) s))))



(define m-add-local-func-nested
  (lambda (func func-closure class-closure s)
    (list (cons (list (var-layer class-closure) (list (cons func (funcs class-closure))
                                          (cons (box func-closure) (func-defs class-closure))))
                (cdr (local class-closure)))
          (global class-closure))))

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
  (lambda (var closure-s closure state)
    (cond
      [(null? closure-s)             #f]
      [(eq? (local closure-s) '())   (locate-global-var var closure-s closure state)]
      [(null? (vars closure-s))      (locate-var var (nextlayer closure-s) closure state)]
      [(eq? var (nextvar closure-s)) #t]
      [else                  (locate-var var (next-part-vars closure-s) closure state)])))


;; returns #t if the given variable exists in the global layer
(define locate-global-var
  (lambda (var closure-s closure state)
    (cond
      [(and (empty-check-vars closure-s) (not (null? (closure-super closure))))
       (locate-var var (closure-body (m-lookup-class (car (closure-super closure)) state))
                   (m-lookup-class (car (closure-super closure)) state) state)]
      [(empty-check-vars closure-s)         #f]
      [(eq? var (global-nextvar closure-s)) #t]
      [else                         (locate-global-var var (global-nextpart-vars closure-s) closure state)])))

(define locate-global-var-simple
  (lambda (var closure-s)
    (cond
      [(empty-check-vars closure-s)         #f]
      [(eq? var (global-nextvar closure-s)) #t]
      [else                         (locate-global-var-simple var (global-nextpart-vars closure-s))])))


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
;; looking for a function
(define m-dot-func
  (lambda (instance func-name params closure s return)
    (cond
      ; the left side of the dot is a declaration
      [(and (list? instance) (eq? (car instance) 'new))
                   (m-funcall func-name (get-params-from-big-boy params closure s) return (m-lookup-class (cadr instance) s) s)]

      [(eq? instance 'super)
                   (m-funcall func-name (get-params-from-big-boy params closure s) return (m-lookup-class (car (closure-super closure)) s) s)]
      
      [else
                   (m-funcall func-name (get-params-from-big-boy params closure s) return (get-instance instance closure s) s)])))
      

;; This method gets the values of parameters from the original class closure in order to access variables that will become out of scope
;; we call it this because that is what we called it in our discussions
;; For clarity, in the code:
;;   main() {
;;    .... a.add(b)
;;   }
;; "big boy" is main()'s closure, while "little boy" is a's closure
;; We hope this makes you chuckle :) (it makes us chuckle, but maybe we just need to sleep)
(define get-params-from-big-boy
  (lambda (params closure s)
    (cond
      [(null? params) '()]
      [(and (list? params) (list? first-param))
                      (cons (m-dot-value (first-param-dot-instance params) (first-param-dot-variable params) closure s)
                            (get-params-from-big-boy (other-params params) closure s))]
      [(list? params) (cons (m-value (first-param params) closure s) (get-params-from-big-boy (other-params params) closure s))]
      [else           (m-value params closure s)])))



;; will return a value
;; looking for a variable
(define m-dot-value
  (lambda (instance variable closure s)
    (cond
      ; the left side of the dot is a declaration
      [(and (list? instance) (eq? (car instance) 'new))
            (m-lookup-var variable (m-lookup-class (cadr instance) s) s)]
      [else (m-lookup-var variable (get-instance instance closure s)  s)])))
    

(define m-lookup-class
  (lambda (class-name s)
    (cond
      [(null? s) (error "class does not exist")]
      [(equal? class-name (next-class s)) (new-closure (next s))]
      [else (m-lookup-class class-name (next-part-classes s))])))


(define m-new-closure
  (lambda (class-name class-super closure)
    (list (class-name class-super closure))))

;takes a state and class name and returns the class the class extends
(define m-lookup-super-class
  (lambda (class-name s)
    (cond
      [(null? s) (error "class does not exist")]
      [(equal? class-name (next-class s)) (next-extends s)]
      [else (m-lookup-super-class class-name (next-part-classes s))])))

;add a class to a state
(define m-add-class
 (lambda (class-dec s)
   (cons (generate-closure (class-body class-dec) (list (class-name class-dec) (class-extends class-dec) empty-state) s) s)))


;;given a class body, generate a closure for the class and add it to the state
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






;given a closure, generate an identical copy of the closure, with new boxes
(define new-closure
  (lambda (closure)
     (list (closure-class-name closure) (closure-super closure) (new-closure-body (closure-body closure)))))

;;create a new body for the closure
(define new-closure-body
  (lambda (closure-body)
    (list (new-local (local closure-body)) (new-layer-gen (global closure-body)))))

(define new-local
  (lambda (local)
    (cond
     [(null? local) '()]
     [else (cons (new-layer-gen (car local)) (new-local (next-part-list local)))])))

(define new-layer-gen
  (lambda (layer)
    (list (list (s-vars layer) (handle-boxes (s-vals layer))) (list (s-just-funcs layer) (handle-boxes (s-func-defs layer))))))


(define handle-boxes
  (lambda (list)
    (box-all (unbox-all list))))

;unboxes all values of a list
(define unbox-all
  (lambda (list)
    (if (null? list)
        '()
        (cons (unbox (next-in-list list)) (unbox-all (next-part-list list))))))

;boxes all values of a list
(define box-all
  (lambda (list)
    (if (null? list)
        '()
        (cons (box (next-in-list list)) (box-all (next-part-list list))))))

(define next-in-list car)
(define next-part-list cdr)
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
(define dot-instance-name cadr)
(define dot-variable-name caddr)

;for m-var-dec
(define assignment cddr)
(define is-new-instance
  (lambda (dec)
    (if (list? (expression dec))
        (caaddr dec)
        (expression dec))))

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

;; get-params-from-big-boy
(define first-param  car)
(define other-params cdr)
(define first-param-dot-instance cadar)
(define first-param-dot-variable caddar)

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
(define s-just-funcs caadr)
(define s-func-defs cadadr)
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

;new state format
;starting state is empty list
;(class with closure, class with closure, class with closure)
;class with closure contains: (name (superclass) (traditional state))
(define next-full-class-closure car)
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


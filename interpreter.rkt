#lang racket
;;;; A Java/C (ish) interpreter
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Will run a txt file containing code by using the run function (run "Filename.txt")
;;;; Order of inputs for ALL m-state and m-state like things

(provide (all-defined-out))         ; allows for testing to be done in interpreter-testing.rkt
(require "functionParser.rkt")        ; loads simpleParser.rkt, which itself loads lex.rkt
(require racket/trace)              ; for debugging

;; Runs the filename, should be provided in quotes
;; e.g. (run "Tests/Test1.txt")
(define run
  (lambda (filename)
    (call/cc
     (lambda (k)
       (runner filename k)))))

(define runner
  (lambda (filename callcc)
    (m-base-layer (parse-t filename) empty-state
                  callcc ;; return
                  (lambda (v) v) ;; break
                  (lambda (v) v) ;; continue
                  (lambda (v) v) ;; try
                  (lambda (v) v) ;; catch
                  (lambda (v) v)))) ;; finally 

;; Takes a file that contains code to be interpreted and returns the parse tree in list format
(define parse-t
  (lambda (filename)
    (parser filename)))

;; Executes code, returns updated state
(define m-state
  (lambda (exp s return break continue try catch finally)
    (cond
      [(null? exp)                         s]


      
      ; check for return
      [(not (list? (first-statement exp))) (m-what-type exp  s return break continue try catch finally)]
      [(null? (rest-of-body exp))          (m-what-type (first-statement exp) s
                                                        return break continue try catch finally)]
      
      [(eq? (first-statement exp) 'begin)  (m-pop (lambda (k) (m-state (rest-of-body exp)
                                                                       (m-push s) return k continue
                                                                       try catch finally)))]
     
      [else                                (m-state (rest-of-body exp)
                                                    (m-what-type (first-statement exp) s return break
                                                                 continue try catch finally)
                                                    return break continue try catch finally)])))

;; Returns state with most recent state popped off
(define m-pop
  (lambda (s)
    (nextlayer s)))

;; Returns state with new empty layer pushed on
(define m-push
  (lambda (s)
    (list (cons new-layer (local s)) (global s))))


;; Works through the top layer of the code then 
(define m-base-layer 
  (lambda (exp s return break continue try catch finally)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(null? exp)      s]

      [(null? (rest-of-body exp))          (m-base-layer (first-statement exp) s
                                                         return break continue try catch finally)]
      ;is it the main
      [(and  (eq?  (statement-body exp) 'main)
             (eq? (statement-type-id exp) 'function)) (m-pop (m-state (cadddr exp)  (m-push s)
                                                                      return break continue
                                                                      try catch finally))]

      ;is  it a function
      [(eq? (statement-type-id exp) 'function)  (m-add-global-func (cadr exp)
                                                                 (list (append (list (caddr exp))
                                                                               (list (cdddr exp))))
                                                           s)]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)      (m-var-dec exp s)]      
     
      [else                                (m-base-layer (rest-of-body exp)
                                                         (m-base-layer (first-statement exp) s return break
                                                                       continue try catch finally)
                                                         return break continue try catch finally)])))

;; Figures out which method should be used to evaluate this, and evaluates this
;; Returns updated state
(define m-what-type
  (lambda (exp s return break continue try catch finally)
    (cond
      ; null checking & if exp is not a list, then it wouldn't change the state
      [(or (null? exp) (not (pair? exp)))      s]

      ;is  it a function
      [(eq? (statement-type-id exp) 'function) (m-add-local-func (cadr exp)
                                                                (list (append (list (caddr exp))
                                                                              (list (cdddr exp))))
                                                          s)]

      ;is it a function call w/o parameters
      [(and (eq? (statement-type-id exp) 'funcall) (null? (cddr exp))) (m-funcall (cadr exp) '() return s)]
      
      ;is it a function call
      [(eq? (statement-type-id exp) 'funcall) (m-funcall (cadr exp) (cddr exp) return s)]
      
      ; is it a new block
      [(eq? (first-statement exp) 'begin)      (m-pop (m-state (rest-of-body exp) (m-push s)
                                                               return break continue try catch finally))]

      ; conditional statement checking (if/while/etc.)
      [(eq? (statement-type-id exp) 'if)       (m-if-statement exp s return break continue try catch finally)]
      [(eq? (statement-type-id exp) 'while)    (call/cc (lambda (k) (m-while-loop exp s return k continue
                                                                                  try catch finally)))]

      ; is it a break
      [(eq? (statement-type-id exp) 'break)    (break (m-pop s))]

      ; is it a continue
      [(eq? (statement-type-id exp) 'continue) (continue s)]

      ; is it a try/catch statement
      [(eq? (statement-type-id exp) 'try)      (call/cc (λ (k) (m-try-catch-finally exp s return break
                                                                                    continue k catch
                                                                                    finally)))]

      ; is it a throw
      [(eq? (statement-type-id exp) 'throw)    (try (m-pop (catch (statement-body exp))))]

      ; is it a declaration
      [(eq? (statement-type-id exp) 'var)      (m-var-dec exp s)]

      ; is it an assignment
      [(eq? (statement-type-id exp) '=)        (m-assign exp s)]

      ; is it a return statement
      [(eq? (statement-type-id exp) 'return)   (m-return (statement-body exp) s return finally)]
      
      ; oh no
      [else                                    (error 'undefined "undefined expression")])))

;m-funcall returns a state
(define m-funcall
  (lambda (name actual return s)
    ;gets the body and the formal parameters of the function
    (let* [(all (m-lookup-func name s))
           (formal (caar all))
           (body (car (caadar all)))]
        (if (eq? (num-in-list actual 0) (num-in-list formal 0))
            ;runs the body
            (call/cc (lambda (k)
                       (m-pop (m-state body (lists-to-assign actual formal (m-push s))
                            k
                            (lambda (v) v) ;; break
                            (lambda (v) v) ;; continue
                            (lambda (v) v) ;; try
                            (lambda (v) v) ;; catch
                            (lambda (v) v))))) ;; finally
            (error 'undefined "Paramater mismatch")))))

;; Takes two lists (l1 actual values)  (l2 formal values)
;; Returns an updated state
;; eg: (lists-to-assign '(1 2 3) '(a b c) s)
(define lists-to-assign
  (lambda (l1 l2 s)
    (if (null? l1)
        s
        (lists-to-assign (cdr l1) (cdr l2) (m-var-dec (cons 'var (cons (car l2) (list (car l1)))) s)))))

;; Sums the number of attoms in a list
;; helper for m-funcall
(define num-in-list
  (lambda (lis acc)
    (if (null? lis)
        acc
        (num-in-list (cdr lis) (+ acc 1)))))

(define m-try-catch-finally
  (lambda (exp s return break continue try catch finally)
    (cond
      ; oh no
      [(and (not (pair? (third-statement exp))) (not (pair? (catch-statement exp))))
       (error 'undefined "try statement missing catch or finally")]
      
      ; check if it has catch (and no finally)
      [(and (not (pair? (third-statement exp))) (eq? (second-identifier exp) 'catch))
       (call/cc (lambda (k) (m-state (try-body exp) s return break continue k
                                     ;; CATCH STATEMENT
                                     (lambda (exception) (m-state (catch-body (second-body exp))
                                                                  ;; MODIFYING THE STATE 
                                                                  (m-var-dec (list 'var (catch-var-name
                                                                                         (second-body exp))
                                                                                   exception) (m-push s))
                                                                  return break continue k catch finally))
                                     finally)))]

      ; check if has finally first (no catch)
      [(and (eq? (third-identifier exp) 'finally) (not (pair? (catch-statement exp))))
       (m-state (third-body exp) (m-state (try-body exp) s return break continue
                                          (lambda (v) s) (lambda (v) s) finally)
                return break continue try catch finally)]

      

      ; check for a catch AND a finally 
      [(and (eq? (second-identifier exp) 'catch) (eq? (third-identifier exp) 'finally))
       (m-state (third-body exp)
                (call/cc (lambda (k) (m-state (try-body exp) s return break continue k
                                              ;; CATCH STATEMENT
                                              (lambda (exception) (m-state (catch-body (second-body exp))
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

      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (cddr exp))))
                                            (call/cc (lambda (k) (m-funcall (cadr exp) '() k s)))]
      
      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (call/cc (lambda (k) (m-funcall (cadr exp) (cddr exp) k s)))]
      

      ; variable checking
      [(not (pair? exp))                      (m-lookup-var exp s)]

      
      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (cddr exp))))
                                            (m-value (m-funcall (cadr exp) '() (λ(v) v) s) s)]
      
      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (m-value (m-funcall (cadr exp) (cddr exp) (λ(v) v) s) s)]


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
  (lambda (exp s return finally)
          (display "exp")(display exp)(newline)

    (cond
      [(eq?   exp #t)                       (return 'true)]
      [(eq?   exp #f)                       (return 'false)]
      [(and (pair? exp) (am-i-boolean exp)) (finally (m-return (m-condition exp s) s return finally))]
      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (cddr exp))))
                                            (return (m-value (m-funcall (cadr exp) '() return s)))]
      
      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (return (m-value (m-funcall (cadr exp) (cddr exp) return s)))]
      
      [(pair? exp)                          (return (m-value exp s))]
      [(eq? (m-value exp s) #t)             (return 'true)]
      [(eq? (m-value exp s) #f)             (return 'false)]
      [else                                 (return (m-value exp s))])))

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
  (lambda (dec s)
    (cond
      ; check variable not already declared
      [(local-locate (variable dec) s)             (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))              (m-add (variable dec) s)]
      ; need to add value as well
      [else                                  (m-update (variable dec)
                                                       (m-value (expression dec) s)
                                                       (m-add (variable dec) s))])))

(define m-global-var-dec
  (lambda (dec s)
    (cond
      ; check variable not already declared
      [(locate-global-var (variable dec) s)             (error "redefining")]
      ; just need to add variable, not value
      [(null? (assignment dec))              (m-add-global-var (variable dec) s)]
      ; need to add value as well
      [else                                  (m-update (variable dec)
                                                       (m-value (expression dec) s)
                                                       (m-add-global-var (variable dec) s))])))
    

#|
define state with abstration with the format:

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


;; takes a variable and a state
;; returns the value or an error
(define m-lookup-var
  (lambda (var s)
    (cond
      [(null? s)                     (error "use before declared")]
      [(null? (local s))             (lookup-global-var var s)]
      [(null? (vars s))               (m-lookup-var var (nextlayer s))]
      [(and (equal? var (nextvar s)) (eq? "init" (unbox (nextval s)))) 
        (error "use before assignment")]
      [(equal? var (nextvar s))                                (unbox (nextval s))]
      [else                                                    (m-lookup-var var (next-part-vars s))])))


;; takes a global variable and a state
;; returns the value or an error
(define lookup-global-var
  (lambda (var s)
    (cond
     [(null? s)              (error "use before declared")]
     [(null? (global s))     (error "use before declared")]
     [(null? (global-vars s)) (error "use before declared")]
     [(and (eq? var (global-nextvar s))(eq? "init" (unbox (global-nextval s)))) (error "use before assignment")]
     [(equal? var (global-nextvar s))   (unbox (global-nextval s))]
     [else                  (lookup-global-var var (global-nextpart-vars s))])))


;; takes a function and a state
;; returns the function closure
(define m-lookup-func
  (lambda (func s)
    (cond
      [(null? s)                      (error "function not found")]
      [(null? (local s))              (lookup-global-func func s)]
      [(null? (funcs s))              (m-lookup-func func (nextlayer s))]
      [(equal? func (nextfunc s))                                (unbox (nextfunc-def s))]
      [else                                                    (m-lookup-func func (next-part-funcs s))])))


;; takes a global function and a state
;; returns the function closure
(define lookup-global-func
  (lambda (func s)
    (cond
     [(or (or (null? s)(null? (global s)))(null? (global-funcs s)))              (error "function not found")]
     [(equal? func (global-nextfunc s))   (unbox (global-nextfunc-def s))]
     [else                  (lookup-global-func func (global-nextpart-funcs s))])))
  

;; takes a variable, the value to be updated, and the state
;; returns the updated state
(define m-update
  (lambda (var update-val s)
    (cond
      [(null? s)        "error"]
      [(not (locate-var var s)) "error"]
      [(local-locate-var var s) (list (local-update var update-val (local s)) (global s))]
      [else (list (local s) (global-update var update-val (global s)))])))

;; takes a variable, the value to be updated, and the local layer of the state
;; returns the updated local layer
(define local-update
  (lambda (var update-val s)
    (cond
      [(null? s)      "error"]
      [(local-layer-locate var (top-layer s)) (cons (local-toplayer-update var update-val (top-layer s) (lambda (v1 v2) (list (list v1 v2) (local-funcs s)))) (rest-of s))]
      [else (cons (top-layer s) (local-update var update-val (cdr s)))])))
    

;; takes a variable, the value to be updated, and the global layer of the state
;; returns the updated global layer
(define global-update
  (lambda (var update-val s)
    (cond
      [(null? s)      "error"]
      [(not (local-layer-locate var s))  "error"] 
      [else (local-toplayer-update var update-val s (lambda (v1 v2) (list (list v1 v2) (s-funcs s))))])))

;;takes a variable, the value to be updated and the layer to be updated
;;returns the variables and updated values of the layer in two lists, to be combined by the calling function
(define local-toplayer-update
  (lambda (var update-val s return)
    (cond
      [(equal? var (s-nextvar s)) (return (s-vars s) (begin  (set-box! (s-nextval s) update-val) (cons (s-nextval s) (rest-of (s-vals s)))))]
      [else                  (local-toplayer-update var update-val  (s-next-part-vars s)  (lambda (v1 v2) (return (cons (s-nextvar s) v1) (cons (s-nextval s) v2))))])))



;; Takes a local variable and a state, adds it to the topmost local section of the state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
(define m-add
  (lambda (var s)
     (list (cons (list (list (cons  var (vars s)) (cons (box "init") (vals s)))(func-layer s)) (cdr (local s))) (global s))))

;; Takes a local function and it's closure, adds the function and it's closure to the topmost local section of the state
(define m-add-local-func
  (lambda (func closure s)
    (list (cons (list (var-layer s) (list (cons func (funcs s)) (cons (box closure) (func-defs s)))) (cdr (local s))) (global s))))

;; Takes a global variable and a state, adds it to the global section of the state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
(define m-add-global-var
  (lambda (var s)
    (list (local s) (list (list (cons var (global-vars s)) (cons (box "init") (global-vals s))) (global-func-layer s)))))

;; Takes a global function and it's closure, adds the function and it's closure to the global section of the state
(define m-add-global-func
  (lambda (func closure s)
    (list (local s) (list (global-var-layer s) (list (cons func (global-funcs s)) (cons (box closure) (global-func-defs s)))))))



;;; the following are helper methods for state functions

;;takes a variable and a state
;; returns true if the variable exists on the top layer of the state, false otherwise
;can no longer use i think
(define local-locate
  (lambda (var s)
    (cond
      [(null? s)         #f]
      [(null? (vars s))  #f]
      [(eq? var (nextvar s))            #t]
      [else                             (local-locate var (next-part-vars s))])))


;;returns #t if the variable exists in the topmost layer
(define local-layer-locate
  (lambda (var s)
    (cond
      [(null? s)  #f]
      [(null? (s-vars s)) #f]
      [(eq? var (s-nextvar s)) #t]
      [else (local-layer-locate var (s-next-part-vars s))]))) 
    

;; returns #t if the var is found in the state, #f otherwise
;; Takes the variable it is locating and a state
(define locate-var
  (lambda (var s)
    (cond
      [(null? s)   #f]
      [(eq? (local s) '())             (locate-global-var var s)]
      [(null? (vars s))      (locate-var var (nextlayer s))]
      [(eq? var (nextvar s)) #t]
      [else                  (locate-var var (next-part-vars s))])))


;; returns #t if the given variable exists in the global layer
(define locate-global-var
  (lambda (var s)
    (cond
      [(null? s)              #f]
      [(null? (global s))     #f]
      [(null? (global-vars s)) #f]
      [(eq? var (global-nextvar s)) #t]
      [else                  (locate-global-var var (global-nextpart-vars s))])))


;; returns #t if the given variable exists in the local layer
(define local-locate-var
   (lambda (var s)
    (cond
      [(null? s)   #f]
      [(null? (local s))      #f]
      [(null? (vars s))      (local-locate-var var (nextlayer s))]
      [(eq? var (nextvar s)) #t]
      [else                  (local-locate-var var (next-part-vars s))])))


;; returns #t if the given function exists in any part of the state
(define locate-func
  (lambda (func s)
    (cond
      [(null? s)   #f]
      [(eq? (local s) '())             (locate-global-func func s)]
      [(null? (funcs s))      (locate-func func (nextlayer s))]
      [(eq? func (nextfunc s)) #t]
      [else                  (locate-func func (next-part-funcs s))])))

;; returns #t if the given global function exists
(define locate-global-func
  (lambda (func s)
    (cond
      [(null? s)              #f]
      [(null? (global s))     #f]
      [(null? (global-funcs s)) #f]
      [(eq? func (global-nextfunc s)) #t]
      [else                  (locate-global-func func (global-nextpart-funcs s))])))


;; Takes a varaiable and a state, adds it to a state with non number uninitilized value "init"
;; (does not take value, to update value, use m-update)
;; Returns the updated state, if used before assigned, should result in error
;; Will accept an empty state '(), a state formated '((()())) or
;; a state formated '(((var1 ...)(val1 ...))((varx ...) (valx ...)))
;;adds local vars only, global vars added durring first pass
(define m-add
  (lambda (var s)
     (list (cons (list (list (cons  var (vars s)) (cons (box "init") (vals s)))(func-layer s)) (cdr (local s))) (global s))))

(define m-add-local-func
  (lambda (func closure s)
    (list (cons (list (var-layer s) (list (cons func (funcs s)) (cons (box closure) (func-defs s)))) (cdr (local s))) (global s))))

(define m-add-global-var
  (lambda (var s)
    (list (local s) (list (list (cons var (global-vars s)) (cons (box "init") (global-vals s))) (global-func-layer s)))))

(define m-add-global-func
  (lambda (func closure s)
    (list (local s) (list (global-var-layer s) (list (cons func (global-funcs s)) (cons (box closure) (global-func-defs s)))))))


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
      [(and (pair? exp) (am-i-boolean exp)) (finally (m-return (m-condition exp s) s return finally))]
      
      ;is it a function call w/o parameters
      [(and (pair? exp) (and (eq? (statement-type-id exp) 'funcall) (null? (cddr exp))))
                                            (return (m-value (m-funcall (cadr exp) '() return s)))]
      
      ;is it a function call
      [(and (pair? exp) (eq? (statement-type-id exp) 'funcall))
                                            (return (m-value (m-funcall (cadr exp) (cddr exp) return s) s))]
      
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
 
; for remove
(define first-val car)

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
(define b '((((()())(()())))((()())(()()))))
(define first-statement car)
(define rest-of-body cdr)

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
(define z '((((c d) (#&1 #&34)) ((f1 f2) (#&(stufffff) #&(stuff2))))(((q)(#&0))((f3 f4)(#&(dd) #&(qqq)))) (((a f)(#&2 #&1))((f8 f9)(#&(yyd) #&(uuu)))))) ;local test
(define qqq  '(((((x) (#&"init")) (() ())) ((() ()) (() ()))) ((() ()) (() ()))))
(define test1 '(((((z y x) (#&30 #&20 #&10)) (() ())) ((() ()) (() ()))) ((() ()) (() ()))))

;; Thank you, sleep well :)

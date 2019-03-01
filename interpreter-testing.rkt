#lang racket
;;;; Testing of interpreter.rkt
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Run tests by using the test function (test)

(require "interpreter.rkt") ; allows for testing of the interpreter

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

;; Performs all the tests needed to prove the validity of the functions, I love this function.
;; Tests with ";should error" next to them are commented out beacuse they are SUPPOSED to throw errors,
;; which halts the rest of the testing
(define (test)

  ;Example:
  ; (diplay "Test #{test number} {test name}") (newline)
  ; (pass? {actual} {expected})
  ; (newline)

  ; checks the code is parsed into a tree as exprected
  (display "Test #1 parse-tree") (newline)                                            ;Test parse-tree
  (pass? (parse-t "Tests/Test1.txt") '((var x) (= x 10) (var y (+ (* 3 x) 5))                ; 1/1
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
  ;(pass? (m-lookup 'd '()) "error)     ;should error                                           ; 4/5
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
  (pass? (m-assign '(var a 2) '((a)(1))) '((a)(2)))                                             ; 1/7
  (pass? (m-assign '(var d 2) '((x y d z)(1 1 1 1))) '((x y d z)(1 1 2 1)))                     ; 2/7
  (pass? (m-assign '(var d 2) '((x y d z)(1 1 "init" 1))) '((x y d z)(1 1 2 1)))                ; 3/7
  (pass? (m-assign '(var d (+ 2 4)) '((x y d z)(1 1 1 1))) '((x y d z)(1 1 6 1)))               ; 4/7
  (pass? (m-assign '(var d (+ x 4)) '((x y d z)(2 3 7 1))) '((x y d z)(2 3 6 1)))               ; 5/7
  (pass? (m-assign '(var d (+ x (* y 2))) '((x y d z)(2 3 7 1))) '((x y d z)(2 3 8 1)))         ; 6/7
  ;(pass? (m-assign '(var 'a 2) '(()()) ;should error                                           ; 7/7
  (newline)

  ; declares a variable
  (display "Test #9 m-var-dec") (newline)                                             ;Test m-var-dec
  (pass? (m-var-dec '(var a) '((q)(1))) '((a q) ("init" 1)))                                    ; 1/9
  (pass? (m-var-dec '(var a) '(()())) '((a)("init")))                                           ; 2/9
  (pass? (m-var-dec '(var a 1) '((d s)(2 3))) '((a d s)(1 2 3)))                                ; 3/9
  (pass? (m-var-dec '(var a (+ x 1)) '((c s x)(2 3 4))) '((a c s x)(5 2 3 4)))                  ; 4/9
  (pass? (m-var-dec '(var a (+ x (* c 3))) '((c s x)(2 3 4))) '((a c s x)(10 2 3 4)))           ; 5/9
  ;(pass? (m-var-dec '(var a) '((d a s)(1 2 3))) "error")             ;should error             ; 6/9
  ;(pass? (m-var-dec '(var a 1) '((d a s)(1 2 3))) "error")           ;should error             ; 7/9
  ;(pass? (m-var-dec '(var a (+ x 1)) '((c s a x)(2 3 5 7))) "error") ;should error             ; 8/9
  ;(pass? (m-var-dec '(var a (+ a 1)) '((c s a x)(2 3 5 4))) "error") ;should error             ; 9/9
  (newline)

  ; tests interpreter functionality
  (display "Test #10 run") (newline)                                                  ;Test run
  (pass? (run "Tests/Test1.txt") 100)                                                           ; 1/26
  (pass? (run "Tests/Test2.txt") 21)                                                            ; 2/26
  (pass? (run "Tests/Test3.txt") 4)                                                             ; 3/26
  (pass? (run "Tests/Test4.txt") -10)                                                           ; 4/26
  (pass? (run "Tests/Test5.txt") 240)                                                           ; 5/26
  (pass? (run "Tests/Test6.txt") "true")                                                        ; 6/26
  (pass? (run "Tests/p1.Test1.txt") 150)                                                        ; 7/26
  (pass? (run "Tests/p1.Test2.txt") -4)                                                         ; 8/26
  (pass? (run "Tests/p1.Test3.txt") 10)                                                         ; 9/26
  (pass? (run "Tests/p1.Test4.txt") 16)                                                         ; 10/26
  (pass? (run "Tests/p1.Test5.txt") 220)                                                        ; 11/26
  (pass? (run "Tests/p1.Test6.txt") 5)                                                          ; 12/26
  (pass? (run "Tests/p1.Test7.txt") 6)                                                          ; 13/26
  (pass? (run "Tests/p1.Test8.txt") 10)                                                         ; 14/26
  (pass? (run "Tests/p1.Test9.txt") 5)                                                          ; 15/26
  (pass? (run "Tests/p1.Test10.txt") -39)                                                       ; 16/26
  (pass? (run "Tests/p1.Test15.txt") "true")                                                    ; 17/26
  (pass? (run "Tests/p1.Test16.txt") 100)                                                       ; 18/26
  (pass? (run "Tests/p1.Test17.txt") "false")                                                   ; 19/26
  (pass? (run "Tests/p1.Test18.txt") "true")                                                    ; 20/26
  (pass? (run "Tests/p1.Test19.txt") 128)                                                       ; 21/26
  (pass? (run "Tests/p1.Test20.txt") 12)                                                        ; 22/26
  ;(pass? (run "Tests/p1.Test11.txt") "error")  ;should error "use before declaring"            ; 23/26
  ;(pass? (run "Tests/p1.Test12.txt") "error")  ;should error "use before declaring"            ; 24/26
  ;(pass? (run "Tests/p1.Test13.txt") "error")  ;should error "use before assigning"            ; 25/26
  ;(pass? (run "Tests/p1.Test14.txt") "error")  ;should error "redefining"                      ; 26/26
  (newline)

  (display "P2 Test run") (newline) 
  (pass? (run "Tests/p2.Test1.txt") 20)                                                       
  (pass? (run "Tests/p2.Test2.txt") 164)                                                      
  (pass? (run "Tests/p2.Test3.txt") 32)                                                         
  (pass? (run "Tests/p2.Test4.txt") 4)                                                        
  (pass? (run "Tests/p2.Test5.txt") 5)                                                        
  (pass? (run "Tests/p2.Test6.txt") 25)                                                        
  (pass? (run "Tests/p2.Test7.txt") 21)                                                        
  (pass? (run "Tests/p2.Test8.txt") 6)                                                       
  (pass? (run "Tests/p2.Test9.txt") -1)
   (pass? (run "Tests/p2.Test10.txt") 789)                                                       
  ;(pass? (run "Tests/p2.Test11.txt") "error")   ;should give error                                                   
  ;(pass? (run "Tests/p2.Test12.txt") "error")   ;should give error 
  ;(pass? (run "Tests/p2.Test13.txt") "error")   ;should give error
  (pass? (run "Tests/p2.Test14.txt") 12)                                                        
  (pass? (run "Tests/p2.Test15.txt") 125)                                                        
  (pass? (run "Tests/p2.Test16.txt") 110)                                                        
  (pass? (run "Tests/p2.Test17.txt") 2000400)                                                        
  (pass? (run "Tests/p2.Test18.txt") 101)                                                       
  ;(pass? (run "Tests/p2.Test19.txt") "error")   ;should give error 
  (newline)


  ) ;left hanging for easy test addition
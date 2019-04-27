#lang racket
;;;; Testing of interpreter.rkt
;;;; EECS 345
;;;; Group #7: Shanti Polara, Catlin Campbell, Cormac Dacker
;;;; Run tests by using the test function (test)

(require "interpreter.rkt") ; allows for testing of the interpreter

;; Performs a quick test to see if the test passed or failed and pr(ints info about test if failure
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
  
  ;(test-p1-test-scripts)
  ;(test-p2-test-scripts)
  ;(test-p3-test-scripts)
  (test-p4-test-scripts)

  ) ;left hanging for easy test addition

;; Tests interpreter functionality P1
(define (test-p1-test-scripts)
  (display "Test P1 test scripts") (newline)                                       ;Test P1
  (pass? (run "Tests/Test1.txt") 100)                                                           ; 1/26
  (pass? (run "Tests/Test2.txt") 21)                                                            ; 2/26
  (pass? (run "Tests/Test3.txt") 4)                                                             ; 3/26
  (pass? (run "Tests/Test4.txt") -10)                                                           ; 4/26
  (pass? (run "Tests/Test5.txt") 240)                                                           ; 5/26
  (pass? (run "Tests/Test6.txt") 'true)                                                         ; 6/26

  
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
  (pass? (run "Tests/p1.Test15.txt") 'true)                                                     ; 17/26
  (pass? (run "Tests/p1.Test16.txt") 100)                                                       ; 18/26
  (pass? (run "Tests/p1.Test17.txt") 'false)                                                    ; 19/26
  (pass? (run "Tests/p1.Test18.txt") 'true)                                                     ; 20/26
  (pass? (run "Tests/p1.Test19.txt") 128)                                                       ; 21/26
  (pass? (run "Tests/p1.Test20.txt") 12)                                                        ; 22/26
  ;(pass? (run "Tests/p1.Test11.txt") "error")  ;should error "use before declaring"            ; 23/26
  ;(pass? (run "Tests/p1.Test12.txt") "error")  ;should error "use before declaring"            ; 24/26
  ;(pass? (run "Tests/p1.Test13.txt") "error")  ;should error "use before assigning"            ; 25/26
  ;(pass? (run "Tests/p1.Test14.txt") "error")  ;should error "redefining"                      ; 26/26
  (newline))

;; Tests interpreter functionality P2
(define (test-p2-test-scripts)
  (display "Test P2 test scripts") (newline)                                       ;Test P2
  (pass? (run "Tests/p2.Test1.txt") 20)                                                         ; 1/
  (pass? (run "Tests/p2.Test2.txt") 164)                                                        ; 2/
  (pass? (run "Tests/p2.Test3.txt") 32)                                                         ; 3/
  (pass? (run "Tests/p2.Test4.txt") 2)                                                          ; 4/
  ;(pass? (run "Tests/p2.Test5.txt") "error") ;should give error                                ; 5/
  (pass? (run "Tests/p2.Test6.txt") 25)                                                         ; 6/
  (pass? (run "Tests/p2.Test7.txt") 21)                                                         ; 7/
  (pass? (run "Tests/p2.Test8.txt") 6)                                                         ; 8/
  (pass? (run "Tests/p2.Test9.txt") -1)                                                         ; 9/
  (pass? (run "Tests/p2.Test10.txt") 789)                                                       ; 10/
  ;(pass? (run "Tests/p2.Test11.txt") "error")   ;should give error                             ; 11/
  ;(pass? (run "Tests/p2.Test12.txt") "error")   ;should give error                             ; 12/
  ;(pass? (run "Tests/p2.Test13.txt") "error")   ;should give error                             ; 13/
  (pass? (run "Tests/p2.Test14.txt") 12)                                                        ; 14/
  (pass? (run "Tests/p2.Test15.txt") 125)                                                       ; 15/
  (pass? (run "Tests/p2.Test16.txt") 110)                                                       ; 16/
  ;(pass? (run "Tests/p2.Test17.txt") 2000400)                                                   ; 17/
  (pass? (run "Tests/p2.Test18.txt") 101)                                                       ; 18/
  ;(pass? (run "Tests/p2.Test19.txt") "error")   ;should give error                              ; 19/
  (pass? (run "Tests/Test7.txt") 2)              ;this tests break                              ; 20/
  ;(pass? (run "Tests/Test9.txt") -1)             ;tests everything                              ; 21/
  (pass? (run "Tests/Test10.txt") 10)
  (pass? (run "Tests/Test11.txt") 120)
  (newline))

(define (test-p3-test-scripts)
  (display "Test P3 test scripts") (newline)
  ;A main with code inside
  (display "Test 1:  ") (pass? (run "Tests/p3.Test1.txt") 10)                                                         ; 1/
  ;A function that uses global variables
  (display "Test 2:  ") (pass? (run "Tests/p3.Test2.txt") 14)                                                         ; 2/
  ;A function that changes global variables
  (display "Test 3:  ") (pass? (run "Tests/p3.Test3.txt") 45)
  ;A recursive function
  (display "Test 4:  ") (pass? (run "Tests/p3.Test4.txt") 55)
  ;Functions with multiple parameters that hide global variables
  (display "Test 5:  ") (pass? (run "Tests/p3.Test5.txt") 1)
  ;Verifying that your code uses static scoping instead of dynamic scoping
  (display "Test 6:  ") (pass? (run "Tests/p3.Test6.txt") 115)
  ;Boolean parameters and return values
  (display "Test 7:  ") (pass? (run "Tests/p3.Test7.txt") 'true)
  ;Multiple function calls in an expression
  (display "Test 8:  ") (pass? (run "Tests/p3.Test8.txt") 20)
  ;A function call in the parameter of a function
  (display "Test 9:  ") (pass? (run "Tests/p3.Test9.txt") 24)
  ;A function call that ignores the return value
  (display "Test 10: ") (pass? (run "Tests/p3.Test10.txt") 2)
  ;A function without a return statement
  (display "Test 11: ") (pass? (run "Tests/p3.Test11.txt") 35)
  ;Mismatched parameters and arguments
  ;(pass? (run "Tests/p3.Test12.txt") "error")   ;should give error
  (display "Test 13: ") (pass? (run "Tests/p3.Test13.txt") 90)
  ;Functions inside functions accessing variables outside
  (display "Test 14: ") (pass? (run "Tests/p3.Test14.txt") 69)         ;heh nice
  ;Functions inside functions with variables of the same name
  (display "Test 15: ") (pass? (run "Tests/p3.Test15.txt") 87)
  ;Functions inside functions inside functions
  (display "Test 16: ") (pass? (run "Tests/p3.Test16.txt") 64)
  ;Functions inside functions accessing out of scope variables
  ;(pass? (run "Tests/p3.Test17.txt") "error")   ;should give error
  ;try/catch finally, but no exception thrown
  (display "Test 18: ") (pass? (run "Tests/p3.Test18.txt") 125)
  ;Throwing an exception inside a function
  ;(pass? (run "Tests/p3.Test19.txt") 100)
  ;Throwing an exception from a function
  ;(pass? (run "Tests/p3.Test20.txt") 2000400)

  (newline))
(define (test-p4-test-scripts)
  (display "Test P4 test scripts") (newline)
  (display "Test 1:  ") (pass? (run "Tests/p4.Test1.txt" "A") 15)                                                         ; 1/
  (display "Test 2:  ") (pass? (run "Tests/p4.Test2.txt" "A") 12)                                                         ; 2/
  (display "Test 3:  ") (pass? (run "Tests/p4.Test3.txt" "A") 125)
  (display "Test 4:  ") (pass? (run "Tests/p4.Test4.txt" "A") 36)
  (display "Test 5:  ") (pass? (run "Tests/p4.Test5.txt" "A") 54)
  (display "Test 6:  ") (pass? (run "Tests/p4.Test6.txt" "A") 110)
  (display "Test 7:  ") (pass? (run "Tests/p4.Test7.txt" "C") 26)
  (display "Test 8:  ") (pass? (run "Tests/p4.Test8.txt" "Square") 117)
  (display "Test 9:  ") (pass? (run "Tests/p4.Test9.txt" "Square") 32)
  (display "Test 10: ") (pass? (run "Tests/p4.Test10.txt" "List") 15)
  (display "Test 11: ") (pass? (run "Tests/p4.Test11.txt" "List") 123456)
  (display "Test 12: ") (pass? (run "Tests/p4.Test12.txt" "List") 5285) 
  (display "Test 13: ") (pass? (run "Tests/p4.Test13.txt" "C") -716)

  (newline)) ; left hanging for easy test addition

(define (test-p4-custom-test-scripts)
  (display "Test P4 custom test scripts") (newline)
  (display "Test 1:  ") (pass? (run "Tests/Test1.txt" "A") 5)
  (display "Test 2:  ") (pass? (run "Tests/Test1.txt" "A") 5)

  (newline)) ; left hanging for easy test addition

(define (test-old-tests-with-classes)
  (display "Test P1 test scripts") (newline)
  (display "Test 1:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test1.txt" "A") 150)
  (display "Test 2:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test2.txt" "A") -4)
  (display "Test 3:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test3.txt" "A") 10)
  (display "Test 4:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test4.txt" "A") -16)
  (display "Test 5:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test5.txt" "A") 220)
  (display "Test 6:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test6.txt" "A") 5)
  (display "Test 7:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test7.txt" "A") 6)
  (display "Test 8:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test8.txt" "A") 10)
  (display "Test 9:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test9.txt" "A") 5)
  (display "Test 10: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test10.txt" "A") -39)
  ; (display "Test 11: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test11.txt" "A") 150) ; should error
  ; (display "Test 12: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test12.txt" "A") 150) ; should error
  ; (display "Test 13: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test13.txt" "A") 150) ; should error
  ; (display "Test 14: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test14.txt" "A") 150) ; shoule error
  (display "Test 15: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test15.txt" "A") 'true)
  (display "Test 16: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test16.txt" "A") 100)
  (display "Test 17: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test17.txt" "A") 'false)
  (display "Test 18: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test18.txt" "A") 'true)
  (display "Test 19: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test19.txt" "A") 128)
  (display "Test 20: ") (pass? (run "Tests/AdaptedTests/p1.p4.Test20.txt" "A") 12)

  (newline)

  (display "Test P2 test scripts") (newline)
  (display "Test 1:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test1.txt" "A") 20)
  (display "Test 2:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test2.txt" "A") 164)
  (display "Test 3:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test3.txt" "A") 32)
  (display "Test 4:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test4.txt" "A") 2)
  ; (display "Test 5:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test5.txt" "A") 220)
  (display "Test 6:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test6.txt" "A") 25)
  (display "Test 7:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test7.txt" "A") 21)
  (display "Test 8:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test8.txt" "A") 6)
  (display "Test 9:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test9.txt" "A") -1)
  (display "Test 10: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test10.txt" "A") 789)
  ; (display "Test 11: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test11.txt" "A") 150) ;should error 
  ; (display "Test 12: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test12.txt" "A") 150) ;should error
  ; (display "Test 13: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test13.txt" "A") 150) ;should error
  (display "Test 14: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test14.txt" "A") 12) 
  (display "Test 15: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test15.txt" "A") 125)
  ; (display "Test 16: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test16.txt" "A") 110)
  ; (display "Test 17: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test17.txt" "A") 2000400)
  (display "Test 18: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test18.txt" "A") 101)
  ; (display "Test 19: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test19.txt" "A") 128) ;should error
  ; (display "Test 20: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test20.txt" "A") 222)

  (newline)

  (display "Test P3 test scripts") (newline)
  (display "Test 1:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test1.txt" "A") 10)
  (display "Test 2:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test2.txt" "A") 14)
  (display "Test 3:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test3.txt" "A") 45)
  (display "Test 4:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test4.txt" "A") 55)
  (display "Test 5:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test5.txt" "A") 1) 
  (display "Test 6:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test6.txt" "A") 115)
  (display "Test 7:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test7.txt" "A") 'true)
  (display "Test 8:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test8.txt" "A") 20)
  (display "Test 9:  ") (pass? (run "Tests/AdaptedTests/p3.p4.Test9.txt" "A") 24)
  ; (display "Test 10: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test10.txt" "A") 2)
  (display "Test 11: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test11.txt" "A") 35) 
  ; (display "Test 12: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test12.txt" "A") 150) ;should error
  (display "Test 13: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test13.txt" "A") 90) 
  (display "Test 14: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test14.txt" "A") 69) 
  (display "Test 15: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test15.txt" "A") 87)
  (display "Test 16: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test16.txt" "A") 64)
  ; (display "Test 17: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test17.txt" "A") 2000400) ;should error 
  (display "Test 18: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test18.txt" "A") 125)
  (display "Test 19: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test19.txt" "A") 100) 
  (display "Test 20: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test20.txt" "A") 2000400)
  #|
  (display "Test 15: ") (pass? (run "Tests/p3.Test15.txt") 87)

|#
  
  (newline))


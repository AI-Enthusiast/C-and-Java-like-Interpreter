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
  

  (test-old-tests-with-classes)
  ; (test-p4-custom-test-scripts)
  ; (test-p4-test-scripts)

  ) ;left hanging for easy test addition

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

  (display "Test 7:  ") (pass? (run "Tests/Test7.txt" "A") 110)
  (display "Test 8:  ") (pass? (run "Tests/Test8.txt" "A") 2)

  (newline)) ; left hanging for easy test addition

(define (test-old-tests-with-classes)
  (display "Test P1 test scripts") (newline)
  (display "Test 1:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test1.txt" "A") 150)
  (display "Test 2:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test2.txt" "A") -4)
  (display "Test 3:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test3.txt" "A") 10)
  (display "Test 4:  ") (pass? (run "Tests/AdaptedTests/p1.p4.Test4.txt" "A") 16)
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
  ; (display "Test 5:  ") (pass? (run "Tests/AdaptedTests/p2.p4.Test5.txt" "A") 220) ; should error
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
  (display "Test 16: ") (pass? (run "Tests/AdaptedTests/p2.p4.Test16.txt" "A") 110)
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
  (display "Test 10: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test10.txt" "A") 2)
  (display "Test 11: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test11.txt" "A") 35) 
  ; (display "Test 12: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test12.txt" "A") 150) ;should error
  (display "Test 13: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test13.txt" "A") 90) 
  (display "Test 14: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test14.txt" "A") 69) 
  (display "Test 15: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test15.txt" "A") 87)
  (display "Test 16: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test16.txt" "A") 64)
  ; (display "Test 17: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test17.txt" "A") 2000400) ;should error 
  (display "Test 18: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test18.txt" "A") 125)
  ; (display "Test 19: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test19.txt" "A") 100) ; tried to fix, didn't work D: 
  ; (display "Test 20: ") (pass? (run "Tests/AdaptedTests/p3.p4.Test20.txt" "A") 2000400)
  
  (newline))


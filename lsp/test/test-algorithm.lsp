(defun TR:test-maximize-multi-integer-linear-inequality()
  
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10.) 9.) '((0) 0.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10.) 10.) '((1) 10.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(0.5) 0.5) '((1) 0.5))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10.) 90.) '((9) 90.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10.) 95.) '((9) 90.))
  
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10. 5.) 1.) '((0 0) 0.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10. 1.) 95.) '((9 5) 95.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10. 1.) 9.) '((0 9) 9.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(1. 10.) 95.) '((95 0) 95.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(1. 10.) 9.) '((9 0) 9.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(3. 2.5) 7.) '((2 0) 6.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(3. 2.) 8.) '((2 1) 8.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(3. 2.1) 8.) '((1 2) 7.2))

  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(5. 3.5 10.) 9.) '((1 1 0) 8.5))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(5. 3. 10.) 9.) '((0 3 0) 9.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(5. 10. 3.) 9.) '((0 0 3) 9.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(10. 5. 3.) 18.) '((1 1 1) 18.))

  ; integer args are ok
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(3 1.) 6.) '((2 0) 6.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(3. 1) 6.) '((2 0) 6.))
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(3. 1.) 6) '((2 0) 6.))
  ; valid args but no solution
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list nil 1.) '(nil 1.))

  ; ensure returning correct types
  (TR:testsuite-test-for-exactness 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(1.5 5. 10.) 4.) '((2 0 0) 3.))
  (TR:testsuite-test-for-exactness 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(2 5 10) 4) '((2 0 0) 4.))

  ; test invalid params: all should return nil
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list 1 1.) nil)
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(1.) nil) nil)
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list 1 nil) nil)
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(-1.) 1.) nil)
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(0. 1.) 1.) nil)
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(1. 0.) 1.) nil)
  (TR:testsuite-test-for-equality 'TR:ILP-get-maximum-objective-given-objective-constraint (list '(1.) -1.) nil)

  (princ)
)

(defun TR:test()
  (TR:test-maximize-multi-integer-linear-inequality)
  (princ)
)

(princ)
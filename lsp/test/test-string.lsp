
(defun TR:test ()

  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "LongString" " " 3) "LongString")
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "Short" " " 7) "Short  ")
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "Short" "*" 7) "Short**")
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "" "*" 5) "*****")
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "Test" "" 5) nil)
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "Test" nil 5) nil)
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "Test" " " 0) "Test")
  (TR:testsuite-test-for-equality 'TR:string-pad-right (list "Test" " " nil) nil)

  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "LongString" " " 3) "LongString")
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "Short" " " 7) "  Short")
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "Short" "*" 7) "**Short")
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "" "*" 5) "*****")
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "Test" "" 5) nil)
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "Test" nil 5) nil)
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "Test" " " 0) "Test")
  (TR:testsuite-test-for-equality 'TR:string-pad-left (list "Test" " " nil) nil)

  (princ)
)

(princ)


(defun TR:test ()
  
  (setq bb '((0. 0.)(10. 5.)))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-center (list bb ) '(5. 2.5))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-size (list bb ) '(10. 5.))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-largest-dimension-length (list bb ) 10.)

  (setq bb '((10. -10.)(20. 50.)))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-center (list bb ) '(15. 20.))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-size (list bb ) '(10. 60.))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-largest-dimension-length (list bb ) 60.)

  (setq bb '((-20. 0.)(100. 10.)))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-center (list bb ) '(40. 5.))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-size (list bb ) '(120. 10.))
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-largest-dimension-length (list bb ) 120.)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-left (list bb ) -20.)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-right (list bb ) 100.)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-bottom (list bb ) 0.)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-top (list bb ) 10.)

  (TR:testsuite-test-for-equality 'TR:boundingbox-get-left (list nil ) nil)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-right (list nil ) nil)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-bottom (list nil ) nil)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-top (list nil ) nil)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-center (list nil ) nil)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-size (list nil ) nil)
  (TR:testsuite-test-for-equality 'TR:boundingbox-get-largest-dimension-length (list nil ) nil)

  (TR:testsuite-test-for-equality 'TR:get-boundingbox-of-two-points (list '(1. -2.) '(3. 4.)) '((1. -2.)(3. 4.)))
  (TR:testsuite-test-for-equality 'TR:get-boundingbox-of-two-points (list '(3. -2.) '(1. 4.)) '((1. -2.)(3. 4.)))
  (TR:testsuite-test-for-equality 'TR:get-boundingbox-of-two-points (list '(1. 4.) '(3. -2.)) '((1. -2.)(3. 4.)))
  (TR:testsuite-test-for-equality 'TR:get-boundingbox-of-two-points (list '(3. 4.) '(1. -2.)) '((1. -2.)(3. 4.)))

  (princ)
)

(princ)

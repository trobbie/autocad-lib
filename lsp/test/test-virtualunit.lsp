
(defun TR:test ()
  ; (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list nil) nil)
  ; (TR:testsuite-test-for-equality 'TR:virtualunit-get-size (list nil) nil)
  ; (TR:testsuite-test-for-equality 'TR:virtualunit-get-group-insertion-point (list nil) nil)
  
  ; (TR:testsuite-test-drawing
  ;   'TR:test-doc3-rect
  ;   '(lambda ( listObjectsAll / vu)
  ;     (setq vu (TR:virtualunit-create-virtual-with-props listObjectsAll 0 '(0 0 0)))

  ;     (TR:testsuite-test-value-for-equality "TR:virtualunit-get-objects on doc3 - result - # distinct objects"
  ;       (length (TR:virtualunit-get-objects vu))
  ;       1)

  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu) '((0. 0. 0.) (10. 5. 0.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-size (list vu) '(10. 5.))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-group-insertion-point (list vu) '(0 0 0))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-rotation (list vu) (imat 4))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu) "")

  ;     (setq vuZ90 (TR:virtualunit-rotate-around-z-axis vu 90))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vuZ90) '((0. 0. 0.) (5. 10. 0.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vuZ90) "RotZ:90")
  
  ;     (setq vu3 (TR:virtualunit-rotate-around-z-axis vuZ90 90)) ; test z-axis rotation, then x-axis
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3) '((0. 0. 0.) (5. 0. 10.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu3) "RotZ:90 RotX:90")

  ;     (setq vu3 (TR:virtualunit-replace-group-insertion-point vuZ90 '(20 30)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3 ) '((20. 30. 0.) (25. 40. 0.)))

  ;     (setq vu3 (TR:virtualunit-rotate-around-z-axis vuZ90 90)) ; test multiple z-axis rotations
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3) '((0. 0. 0.) (10. 5. 0.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu3) "RotZ:90 RotZ:90")

  ;     (setq vu3 (TR:virtualunit-rotate-around-x-axis vu 90))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3) '((0. 0. 0.) (10. 0. 5.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu3) "RotX:90")

  ;     (setq vu3 (TR:virtualunit-rotate-around-z-axis vu3 90)) ; test x-axis rotation, then z-axis
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3) '((0. 0. 0.) (0. 10. 5.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu3) "RotY:90 RotZ:90")

  ;     (setq vu3 (TR:virtualunit-rotate-around-y-axis vu 90))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3) '((0. 0. 0.) (0. 5. 10.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu3) "RotY:90")

  ;     (setq vu3 (TR:virtualunit-rotate-around-z-axis vu3 90)) ; test y-axis rotation, then z-axis
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-boundingbox (list vu3) '((0. 0. 0.) (5. 0. 10.)))
  ;     (TR:testsuite-test-for-equality 'TR:virtualunit-get-matrix-description (list vu3) "RotY:90 RotZ:90")

  ;     (princ)
  ;   )
  ; )
  (princ)
)

(princ)
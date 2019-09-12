
(defun TR:test-objectlist-get-boundingbox ( / ss )
  (TR:testsuite-test-drawing
    'TR:test-doc1-rects
    '(lambda ( listObjects )
      (TR:testsuite-test-for-equality 'TR:objectlist-get-boundingbox (list listObjects) '((0.0 0.0)(10.0 5.0)))
    )
  )
  (princ)
)

(defun TR:test-objectlist-calculate-total-length ( / ss listObjects )
  (TR:testsuite-test-drawing
    'TR:test-doc1-rects
    '(lambda ( listObjects )
      (TR:testsuite-test-for-equality 'TR:objectlist-calculate-total-length (list listObjects ) 34)
    )
  )

  (TR:testsuite-test-drawing
    'TR:test-doc2-supported-objects
    '(lambda ( listObjects )
      (TR:testsuite-test-for-equality 'TR:objectlist-calculate-total-length (list listObjects ) (+ pi 20.0))
    )
  )

  (princ)
)

(defun TR:test ()
  (TR:test-objectlist-get-boundingbox)
  (TR:test-objectlist-calculate-total-length)
)

(princ)
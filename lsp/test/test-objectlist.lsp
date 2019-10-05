
(defun TR:test-objectlist-get-boundingbox ( / listObjectsAll)
  (TR:testsuite-test-drawing
    'TR:test-doc1-rects
    '(lambda ( listObjectsAll )
      (TR:testsuite-test-for-equality 'TR:objectlist-get-boundingbox (list listObjectsAll) '((0.0 0.0)(10.0 5.0)))
    )
  )
  (princ)
)

(defun TR:test-objectlist-calculate-total-length ( / listObjectsAll )
  (TR:testsuite-test-drawing
    'TR:test-doc1-rects
    '(lambda ( listObjectsAll )
      (TR:testsuite-test-for-equality 'TR:objectlist-calculate-total-length (list listObjectsAll ) 34)
    )
  )

  (TR:testsuite-test-drawing
    'TR:test-doc2-supported-objects
    '(lambda ( listObjectsAll )
      (TR:testsuite-test-for-equality 'TR:objectlist-calculate-total-length (list listObjectsAll ) (+ pi 20.0))
    )
  )

  (princ)
)

(defun TR:test ()
  (TR:test-objectlist-get-boundingbox)
  (TR:test-objectlist-calculate-total-length)
)

(princ)



(defun TR:test ()

  (TR:testsuite-test-drawing
    'TR:test-doc1-rects
    '(lambda ( listObjectsAll )
      (setq explodedObjects (TR:object-explode listObjectsAll))
      (TR:testsuite-test-value-for-equality "TR:object-explode on doc1 - # distinct objects" (length explodedObjects) 8)
    )
  )

  (TR:testsuite-test-drawing
    'TR:test-doc3-rect
    '(lambda ( listObjectsAll )
      (setq explodedObjects (TR:object-explode listObjectsAll))
      (TR:testsuite-test-value-for-equality "TR:object-explode on doc3 - # distinct objects" (length explodedObjects) 4)
    )
  )


  (TR:testsuite-test-for-equality 'TR:object-explode nil nil)
  (TR:testsuite-test-for-equality 'TR:object-explode '() nil)

  (princ)
)

(princ)
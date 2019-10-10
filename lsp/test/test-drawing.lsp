

(defun TR:test ( / explodedObjects)

  (TR:testsuite-test-drawing
    'TR:test-doc3-rect
    '(lambda ( listObjectsAll )
      (setq explodedObjects (TR:object-explode (car listObjectsAll))) ; should only be one object in this list
      (TR:testsuite-test-value-for-equality "TR:object-explode on doc3 - result - # distinct objects"
        (length explodedObjects)
        4)
    )
  )

  (TR:testsuite-test-for-equality 'TR:object-explode (list nil) nil)

  (princ)
)

(princ)
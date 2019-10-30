
(defun TR:test-virtualunit ()

   (TR:testsuite-test-drawing
    'TR:test-doc3-rect
    '(lambda ( listObjectsAll / vu)
      (setq vu (TR:virtualunit-create-virtual-with-props listObjectsAll 0 '(0 0 0)))

      (TR:testsuite-test-value-for-equality "TR:virtualunit-get-objects on doc1 - result - # distinct objects"
        (length (TR:virtualunit-get-objects vu))
        1)

    )
  )

  (princ)
)

(defun TR:test ()
  (TR:test-virtualunit)
  (princ)
)

(princ)
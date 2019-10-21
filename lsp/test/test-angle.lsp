
(defun TR:test ()

  (TR:testsuite-test-for-equality 'TR:degrees->radians (list 180) pi)
  (TR:testsuite-test-for-equality 'TR:degrees->radians (list 360.0) (* 2 pi))
  (TR:testsuite-test-for-equality 'TR:degrees->radians (list "notnumber") nil)
  (TR:testsuite-test-for-equality 'TR:degrees->radians (list (list 180)) nil)

  (TR:testsuite-test-for-equality 'TR:radians->degrees (list pi) 180.0)
  (TR:testsuite-test-for-equality 'TR:radians->degrees (list (* 2 pi)) 360.0)
  (TR:testsuite-test-for-equality 'TR:radians->degrees (list "notnumber") nil)
  (TR:testsuite-test-for-equality 'TR:radians->degrees (list (list pi)) nil)

  (TR:testsuite-test-for-equality 'TR:angle-get-next-coterminal (list 0)  (* pi 2))
  (TR:testsuite-test-for-equality 'TR:angle-get-next-coterminal (list 0.1) (+ 0.1 (* pi 2)) )

  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list 0) 0)
  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list (* pi 2)) 0)
  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list (* pi 1)) (* pi 1))
  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list (* pi 2.5)) (* pi 0.5))
  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list (* pi 4.5)) (* pi 0.5))
  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list (* pi -0.5)) (* pi 1.5))
  (TR:testsuite-test-for-equality 'TR:angle-get-normalized (list (* pi -2.5)) (* pi 1.5))

  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.5) (* pi 0.51)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.5) (* pi 0.49)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.49) (* pi 0.5)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.51) (* pi 0.5)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.01) (* pi 0.49)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.49) (* pi 1.99)) T)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.49) (* pi 0.01)) T)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.51) (* pi 0.01)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.51) (* pi 0.49)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.51) (* pi 1.99)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 0.99) (* pi 0.51)) T)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 1.49) (* pi 0.49)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 1.49) (* pi 0.51)) T)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 1.99) (* pi 0.49)) F)
  (TR:testsuite-test-for-equality 'TR:angle-exclusively-between-start-end-angles (list (* pi 0.5) (* pi 1.99) (* pi 0.51)) T)

  (princ)
)

(princ)

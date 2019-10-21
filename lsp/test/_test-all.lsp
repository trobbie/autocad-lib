
(defun TR:load-tests ( / loadOrder relpath)
  (terpri)(princ "Running tests for TR library.  Assuming TR library _all.lsp has been loaded.")

  ; assume each file listed contains a (tr:test) function
  (setq loadOrder (list
    "_test-docs.lsp"
    "test-algorithm.lsp"
    "test-angle.lsp"
    "test-boundingbox.lsp"
    "test-drawing.lsp"
    "test-objectlist.lsp"
    "test-string.lsp"
    "test-util.lsp"
  ))

  (cond ((TR:testsuite-initialize)
    ; (aa:test) functions in each file would call TR:testsuite functions that give errors and
    ; increment the counters
    (foreach relpath loadOrder
      ; "undefine" test in case the file does not redefine one
      (defun TR:test () (princ))
      (princ (strcat "\nLoading and testing: " relpath))
      (load (strcat *TR:srcFolder* "\\lsp\\test\\" relpath))
      (TR:test)
    )

    (TR:testsuite-finalize "TR library")
  ))

  (princ)
)

(TR:load-tests)
(princ)

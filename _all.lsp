(vl-load-com)

; set to absolute path
;(setq *TR:srcFolder* "C:\\temp\\autocad")

(defun TR:load ( / loadOrder relpath)
  (setq loadOrder (list
   "testsuite.lsp"
   "util.lsp"
   "objectlist.lsp"
   "boundingbox.lsp"
   "collection.lsp"
   "groups.lsp"
   "view.lsp"
   "drawing.lsp"
   "calculations.lsp"
   "matrix.lsp"
  ))

  (foreach relpath loadOrder
    (load (strcat *TR:srcFolder* "\\lsp\\" relpath))
  )

  (princ)
)
(TR:load)
(princ)
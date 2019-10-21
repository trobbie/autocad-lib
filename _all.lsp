(vl-load-com)

; set to absolute path
;(setq *TR:srcFolder* "C:\\temp\\autocad")

(defun TR:load ( / loadOrder relpath)
  (setq loadOrder (list
   "algorithm.lsp"
   "boundingbox.lsp"
   "angle.lsp"
   "collection.lsp"
   "drawing.lsp"
   "groups.lsp"
   "matrix.lsp"
   "objectlist.lsp"
   "polyline.lsp"
   "string.lsp"
   "testsuite.lsp"
   "util.lsp"
   "view.lsp"
   "virtualunit.lsp"
  ))

  (foreach relpath loadOrder
    (load (strcat *TR:srcFolder* "\\lsp\\" relpath))
  )

  (princ)
)
(TR:load)
(princ)
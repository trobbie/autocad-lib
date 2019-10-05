
; use "_NONE" before coordinate values to avoid interference from object snap modes (ensures no grabbing)

(defun TR:test-doc1-rects ()
  (terpri)
  (command ".RECTANGLE" "_NONE" "*0,0" "_NONE" "10,5") ; 10x5
  (command ".RECTANGLE" "_NONE" "*1,1" "_NONE" "2,2") ; 1x1

  (princ)
)

(defun TR:test-doc2-supported-objects ()
  (terpri)
  ; creates rectangle with curved edges at the horizontals
  (command ".ARC" "_NONE" "*0.5,1.0" "e" "_NONE" "@0.0,-1.0" "r" "_NONE" "0.5")
  (command ".LINE" "*0.5,0.0" "*10.5,0.0" "")
  (command ".ARC" "_NONE" "*10.5,0.0" "e" "_NONE" "@0.0,1.0" "r" "_NONE" "0.5")
  (command ".LINE" "*10.5,1.0" "0.5,1.0" "")
  ; TODO: create circle
  ; TODO: create polyline w/ bulges/arcs
  ; TODO: add labels (to ensure ignored)
  ; 
  (princ)
)

(defun TR:test-doc3-rect ()
  (terpri)
  (command ".RECTANGLE" "_NONE" "*0,0" "_NONE" "10,5") ; 10x5

  (princ)
)

(princ)

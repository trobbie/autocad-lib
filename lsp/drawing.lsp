(vl-load-com)


;;;--------------------------------------------------------------;
;;; Function: TR:move-all-to-origin                              ;
;;;--------------------------------------------------------------;
;; Move all objects to 3d origin (using minimum extents)
;;;--------------------------------------------------------------;
(defun TR:move-all-to-origin( )

  ; call ZOOM "all" or "extents" to have system environment
  ;  extents (such as EXTMIN) recalculated and "shrink" back to 
  ;  actual size
  (command "ZOOM" "all")
  (command "MOVE" "all" "" (getvar "extmin") "*0,0,0")
  
  (princ)
)

;;;--------------------------------------------------------------;
;;; Function: TR:object-scale-uneven                             ;
;;;--------------------------------------------------------------;
;; Scale object unevenly (where x and y scaling is independent)
;;;--------------------------------------------------------------;
;; Author: heavily influenced by jdvillarreal and BeekeeCZ from
;; the autodesk forums.
;;;--------------------------------------------------------------;
(defun TR:object-scale-uneven ( object xScale yScale ptBase / ptBase3d )

  (setq ptBase3d (TR:point->3d-point ptBase))
  (command "_.-BLOCK"  "TmpBlk" "_none" ptBase3d (TR:objectlist->pickset (list object)) ""
           "_.-INSERT" "TmpBlk" "_R" 0 "_X" xScale "_Y" yScale "_none" ptBase3d
           "_.EXPLODE" "_L"
           "_.-PURGE" "_B" "TmpBlk" "_N")
  (car (TR:objectlist-join (LM:ss->vla (ssget "_P"))))
)

;;;--------------------------------------------------------------;
;;; Function: TR:object-set-color-rgb                            ;
;;;--------------------------------------------------------------;
;; Sets the color of a given object with the red-green-blue value.
;;;--------------------------------------------------------------;
(defun TR:object-set-color-rgb ( obj r g b / rgb)
  (setq rgb (vlax-create-object "AutoCAD.AcCmColor.20"))
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-SetRGB (list rgb 128 128 128))))
    (vl-catch-all-apply 'vla-put-truecolor (list obj rgb))
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:object-explode                                  ;
;;;--------------------------------------------------------------;
;; Explode a given object and return a list of the exploded vla-objects
;; The given object still remains in the document.
;;;--------------------------------------------------------------;
(defun TR:object-explode ( o )
  (if o
    (vlax-safearray->list (vlax-variant-value (vla-Explode o)))
    nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:create-2d-rectangle                             ;
;;;--------------------------------------------------------------;
;; Return a vla-object representing the rectangle, given an 
;; insertion point and rectangle size
;;
;; ptInsert = list of two real numbers; bottom-left 
;;   coordinate of the new rectangle
;; listSize = list of two real numbers; width and height, respectively
;; Returns = vla-object of type LWPolyline
;;;--------------------------------------------------------------;
(defun TR:create-2d-rectangle( ptInsert listSize / listPoints arrayPoints modelSpace listPolylines)

  ;; Define the 2D polyline points
  (setq listPoints (list
    (car ptInsert)
      (cadr ptInsert)
    (+ (car ptInsert) (car listSize))
      (cadr ptInsert)
    (+ (car ptInsert) (car listSize))
      (+ (cadr ptInsert) (cadr listSize))
    (car ptInsert)
      (+ (cadr ptInsert) (cadr listSize))
    (car ptInsert) (cadr ptInsert)
  ))

  (setq arrayPoints (vlax-make-safearray vlax-vbDouble '(0 . 9)))
  (vlax-safearray-fill arrayPoints listPoints)

  (setq modelSpace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq objPolyline (vla-AddLightWeightPolyline modelSpace arrayPoints))

  objPolyline ;return
)

;;;--------------------------------------------------------------;
;;; Function: TR:create-mtext                                    ;
;;;--------------------------------------------------------------;
;; Return an MText object, given an insertion point, width, and text
;;
;; ptInsert = list of two real numbers; represents bottom-left 
;;   coordinate of the new rectangle
;; width = real number; width of bounding box of the mtext object
;; text = string; text contents
;; Returns = vla-object of type MText
;;;--------------------------------------------------------------;
(defun TR:create-mtext( ptInsert width text / modelSpace)
  (setq modelSpace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vla-AddMText modelSpace 
    (vlax-3d-point ptInsert)
    width 
    text)
)

(princ)

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
(defun TR:create-mtext( ptInsert width text )
  (setq modelSpace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vla-AddMText modelSpace 
    (vlax-3d-point ptInsert)
    width 
    text)
)

(princ)

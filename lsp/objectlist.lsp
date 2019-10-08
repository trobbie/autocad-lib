(vl-load-com)

;; Object lists = list of vla-objects

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-boundingbox                      ;
;;;--------------------------------------------------------------;
;; Return the bottom-left (BL) and top-right (TR) points of a 
;; rectangle bounding a list of objects.  Note:  
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: ((min-x min-y min-z) (max-x max-y max-z))
;;;--------------------------------------------------------------;
;; Author: Lee Mac (with minor edits - TR)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-boundingbox ( listObjects / listBLpoints listTRpoints o o_bb a b)
  (foreach o listObjects
    (setq o_bb (TR:object-get-boundingbox o)
          listBLpoints (cons (TR:boundingbox-get-bottomleft o_bb) listBLpoints)
          listTRpoints (cons (TR:boundingbox-get-topright o_bb) listTRpoints)
    )
  )
  ; list of Bottom-Left and Top-Right points are now created
  (mapcar
    '(lambda( a b )
       (apply 'mapcar (cons a b)) 
    )
    '(min max)
    (list listBLpoints listTRpoints)
  )
  ; returns list of "min of x, y, and z values amongst BL points" 
  ;             and "max of x, y, and z amongst TR points"
)

;;;--------------------------------------------------------------;
;; Wrappers for objectlist, using boundingbox
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-left ( listObjects )
  (TR:boundingbox-get-left (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-right ( listObjects )
  (TR:boundingbox-get-right (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-bottom ( listObjects )
  (TR:boundingbox-get-bottom (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-top ( listObjects )
  (TR:boundingbox-get-top (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-size ( listObjects )
  (TR:boundingbox-get-size (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-center ( listObjects )
  (TR:boundingbox-get-center (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-bottomleft ( listObjects )
  (TR:boundingbox-get-bottomleft (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-bottomright ( listObjects )
  (TR:boundingbox-get-bottomright (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-topleft ( listObjects )
  (TR:boundingbox-get-topleft (TR:objectlist-get-boundingbox listObjects))
)
(defun TR:objectlist-get-topright ( listObjects )
  (TR:boundingbox-get-topright (TR:objectlist-get-boundingbox listObjects))
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist->safearray                           ;
;;;--------------------------------------------------------------;
;; Convert a list of objects to a safearray of objects.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: a safearray of values of type VLA-OBJECT
;;;--------------------------------------------------------------;
(defun TR:objectlist->safearray( listObjects )
  (vlax-safearray-fill
    (vlax-make-safearray vlax-vbObject (cons 0 (1- (length listObjects))))
    listObjects)
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist->pickset                             ;
;;;--------------------------------------------------------------;
;; Convert a list of objects to a pickset (selection set) that
;; can be used with autocad commands.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: a pickset, or nil if listObjects is nil or empty
;;;--------------------------------------------------------------;
(defun TR:objectlist->pickset ( listObjects / ss)
  (cond
    ((> (length listObjects) 0)
      (setq ss (ssadd))
      (foreach o listObjects
	      (ssadd (vlax-vla-object->ename o) ss)
      )
      ss
    )
    (T
      nil
    )
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-selected-objects                 ;
;;;--------------------------------------------------------------;
;; Returns a list of objects representing the currently 
;; selected objects, or if none selected, first asks user to
;; select.  Any selection is no longer selected after this returns.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: list of vla-objects that have been selected
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-selected-objects ( / ss listObjects )
  ; use sssetfirst to ensure it remains selected/highlighted (i.e. unaltered)
  (sssetfirst nil (ssget))
  (setq ss (vla-get-ActiveSelectionSet (vla-get-ActiveDocument (vlax-get-acad-object))))
  (setq listObjects (TR:collection->objectlist ss))
  (vla-delete ss) ; this vla selectionset is no longer needed
  listObjects
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-offset                               ;
;;;--------------------------------------------------------------;
;; Move a list of objects an amount specified by listOffset and
;; and return the updated objects.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; listOffset = list of two real numbers representing offset in X
;;   and Y direction, respectively
;; Returns: list of vla-objects that have been moved
;;;--------------------------------------------------------------;
(defun TR:objectlist-offset ( listObjects listOffset / xOffset yOffset )
  (mapcar
    '(lambda (o)
      (vla-move o
        (vlax-3d-point 0 0 0) ; from
        (vlax-3d-point listOffset); to
      )
      o
    )
    listObjects
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-move-to-origin                       ;
;;;--------------------------------------------------------------;
;; Shift the list of objects so that bottom-left point of bounding
;; area will be at origin.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: list of vla-objects that have been moved
;;;--------------------------------------------------------------;
(defun TR:objectlist-move-to-origin ( listObjects / bb o )
  (cond 
    ((not listObjects)
      nil ;return
    )
    (T
      (setq bb (TR:objectlist-get-boundingbox listObjects))
      (TR:objectlist-offset listObjects
        (list 
          (* -1.0 (TR:boundingbox-get-left bb))
          (* -1.0 (TR:boundingbox-get-bottom bb))
        )
      )
    )
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-scale-from-bottomleft-with-scale-factor ;
;;;--------------------------------------------------------------;
;; Scale the objectlist to match the width specified, keeping
;; the bottom-left point stationary.
;;
;; listObjects - list of vla-objects to scale
;; scaleFactor - real number representing scale factor
;; Returns scaled object
;;;--------------------------------------------------------------;
(defun TR:objectlist-scale-from-bottomleft-with-scale-factor ( listObjects scaleFactor / o ptBase )
  (setq ptBase (vlax-3d-point (TR:boundingbox-get-bottomleft (TR:objectlist-get-boundingbox listObjects))))
  (foreach o listObjects
    (vla-ScaleEntity o ptBase scaleFactor)
  )
  listObjects
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-join                                 ;
;;;--------------------------------------------------------------;
;; Join the given list of objects if possible.  This calls PEDIT
;; using the "JOIN" option and will not guarantee the objects to
; ;be joined.  Return the resulting object list.
;;;--------------------------------------------------------------;
(defun TR:objectlist-join ( listObjects / priorPeditaccept)
    ; set PEDITACCEPT env var to avoid getting a "convert to polyline?" question
    (setq priorPeditaccept (getvar "peditaccept"))
    (setvar "peditaccept" 1)
    (command "_.pedit" "_m" (TR:objectlist->pickset listObjects) "" "_j" "" "")
    (setvar "peditaccept" priorPeditaccept)
    (LM:ss->vla (ssget "_P"))
  )

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-print                              ;
;;;--------------------------------------------------------------;
;; Print data of the objects in a list.
;;;--------------------------------------------------------------;
(defun TR:objectlist-print ( listObjects / priorPeditaccept)
  (terpri)(princ "Object list:")
  (foreach o listObjects
    (princ (strcat "\n  " (vl-princ-to-string o)
      "- Type: " (vlax-get-property o 'ObjectName)
      " Data: " (vl-princ-to-string (entget (vlax-vla-object->ename o)))
    ))
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-calculate-total-length               ;
;;;--------------------------------------------------------------;
;; Sums all lengths of objects in the supplied list.  If an
;; object is not supported for finding length, a warning is 
;; printed for that object, but operation continues.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: real number representing the total length of all
;;   supported objects in the list
;;;--------------------------------------------------------------;
(defun TR:objectlist-calculate-total-length ( listObjects / o_name)
  (apply '+
    (mapcar
      '(lambda (o)
        (setq o_name (vlax-get-property o 'ObjectName))
        (cond
          ; "AcDbPolyline" "AcDbLine"
          ((vlax-property-available-p o "Length")
            (vlax-get-property o "Length")
          )
          ((and (= o_name "AcDbArc")
                (vlax-property-available-p o "ArcLength"))
            (vlax-get-property o "ArcLength")
          )
          ((and (= o_name "AcDbCircle")
                (vlax-property-available-p o "Circumference"))
            (vlax-get-property o "Circumference")
          )
          ((vl-position o_name (list "AcDbRotatedDimension" "AcDbText"))
            ; ignore annotation-like objects
            (float 0)
          )
          (T ; then not supported
            (terpri)(princ (strcat "Warning: unsupported object found of type: " (vlax-get-property o 'ObjectName)))
            (float 0) ; not included in length
          )
        )
      )
      listObjects
    )
  )
)

(princ)

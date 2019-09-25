(vl-load-com)

;; Object lists = list of vla-objects

(defun TR:objectlist-map ( listObjects func)
  (mapcar func listObjects)
)

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
;;; Function: TR:objectlist->safearray                           ;
;;;--------------------------------------------------------------;
;; Convert a list of objects to a safearray of objects.
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: a safearray of values of type VLA-OBJECT
;;;--------------------------------------------------------------;
(defun TR:objectlist->safearray( listObjects )
  (vlax-safearray-fill
    (vlax-make-safearray vlax-vbObject (cons 0 (1- (length listObjects))))
    listObjects)
)

(defun TR:objectlist-get-selected-objects ( )
  ; use sssetfirst to ensure it remains selected/highlighted (i.e. unaltered)
  (sssetfirst nil (ssget))
  (TR:collection->objectlist
     (vla-get-ActiveSelectionSet (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
)

; move a list of objects an amount specified by listOffset 
;; listObjects - ([<vla-object> <vla-object> ...])
;; listOffset = list of two real numbers representing offset in X and Y direction, respectively
;; Returns: list of vla-objects that have been moved
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

; shift the objectlist so that bottom-left point of bounding area will be at origin
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

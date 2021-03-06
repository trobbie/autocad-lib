(vl-load-com)
;;;--------------------------------------------------------------;
;; Object list = list of vla-objects
;;;--------------------------------------------------------------;

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-boundingbox                      ;
;;;--------------------------------------------------------------;
;; Return the min and max extents (as points) of a bounding box
;; surrounding the given list of objects.  If the objects are in
;; 2d, then this would be the bottom-left and top-right points.
;; listObjects - ([<vla-object> <vla-object> ...])
;; Returns: ((min-x min-y [min-z]) (max-x max-y [max-z]))
;;;--------------------------------------------------------------;
;; Author: Lee Mac (with minor edits - TR)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-boundingbox ( listObjects / listMinPts listMaxPts o o_bb a b)
  (foreach o listObjects
    (setq o_bb (TR:object-get-boundingbox3d o)
          listMinPts (cons (TR:boundingbox-get-min-extent o_bb) listMinPts)
          listMaxPts (cons (TR:boundingbox-get-max-extent o_bb) listMaxPts)
    )
  )
  (mapcar
    '(lambda( a b )
       (apply 'mapcar (cons a b)) 
    )
    '(min max)
    (list listMinPts listMaxPts)
  )
  ; returns list of "min of x, y, and z values amongst min-extents of each object" 
  ;             and "max of x, y, and z values amongst max-extents of each object"
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
  
  ; ensure the selection is clear in the document
  (sssetfirst nil nil)

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
;;; Function: TR:objectlist-copy                                 ;
;;;--------------------------------------------------------------;
;; Copy a list of objects and place its bottom-left corner at
;; the given insert point.
;;
;; listObjects - ([<vla-object> <vla-object> ...])
;; ptInsert = (x y [z]), real numbers representing insert point
;; Returns: list of new vla-objects that resulted from the copy
;;;--------------------------------------------------------------;
(defun TR:objectlist-copy (listObjects ptInsert / oCopy)
  (mapcar 
    '(lambda (o)
      (vla-Move 
        (setq oCopy (vla-Copy o))
        (vlax-3d-point (TR:objectlist-get-bottomleft listObjects))
        (vlax-3d-point ptInsert)
      )
      oCopy
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
        (mapcar
          '-
          (TR:boundingbox-get-bottomleft bb)
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
(defun TR:objectlist-scale-from-bottomleft-with-scale-factor ( listObjects scaleFactor ptBase / o )
  (foreach o listObjects
    (vla-ScaleEntity o (vlax-3d-point ptBase) scaleFactor)
  )
  listObjects
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-join                                 ;
;;;--------------------------------------------------------------;
;; Join the given list of objects if possible.  This calls PEDIT
;; using the "JOIN" option and will not guarantee the objects to
;; be joined (e.g. if points didn't match).  This may create multiple
;; polylines, so ideally only call this assuming the list of objects will
;; create at most one joined polyline, since the return value only 
;; returns one joined polyline.
;; Side effect: not all objects in given list will still exist in
;; document after calling this.
;; Returns: the last polyline created from joining.  Note: if
;; multiple polylines were created, this only returns one arbitrarily.
;;;--------------------------------------------------------------;
(defun TR:objectlist-join ( listObjects / priorPeditaccept)
  ; set PEDITACCEPT env var to avoid getting a "convert to polyline?" question
  (setq priorPeditaccept (getvar "peditaccept"))
  (setvar "peditaccept" 1)
  (command "_.pedit" "_m" (TR:objectlist->pickset listObjects) "" "_j" "" "")
  (setvar "peditaccept" priorPeditaccept)
  (LM:ss->vla (ssget "_L"))
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-delete                               ;
;;;--------------------------------------------------------------;
;; Delete all objects in the given list.  Print warning if an object
;; could not be deleted.
;;;--------------------------------------------------------------;
(defun TR:objectlist-delete ( listObjects / o)
  (foreach o listObjects
    (cond ((and o (not (vlax-erased-p o)))
      (vla-Delete o)
      (if (not (vlax-erased-p o))
        (princ (strcat "\nWARNING: Object could not be deleted: bounding box = " (vl-princ-to-string (TR:object-get-boundingbox o))))
      )
    ))
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-explode                              ;
;;;--------------------------------------------------------------;
;; Explode the given list of objects.  Afterwards objects in given
;; list will still remain in the document.
;; Returns: the list of objects representing the exploded listObjects
;;;--------------------------------------------------------------;
(defun TR:objectlist-explode ( listObjects )
  (apply
    'append
    (mapcar
      '(lambda (o)
        (TR:object-explode o)
      )
      listObjects
    )
  )
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
          ((vl-position o_name (list "AcDbRotatedDimension" "AcDbText" "AcDbMText" "AcDb2LineAngularDimension"))
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


;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-all-dim-related-points           ;
;;;--------------------------------------------------------------;
;; Find all points within the given list of objects that are
;; pertinent when dealing with DIM objects
;; E.g. for polylines, all points found would be considered;
;;      for a circle, the top/bottom/left/right extents would
;;
;; listObjects = ([<vla-object> <vla-object> ...])
;; Returns: list of 2d points, where a point is of form: (real-x real-y)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-all-dim-related-points ( listObjects 
  / listPoints o pt2d pt3d o_name pt2dPrior msg bulge bulgePt i listBulgePoints )
  (setq listPoints nil)
  (foreach o listObjects
    (setq o_name (vlax-get-property o 'ObjectName))
    (cond
      ((= o_name "AcDbPolyline")
        (setq i 0)
        (setq pt2dPrior nil)
        (foreach pt2d (TR:polyline-get-coordinates o nil)
            ; Arcs (designated by "bulge" values) supported within polylines:
            ; if bulge is non-zero, then the polyline arcs between this vertex and next
            ; and this may contain points at the quadrant extents
          (cond (pt2dPrior
            (setq bulge (vla-GetBulge o (1- i)))
            (setq listBulgePoints (TR:get-arc-bulge-extents-exclusive pt2dPrior pt2d bulge))
            ; listBulgePoints should not include start and end points
            (foreach bulgePt listBulgePoints
              (setq listPoints (cons pt2d listPoints))
            )
          ))
          (setq listPoints (cons pt2d listPoints))

          (setq pt2dPrior pt2d) ; in case next point has a bulge value
          (setq i (1+ i))
        )
      );_end-of cond "AcDbPolyline"
      ((= o_name "AcDbLine")
        (setq pt3d (vlax-safearray->list (vlax-variant-value (vlax-get-property o 'StartPoint))))
        (setq listPoints (cons (TR:point->2d-point pt3d) listPoints))
        (setq pt3d (vlax-safearray->list (vlax-variant-value (vlax-get-property o 'EndPoint))))
        (setq listPoints (cons (TR:point->2d-point pt3d) listPoints))
      );_end-of cond "AcDbLine"
      ((= o_name "AcDbCircle")
        (setq center (vlax-safearray->list (vlax-variant-value (vlax-get-property o 'Center)))
              radius (vlax-get-property o 'Radius))
        (setq pt2d (list (car center) (- (cadr center) radius)))
        (setq listPoints (cons pt2d listPoints))
        (setq pt2d (list (car center) (+ (cadr center) radius)))
        (setq listPoints (cons pt2d listPoints))
        (setq pt2d (list (- (car center) radius) (cadr center)))
        (setq listPoints (cons pt2d listPoints))
        (setq pt2d (list (+ (car center) radius) (cadr center)))
        (setq listPoints (cons pt2d listPoints))
      );_end-of cond "AcDbCircle"
      ((= o_name "AcDbArc")
        ; add top/bottom/leftmost/rightmost points of circle if arc traverses  the point
        ; we use shortcut here: if one of these four points are on the bounding box of the arc, use it
        (setq o_bb (TR:object-get-boundingbox o))
        (setq o_center3d (vlax-safearray->list (vlax-variant-value (vlax-get-property o 'Center))))
        (setq o_radius (vlax-get-property o 'Radius))

        (setq pt2d (list (car o_center3d) (+ (cadr o_center3d) o_radius))) ; possible top of arc
        (if (= (cadr pt2d) (TR:boundingbox-get-top o_bb))
          (setq listPoints (cons pt2d listPoints))
        )
        (setq pt2d (list (car o_center3d) (- (cadr o_center3d) o_radius))) ; possible bottom of arc
        (if (= (cadr pt2d) (TR:boundingbox-get-bottom o_bb))
          (setq listPoints (cons pt2d listPoints))
        )
        (setq pt2d (list (- (car o_center3d) o_radius) (cadr o_center3d))) ; possible leftmost of arc
        (if (= (car pt2d) (TR:boundingbox-get-left o_bb))
          (setq listPoints (cons pt2d listPoints))
        )
        (setq pt2d (list (+ (car o_center3d) o_radius) (cadr o_center3d))) ; possible rightmost of arc
        (if (= (car pt2d) (TR:boundingbox-get-right o_bb))
          (setq listPoints (cons pt2d listPoints))
        )
      );_end-of "AcDbArc"
      ((= o_name "AcDbBlockReference")
         ; for blocks, just use the corners of its bounding box
         ; TODO: dive into the block and find the specific points that touch the bounding box
         (setq o_bb (TR:object-get-boundingbox o))
         (setq listPoints (cons (TR:boundingbox-get-bottomleft o_bb) listPoints))
         (setq listPoints (cons (TR:boundingbox-get-bottomright o_bb) listPoints))
         (setq listPoints (cons (TR:boundingbox-get-topleft o_bb) listPoints))
         (setq listPoints (cons (TR:boundingbox-get-topright o_bb) listPoints))
      )  ;_end-of "AcDbBlockReference"
      ((vl-position o_name (list "AcDbRotatedDimension" "AcDbText" "AcDbMText" "AcDb2LineAngularDimension"))
        nil
      )
      (T ; default of cond
        (setq msg (strcat "WARNING: template-objectlist-get-all-points(): ignored object of type "  o_name))
        (terpri)(princ msg)
        (alert msg)
      )
    );_end-of cond
  );_end-of foreach
  listPoints
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-topmost-of-leftmost-vertices     ;
;;;--------------------------------------------------------------;
;; Return top-most of the left-most points in object list.
;; Z-dimesion is ignored.
;;
;; listObjects = ([<vla-object> <vla-object> ...])
;; Returns: (real-x real-y)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-topmost-of-leftmost-vertices( listObjects / pt_topleft pt)
  (setq pt_topleft nil)
  (foreach pt (TR:objectlist-get-all-dim-related-points listObjects)
    (if (not pt_topleft)
      (setq pt_topleft pt)
      ;else possibly update pt_topleft
      (cond
        ((equal (car pt) (car pt_topleft) *AA:POINT-BOUNDARIES-FUZZ*)
          (if (>= (cadr pt) (cadr pt_topleft))
            (setq pt_topleft pt)
          )
        )
        ((< (car pt) (car pt_topleft))
          (setq pt_topleft pt)
        )
      );_ end-of cond
    )
  )
  pt_topleft
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-topmost-of-rightmost-vertices    ;
;;;--------------------------------------------------------------;
;; Return top-most of the right-most points in object list.
;; Z-dimesion is ignored.
;;
;; listObjects = ([<vla-object> <vla-object> ...])
;; Returns: (real-x real-y)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-topmost-of-rightmost-vertices( listObjects / pt_topright pt)
  (setq pt_topright nil)
  (foreach pt (TR:objectlist-get-all-dim-related-points listObjects)
    (if (not pt_topright)
      (setq pt_topright pt)
      ;else possibly update pt_topright
      (cond
        ((equal (car pt) (car pt_topright) *AA:POINT-BOUNDARIES-FUZZ*)
          (if (>= (cadr pt) (cadr pt_topright))
            (setq pt_topright pt)
          )
        )
        ((> (car pt) (car pt_topright))
          (setq pt_topright pt)
        )
        );_ end-of cond
    )
  )
  pt_topright
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-rightmost-of-bottommost-vertices ;
;;;--------------------------------------------------------------;
;; Return right-most of the bottom-most points in object list.
;; Z-dimesion is ignored.
;;
;; listObjects = ([<vla-object> <vla-object> ...])
;; Returns: (real-x real-y)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-rightmost-of-bottommost-vertices( listObjects / pt_rightbottom pt )
  (setq pt_rightbottom nil)
  (foreach pt (TR:objectlist-get-all-dim-related-points listObjects)
    (if (not pt_rightbottom)
      (setq pt_rightbottom pt)
      ;else possibly update pt_rightbottom
      (cond
        ((equal (cadr pt) (cadr pt_rightbottom) *AA:POINT-BOUNDARIES-FUZZ*)
          (if (>= (car pt) (car pt_rightbottom))
            (setq pt_rightbottom pt)
          )
        )
        ((< (cadr pt) (cadr pt_rightbottom))
          (setq pt_rightbottom pt)
        )
      );_ end-of cond
    )
  )
  pt_rightbottom
)

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist-get-rightmost-of-topmost-vertices    ;
;;;--------------------------------------------------------------;
;; Return right-most of the top-most points in object list.
;; Z-dimesion is ignored.
;;
;; listObjects = ([<vla-object> <vla-object> ...])
;; Returns: (real-x real-y)
;;;--------------------------------------------------------------;
(defun TR:objectlist-get-rightmost-of-topmost-vertices( listObjects / pt_righttop pt)
  (setq pt_righttop nil)
  (foreach pt (TR:objectlist-get-all-dim-related-points listObjects)
    (if (not pt_righttop)
      (setq pt_righttop pt)
      ;else possibly update pt_righttop
      (cond
        ((equal (cadr pt) (cadr pt_righttop) *AA:POINT-BOUNDARIES-FUZZ*)
          (if (>= (car pt) (car pt_righttop))
            (setq pt_righttop pt)
          )
        )
        ((> (cadr pt) (cadr pt_righttop))
          (setq pt_righttop pt)
        )
      );_ end-of cond
    )
  )
  pt_righttop
)
(princ)

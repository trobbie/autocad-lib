;;;--------------------------------------------------------------;
;; Virtual Unit
;;;--------------------------------------------------------------;
; A definition of a unit, whose actual drawing objects are not created 
; until the proper function is called.  The referenced objects in the description can be
; anywhere in the document, and remain untouched (read-only) by these functions.  The
; intention is that many calculations can be done without having to recreate a separate
; object, only creating a new drawing object when finished.  Groups of virtual units
; are envisioned too.
;
; Assumptions:
; - The referenced objects won't chanage
;
; Format:
;   (("Objects" . (list of <vla-object>))
;    ("GroupInsertionPoint" . <list of 3 real numbers, representing insertion point>)
;    ("DescMatrix" . <string describing the transformation done resulting in "matrixRotation">)
;    ("MatrixRotation" . <4x4 matrix of real numbers, representing the transformation matrix used for rotation>)
;   )
;
; There is an implied origin translation at the beginning where the entire unit
; is seen as moving to the WCS origin before rotation.  This always involves an exact
; translation from the _center_ of the unit's bounding box to (0,0,0).
;
; Objects
; List of already existing vla-objects, seen as read-only.
; TODO: consider using Groups here.

; GroupInsertionPoint
; The group insertion 3d point is where the unit's _bottom-left_ point of the rotated
; unit's bounding box will be placed relative to a group origin.
; Think an array of virtualunits on the screen, and GroupInsertionPoint species this 
; virtualunit's location within the array.  For the GroupInsertionPoint, there is another
; implied offset (from unit center to bottom-left corner) involved between rotation and
; GroupInsertionPoint.
; If unit is not part of a group, this should be '(0 0 0).
; The bottom-left corner provides a consistant and easily-visualized reference point
; when relating to other virtual units.
;
; MatrixRotation
; The MatrixRotation represents the transform matrix applied to all objects from the
; implied origin (center of bounding box surrounding all objects).  Use the rotation
; function to apply additional cumulative rotations, in order of the calls.
;
; DescRotation
; A user-friendly string description of the rotations that have been applied.  Without
; this, programmers looking at the MatrixRotation would have a hard time understanding
; the intent.
;
;
; Additional concepts:
; InsertionPoint (not defined inside virtualunit)
; Not defined until drawing objects are created, and allow a group of virtualunits to be
; placed together at an insertion point.  This is different than the GroupInsertionPoint,
; which is defined before drawing object creation and specifies any placement of this
; virtualunit within a group.
;
; When creating a drawing object. we will always create a _copy_ of  the original
; objects, then performing the appropriate translations and transformations.

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-get-property                        ;
;;;--------------------------------------------------------------;
;; Return the value of a given property name.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-get-property ( vu propertyName )
  (cdr (assoc propertyName vu))
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-set-property                        ;
;;;--------------------------------------------------------------;
;; Return the virtualunit that is given, but with the property given changed.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-set-property ( vu propertyName propertyValue )
  (subst (cons propertyName propertyValue) (assoc propertyName vu) vu)
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-create-virtual                      ;
;;;--------------------------------------------------------------;
;; Return a virtual unit given a list of objects
;;;--------------------------------------------------------------;
(defun TR:virtualunit-create-virtual ( listObjects )
  (TR:virtualunit-create-virtual-with-props listObjects 0 '(0 0 0))
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-create-virtual-with-props           ;
;;;--------------------------------------------------------------;
;; Return a virtual unit given a list of objects, rotation amount 
;; around z-axis, and insertion point.  We use a rotation around z-axis
;; since it is the most common rotation.  Additional rotations can
;; be done using appropriate rotation functions on this created
;; virtual unit (applied in order called).
;;;--------------------------------------------------------------;
(defun TR:virtualunit-create-virtual-with-props ( listObjects degreesRotationZ ptInsert / vu )
  (setq vu 
    (list 
      (cons "Objects" listObjects)
      (cons "GroupInsertionPoint" (TR:point->3d-point ptInsert))
      (cons "MatrixRotation" (imat 4))
      (cons "DescRotation" "")
    )
  )
  (TR:virtualunit-rotate-around-z-axis vu degreesRotationZ)
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-copy-with-new-rotation-z-axis       ;
;;;--------------------------------------------------------------;
;; Return the virtualunit that is given, but with the rotation value
;; (in all directions) being replaced with one given rotation amount
;; around the z-axis.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-copy-with-new-rotation-z-axis ( vu degreesRotationZ )
  (list 
    (cons "Objects" (TR:virtualunit-get-property vu "Objects"))
    (cons "GroupInsertionPoint" (TR:virtualunit-get-property vu "GroupInsertionPoint"))
    (cons "MatrixRotation" (imat 4))
    (cons "DescRotation" "")
  )
  (TR:virtualunit-rotate-around-z-axis vu degreesRotationZ)
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-copy-with-new-group-insertion-point ;
;;;--------------------------------------------------------------;
;; Return the virtualunit that is given, but with the insertion
;; point changed to the one given
;;;--------------------------------------------------------------;
(defun TR:virtualunit-copy-with-new-group-insertion-point ( vu ptInsert )
  (list 
    (cons "Objects" (TR:virtualunit-get-property vu "Objects"))
    (cons "GroupInsertionPoint" (TR:point->3d-point ptInsert))
    (cons "MatrixRotation" (TR:virtualunit-get-property vu "MatrixRotation"))
    (cons "DescRotation" (TR:virtualunit-get-property vu "DescRotation"))
  )
)

;;;--------------------------------------------------------------;
;;; Convenience functions:
;;;--------------------------------------------------------------;
(defun TR:virtualunit-get-objects ( vu )
  (TR:virtualunit-get-property vu "Objects")
)
(defun TR:virtualunit-get-matrix-description ( vu )
  (TR:virtualunit-get-property "DescMatrix")
)
(defun TR:virtualunit-get-matrix-rotation ( vu )
  (TR:virtualunit-get-property "MatrixRotation")
)
(defun TR:virtualunit-get-group-insertion-point ( vu )
  (TR:virtualunit-get-property vu "GroupInsertionPoint")
)
(defun TR:virtualunit-set-group-insertion-point ( vu ptInsert )
  (TR:virtualunit-set-property vu "GroupInsertionPoint" (TR:point->3d-point ptInsert))
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-rotate-around-x-axis                ;
;;;--------------------------------------------------------------;
;; Return given virutal unit after having rotated it an additional
;; given amount around the x-axis.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-rotate-around-x-axis ( vu degreesX / desc )
  (cond ((not (= degreesX 0)) ; do no rotation if no degrees
    ; note to do an additional rotation with matrix multiplication, the new rotation must be the first matrix
    (TR:virtualunit-set-property vu "MatrixRotation"
      (mxm 
        (TR:matrix-rotate-around-x-axis (TR:degrees->radians degreesX))
        (TR:virtualunit-get-matrix-rotation vu)
      )
    )
    (setq desc (TR:virtualunit-get-matrix-description vu))
    (TR:virtualunit-set-property vu "DescRotation"
      (strcat desc 
            (if desc " " "")
            "RotX:" (rtos degreesX 2 0)
      )
    )
  ))
  vu
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-rotate-around-y-axis                ;
;;;--------------------------------------------------------------;
;; Return given virutal unit after having rotated it an additional
;; given amount around the y-axis.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-rotate-around-y-axis ( vu degreesY / desc )
  (cond ((not (= degreesY 0)) ; do no rotation if no degrees
    ; note to do an additional rotation with matrix multiplication, the new rotation must be the first matrix
    (TR:virtualunit-set-property vu "MatrixRotation"
      (mxm 
        (TR:matrix-rotate-around-y-axis (TR:degrees->radians degreesY))
        (TR:virtualunit-get-matrix-rotation vu)
      )
    )
    (setq desc (TR:virtualunit-get-matrix-description vu))
    (TR:virtualunit-set-property vu "DescRotation"
      (strcat desc 
            (if desc " " "")
            "RotY:" (rtos degreesY 2 0)
      )
    )
  ))
  vu
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-rotate-around-z-axis                ;
;;;--------------------------------------------------------------;
;; Return given virutal unit after having rotated it an additional
;; given amount around the z-axis.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-rotate-around-z-axis ( vu degreesZ / desc )
  (cond ((not (= degreesZ 0)) ; do no rotation if no degrees
    ; note to do an additional rotation with matrix multiplication, the new rotation must be the first matrix
    (TR:virtualunit-set-property vu "MatrixRotation"
      (mxm 
        (TR:matrix-rotate-around-z-axis (TR:degrees->radians degreesZ))
        (TR:virtualunit-get-matrix-rotation vu)
      )
    )
    (setq desc (TR:virtualunit-get-matrix-description vu))
    (TR:virtualunit-set-property vu "DescRotation"
      (strcat desc 
            (if desc " " "")
            "RotZ:" (rtos degreesZ 2 0)
      )
    )
  ))
  vu
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-get-boundingbox                     ;
;;;--------------------------------------------------------------;
;; Return the bounding box of the drawing objects that would be
;; create from this virtual unit.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-get-boundingbox ( vu / tempListObjects bb )
  ; TODO: way to do this w/o creating/deleting a vla-object?

  (setq tempListObjects (TR:virtualunit-create-drawing-objects vu '(0 0 0)))
  (setq bb (TR:objectlist-get-boundingbox tempListObjects))
  (TR:objectlist-delete tempListObjects)

  bb
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-get-size                            ;
;;;--------------------------------------------------------------;
;; Return the size of the bounding box of the drawing objects that
;; would be create from this virtual unit.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-get-size ( vu )
  (TR:boundingbox-get-size (TR:objectlist-get-boundingbox (TR:virtualunit-get-objects vu)))
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunit-create-drawing-objects              ;
;;;--------------------------------------------------------------;
;; Return and create the drawing objects in the active document
;; that are defined by this virtual unit definition.  See the
;; top-level description of a virtual unit for explanation of how
;; this is done.
;;
;; This creation will not change the source objects nor this 
;; virtual unit.  After creation, there will be no connection
;; between the created objects and the virtual unit.
;;;--------------------------------------------------------------;
(defun TR:virtualunit-create-drawing-objects ( vu ptInsert 
  / listObjectsOrig listCopies bbOrig o oCopy bbRotated bbCenter )
  
  (setq listObjectsOrig (TR:virtualunit-get-objects vu))
  (setq bbOrig (TR:objectlist-get-boundingbox listObjectsOrig))
  (setq bbCenter (TR:boundingbox-get-center bbOrig))
  (setq transMatrix (vlax-tmatrix (TR:virtualunit-get-matrix-rotation vu)))
  (setq listCopies 
    (mapcar
      '(lambda (o / oCopy)
        ; create copy
        (setq oCopy (vla-Copy o))
        ; do implied translation to origin
        (vla-Move oCopy 
          (vlax-3d-point bbCenter)
          (vlax-3d-point 0 0 0)
        )
        ; do transformation
        (vla-TransformBy oCopy transMatrix)
        oCopy
      )
      listObjectsOrig
    )
  )

  ; move to insertion point
  (setq bbRotated (TR:objectlist-get-boundingbox listCopies))
  ; consider implied offset from BL to origin
  (setq offset 
    (mapcar '(lambda(x) (- x))
            (TR:boundingbox-get-bottomleft bbRotated))
  )
  ; move the BL (at origin now) to the insertion point
  (setq offset 
    (mapcar '+ offset
              (TR:virtualunit-get-group-insertion-point vu)
               ptInsert)
  )
  (TR:objectlist-offset listCopies offset)
)

;;;--------------------------------------------------------------;
;;; Function: TR:virtualunitlist-copy                           ;
;;;--------------------------------------------------------------;
;; Return a list of virtual units that is a copy of the given list
;; of virtual units having applied the given (group) offset. 
;;;--------------------------------------------------------------;
(defun TR:virtualunitlist-copy ( listVUs ptInsertOffset )
  (mapcar 
    '(lambda (vu)
      (TR:virtualunit-copy-with-new-group-insertion-point vu 
        (mapcar '+ (TR:virtualunit-get-group-insertion-point vu) ptInsertOffset)
      )
    )
    listVUs
  )
)

(princ)

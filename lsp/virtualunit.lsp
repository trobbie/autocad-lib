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

(defun TR:virtualunit-get-property ( vu propertyName )
  (cdr (assoc propertyName vu))
)

(defun TR:virtualunit-set-property ( vu propertyName propertyValue )
  (subst (cons propertyName propertyValue) (assoc propertyName vu) vu)
)

(defun TR:virtualunit-create-virtual ( listObjects )
  (TR:virtualunit-create-virtual-with-props listObjects 0 '(0 0 0))
)

; the most common rotation is around z-axis; for other axis rotations, use the appropriate rotation function later
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

; Returns a new virtual unit that has the same properties as the given one except
; that it has an absolute rotation of the given degrees around z-axis, instead of the
; rotation assigned in the given one
(defun TR:virtualunit-copy-with-new-rotation-z-axis ( vu degreesRotationZ )
  (list 
    (cons "Objects" (TR:virtualunit-get-property vu "Objects"))
    (cons "GroupInsertionPoint" (TR:virtualunit-get-property vu "GroupInsertionPoint"))
    (cons "MatrixRotation" (imat 4))
    (cons "DescRotation" "")
  )
  (TR:virtualunit-rotate-around-z-axis vu degreesRotationZ)
)

; Returns a new virtual unit that has the same properties as the given one except
; that the InsertionPoint has been replaced with the given insertion point
(defun TR:virtualunit-copy-with-new-group-insertion-point ( vu ptInsert )
  (list 
    (cons "Objects" (TR:virtualunit-get-property vu "Objects"))
    (cons "GroupInsertionPoint" (TR:point->3d-point ptInsert))
    (cons "MatrixRotation" (TR:virtualunit-get-property vu "MatrixRotation"))
    (cons "DescRotation" (TR:virtualunit-get-property vu "DescRotation"))
  )
)

(defun TR:virtualunit-list-copy ( listVUs ptInsertOffset )
  (mapcar 
    '(lambda (vu)
      (TR:virtualunit-copy-with-new-group-insertion-point vu 
        (mapcar '+ (TR:virtualunit-get-group-insertion-point vu) ptInsertOffset)
      )
    )
    listVUs
  )
)

(defun TR:virtualunit-get-objects ( vu )
  (TR:virtualunit-get-property vu "Objects")
)

(defun TR:virtualunit-get-matrix-description ( vu )
  (TR:virtualunit-get-property "DescMatrix")
)

(defun TR:virtualunit-get-matrix-rotation ( vu )
  (TR:virtualunit-get-property "MatrixRotation")
)

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

(defun TR:virtualunit-get-group-insertion-point ( vu )
  (TR:virtualunit-get-property vu "GroupInsertionPoint")
)
(defun TR:virtualunit-set-group-insertion-point ( vu ptInsert )
  (TR:virtualunit-set-property vu "GroupInsertionPoint" (TR:point->3d-point ptInsert))
)

(defun TR:virtualunit-get-boundingbox ( vu / tempListObjects bb )
  ; TODO: way to do this w/o creating/deleting a vla-object?

  (setq tempListObjects (TR:virtualunit-create-drawing-objects vu '(0 0 0)))
  (setq bb (TR:objectlist-get-boundingbox tempListObjects))
  (TR:objectlist-delete tempListObjects)

  bb
)

; return list of sizes of each dimension of the bounding box
(defun TR:virtualunit-get-size ( vu )
  (TR:boundingbox-get-size (TR:objectlist-get-boundingbox (TR:virtualunit-get-objects vu)))
)

; create a _copy_ of the original read-only objects, then performing the 
; appropriate translations and transformations, and finally move so
; bottom-left point is at insertion point (ptInsert).
(defun TR:virtualunit-create-drawing-objects ( vu ptInsert / listObjectsOrig listCopies bbOrig o oCopy bbRotated bbCenter )
  
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

(princ)

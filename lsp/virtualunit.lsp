; Virtual unit = a definition of a unit, whose actual drawing objects are not created 
; until the proper function is called.  The referenced objects in the description can be
; anywhere in the document, and remain untouched (read-only) by these functions.  The
; intention is that many calculations can be done without having to recreate a separate
; object, only creating a new drawing object when finished.
;
; Assumptions:
; - The referenced objects won't chanage
;
; Format:
;   (("Objects" . (list of <vla-object>))
;    ("RotationDegrees" . <real number, representing degrees of rotation>)
;    ("InsertionPoint" . <list of 3 real numbers, representing insertion point>)
;   )
;
; There is an implied origin translation at the beginning where the entire unit
; is seen as moving to the WCS origin before rotation.  This always involves an exact
; translation from the _center_ of the unit's bounding box to (0,0,0).
;
; Objects:
; List of already existing vla-objects, seen as read-only.
; TODO: consider using Groups here.
;
; RotationDegrees:
; The RotationDegrees represents taking all objects from the implied origin 
; (center of bounding box surrounding all objects) and performing rotation.
;
; InsertionPoint:
; The insertion 3d point is where the entire unit will be placed so that the _bottom-left_
; point of the rotated unit's bounding box is put there.  This means there is another
; implied offset (from origin to bottom-left corner) involved between rotation and insertion.
; The bottom-left corner provides a consistant and easily-visualized reference point
; when relating to other virtual units.
;
; When creating a drawing object. we will always create a _copy_ of  the original
; objects, then performing the appropriate translations and transformations.

(defun TR:virtualunit-get-property ( vu propertyName )
  (cdr (assoc propertyName vu))
)

(defun TR:virtualunit-set-property ( vu propertyName propertyValue)
  (subst (cons propertyName propertyValue) (assoc propertyName vu) vu)
)

(defun TR:virtualunit-create-virtual ( listObjects )
  (TR:virtualunit-create-virtual-with-props listObjects 0 '(0 0 0))
)

(defun TR:virtualunit-create-virtual-with-props ( listObjects degreesRotation ptInsert)
  (list 
    (cons "Objects" listObjects)
    (cons "RotationDegrees" degreesRotation)
    (cons "InsertionPoint" (TR:point->3d-point ptInsert))
  )
)

; Returns a new virtual unit that has the same properties as the given one except
; that it has an absolute rotation of the given degrees, instead of what was
; assigned in the given one
(defun TR:virtualunit-copy-with-new-rotation-degrees ( vu degrees)
  (TR:virtualunit-create-virtual-with-props 
    (TR:virtualunit-get-objects vu)
    degrees
    (TR:virtualunit-get-property vu "InsertionPoint")
  )
)

; Returns a new virtual unit that has the same properties as the given one except
; that the InsertionPoint has been replaced with the given insertion point
(defun TR:virtualunit-copy-with-new-insertion-point ( vu ptInsert)
  (TR:virtualunit-create-virtual-with-props 
    (TR:virtualunit-get-objects vu)
    (TR:virtualunit-get-property vu "RotationDegrees")
    (TR:point->3d-point ptInsert)
  )
)

(defun TR:virtualunit-get-objects( vu )
  (TR:virtualunit-get-property vu "Objects")
)

(defun TR:virtualunit-get-tmatrix( vu )
  (TR:tMatrix-rotate (TR:degrees->radians (TR:virtualunit-get-property vu "RotationDegrees")))
)

(defun TR:virtualunit-get-tmatrix-as-list( vu )
  (TR:matrix-rotate (TR:degrees->radians (TR:virtualunit-get-property vu "RotationDegrees")))
)

(defun TR:virtualunit-set-rotation-degrees ( vu degrees )
  (TR:virtualunit-set-property vu "RotationDegrees" degrees)
)

(defun TR:virtualunit-get-insertion-point ( vu )
  (TR:virtualunit-get-property vu "InsertionPoint")
)
(defun TR:virtualunit-set-insertion-point ( vu ptInsert )
  (TR:virtualunit-set-property vu "InsertionPoint" (TR:point->3d-point ptInsert))
)

(defun TR:virtualunit-get-boundingbox ( vu / tempListObjects bb)
  ; TODO: way to do this w/o creating/deleting a vla-object?

  (setq tempListObjects (TR:virtualunit-create-drawing-objects vu))
  (setq bb (TR:objectlist-get-boundingbox tempListObjects))
  (foreach o tempListObjects
    (vla-delete o)
  )
  ;(TR:objectlist-get-boundingbox (TR:virtualunit-get-objects vu))
  bb
)

; return list of sizes of each dimension of the bounding box
(defun TR:virtualunit-get-size ( vu )
  (TR:boundingbox-get-size (TR:objectlist-get-boundingbox (TR:virtualunit-get-objects vu)))
)

; create a _copy_ of the original read-only objects, then performing the 
; appropriate translations and transformations.
(defun TR:virtualunit-create-drawing-objects ( vu / listObjectsOrig listCopies bbOrig o oCopy bbRotated bbCenter )

  (setq listObjectsOrig (TR:virtualunit-get-objects vu))
  (setq bbOrig (TR:objectlist-get-boundingbox listObjectsOrig))
  (setq bbCenter (TR:boundingbox-get-center bbOrig))
  (setq transMatrix (TR:virtualunit-get-tmatrix vu))
  (setq listCopies 
    (mapcar
      '(lambda (o)
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
    (mapcar '+ (TR:virtualunit-get-insertion-point vu)
               offset)
  )
  (TR:objectlist-offset listCopies offset)
)

(princ)

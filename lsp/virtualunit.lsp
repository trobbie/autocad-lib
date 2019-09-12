; Virtual unit = a description of a unit, whose actual drawing objects are not created 
; until the proper function is called.  The referenced objects in the description can be
; anywhere in the document, and remain untouched (read-only) by these functions.
;
; Format:
;   (("Objects" . (list of <vla-object>))
;    ("RotationDegrees" . <real number, representing degrees of rotation>)
;    ("InsertionPoint" . <list of 3 real numbers, representing insertion point>)
;   )
;
; This is intended to reference a list of existing vla-objects, and use it for
; calculations without having to create other drawing objects in the document.
; 
; There is an implied origin translation at the beginning where the entire unit
; is seen as moving to the WCS origin.  This always involves an exact translation
; from the _center_ of the unit's bounding box to (0,0,0).
;
; The RotationDegrees represents taking the read-only unit objects from the
; implied origin (center of bounding box) and performing rotation.
;
; The insertion 3d point is where the entire unit will be placed so that the bottom-left
; point of the rotated unit's bounding box is put there.
; The bottom-left corner provides a consistant and easily-visualized reference point
; when relating to other virtual units.
;
; When creating a drawing object. we will always create a _copy_ of  the original
; read-only objects, then performing the appropriate translations and transformations.

(defun TR:virtualunit-get-property ( vu propertyName )
  (cdr (assoc propertyName vu))
)

(defun TR:virtualunit-set-property ( vu propertyName propertyValue)
  (subst (cons propertyName propertyValue) (assoc propertyName vu) vu)
)

(defun TR:virtualunit-create-virtual ( listObjects degreesRotation ptInsert)
  (list 
    (cons "Objects" listObjects)
    (cons "RotationDegrees" degreesRotation))
    (cons "InsertionPoint" (TR:point-to-3d-point ptInsert))
  )
)

; Returns a new virtual unit that mirrors the same properties as the given one, but
; replaces the transformation matrix to rotate with given degrees (this is absolute,
; not an additional rotation)
(defun TR:virtualunit-copy-with-absolute-rotation ( vu degrees)
  (TR:virtualunit-create-virtual 
    (TR:virtualunit-get-objects vu)
    degrees
    (TR:virtualunit-get-property vu "InsertionPoint")
  )
)

(defun TR:virtualunit-get-objects( vu )
  (TR:virtualunit-get-property vu "Objects")
)

(defun TR:virtualunit-get-tmatrix( vu )
  (TR:tMatrix-rotate (TR:virtualunit-get-property vu (TR:degrees->radians "RotationDegrees")))
)

(defun TR:virtualunit-get-tmatrix-as-list( vu )
  (TR:matrix-rotate (TR:virtualunit-get-property vu (TR:degrees->radians "RotationDegrees")))
)

(defun TR:virtualunit-replace-insertion-point ( vu ptInsert )
  (TR:virtualunit-set-property vu "InsertionPoint" (TR:point-to-3d-point ptInsert))
)

; create a _copy_ of the original read-only objects, then performing the 
; appropriate translations and transformations.
(defun TR:virtualunit-create-drawing-objects ( vu / listObjectsOrig bbOrig o oCopy )
  (setq listObjectsOrig (TR:virtualunit-get-objects vu))
  (setq bbOrig (TR:objectlist-get-boundingbox listObjectsOrig))
  (setq bbCenter (TR:boundingbox-get-center bbOrig))
  (setq transMatrix (TR:virtualunit-get-tmatrix vu))

  (setq listCopies 
    (TR:objectlist-map listObjectsOrig
      '(lambda (o)
        ; create copy
        (setq oCopy (vla-Copy o))
        ; move copy to origin
        (vla-Move (vla-Copy o) 
          (vlax-3d-point bbCenter)
          (vlax-3d-point 0 0 0)
        )
		; transform
		(vla-TransformBy o transMatrix)
        oCopy
      )
    )
  )
  ; move to insertion point
  (setq bbRotated (TR:objectlist-get-boundingbox listCopies))
  (TR:objectlist-map listCopies
    '(lambda (o)
      (vla-Move o 
          (vlax-3d-point (TR:boundingbox-get-bottomleft bbRotated))
          (vlax-3d-point (TR:virtualunit-get-insertion-point vu))
      )
    )
  )
  listCopies
)

(princ)
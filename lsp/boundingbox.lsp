
 ; determine max precision needed for objects
 (setq *TR:DECIMAL-PRECISION* 6)

;;;--------------------------------------------------------------;
;;; Bounding Box 
;;;--------------------------------------------------------------;
;; List of two points.
;; If source objects were 2d, then the two points are the 
;;   bottom-left and top-right 2d points of the bounding box
;;   surround the object.
;; If source objects were 3d, then the two points are the 
;;   minimum-extent 3d point and maximum-extent 3d point of the bounding
;;   box surrounding the objects.
;;;--------------------------------------------------------------;

(defun TR:boundingbox-get-bottomleft ( bb )
  (if bb
    (car bb)
    nil
  )
)
(defun TR:boundingbox-get-bottomright ( bb )
  (if bb
    (list (TR:boundingbox-get-right bb) (TR:boundingbox-get-bottom bb))
    nil
  )
)
(defun TR:boundingbox-get-topright ( bb )
  (if bb
    (cadr bb)
    nil
  )
)
(defun TR:boundingbox-get-topleft ( bb )
  (if bb
    (list (TR:boundingbox-get-left bb) (TR:boundingbox-get-top bb))
    nil
  )
)
(defun TR:boundingbox-get-left ( bb )
  (if bb
    (car (TR:boundingbox-get-bottomleft bb))
    nil
  )
)
(defun TR:boundingbox-get-right ( bb )
  (if bb
    (car (TR:boundingbox-get-topright bb))
    nil
  )
)
(defun TR:boundingbox-get-bottom ( bb )
  (if bb
    (cadr (TR:boundingbox-get-bottomleft bb))
    nil
  )
)
(defun TR:boundingbox-get-top ( bb )
  (if bb
    (cadr (TR:boundingbox-get-topright bb))
    nil
  )
)
; can use with either 2d or 3d bounding boxes
(defun TR:boundingbox-get-min-extent ( bb )
  (if bb
    (car bb)
    nil
  )
)
; can use with either 2d or 3d bounding boxes
(defun TR:boundingbox-get-max-extent ( bb )
  (if bb
    (cadr bb)
    nil
  )
)
(defun TR:boundingbox-get-width ( bb )
  (if bb
    (car (TR:boundingbox-get-size bb))
    nil
  )
)
(defun TR:boundingbox-get-height ( bb )
  (if bb
    (cadr (TR:boundingbox-get-size bb))
    nil
  )
)
(defun TR:boundingbox-get-depth ( bb )
  (if (and bb (>= (length (car bb)) 3) (>= (length (cadr bb)) 3))
    (caddr (TR:boundingbox-get-size bb))
    nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:boundingbox-get-size                            ;
;;;--------------------------------------------------------------;
;; Return size of bounding box, e.g. width (x length) and height
;; y length.  Returns the depth (z length) if bounding box contains
;; z values.
;;
;; Returns: (width length [depth])
;;;--------------------------------------------------------------;
(defun TR:boundingbox-get-size ( bb )
  (if bb
    (mapcar
      '(lambda (minVal maxVal)
        (- maxVal minVal)
      )
      (car bb)
      (cadr bb)
    )
    nil ; else bb was nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:boundingbox-get-center                          ;
;;;--------------------------------------------------------------;
;; Return center/middle of bounding box.  A z component is included if
;; the passed bounding box contains a z component.
;;
;; Returns: (mid-x mid-y [mid-z])
;;;--------------------------------------------------------------;
(defun TR:boundingbox-get-center ( bb )
  (if bb
    (mapcar '(lambda (x) (/ x 2.))
      (mapcar '+ 
        (car bb)
        (cadr bb)
      )
    )
    nil ; else bb was nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: boundingbox-get-largest-dimension-length           ;
;;;--------------------------------------------------------------;
;; Return the largest size dimension value (e.g. width or height)
;; of the bounding box.  If the given bounding box contains z
;; values, the depth is also considered.
;;
;; Returns: real number, representing a length
;;;--------------------------------------------------------------;
(defun TR:boundingbox-get-largest-dimension-length ( bb )
  (if bb
    (apply 'max (TR:boundingbox-get-size bb))
    nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:object-get-boundingbox                          ;
;;;--------------------------------------------------------------;
;; Return 2d bounding box of vla object, in WCS coordinates, the
;; z values ignored (i.e. projected upon the xy plane).
;;
;; o - vla object
;; Returns: ((min-x min-y) (max-x max-y))
;;;--------------------------------------------------------------;
(defun TR:object-get-boundingbox( o / bb3d)
  (setq bb3d (TR:object-get-boundingbox3d o))
  (if bb3d
    (list (TR:point->2d-point (car bb3d)) (TR:point->2d-point (cadr bb3d)))
    nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:object-get-boundingbox3d                        ;
;;;--------------------------------------------------------------;
;; Return 3d bounding box of vla object, in WCS coordinates.
;;
;; o - vla object
;; Returns: ((min-x min-y min-z) (max-x max-y max-z))
;;;--------------------------------------------------------------;
(defun TR:object-get-boundingbox3d( o / o_minExt o_maxExt)
  (cond (o
    (vla-GetBoundingBox o 'o_minExt 'o_maxExt)
    (setq o_minExt (mapcar '(lambda(n) (LM:roundto n *TR:DECIMAL-PRECISION*)) (vlax-safearray->list o_minExt)))
    (setq o_maxExt (mapcar '(lambda(n) (LM:roundto n *TR:DECIMAL-PRECISION*)) (vlax-safearray->list o_maxExt)))
    (list o_minExt o_maxExt)
  )
  (T
    nil
  ))
)

;;;--------------------------------------------------------------;
;; Wrappers for object, using boundingbox
;;;--------------------------------------------------------------;
(defun TR:object-get-left ( o )
  (TR:boundingbox-get-left (TR:object-get-boundingbox o))
)
(defun TR:object-get-right ( o )
  (TR:boundingbox-get-right (TR:object-get-boundingbox o))
)
(defun TR:object-get-bottom ( o )
  (TR:boundingbox-get-bottom (TR:object-get-boundingbox o))
)
(defun TR:object-get-top ( o )
  (TR:boundingbox-get-top (TR:object-get-boundingbox o))
)
(defun TR:object-get-size ( o )
  (TR:boundingbox-get-size (TR:object-get-boundingbox o))
)
(defun TR:object-get-center ( o )
  (TR:boundingbox-get-center (TR:object-get-boundingbox o))
)

;;;--------------------------------------------------------------;
;;; Function: TR:get-boundingbox-of-two-points                   ;
;;;--------------------------------------------------------------;
;; Returns the 2d bounding box surrounding (and including) two points.
;;;--------------------------------------------------------------;
(defun TR:get-boundingbox-of-two-points( pt1 pt2 )
  (if (and pt1 pt2)
    (list
      (list (min (car pt1) (car pt2))
            (min (cadr pt1) (cadr pt2)))
      (list (max (car pt1) (car pt2))
            (max (cadr pt1) (cadr pt2)))
    )
    nil ;else a pt was nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:boundingbox-encloses-boundingbox                ;
;;;--------------------------------------------------------------;
;; Return non-nil if boundingbox encloses another. Border cases
;; are seen as included up to a fuzz factor.
;;
;; bb - bounding box to test if enclosing
;; bb2 - bounding box to test if being enclosed
;; Returns: non-nil if bb encloses bb2 
;;;--------------------------------------------------------------;
(setq *TR:BOUNDINGBOX-BOUNDARIES-FUZZ* 0.000001)
(defun TR:boundingbox-encloses-boundingbox (bb bb2 / bb_BL bb_TR bb2_BL bb2_TR)
  (setq bb_BL (car bb)
        bb_TR (cadr bb)
        bb2_BL (car bb2)
        bb2_TR (cadr bb2))
  (and (or (>= (car bb2_BL) (car bb_BL))
           (equal (car bb2_BL) (car bb_BL) *TR:BOUNDINGBOX-BOUNDARIES-FUZZ*))
       (or (<= (car bb2_TR) (car bb_TR))
           (equal (car bb2_TR) (car bb_TR) *TR:BOUNDINGBOX-BOUNDARIES-FUZZ*))
       (or (>= (cadr bb2_BL) (cadr bb_BL))
           (equal (cadr bb2_BL) (cadr bb_BL) *TR:BOUNDINGBOX-BOUNDARIES-FUZZ*))
       (or (<= (cadr bb2_TR) (cadr bb_TR))
           (equal (cadr bb2_TR) (cadr bb_TR) *TR:BOUNDINGBOX-BOUNDARIES-FUZZ*)))
)

(princ)

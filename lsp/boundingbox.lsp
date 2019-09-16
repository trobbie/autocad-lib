
; Bounding Box = list of bottom-left and top-right 2d points

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

(defun TR:boundingbox-get-width ( bb )
  (if bb
    (- (TR:boundingbox-get-right bb) (TR:boundingbox-get-left bb))
    nil
  )
)

(defun TR:boundingbox-get-height ( bb )
  (if bb
    (- (TR:boundingbox-get-top bb) (TR:boundingbox-get-bottom bb))
    nil
  )
)

; bb can be 2d or 3d
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

; bb can be 2d or 3d
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

; gets the largest size dimension value (e.g. width, height, or depth) of the bounding box
; if bb is a 2d bounding box, only the width and height would be returned
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
;; o - vla object
;; Returns: ((min-x min-y min-z) (max-x max-y max-z))
;;;--------------------------------------------------------------;
(defun TR:object-get-boundingbox3d( o / o_minExt o_maxExt)
  (cond (o
    (vla-GetBoundingBox o 'o_minExt 'o_maxExt)
    (setq o_minExt (vlax-safearray->list o_minExt))
    (setq o_maxExt (vlax-safearray->list o_maxExt))
    (list o_minExt o_maxExt)
  )
  (T
    nil
  ))
)

;;;--------------------------------------------------------------;
;;; Function: TR:get-boundingbox-of-two-points                   ;
;;;--------------------------------------------------------------;
;;; Description:                                                 ;
;;; Returns the 2d bounding box of two points.                      ;
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

(princ)

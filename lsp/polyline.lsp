;;;--------------------------------------------------------------;
;;; Function: TR:polyline-get-coordinates                        ;
;;;--------------------------------------------------------------;
;; Return a list of (x y) coordinates representing the vertices
;; of a given polyline object.
;;
;; o = vla-object of type polyline
;; includeClosingPoint = if non-nil, then append the first point
;;   to the end of the list if it is a closed polyline
;; Returns: list of (x y) coordinates representing the vertices
;;;--------------------------------------------------------------;
(defun TR:polyline-get-coordinates ( o includeClosingPoint / pts)
  (cond 
    ((= (vlax-get-property o 'ObjectName) "AcDbPolyline")
      (setq pts (mapcar 'cdr (acet-list-m-assoc 10 (entget (vlax-vla-object->ename o)))))
      ; if polyline is closed, append the first point to the end
      (if (and includeClosingPoint
               (= 1 (cdr (car (acet-list-m-assoc 70 (entget (vlax-vla-object->ename o)))))))
        (setq pts (append pts (list (car pts))))
      )
      pts
    )
    (T
      nil
    )
  )
)

;;;--------------------------------------------------------------;
;;; Function: EE:polyline-get-coordinates                        ;
;;;--------------------------------------------------------------;
;; Return whether polyline is drawn CW or CCW
;;;--------------------------------------------------------------;
;; Author: Evgeniy Elpanov (minor edits: TR)
;;;--------------------------------------------------------------;
(defun EE:polyline-is-drawn-clockwise (ename / lw lst maxp minp)
  (setq lw (vlax-ename->vla-object ename))
  (vla-GetBoundingBox lw 'minp 'maxp)
  (setq
    minp (vlax-safearray->list minp)
    maxp (vlax-safearray->list maxp)
    lst (mapcar
          (function
            (lambda (x)
              (vlax-curve-getParamAtPoint
                lw
                (vlax-curve-getClosestPointTo lw x)
              );_ vlax-curve-getParamAtPoint
            ) ;_ lambda
          ) ;_ function
          (list minp
                (list (car minp) (cadr maxp))
                maxp
                (list (car maxp) (cadr minp))
          ) ;_ list
        ) ;_ mapcar
  )
  (if 
    (or
      (<= (car lst) (cadr lst) (caddr lst) (cadddr lst))
      (<= (cadr lst) (caddr lst) (cadddr lst) (car lst))
      (<= (caddr lst) (cadddr lst) (car lst) (cadr lst))
      (<= (cadddr lst) (car lst) (cadr lst) (caddr lst))
      ) ;_ or
    T
    nil
  )
)

(princ)
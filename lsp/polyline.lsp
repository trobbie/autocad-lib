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


(princ)
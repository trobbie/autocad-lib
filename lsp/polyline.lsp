;;;--------------------------------------------------------------;
;;; Function: TR:polyline-get-coordinates                        ;
;;;--------------------------------------------------------------;
;; Return a list of (x y) coordinates representing the vertices
;; of a given polyline object.
;;
;; o = vla-object of type polyline
;; Returns: list of (x y) coordinates representing the vertices
;;;--------------------------------------------------------------;
(defun TR:polyline-get-coordinates ( o )
  (cond ((= (vlax-get-property o 'ObjectName) "AcDbPolyline" )
	(setq pts (mapcar 'cdr (acet-list-m-assoc 10 (entget (vlax-vla-object->ename o)))))
  )
  (T
    nil
  ))
)


(princ)
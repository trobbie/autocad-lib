

;;;--------------------------------------------------------------;
;;; Function: TR:degrees->radians                                ;
;;;--------------------------------------------------------------;
;; Convert degrees to radians
;; degrees - degree value, as number
;; Returns: calculated radians, as real number; if degrees is not
;;   a number, returns nil.
;;;--------------------------------------------------------------;
(defun TR:degrees->radians( degrees )
  (if (numberp degrees)
    (* pi (/ degrees 180.0))
    nil
  )
)
;;;--------------------------------------------------------------;
;;; Function: TR:radians->degrees                                ;
;;;--------------------------------------------------------------;
;; Convert radians to degrees
;; radians - radians value, as number
;; Returns: calculated degrees, as real number; if radians is not
;;   a number, returns nil.
;;;--------------------------------------------------------------;
(defun TR:radians->degrees( radians )
  (if (numberp radians)
    (*  180.0 (/ radians pi))
    nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:angle-exclusively-between-start-end-angles      ;
;;;--------------------------------------------------------------;
;; Returns whether thisAngle is found between the startAngle and
;; endAngle values on the unit circle.
;; Angles are in radians
;;;--------------------------------------------------------------;
(defun TR:angle-exclusively-between-start-end-angles ( thisAngle startAngle endAngle / thisAngleCoterminal)
  (setq startAngle (TR:angle-get-normalized startAngle))
  (setq endAngle (TR:angle-get-normalized endAngle))
  ; ensure endAngle has a greater value than startAngle
  (if (> startAngle endAngle) 
    (setq endAngle (TR:angle-get-next-coterminal endAngle))
  )
  (setq thisAngleCoterminal (TR:angle-get-next-coterminal thisAngle))
  (or
    (and (> thisAngle startAngle) (< thisAngle endAngle))
    (and (> thisAngleCoterminal startAngle) (< thisAngleCoterminal endAngle))
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:angle-get-next-coterminal                       ;
;;;--------------------------------------------------------------;
;; Find next coterminal angle on the unit circle.  The next angle
;; is determined by adding a positve full circle to the given angle.
;; Angles are in radians 
;;;--------------------------------------------------------------;
(defun TR:angle-get-next-coterminal ( a )
  (+ a (* 2 pi))
)

;;;--------------------------------------------------------------;
;;; Function: TR:angle-get-normalized                            ;
;;;--------------------------------------------------------------;
;; Return the normalized value of the given angle.  The normalized
;; angle is the one between 0 (inclusive) and 2*pi (exclusive).
;; Angles are in radians
;;;--------------------------------------------------------------;
(defun TR:angle-get-normalized ( a / temp )
  (if (< (setq temp (rem a (* 2 pi))) 0)
    (+ temp (* 2 pi))
    temp
  )
)

(princ)


; angles are in radians
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

(defun TR:angle-get-next-coterminal ( a )
  (+ a (* 2 pi))
)

(defun TR:angle-get-normalized ( a / temp )
  (setq temp (rem a (* 2 pi)))
  (if (< temp 0)
    (+ temp (* 2 pi))
    ; else
    temp
  )
)

(princ)

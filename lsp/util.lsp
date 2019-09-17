(vl-load-com)

;;;--------------------------------------------------------------;
;;; Function: TR:ceiling                                         ;
;;;--------------------------------------------------------------;
;; Return the ceiling (as integer) of the given real number
;; x - any real number
;; Returns: <ceiling> as integer; if x is not a number, return nil
;;;--------------------------------------------------------------;
;; Author: Public domain
;;;--------------------------------------------------------------;
(defun TR:ceiling (x / n)
  (if (numberp x)
    (if (or (= (setq n (fix x)) x) (< x 0))
      n
      (1+ n)
    )
	nil ; if x is not a number
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:floor                                           ;
;;;--------------------------------------------------------------;
;; Return the floor of the given real number
;; x - any real number
;; Returns: <floor> as integer; if x is not a number, return nil
;;;--------------------------------------------------------------;
;; Author: Public domain
;;;--------------------------------------------------------------;
(defun TR:floor (x / n)
  (if (numberp x)
    (if (or (= (setq n (fix x)) x) (< 0 x))
      n
      (1- n)
	)
	nil ; if x is not a number
  )
)

;;;--------------------------------------------------------------;
;;; Function: LM:Bulge->Arc                                      ;
;;;--------------------------------------------------------------;
;; Bulge to Arc
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns: (<center> <start angle> <end angle> <radius>)
;;;--------------------------------------------------------------;
;; Author: Lee Mac
;;;--------------------------------------------------------------;

(defun LM:Bulge->Arc ( p1 p2 b / a c r )
    (setq a (* 2 (atan b))
          r (/ (distance p1 p2) 2 (sin a))
          c (polar p1 (+ (- (/ pi 2) a) (angle p1 p2)) r)
    )
    (if (minusp b)
        (list c (angle c p2) (angle c p1) (abs r))
        (list c (angle c p1) (angle c p2) (abs r))
    )
)

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
;;; Function: TR:point->2d-point                                 ;
;;;--------------------------------------------------------------;
;; Return flattened version of any point, ignoring any
;; values of three dimensions or more.
;; Used in domains where coordinates are assumed 2d but use 
;; functions assuming 3d coordinates.
;;
;; pt - (x y ...), first two values must be numeric
;; Returns: (x y), all values of same type as given type;
;;   if pt is not a list of at least two numbers or would return 
;;   non-numeric values, then return nil
;;;--------------------------------------------------------------;
(defun TR:point->2d-point( pt )
  (if (and (= (type pt) 'LIST) (>= (length pt) 2))
    (cond
      ((and (vl-position (type (car pt)) (list 'REAL 'INT))
            (vl-position (type (cadr pt)) (list 'REAL 'INT)))
       (list(car pt) (cadr pt))
      )
      (T
        nil; return nil
      )
    );_end-of cond
    ; else
    nil; return nil
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:2d-point->3d-point                              ;
;;;--------------------------------------------------------------;
;; Convert pt2d to be a list of three numbers instead of two,
;; with the last value of 0.  
;; Used in domains where coordinates are assumed 2d but use 
;; functions assuming 3d coordinates.
;;
;; pt2d - (x y), all values are numeric
;; Returns: (x y, 0.0), all values of same type as given respective
;;   type; if pt2d is not a list of two numbers, return nil
;;;--------------------------------------------------------------;
(defun TR:2d-point->3d-point( pt2d )
  (if (and (= (type pt2d) 'LIST)
           (= 2 (length pt2d))
           (vl-position (type (car pt2d)) (list 'REAL 'INT))
           (vl-position (type (cadr pt2d)) (list 'REAL 'INT)))
    (list (car pt2d) (cadr pt2d) (float 0))
    nil; return nil
  )
)


;;;--------------------------------------------------------------;
;;; Function: TR:string-pad-right                                ;
;;;--------------------------------------------------------------;
;; Pad the text with the characters supplied as strPad up to a 
;; total length of totalLength.
;; strText - string to pad
;; strPad  - string to use to pad
;; minLength - minimum length of resulting string
;; Returns: strText if strText is at least minLength in length,
;;   else strText with strPad appended until at least minLength.
;;   If strPad is empty, or minLength is not a positive number,
;;   return nil.
;;;--------------------------------------------------------------;
(defun TR:string-pad-right ( strText strPad minLength )
  (cond 
    ((or (not strPad)
         (= (strlen strPad) 0)
         (not (numberp minLength))
         (< minLength 0))
      nil ; will never reach minLength, so unacceptable
    )
    (T
      (if (>= (strlen strText) minLength)
        strText
        (TR:string-pad-right (strcat strText strPad) strPad minLength)
      )
    )
  )
)

;;--------------=={ List Symmetric Difference }==-------------;;
;;                                                            ;;
;;  Returns items appearing exclusively in each list and not  ;;
;;  in their intersection.                                    ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  l1,l2 - lists for which to return symmetric difference    ;;
;;------------------------------------------------------------;;
;;  Returns:  List of items in the set (l1\l2)U(l2\l1)        ;;
;;------------------------------------------------------------;;
(defun LM:ListSymDifference ( l1 l2 )
  (append
    (vl-remove-if '(lambda ( x ) (member x l2)) l1)
    (vl-remove-if '(lambda ( x ) (member x l1)) l2)
  )
)

;;---------------------=={ Permutations }==-------------------;;
;;                                                            ;;
;;  Returns a list of all permutations of elements in a list  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  l - list to process                                       ;;
;;------------------------------------------------------------;;
;;  Returns: List of all permutations of elements in the list ;;
;;------------------------------------------------------------;;
(defun LM:Permutations ( l )
  (if (cdr l)
    (
      (lambda ( f )
        (f
          (apply 'append
            (mapcar
              (function
                (lambda ( a )
                  (mapcar (function (lambda ( b ) (cons a b)))
                    (LM:Permutations
                      (   (lambda ( f ) (f a l))
                        (lambda ( a l )
                          (if l
                            (if (equal a (car l))
                                (cdr l)
                              (cons (car l) (f a (cdr l)))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              l
            )
          )
        )
      )
      (lambda ( l ) (if l (cons (car l) (f (vl-remove (car l) (cdr l))))))
    )
    (list l)
  )
)

;;;--------------------------------------------------------------;
(princ)

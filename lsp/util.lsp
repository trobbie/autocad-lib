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
;;; Function: TR:point->3d-point                                 ;
;;;--------------------------------------------------------------;
;; Convert pt to be a list of three numbers.  If a 2d point, the
;; z coordinate is assigned 0.  If the list is at least three in
;; length, return the first three numbers as is.
;; Used in domains where working with either 2d or 3d drawings.
;;
;; pt - (x y [z...]), all values are numeric
;; Returns: (x y z), all values of same type as given respective
;;   type; if input is 2d, then z is assigned 0.  If pt is not a 
;;   list of at least two numbers, return nil.
;;;--------------------------------------------------------------;
(defun TR:point->3d-point( pt )
  (cond
    ((and (= (type pt) 'LIST)
          (= 2 (length pt))
          (vl-position (type (car pt)) (list 'REAL 'INT))
          (vl-position (type (cadr pt)) (list 'REAL 'INT)))
      (list (car pt) (cadr pt) (float 0))
    )
    ((and (= (type pt) 'LIST)
          (>= (length pt) 3)
          (vl-position (type (car pt)) (list 'REAL 'INT))
          (vl-position (type (cadr pt)) (list 'REAL 'INT))
          (vl-position (type (caddr pt)) (list 'REAL 'INT)))
      (list (car pt) (cadr pt) (caddr pt))
    )
    (T
      nil; return nil
    )
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
;; Returns: (x y 0.0), all values of same type as given respective
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

;;;--------------------------------------------------------------;
;;; Function: TR:list-up-to-member                               ;
;;;--------------------------------------------------------------;
;; Return elements of given list from first element up to and
;; including the given element.  If given element not found, or
;; list is empty, return nil.
;;
;; l = list
;; e = element in list
;;;--------------------------------------------------------------;
(defun TR:list-up-to-member ( l e )
  (reverse (member e (reverse l)))
)

;;------------=={ SelectionSet -> VLA Objects }==-------------;;
;;                                                            ;;
;;  Converts a SelectionSet to a list of VLA Objects          ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  ss - Valid SelectionSet (Pickset)                         ;;
;;------------------------------------------------------------;;
;;  Returns:  List of VLA Objects, else nil                   ;;
;;------------------------------------------------------------;;

(defun LM:ss->vla ( ss / i l )
    (if ss
        (repeat (setq i (sslength ss))
            (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l))
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
;;; Function: JB:min-dist                                        ;
;;;--------------------------------------------------------------;
;; Author: Joe Burke (TR: minor modifications)
;;;--------------------------------------------------------------;
;; Find the minimum offset between between two entities.
;; e1 - name of entity 1
;; e2 - name of entity 2
;; showMinLine - whether to show a visual line where the smallest
;;   distance was found
;; Returns: real number representing minimum distance betwen two entities
;;;--------------------------------------------------------------;
(defun JB:min-dist (e1 e2 showMinLine / step e1 e2 p1 p2 len idx dis dlst curveOK getent hasb e3)
  ;;  CAB test to see if vlax-curve can be used on an object
  (defun curveOK (ent)                  ; returns nil if not allowed
    (not (vl-catch-all-error-p
      (vl-catch-all-apply 'vlax-curve-getendparam (list ent)))
    )
  )
  ;;  get the min distance between 2 objects nearest to pt
  ;;  pt should be a point on obj1
  (defun 2ObjDist (e1 e2 p1 / p2 d fuzz)
    (setq fuzz 1E-8)
    (while (not
         (minusp
	       (- (distance p1 (setq p2 (vlax-curve-GetClosestPointTo e2 p1)))
		  (setq d (distance p2 (setq p1 (vlax-curve-GetClosestPointTo e1 p2))))
		  fuzz
	       )
	     )
	   )
    )
    (list d p1 p2)
  )
  (defun NoBulgeDist (e1 e2 / p1 p2 i d)
    (setq i (vlax-curve-getEndParam e1))
    (while (and (>= i 0))
      (setq p2	 (vlax-curve-getClosestPointTo
		   e2
		   (setq p1 (vlax-curve-getPointAtParam e1 i))
		 )
	    hasb (or hasb (not (equal (vlax-curve-getSecondDeriv e1 i) '(0 0 0))))
	    i	 (1- i)
	    d	 (distance p1 p2)
	    dlst (if (or (< d (car dlst)) (null dlst))
		   (list d p1 p2)
		   dlst
		 )
      )
    )
  )
  ;;=========================================================
  ;(setq e1 (getent "\nFirst entity: "))
  ;(setq e2 (getent "\nSecond entity: "))
  (if (vlax-invoke
	      (vlax-ename->vla-object e1)
	      'IntersectWith
	      (vlax-ename->vla-object e2)
	      acExtendNone
      )
    (setq dis 0.0)
    (progn (NoBulgeDist e2 e1)
	   (setq dlst (cons (car dlst) (reverse (cdr dlst))))
	   (NoBulgeDist e1 e2)
	   (if hasb
	     (progn (if	(> (vlax-curve-getDistAtParam e1 (vlax-curve-getEndParam e1))
			   (vlax-curve-getDistAtParam e2 (vlax-curve-getEndParam e2))
			)
		      (setq e3	 e1
			    e1	 e2
			    e2	 e3
			    dlst (cons (car dlst) (reverse (cdr dlst)))
		      )
		    )
		    ;| find the min distance within the steps. The accuracy is limited by the
	   number of steps, then 2ObjDist is used to increase the accuracy|;
		    (setq idx  0.0
			  len  (vlax-curve-getdistatparam e1 (vlax-curve-getendparam e1))
			  step (/ len 1000)	  ; 100 may need to increase for very large unit objects
		    )
		    (while (and (<= idx len) (setq p1 (vlax-curve-getpointatdist e1 idx)))
		      (setq p2	(vlax-curve-getclosestpointto e2 p1)
			    dis	(distance p1 p2)
			    idx	(+ idx step)
		      )
		      (if (< dis (car dlst))
			(setq dlst (list dis p1 p2))
		      )
		    )
	     )
	   )
	   (setq dlst (2ObjDist e1 e2 (cadr dlst)))
	   (setq dis (car dlst))
     (setq coordOffset (mapcar '- (cadr dlst) (caddr dlst)))
	   (if showMinLine 
       (grdraw (trans (cadr dlst) 0 1) (trans (caddr dlst) 0 1) 6 1)
     )
    )
  )
  dis
)
;;;--------------------------------------------------------------;
(princ)

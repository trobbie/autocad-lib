
;; Matrix Determinant (Upper Triangular Form)  -  ElpanovEvgeniy
;; Args: m - nxn matrix

(defun detm ( m / d )
    (cond
        (   (null m) 1)
        (   (and (zerop (caar m))
                 (setq d (car (vl-member-if-not (function (lambda ( a ) (zerop (car a)))) (cdr m))))
            )
            (detm (cons (mapcar '+ (car m) d) (cdr m)))
        )
        (   (zerop (caar m)) 0)
        (   (*  (caar m)
                (detm
                    (mapcar
                        (function
                            (lambda ( a / d ) (setq d (/ (car a) (float (caar m))))
                                (mapcar
                                    (function
                                        (lambda ( b c ) (- b (* c d)))
                                    )
                                    (cdr a) (cdar m)
                                )
                            )
                        )
                        (cdr m)
                    )
                )
            )
        )
    )
)

;; Matrix Determinant (Laplace Formula)  -  Lee Mac
;; Args: m - nxn matrix
 
(defun detm ( m / i j )
    (setq i -1 j 0)
    (cond
        (   (null (cdr  m)) (caar m))
        (   (null (cddr m)) (- (* (caar m) (cadadr m)) (* (cadar m) (caadr m))))
        (   (apply '+
                (mapcar
                   '(lambda ( c ) (setq j (1+ j))
                        (* c (setq i (- i))
                            (detm
                                (mapcar
                                   '(lambda ( x / k )
                                        (setq k 0)
                                        (vl-remove-if '(lambda ( y ) (= j (setq k (1+ k)))) x)
                                    )
                                    (cdr m)
                                )
                            )
                        )
                    )
                    (car m)
                )
            )
        )
    )
)

;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

(defun invm ( m / c f p r )
    
    (defun f ( p m )
        (mapcar '(lambda ( x ) (mapcar '(lambda ( a b ) (- a (* (car x) b))) (cdr x) p)) m)
    )
    (setq  m (mapcar 'append m (imat (length m))))
    (while m
        (setq c (mapcar '(lambda ( x ) (abs (car x))) m))
        (repeat (vl-position (apply 'max c) c)
            (setq m (append (cdr m) (list (car m))))
        )
        (if (equal 0.0 (caar m) 1e-14)
            (setq m nil
                  r nil
            )
            (setq p (mapcar '(lambda ( x ) (/ (float x) (caar m))) (cdar m))
                  m (f p (cdr m))
                  r (cons p (f p r))
            )
        )
    )
    (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun imat ( n / i j l m )
    (repeat (setq i n)
        (repeat (setq j n)
            (setq l (cons (if (= i j) 1.0 0.0) l)
                  j (1- j)
            )
        )
        (setq m (cons l m)
              l nil
              i (1- i)
        )
    )
    m
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix Trace  -  Lee Mac
;; Args: m - nxn matrix

(defun trc ( m )
    (if m (+ (caar m) (trc (mapcar 'cdr (cdr m)))) 0)
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix + Matrix  -  Lee Mac
;; Args: m,n - nxn matrices

(defun m+m ( m n )
    (mapcar '(lambda ( r s ) (mapcar '+ r s)) m n)
)

;; Matrix x Scalar  -  Lee Mac
;; Args: m - nxn matrix, n - real scalar

(defun mxs ( m s )
    (mapcar '(lambda ( r ) (mapcar '(lambda ( n ) (* n s)) r)) m)
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
    (mapcar '(lambda ( n ) (* n s)) v)
)

;; Vector Dot Product  -  Lee Mac
;; Args: u,v - vectors in R^n

(defun vxv ( u v )
    (apply '+ (mapcar '* u v))
)

;; Vector Cross Product  -  Lee Mac
;; Args: u,v - vectors in R^3

(defun v^v ( u v )
    (list
        (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
        (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
        (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
    )
)

;; Unit Vector  -  Lee Mac
;; Args: v - vector in R^2 or R^3

(defun vx1 ( v )
    (   (lambda ( n ) (if (equal 0.0 n 1e-10) nil (mapcar '/ v (list n n n))))
        (distance '(0.0 0.0 0.0) v)
    )
)

;; Vector Norm (R^n)  -  Lee Mac
;; Args: v - vector in R^n

(defun |v| ( v )
    (sqrt (apply '+ (mapcar '* v v)))
)

;; Unit Vector (R^n)  -  Lee Mac
;; Args: v - vector in R^n

(defun unit ( v )
    ((lambda ( n ) (if (equal 0.0 n 1e-10) nil (vxs v (/ 1.0 n)))) (|v| v))
)

;;------------------=={ Scale by Matrix }==-------------------;;
;;                                                            ;;
;;  Scales a VLA-Object or Point List using a                 ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1     - Base Point for Scaling Transformation            ;;
;;  scale  - Scale Factor by which to scale object            ;;
;;------------------------------------------------------------;;

(defun LM:ScaleByMatrix ( target p1 scale / m )

  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list scale 0. 0.)
        (list 0. scale 0.)
        (list 0. 0. scale)
      )
    )
    (mapcar '- p1 (mxv m p1))
  )
)

;;----------------=={ Translate by Matrix }==-----------------;;
;;                                                            ;;
;;  Translates a VLA-Object or Point List using a             ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1, p2 - Points representing vector by which to translate ;;
;;------------------------------------------------------------;;

(defun LM:TranslateByMatrix ( target p1 p2 )

  (LM:ApplyMatrixTransformation target
    (list
      (list 1. 0. 0.)
      (list 0. 1. 0.)
      (list 0. 0. 1.)
    )
    (mapcar '- p2 p1)
  )
)

;;------------------=={ Rotate by Matrix }==------------------;;
;;                                                            ;;
;;  Rotates a VLA-Object or Point List using a                ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1     - Base Point for Rotation Transformation           ;;
;;  ang    - Angle through which to rotate object             ;;
;;------------------------------------------------------------;;

(defun LM:RotateByMatrix ( target p1 ang )
  
  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list (cos ang) (- (sin ang)) 0.)
        (list (sin ang)    (cos ang)  0.)
        (list    0.           0.      1.)
      )
    )
    (mapcar '- p1 (mxv m p1))
  )
)

;;-----------------=={ Reflect by Matrix }==------------------;;
;;                                                            ;;
;;  Reflects a VLA-Object or Point List using a               ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1, p2 - Points representing vector in which to reflect   ;;
;;------------------------------------------------------------;;

(defun LM:ReflectByMatrix ( target p1 p2 )
  (
    (lambda ( a / m )
      (LM:ApplyMatrixTransformation target
        (setq m
          (list
            (list (cos a)    (sin a)  0.)
            (list (sin a) (- (cos a)) 0.)
            (list    0.         0.    1.)
          )
        )
        (mapcar '- p1 (mxv m p1))
      )
    )
    (* 2. (angle p1 p2))
  )
)

;;-----------=={ Apply Matrix Transformation }==--------------;;
;;                                                            ;;
;;  Transforms a VLA-Object or Point List using a             ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to Transform            ;;
;;  matrix - 3x3 Matrix by which to Transform object          ;;
;;  vector - 3D translation vector                            ;;
;;------------------------------------------------------------;;

(defun LM:ApplyMatrixTransformation ( target matrix vector ) (vl-load-com)
  (cond
    ( (eq 'VLA-OBJECT (type target))
     
      (vla-TransformBy target
        (vlax-tMatrix
          (append (mapcar '(lambda ( x v ) (append x (list v))) matrix vector)
           '((0. 0. 0. 1.))
          )
        )
      )
    )
    ( (listp target)

      (mapcar
        (function
          (lambda ( point ) (mapcar '+ (mxv matrix point) vector))
        )
        target
      )
    )        
  )
)

(defun TR:matrix-rotate ( radians )
  (list
    (list (cos radians) (- (sin radians)) 0. 0.)
    (list (sin radians) (cos radians) 0. 0.)
    (list 0. 0. 1. 0.)
	(list 0. 0. 0. 1.)
  )
)

(defun TR:tMatrix-rotate ( radians )
  (vlax-tmatrix (TR:matrix-rotate radians))
)


(princ)
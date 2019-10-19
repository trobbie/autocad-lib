

;;;-------------------------------------------------------------------;
;;; Function: TR:ILP-get-maximum-objective-given-objective-constraint ;
;;;-------------------------------------------------------------------;
;; ILP - Integer Linear Programming (algorithm)
;; Find the whole number solutions that maximize the objective linear function expressed
;; by the given positive coefficients, with the maximum constraining value supplied.
;;
;; For positive listCoefficients (A1 A2 A3 ...), find whole numbers (x1 x2 x3 ...) that produce
;; the maximum result possible, given:
;;   A1x1 + A2x2 + A3x3 + ... = result <= objectiveConstraint
;;
;; When multiple solutions exist giving same optimized result, return the solution that gives more value
;; to earlier variables. E.e. x2 has more value than x3, so returns a solution with higher x2 (assuming
;; x1 was the same in all others) regardless of how large x3 may be in other solutions.
;;
;; listCoefficients - list of positive real numbers; coefficients of the variables in objective linear function
;; objectiveConstraint - real number; maximum constraining value of objective function
;; Returns: ((x1 x2 x3 ...) realSum) where the first element is a 
;;   list of whole number variables found to maximize the linear function,
;;   and the second element the maximized result of the objective linear function
;;;--------------------------------------------------------------;
;; Author: Trevor Robbie @2019
;;;--------------------------------------------------------------;
(defun TR:ILP-get-maximum-objective-given-objective-constraint ( listCoefficients objectiveConstraint / result )
  ; add integers iStart to iEnd to front of list l
  (defun _consecutive-integer-list (l iStart iEnd)
    (if (> iStart iEnd)
      l
      (_consecutive-integer-list (cons iEnd l) iStart (1- iEnd))
    )
  )

  ; finds maximum integer such that when mulitipled by multiplier is no more than constraint value
  (defun _maximum-integer-multiplied-and-under-constraint ( multiplier constraint )
    (TR:floor (/ constraint multiplier))
  )

  ; return leftover portions after subtracting from maxValue the portion 
  ; that the variable may remove (based on its coefficient)
  (defun _leftover-after-var-portion (c x maxValue)
    (- maxValue (* x c))
  )

  ; Find list of integer variables that minimizes the leftover once all listCoefficients have been traversed
  ; each coefficient in the list corresponds to a integer variable
  ; result is of form ((x1 x2 ...) leftover)
  (defun _minimize-leftover-by-removing-var-portions ( listCoefficients leftover listPriorVars / maxX )
    (cond
    ((not listCoefficients)
      (list listPriorVars leftover)
    )
    ((= (length listCoefficients) 1) ; i.e. last variable to test, so simply max out this value
      (setq maxX (_maximum-integer-multiplied-and-under-constraint (car listCoefficients) leftover))
      (list 
        (append listPriorVars (list maxX))
        (_leftover-after-var-portion (car listCoefficients) maxX leftover)
      )
    )
    (T ; else recurse
      ; after finding result of mapcar on all x values, find minimum leftover to return
      (car (vl-sort
        (mapcar 
          '(lambda (x)
            ; apply to next variable/coefficient
            (_minimize-leftover-by-removing-var-portions
              (cdr listCoefficients) 
              (_leftover-after-var-portion (car listCoefficients) x leftover)
              (append listPriorVars (list x))
            )
          )
          (_consecutive-integer-list nil 
            0 
            (_maximum-integer-multiplied-and-under-constraint (car listCoefficients) leftover)
          ) ; e.g. x = (0 1 2 3)
        )
        '(lambda (r1 r2 / i) ; sorting
          (cond 
            ((< (cadr r1) (cadr r2)) T) ; sort first by ascending leftover
            ((= (cadr r1) (cadr r2)) ; then for equal leftover, give value to larger earlier variables
              ; e.g. (3 1) is valued more than (2 3)
              ; e.g. (1 3 4) is valued more than (1 2 9)
              (setq i 0)
              (while (and (< i (length (car r1)))
                          (= (nth i (car r1)) (nth i (car r2))))
                (setq i (1+ i))
              )
              (> (nth i (car r1)) (nth i (car r2)))
            )
            (T nil)
          )
        )
      ))
    ))
  )

  (setq result
    (cond 
    ((or (not (listp listCoefficients))
          (not (numberp maxValue))
          (< objectiveConstraint 0)
          (vl-member-if 
            '(lambda (x)
              (or (not (numberp x))
                  (<= x 0)
              )
            )
            listCoefficients
          )
      )
      nil ; invalid args
    )
    (T ; valid args
      (apply
        '(lambda (result)
          (list (car result)
                (if (car result) (- objectiveConstraint (cadr result)) objectiveConstraint)
          )
        )
        (list (_minimize-leftover-by-removing-var-portions listCoefficients objectiveConstraint nil))
      )
    ))
  )
  result
)

(princ)


;;;--------------------------------------------------------------;
;;; Function: TR:string-pad-right                                ;
;;;--------------------------------------------------------------;
;; Pad the text with the characters supplied (strPad) up to a 
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
;;; Function: TR:string-pad-left                                 ;
;;;--------------------------------------------------------------;
;; Pad the beginning of text with the characters supplied (strPad) up to a 
;; total length of totalLength.
;; strText - string to pad
;; strPad  - string to use to pad
;; minLength - minimum length of resulting string
;; Returns: strText if strText is at least minLength in length,
;;   else strText with strPad prepended until at least minLength.
;;   If strPad is empty, or minLength is not a positive number,
;;   return nil.
;;;--------------------------------------------------------------;
(defun TR:string-pad-left ( strText strPad minLength )
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
        (TR:string-pad-left (strcat strPad strText) strPad minLength)
      )
    )
  )
)

(princ)

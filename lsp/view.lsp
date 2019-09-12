
;;;--------------------------------------------------------------;
;;; Function: TR:zoom90percent                                   ;
;;;--------------------------------------------------------------;
;;; Description:                                                 ;
;;; Show all drawings at 90% of extents                          ;
;;;                                                              ;
;;; Useful to use in a C: function that is named for keyboard    ;
;;; shortcut convenience.                                        ;
;;;--------------------------------------------------------------;
(defun TR:zoom90percent()
  (command "ZOOM" "extents")
  (command "ZOOM" ".9x")
  (princ)
)
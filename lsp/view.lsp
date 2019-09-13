
;;;--------------------------------------------------------------;
;;; Function: TR:zoom90percent                                   ;
;;;--------------------------------------------------------------;
;;; Description:                                                 ;
;;; Show all drawings at 90% of extents                          ;
;;;                                                              ;
;;; Useful to use in a C: function that is named for keyboard    ;
;;; shortcut convenience.                                        ;
;;;--------------------------------------------------------------;
(defun TR:zoom-90( )
  (cond
    ((setq ss (ssget "_I"))
      (terpri)(princ "TEST: ")
      (command "._ZOOM" "object" ss "")
    )
    (T ; if no objects selected, zoom to the extents
      (command ".ZOOM" "_NONE" "_e")
      (terpri)(princ "TESTB: ")
    )
  )
  (command ".ZOOM" "_NONE" ".9x")
  (princ)
)
(vl-load-com)

;; Create a group named grpName in the current drawing
(defun TR:groups-create( grpName / doc grpObject)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq grpObject (vla-Add (vla-get-Groups doc) grpName))
  grpObject
)
;; Delete a group named grpName in the current drawing
;; Group is removed from the Groups collection
(defun TR:groups-delete( grpName / doc grpObject)
  (TR:collection-ensure-deleted-item (vla-get-Groups doc) grpName)
)

(defun TR:group-add-object( grpObject itemObject / doc grpObject)
;TODO
  (princ)
)

(princ)

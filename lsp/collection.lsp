(vl-load-com)

;;; Collection = a VLA object of type VLA collection
;;; VLA collection is a super-class for many VLA classes,
;;; including selection sets, blocks, groups, documents, layers,
;;; etc.

;;;--------------------------------------------------------------;
;;; Function: TR:objectlist->collection                          ;
;;;--------------------------------------------------------------;
;; Converts an object list to a collection
;; listObjects - list of vla-objects
;; Returns: a vla collection where items are of type vlax-vbObject
;;;--------------------------------------------------------------;
(defun TR:objectlist->collection ( listObjects / objCollection count o)
  (setq objCollection (vlax-make-safearray vlax-vbObject (cons 0 (- (length listObjects) 1)))
        count 0)

  (foreach o listObjects
   (vlax-safearray-put-element objCollection count o)
    (setq count (1+ count))
  )
  objCollection ;return
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection-has-items                            ;
;;;--------------------------------------------------------------;
;; Return T if the collection is non-nil and contains items;
;; else nil.
;;;--------------------------------------------------------------;
(defun TR:collection-has-items ( collection )
  (and collection (> (vla-get-count collection) 0))
)

(defun TR:collection-get-item ( collection itemName )
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list collection itemName)))
    ; on error (item not found)
    nil
    ; on success (item found)
    (vla-item collection itemName)
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection-add-item                             ;
;;;--------------------------------------------------------------;
;; Return the item with given itemName from a collection,
;; replacing any existing item if specified, or adding it if not
;; found.
;; collection - collection (as defined above).  
;;   Exception: Unsupported collections include
;;   Blocks, UCSs, hyperlinks, and PlotConfigurations
;;   (these have different arguments for vla-add)
;; itemName - name of the item
;; replaceExisting - if non-nil, delete any existing item first
;; Returns: the item specified by itemName now ensured to be
;;   a member of the collection
;;;--------------------------------------------------------------;
(defun TR:collection-add-item ( collection itemName replaceExisting)
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list collection itemName)))
    ; on error (item not found)
    (vla-add collection itemName)
    ; on success (item found)
    (cond
      (replaceExisting
        (vla-delete (vla-item collection itemName))
        (vla-add collection itemName)
      )
      (T ; return existing item
        (vla-item collection itemName)
      )
    );_end-of cond
  );_end-of if
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection-add-item-and-fill                    ;
;;;--------------------------------------------------------------;
;; Return result from TR:collection-add (see comments)
;; Also fill the new item using the given func-fill function.
;;;--------------------------------------------------------------;
(defun TR:collection-add-and-fill ( collection itemName replaceExisting func-fill / itemObject)

  (setq itemObject (TR:collection-add-item collection itemName T)) ; add, replacing if needs be

  (apply func-fill (list itemObject))

  itemObject ;return
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection-ensure-deleted-item                  ;
;;;--------------------------------------------------------------;
;; Delete an item with given itemName from a collection.  The
;; object id deleted and also removed from the collection.
;; collection - collection (as defined above).  
;; itemName - name of the item
;; Returns: T if item was found to delete; else nil
;;;--------------------------------------------------------------;
(defun TR:collection-ensure-deleted-item( collection itemName )
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list collection itemName)))
    ; on error
    nil ; return nil to inform nothing was done
    ; on success
    (progn
      (vla-delete (vla-item collection itemName))
      T ; return T to inform item was deleted
    )
  )
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection-find-object                          ;
;;;--------------------------------------------------------------;
;; Return the object given if part of the collection given.  If
;; not found, return nil.
;; collection - collection (as defined above).  
;; objToFind - vla object to find
;; Returns: same object as given if found; else nil if not found
;;;--------------------------------------------------------------;
(defun TR:collection-find-object( collection objToFind / objFound o)
  (setq objFound nil)
  (vlax-map-collection collection
    '(lambda (o)
         (if (= o objToFind)
           (setq objFound o)
         )
     );_ end-of lambda
  )
  ; return
  objFound
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection->objectlist                          ;
;;;--------------------------------------------------------------;
;; Converts a collection of items into a list of vla-objects
;;;--------------------------------------------------------------;
(defun TR:collection->objectlist ( collection / l i )
  (if collection
    (vlax-for i collection
      (setq l (cons i l))
    )
  )
  l ;return
)

;;;--------------------------------------------------------------;
;;; Function: TR:collection-add-object                           ;
;;;--------------------------------------------------------------;
;; Return the collection after having added the given vla-object
;;;--------------------------------------------------------------;
(defun TR:collection-add-object( collection o )
  (vla-AddItems collection (TR:objectlist->safearray (list o)))
  collection; return
)

(princ)

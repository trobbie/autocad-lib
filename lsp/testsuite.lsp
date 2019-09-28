; TEST SUITE functions
; These functions are intended to be used to run tests for a whole library.  It is assumed
; that the test loader will reset *TR:testsuiteCounterSuccesses* and *TR:testsuiteCounterFailures* to 0, as these functions
; will increment these values to track the health of the entire library.

; Tests are intended to be run in a new document to not interfere with any current
; drawings.

; Tolerance (i.e. fuzz) is used for comparing real numbers.

(vl-load-com)

(setq *TR:TESTSUITE-FUZZ* 0.000001)
(setq *TR:TESTSUITE-TEST-DOC* nil)
(setq *TR:TESTSUITE-PRINT-SUCCESS* nil)

;;;--------------------------------------------------------------;
;;; Function: TR:testsuite-test-drawing                          ;
;;;--------------------------------------------------------------;
;; Perform tests using current document, deleting all objects
;; initially found, filling the document using func-create-drawing
;; and running the test func-test on the created objects
;;;--------------------------------------------------------------;
(defun TR:testsuite-test-drawing ( func-create-drawing func-test / listObjects modelspace priorCmdecho priorOsmode error-obj )
  (setq modelspace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  
  ; delete all objects in model space
  (setq listObjects (TR:collection->objectlist modelspace))
  (mapcar 'vla-Delete listObjects) 
 
  ; support "autocad" commands (command) inside func-create-drawing
  ;   CMDECHO: temporarily disable CMDECHO so as not to crowd the console output
  ;   OSMODE: ensure OSNAP is off
  (setq priorCmdecho (getvar "CMDECHO"))
  (setq priorOsmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  
  ; create all objects for the test
  (if (vl-catch-all-error-p (setq error-obj (vl-catch-all-apply func-create-drawing)))
    (progn
      (setq *TR:testsuiteCounterFailures* (1+ *TR:testsuiteCounterFailures*))
      (princ (strcat "ERROR: could not create test drawing: " (vl-princ-to-string error-obj)))
    )
    (progn
      ; perform test
      (setq listObjects (TR:collection->objectlist modelspace))
      (apply func-test (list listObjects))
    )
  )
  (setvar "CMDECHO" priorCmdecho)
  (setvar "OSMODE" priorOsmode)

  (mapcar 'vla-Delete listObjects) ; delete all objects created
  
  (princ)
)

;;;--------------------------------------------------------------;
;;; Function: TR:test-args-for-exactness                         ;
;;;--------------------------------------------------------------;
;; Tests if two arguments are exactly the same, value and type.
;; If types are ever LIST, then test value/type comparison of each
;; element.
;;;--------------------------------------------------------------;
(defun TR:test-args-for-exactness ( arg1 arg2 )
  (cond
    ((and (= (type arg1) 'LIST)
          (= (type arg2) 'LIST))
      (and ; return true if all the results in the list from mapcar return true
        (mapcar 
          'TR:test-args-for-exactness
          arg1
          arg2
        )
      )
    )
    (T ; arg's are not both lists
      ; allow fuzz since the point of exactness is to deal with floating point numbers
      ; but do make sure the types are the same
      (and (equal arg1 arg2 *TR:TESTSUITE-FUZZ*) 
          (= (type arg1) (type arg2)))
    )
  )
)

(defun TR:testsuite-test-for-exactness ( funcToTest listParams valueForComparison)
  (TR:testsuite-test-func-with-expression funcToTest listParams valueForComparison
    '(lambda( arg1 arg2 ) (TR:test-args-for-exactness arg1 arg2))
    (strcat "Result should be " (vl-princ-to-string valueForComparison))
  )
  (princ)
)

(defun TR:testsuite-test-for-equality ( funcToTest listParams valueForComparison )
  (TR:testsuite-test-func-with-expression funcToTest listParams valueForComparison
    '(lambda( arg1 arg2 )
      (equal arg1 arg2 *TR:TESTSUITE-FUZZ*)
    )
    (strcat "Result should be " (vl-princ-to-string valueForComparison))
  )
  (princ)
)

(defun TR:testsuite-test-for-inequality ( funcToTest listParams valueForComparison )
  (TR:testsuite-test-func-with-expression funcToTest listParams valueForComparison
    '(lambda( arg1 arg2 )
      (not (equal arg1 arg2 *TR:TESTSUITE-FUZZ*))
    )
    (strcat "Result should NOT be " (vl-princ-to-string valueForComparison))
  )
  (princ)
)

; Normally, you'd call TR:testsuite-test-value-* after a function is called whose results need to be stored
; independent of this test, or to test multiple conditions without needing to call the function once.

; tests if funcToTest returns valueForComparison exactly, value and type
(defun TR:testsuite-test-value-for-exactness ( testName valueToTest valueForComparison )
  (TR:testsuite-test-value-with-expression testName valueToTest valueForComparison
    '(lambda( arg1 arg2 ) (TR:test-args-for-exactness arg1 arg2))
    (strcat "Result should be " (vl-princ-to-string valueForComparison))
  )
  (princ)
)

(defun TR:testsuite-test-value-for-equality ( testName valueToTest valueForComparison )
  (TR:testsuite-test-value-with-expression testName valueToTest valueForComparison
    '(lambda( arg1 arg2 )
      (equal arg1 arg2 *TR:TESTSUITE-FUZZ*)
    )
    (strcat "Result should be " (vl-princ-to-string valueForComparison))
  )
  (princ)
)

(defun TR:testsuite-test-value-for-inequality ( testName valueToTest valueForComparison )
  (TR:testsuite-test-value-with-expression testName valueToTest valueForComparison
    '(lambda( arg1 arg2 )
      (not (equal arg1 arg2 *TR:TESTSUITE-FUZZ*))
    )
    (strcat "Result should NOT be " (vl-princ-to-string valueForComparison))
  )
  (princ)
)

; helper function
; funcSuccess => lambda expression that returns nil (indicating failure) or non-nil (indicating success)
(defun TR:testsuite-test-value-with-expression ( testName valueToTest valueForComparison funcSuccess failureMessage
  / listOldSymbols listSymbolsDiff )

  (cond
  ((apply funcSuccess (list valueToTest valueForComparison))
    (setq *TR:testsuiteCounterSuccesses* (1+ *TR:testsuiteCounterSuccesses*))
    (cond (*TR:TESTSUITE-PRINT-SUCCESS*
      (terpri)(princ (strcat "SUCCESS: " testName " => " (vl-princ-to-string valueToTest) ". " failureMessage))
    ))
  )
  (T
    (setq *TR:testsuiteCounterFailures* (1+ *TR:testsuiteCounterFailures*))
    (terpri)(princ (strcat "FAIL: " testName " => " (vl-princ-to-string valueToTest) ". " failureMessage))
  )
  )
  (princ)
)

; helper function
; funcSuccess => lambda expression that returns nil (indicating failure) or non-nil (indicating success)
(defun TR:testsuite-test-func-with-expression ( funcToTest listParams valueForComparison funcSuccess failureMessage 
  / prior-error-handler *error* result )

  (setq *TR:testsuite-prior-error-handler* *error*)
  (setq *error* TR:testsuite-error-handler)
  (setq result (apply funcToTest listParams))
  (setq *error* *TR:testsuite-prior-error-handler*)

  (TR:testsuite-test-value-with-expression 
    (strcat "Function " (vl-princ-to-string funcToTest) (vl-princ-to-string listParams))
    result
    valueForComparison
    funcSuccess
    failureMessage)
  (princ)
)

(defun TR:testsuite-error-handler ()
  (setq *error* *TR:testsuite-prior-error-handler*)
  (alert errmsg)
  (princ)
)

(defun TR:testsuite-create-test-dwg( / docs)
  (setq docs (vla-get-documents (vlax-get-acad-object)))
  (setq *TR:TESTSUITE-TEST-DOC* (TR:collection-add-item docs "acad.lsp" T))
)

(defun TR:testsuite-get-doc ()
  *TR:TESTSUITE-TEST-DOC*
)

(defun TR:testsuite-start-varcheck ()
  (setq *TR:listOldSymbols* (atoms-family 1))
  (terpri)(princ "Number of symobls (pre-test): ")(princ (length *TR:listOldSymbols*))
)

(defun TR:testsuite-end-varcheck ()
  (setq listSymbolsDiff (vl-remove-if
    ; don't include testsuite vars and test functions
    '(lambda ( x )
       (or (member x (list "TESTSUITENAME" "*TR:LISTOLDSYMBOLS*")) 
           (wcmatch x "*:TEST*")
       )
    )
    (LM:ListSymDifference *TR:listOldSymbols* (atoms-family 1))
  ))
  (cond ((> (length listSymbolsDiff) 1); ignore the local testsuiteName will 
    (terpri)(princ "Global Variables unintentionally introduced? -> ")(princ listSymbolsDiff)
  ))
  (princ)
)

; return T on success; else if nil, do not continue with tests
(defun TR:testsuite-initialize ( / listObjects)
  ; also initialize test suite variables
  (setq *TR:testsuiteCounterSuccesses* 0)
  (setq *TR:testsuiteCounterFailures* 0)

  (TR:testsuite-start-varcheck)

  ; When test suites are used, the intention is to perform tests in a different environment
  ; than any currently used.  Therefore, it is advised the testsuite is run in a separate
  ; document.  Since we cannot run autolisp code inside a different document than the
  ; one that calls this function (unless working in SDI mode), we must rely on outside help.

  ; As a work-around, we will check current document for any drawing objects, and disallow user
  ; to continue if any objects found. This will encourage creating a new document first.
  (setq listObjects (TR:collection->objectlist (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))))
  (cond
    ((> (length listObjects) 0)
      (alert "WARNING: Objects found in the current document.\n\nTo continue, please open new document.")
      nil
    )
    (T
      T ; signals that initialization was successful
    )
  )
  
)

(defun TR:testsuite-finalize ( testsuiteName / listSymbolsDiff)

  (terpri)(princ "*****************************")
  (terpri)(princ "Total successful tests: ")(princ *TR:testsuiteCounterSuccesses*)
  (terpri)(princ "Total failing tests: ")(princ *TR:testsuiteCounterFailures*)
  (TR:testsuite-end-varcheck)
  (terpri)(princ "*****************************")

  (cond
    ((> *TR:testsuiteCounterFailures* 0) 
      (alert (strcat "WARNING: " (if testsuiteName testsuiteName "Test") " has failed testing"))
      ;Note: leave the test DWG open in case tests we wish to examine
    )
  )

  (setq *TR:testsuiteCounterSuccesses* nil)
  (setq *TR:testsuiteCounterFailures* nil)
  (setq *TR:listOldSymbols* nil)

  (princ)
)

(princ)

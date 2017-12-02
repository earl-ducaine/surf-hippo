;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Compuation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#


;;; SYS Source file: models.lisp
					
;; The create model functions.

(in-package "SURF-HIPPO")

;; Element Type Parameter Lists are stored in each model's :PARAMETER-TYPE-LIBRARY slot. The *-TYPE-DEF macros are for adding entries to these lists.

;; (defmacro delete-all-cars (body a-list) `(loop for form in ,a-list unless (eql (car ,body) (car form)) collect form))

(defvar *model-hash-table* (make-hash-table :test #'equal)) ; A table of all models, corresponding to the entries in *MODEL-NAMES*. 

(defun models () (hash-table-list *model-hash-table*))

;; For adding the current filespec to a new entry in the parameter library.
(defun nconc-current-file-to-body (body) (if *load-truename* (nconc body (list (cons 'source-file (namestring *load-truename*))))  body))

(defmacro unquote-body-if-necessary (body)
  ;; This allows type-defs to NOT quote the definition list. Thus,
  ;;
  ;;  (foo-type-def
  ;;   (a-kind-of-foo
  ;;    (foo-property-1 . 123)
  ;;    (foo-property-2 . 456)
  ;;    etc...))
  ;;
  ;; is ok.
  (let ((body-var (gensym)))
    `(let ((,body-var (quote ,body)))
      (if (eq 'quote (CAR ,body-var))
	  (cadr ,body-var)
	  ,body-var))))

#|
(defmacro element-type-def (type-symbol body)
  `(let* ((model (get ,type-symbol `model))
	  (body (unquote-body-if-necessary ,body))
	  (body-w-current-file (nconc-current-file-to-body body))
	  (new-type (car body-w-current-file)))
    (setf (model-parameter-type-library model) (delete new-type (model-parameter-type-library model) :key 'car :test 'equal)) ; Remove any existing definition.
    (push body-w-current-file (model-parameter-type-library model))
    nil))

(defmacro element-type-def (type-symbol body &optional (unquote-body-if-necessary t))
  (if unquote-body-if-necessary
      `(let* ((model (get ,type-symbol `model))
	      (body (unquote-body-if-necessary ,body))
	      (body-w-current-file (nconc-current-file-to-body body))
	      (new-type (car body-w-current-file)))
	(setf (model-parameter-type-library model) (delete new-type (model-parameter-type-library model) :key 'car :test 'equal)) ; Remove any existing definition.
	(push body-w-current-file (model-parameter-type-library model))
	nil)
      `(let* ((model (get ,type-symbol `model))
	      (body ,body)
	      (body-w-current-file (nconc-current-file-to-body body))
	      (new-type (car body-w-current-file)))
	(setf (model-parameter-type-library model) (delete new-type (model-parameter-type-library model) :key 'car :test 'equal)) ; Remove any existing definition.
	(push body-w-current-file (model-parameter-type-library model))
	nil)))

;; (defmacro quote-only-symbol (val) `(if (and (symbolp ,val) (not (keywordp ,val))) `',,val ,val))

|#

(defmacro element-type-def (type-symbol body &optional allow-backquoted-constructions)
  (let ((body-let-form (if allow-backquoted-constructions
			   `(body ,body)
			   `(body (unquote-body-if-necessary ,body)))))
    `(let* ((model (get ,type-symbol `model))
	    ,body-let-form
	    (body-w-current-file (nconc-current-file-to-body body))
	    (new-type (car body-w-current-file)))
      (setf (model-parameter-type-library model) (delete new-type (model-parameter-type-library model) :key 'car :test 'equal))	; Remove any existing definition.
      (push body-w-current-file (model-parameter-type-library model))
      nil)))

#|
(defun dotted-list-w-list-of-lists-cdr-p (candidate)
  (and (listp (cdr candidate))
       (not (equal (list (cadr candidate)) (cdr candidate)))))

(defun get-type-def-params-value (key type-def-params &optional (test #'eq))
  ;; Return the datum associated with KEY in ASSOCIATION-LIST, where KEY is matching using the function TEST [default 'EQ].
  (let ((assoc-value (assoc key type-def-params :test test)))
    ;; assoc-value may be:
    ;;  (key . number)
    ;;  (key . function-name)
    ;;  (key . numeric-list)
    ;;  (key . value-list)
    ;;  (key . array)
    ;;  (key . string)
    ;;  (key . (funspec arg arg ...))
    ;;  (key . (lambda (arg arg) ...))
    ;;  (key . #'(lambda (arg arg) ...))
    (equal (list (car (cdr assoc-value))) (cdr assoc-value))

    (if (not (listp assoc-cdr-value))
	assoc-cdr-value)		; a-list entry was a dotted list, that is (key . non-list-value)
      (if list (assoc-cdr-value
	     )))))
|#


;; *************************************** ;; ***************************************

(defun generate-model-current-value-function (model-type data-type)
  ;; Create a symbol that should correspond to a defined function for accessing the current value of an instance of MODEL-TYPE of data type DATA-TYPE, by
  ;; concatenating MODEL-TYPE, a "-", and the DATA-TYPE. Thus, for example, the generated current value function for a segment voltage is SEGMENT-VOLTAGE.
  (read-from-string (format nil "~A-~a" model-type data-type)))

(defun generate-model-saved-data-function (model-type data-type)
  ;; Create a symbol that should correspond to a defined function for accessing the saved data of an instance of MODEL-TYPE of data type DATA-TYPE, by
  ;; concatenating MODEL-TYPE, a "-", the DATA-TYPE, and "-data". Thus, for example, the generated saved data function for a segment voltage is
  ;; SEGMENT-VOLTAGE-DATA.
  (read-from-string (format nil "~A-~a-data" model-type data-type)))

(defmacro DATA-TYPE-ACCESS-INFO (model-type symbols units-string &optional current-value-function saved-data-function)
  ;; SYMBOLS are one or more symbols indicating the synonyms for the data-type, e.g. for the soma and segment models, a single symbol such as VOLTAGE, or a
  ;; list of synonyms such as VOLTAGE-DERIVATIVE, DVDT, and NODE-VOLTAGE-DERIVATIVE. If a list of synonyms is provided, then the first one is used as a
  ;; reference REF-SYMBOL for generating various keys, etc. The reference function that accesses the model's current value corresponding to the data type
  ;; is given by the optional CURRENT-VALUE-FUNCTION argument, if supplied. If not supplied, a symbol is generated by calling
  ;; GENERATE-MODEL-CURRENT-VALUE-FUNCTION. Likewise, the reference function that accesses the model's saved data is given by the optional
  ;; SAVED-DATA-FUNCTION argument - if not supplied, then the assumed reference function is given by calling GENERATE-MODEL-SAVED-DATA-FUNCTION.  just
  ;; described.
  (let* ((symbols (coerce-to-list symbols))
	 (ref-symbol (car symbols)))
    `(list
      ',(atomize-list symbols)
      '(units . ,units-string)
      '(data-param-key . ,(read-from-string (format nil ":~a-data" ref-symbol)))
      '(sparse-data-param-key . ,(read-from-string (format nil ":sparse-~A-data" ref-symbol)))
      '(ordered-sparse-data-param-key . ,(read-from-string (format nil ":ordered-sparse-~A-data" ref-symbol)))
      '(current-value-function . ,(or current-value-function (generate-model-current-value-function model-type ref-symbol)))
      '(saved-data-function . ,(or saved-data-function (generate-model-saved-data-function model-type ref-symbol))))))
  
(defmacro model-output-wrapper (model &body body)
  `(let* ((model-arg ,model)
	  (model (when model-arg (or (gethash (model-child-structure-type model-arg) *model-hash-table*) model-arg))))
    ,@body))

(defun model-output-data-keys (model)
  (mapcar #'(lambda (data-type-and-access-info) (cdr (assoc 'data-param-key (cdr data-type-and-access-info)))) (model-data-types-and-access-info model)))

(defun model-output-data-types (model)
  (model-output-wrapper model (mapcar #'(lambda (data-type-and-access-info) (car data-type-and-access-info)) (model-data-types-and-access-info model))))

(defun model-output-default-data-type (model)
;; The first entry in DATA-TYPES-AND-ACCESS-INFO gives the default data type. If there are synonyms, take the first one.
  (let ((first-type (car (model-output-data-types model))))
    (if (consp first-type) (car first-type) first-type)))

(defun data-type-matches-with-type-info (data-type type-info)
  (if (consp (car type-info)) (member data-type (car type-info)) (equal data-type (car type-info))))

(defun model-output-data-type-info (model data-type)
  ;; (cdr (assoc data-type (model-data-types-and-access-info model)))
  (loop for type-info in (model-data-types-and-access-info model)
	when (data-type-matches-with-type-info data-type type-info)
	do (return (cdr type-info))))

(defun model-output-data-info (model data-type info-key &optional (new-value nil new-value-supplied))
  (let* ((data-type (or data-type (model-output-default-data-type model)))
	 (data-type-a-list (model-output-data-type-info model data-type))
	 (assoc-result (assoc info-key data-type-a-list)))
    (if assoc-result
      (cdr (if new-value-supplied (rplacd assoc-result new-value) assoc-result))
      (loop for type-info in (model-DATA-TYPES-AND-ACCESS-INFO model)
	    when (data-type-matches-with-type-info data-type type-info)
	    do (push (cons info-key new-value) (cdr type-info)) (return new-value)))))

(defun set-model-save-data-p (model &optional data-type) (model-output-data-info model data-type 'save-data-p t))
(defun clear-model-save-data-p (model &optional data-type) (model-output-data-info model data-type 'save-data-p))
(defun model-save-data-p (model &optional data-type) (model-output-data-info model data-type 'save-data-p))
(defun model-save-data-instances (model &optional data-type) (model-output-data-info model data-type 'save-data-instances))
(defun clear-model-save-data-instances (model &optional data-type) (model-output-data-info model data-type 'save-data-instances nil))
(defun set-model-save-data-instances (model instances-to-save-data &optional data-type)
  (model-output-data-info model data-type 'save-data-instances instances-to-save-data))

(defun add-to-model-save-data-instances (model new-instances-to-save-data &optional data-type)
  (model-output-data-info model data-type 'save-data-instances
			      (union (model-output-data-info model data-type 'save-data-instances) (coerce-to-list new-instances-to-save-data))))

(defun remove-from-model-save-data-instances (model save-data-instances-to-remove &optional data-type)
  (let ((original-save-data-instances (model-output-data-info model data-type 'save-data-instances)))
    (model-output-data-info model data-type 'save-data-instances
			    (loop for inst-to-remove in (coerce-to-list save-data-instances-to-remove)
				  do (setq original-save-data-instances (remove inst-to-remove original-save-data-instances))
				  finally (return original-save-data-instances)))))

(defun eliminate-element-from-model-save-data-instances (element)
  (let* ((element (element element))
	 (model (element-model element)))
    (loop for data-type in (model-output-data-types model)
	  do (remove-from-model-save-data-instances model element data-type))))

(defun enable-element-save-data (element &optional data-type model-type)
  "Enable saving of DATA-TYPE (as in ELEMENT-DATA) of elements in ELEMENT of MODEL-TYPE. If ELEMENT is an element type, then all
elements of that type are affected. For elements that can generate more than one type of simulation data, setting DATA-TYPE to
:ALL will enable saving of all data types (except for events). DATA-TYPE may also be a list of data types, with the deffault given
by the function DEFAULT-DATA-TYPE. For saving element data that will also be used for plotting, use ENABLE-ELEMENT-PLOT. "
  (loop for elt in (coerce-to-list (element element model-type)) do
	(let ((data-types (coerce-to-list (if (or (eq data-type 'all) (eq data-type :all)) (all-data-types elt)
					      (or data-type (default-data-type elt))))))
	  (loop for d-type in data-types do (add-to-model-save-data-instances (element-model elt) elt (atom-or-car d-type))))))

(defun disable-element-save-data (element &optional data-type (abort-disable-if-plotted t) model-type)
  "Disables saving of DATA-TYPE, in opposition to ENABLE-ELEMENT-SAVE-DATA. If ABORT-DISABLE-IF-PLOTTED is T [default], and if the
data type for this element is currently earmarked for plotting, the disabling of saving data is aborted."
  (loop for elt in (coerce-to-list (element element model-type)) do
	(let ((data-types (coerce-to-list (if (or (eq data-type 'all) (eq data-type :all))
					    (all-data-types elt)
					    (or data-type (default-data-type elt))))))
	  (loop for d-type in data-types
		unless (and abort-disable-if-plotted (element-plot-enabled-p elt (atom-or-car d-type) model-type))
		do (remove-from-model-save-data-instances (element-model elt) elt (atom-or-car d-type))))))

(defun transfer-plot-xx-structure-lists-to-model-save-data-instances ()
  (loop for plot-list-info in *plot-lists-info* do
	(let ((data-type (plot-list-info-structure-slot plot-list-info)))
	  (loop for model-type in (plot-list-info-types plot-list-info) do
		(let ((model (element-model model-type))
		      (plotted-model-insts (loop for elt in (symbol-value (plot-list-info-structures plot-list-info)) when (typep elt model-type) collect elt)))
		  (add-to-model-save-data-instances model plotted-model-insts data-type))))))

(defun model-output-data-key (model &optional data-type) (model-output-data-info model data-type 'data-param-key))
(defun model-output-data-units (model &optional data-type) (model-output-data-info model data-type 'units))
(defun model-output-sparse-data-key (model &optional data-type) (model-output-data-info model data-type 'sparse-data-param-key))
(defun model-output-ordered-sparse-data-key (model &optional data-type) (model-output-data-info model data-type 'ordered-sparse-data-param-key))
(defun model-output-current-value-function (model &optional data-type) (model-output-data-info model data-type 'current-value-function))
(defun model-output-saved-data-function (model &optional data-type) (model-output-data-info model data-type 'saved-data-function))	       

(defun create-model (name &key
			  (parent-structure-type (unless (search "-TYPE" (string name)) (read-from-string (format nil "~A-type" name))))
			  (child-structure-type (when (search "-TYPE" (string name)) (read-from-string (string-head (string name) (search "-TYPE" (string name))))))
			  output-data-structure-variables DATA-TYPES-AND-ACCESS-INFO 
			  (save-output-data-routine (read-from-string (format nil "save-~A-data" name)))
			  (edit-routine (read-from-string (format nil "edit-~A" name)))
			  (eval-routine (read-from-string (format nil "eval-~A" name)))
			  (print-routine (read-from-string (format nil "print-~A" name)))
			  (short-print-routine (read-from-string (format nil "print-~A-brief" name)))
			  (document-routine (read-from-string (format nil "document-~A" name)))
			  (create-routine (read-from-string (format nil "create-~A" name))))
  (let* ((name (if (stringp name) (read-from-string name) name))
	 (top-pointer-symbol (read-from-string (format nil "*~a*" name)))
	 (model (make-model :name name :child-structure-type child-structure-type :parent-structure-type parent-structure-type
			    :parameter-type-library '() :hash-table (make-hash-table :test #'equal)
			    :output-data-structure-variables output-data-structure-variables
			    :DATA-TYPES-AND-ACCESS-INFO DATA-TYPES-AND-ACCESS-INFO
			    :save-output-data-routine (when (fboundp save-output-data-routine) save-output-data-routine)
			    :edit-routine (when (fboundp edit-routine) edit-routine)
			    :eval-routine (when (fboundp eval-routine) eval-routine)
			    :print-routine (when (fboundp print-routine) print-routine)
			    :short-print-routine (when (fboundp short-print-routine) short-print-routine) 
			    :document-routine (when (fboundp document-routine) document-routine)
			    :create-routine (when (fboundp create-routine) create-routine)
			    :top-pointer-symbol top-pointer-symbol)))
    (define-whatevers-function name)
    ;; This screws up structures with slots that give the same name as defined in whatever, e.g. cell-type
;;    (define-whatever-function name)
    (define-node-whatevers-function name)
    (define-whatevers-names-function name)
    (define-model-hash-functions name)
    (define-type-def-macro name model)
    (define-QUOTEd-type-def-macro name model)
    (define-top-pointer-symbol name top-pointer-symbol)
    (setf (get name 'model) model
	  (gethash name *model-hash-table*) model)))

(defun update-model (model &key
			   (name (model-name model))
			   (save-output-data-routine (or (model-save-output-data-routine model) (read-from-string (format nil "save-~A-data" name))))
			   (edit-routine (or (model-edit-routine model) (read-from-string (format nil "edit-~A" name))))
			   (eval-routine (or (model-eval-routine model) (read-from-string (format nil "eval-~A" name))))
			   (print-routine (or (model-print-routine model) (read-from-string (format nil "print-~A" name))))
			   (short-print-routine (or (model-short-print-routine model) (read-from-string (format nil "print-~A-brief" name))))
			   (document-routine (or (model-document-routine model) (read-from-string (format nil "document-~A" name))))
			   (create-routine (or (model-create-routine model) (read-from-string (format nil "create-~A" name)))))
  (setf (model-save-output-data-routine model) (when (fboundp save-output-data-routine) save-output-data-routine))
  (setf (model-edit-routine model) (when (fboundp edit-routine) edit-routine))
  (setf (model-eval-routine model) (when (fboundp eval-routine) eval-routine))
  (setf (model-print-routine model) (when (fboundp print-routine) print-routine))
  (setf (model-short-print-routine model) (when (fboundp short-print-routine) short-print-routine))
  (setf (model-document-routine model) (when (fboundp document-routine) document-routine))
  (setf (model-create-routine model) (when (fboundp create-routine) create-routine)))

#|
(defun define-top-pointer-symbol (name top-pointer-symbol)
  (eval					; There must be a better (correct?) way than doing this EVAL....
   `(progn
      (defvar ,top-pointer-symbol nil ,(format nil "The last created ~(~A~)." name)))))
|#
(defun define-top-pointer-symbol (name top-pointer-symbol)
  (eval				; There must be a better (correct?) way than doing this EVAL....
   `(defvar ,top-pointer-symbol nil ,(format nil "The last created ~(~A~)." name))))

(defun generate-type-def-macro-name (name) (read-from-string (format nil "~A-def" name)))
(defun generate-quoted-type-def-macro-name (name) (read-from-string (format nil "~A-quoted-def" name)))

#|
(let* ((name 'fpp-type)
       (model 'asdf)
       (type-def-macro-name (generate-type-def-macro-name name))
       (type-def-macro-comment (format nil "Parameter wrapper for ~(~A~) library definitions." name)))
  (when t
    `(defmacro ,type-def-macro-name (body) ,type-def-macro-comment (element-type-def (quote ,name) `,body))))
(defun define-type-def-macro (name model)
  (let* ((type-def-macro-name (generate-type-def-macro-name name))
	 (type-def-macro-comment (format nil "Parameter wrapper for ~(~A~) library definitions." name)))
    (when (model-child-structure-type model)
      (defmacro type-def-macro-name (body) type-def-macro-comment `(element-type-def (quote ,(quote ,name)) ,body)))))
|#
(defun define-type-def-macro (name model)
  (let* ((type-def-macro-name (generate-type-def-macro-name name))
	 (type-def-macro-comment (format nil "Parameter wrapper for ~(~A~) library definitions." name)))
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(progn
	,(when (model-child-structure-type model)
	   `(defmacro ,type-def-macro-name (body) ,type-def-macro-comment `(element-type-def (quote ,(quote ,name)) ,body)))))))



(defun define-quoted-type-def-macro (name model)
  (let* ((type-def-macro-name (generate-quoted-type-def-macro-name name))
	 (type-def-macro-comment (format nil "Parameter wrapper for ~(~A~) library definitions that allows substitution in backquoted forms." name)))
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(progn
	,(when (model-child-structure-type model)
	   `(defmacro ,type-def-macro-name (body) ,type-def-macro-comment `(element-type-def (quote ,(quote ,name)) ,body t)))))))

(defun define-model-hash-functions (name)
  (let* ((hash-function-name (read-from-string (format nil "~A-hash-table" name)))
	 (hash-function-doc (concatenate 'string "With KEY, returns the associated hash value from the "
					 (format nil "~A model hash table; otherwise returns the table. " name)
					 "Entries can be added using SETF.")))
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(progn
	(defun ,hash-function-name (&optional (key nil key-supplied-p))
	  ,hash-function-doc
	  (let ((table (model-hash-table (get (quote ,name) (quote model)))))
	    (if key-supplied-p (gethash key table) table)))
	(defun (setf ,hash-function-name) (new-value key)
	  (setf (gethash key (,hash-function-name)) new-value))))))
	
(defun define-node-whatevers-function (name)
  (let* ((thing-string (string-downcase (string name)))
	 (elt-type-sym (read-from-string (format nil "~A-type" thing-string)))
	 (elt-type-sym-fboundp (fboundp elt-type-sym))
         (node-functions-args-symbol (read-from-string (format nil "(element~A)" (if elt-type-sym-fboundp (format nil " &optional ~A" elt-type-sym) ""))))
	 (node-functions-doc-string (format nil "Return all ~As associated with the cell element of ELEMENT.~a"
					    thing-string
					    (if elt-type-sym-fboundp
						(format nil " If ~A is supplied, then only return ~As that match ~A." elt-type-sym thing-string elt-type-sym)
						""))))	 
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(defun ,(read-from-string (format nil "node-~as" name)) ,node-functions-args-symbol
       ,node-functions-doc-string
       ,(if (not elt-type-sym-fboundp)
	    `(cell-element-elements element ',name)
	    `(let* ((all-elts-of-type (cell-element-elements element ',name))
		    (element-type (element ,elt-type-sym)))
	      (if ,elt-type-sym (delete-if-not #'(lambda (obj) (eq (,elt-type-sym obj) element-type)) all-elts-of-type) all-elts-of-type)))))))
	
#|
(defun define-whatevers-function (name)
  (let* ((whatevers-doc-format-string (concatenate 'string
						   "Returns a list of all ~(~A~)s associated with the cell elements referenced by ELEMENT, "
						   "if supplied; otherwise, all ~(~A~)s in circuit."))
	 (whatevers-symbol-collector-function (read-from-string (format nil "~As" name)))
	 (whatevers-function-doc-string (format nil whatevers-doc-format-string name name)))
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(defun ,whatevers-symbol-collector-function (&optional (element nil element-supplied-p))
       ,whatevers-function-doc-string
       (if element-supplied-p (element-collector (quote ,name) element) (element-collector (quote ,name)))))))
|#

(defun define-whatevers-function (name)
  (let* ((whatevers-doc-format-string (concatenate 'string
						   "Returns a list of all ~(~A~)s associated with the cell elements referenced by ELEMENT, "
						   "if supplied; otherwise, all ~(~A~)s in circuit."))
	 (whatevers-symbol-collector-function (read-from-string (format nil "~As" name)))
	 (whatevers-function-doc-string (format nil whatevers-doc-format-string name name)))
    (compile whatevers-symbol-collector-function
	     `(lambda (&optional (element nil element-supplied-p))
	       (if element-supplied-p (element-collector (quote ,name) element) (element-collector (quote ,name)))))
    (setf (documentation whatevers-symbol-collector-function 'function) whatevers-function-doc-string)
    whatevers-symbol-collector-function))

#|
(defun define-whatever-function (name)
  (let* ((whatever-doc-format-string "Returns the ~(~A~) associated with the cell element referenced by ELEMENT.")
	 (whatever-symbol-collector-function (read-from-string (format nil "~A" name)))
	 (whatever-function-doc-string (format nil whatever-doc-format-string name)))
    (eval				; There must be a better (correct?) way than doing this EVAL....
     `(defun ,whatever-symbol-collector-function (element)
       ,whatever-function-doc-string
       (element element (quote ,name))))))
|#

;; Generate SEGMENT-NAMES, ELECTRODE-NAMES, etc. etc. Must be loaded after element_functions.lisp

(defun define-whatevers-names-function (sym)
  (let ((function-name (read-from-string (format nil "~A-names" sym)))
	(symbol-collector-function (read-from-string (format nil "~As" sym))))
    (when (fboundp symbol-collector-function)
      (eval				; There must be a better (correct?) way than doing this EVAL....
       `(defun ,function-name () (coerce-to-list (element-name (,symbol-collector-function))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; need to defstruct NAME as well, e.g. (defstruct axon-type ...)
;; also TYPE-DEF macro e.g. (defmacro axon-type-def (body) ...)
(defun add-object-type (name
			&key
			parameter-type-library
			save-output-data-routine
			output-data-structure-variables
			output-data-keys
			edit-routine
			eval-routine
			print-routine
			short-print-routine
			document-routine
			create-routine)
  "For adding type NAME to the element system."
  (let ((type-sym (typecase name
		    (string (read-from-string name))
		    (t name))))
    (push type-sym *model-names*)
    (create-model type-sym
		  :parameter-type-library parameter-type-library
		  :save-output-data-routine save-output-data-routine
		  :output-data-structure-variables output-data-structure-variables
		  :output-data-keys output-data-keys
		  :edit-routine edit-routine
		  :eval-routine eval-routine
		  :print-routine print-routine
		  :short-print-routine short-print-routine
		  :document-routine document-routine
		  :create-routine create-routine)))
|#





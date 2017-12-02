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


;;; SYS Source file: circuit-input.lisp

(in-package "SURF-HIPPO")

(defvar *within-topload* nil)

(defvar *disable-process-circuit-structure* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defmacro circuit-load (&body body)
  "Load circuit definition expressed by the optional BODY. If not called recursively, and when *INITIALIZE-ON-CIRCUIT-LOAD* is T (the default), 
all circuits are cleared with INITIALIZE-GLOBALS-FOR-CIRCUIT. BODY may be a (function) symbol, a filename, or a series of Lisp forms, as long as they return
NIL. A filename may be a full pathname, or files under the current value of *SURF-HOME*, *CIRCUIT-DIRECTORY* or *REFERENCE-DIRECTORY*."
  `(progn
    (when (and (not *within-topload*) *initialize-on-circuit-load*) (initialize-globals-for-circuit))
    (let* ((*initialize-on-circuit-load* (unless *within-topload* *initialize-on-circuit-load*))
	   (*within-topload* t)
	   (form (progn ,@body)))
      ;; (print form)
      (if (or (stringp form) (extract-function form))
	  (input-*circuit*s form *within-topload*)
	  (progn (when form (sim-error (format nil "~A is not a name string or function name that describes a circuit!" form)))
		 (when (and *soma* *cell*)
		   (progn (setq *circuit-source* :forms)
			  (process-circuit-structure t)))))
      (if form
	  (unless *within-topload* (process-circuit-structure t))
	  (initialize-globals-for-circuit)
	  ))
    nil))
|#

(defmacro circuit-load (&body body)
  "Load circuit definition expressed by the optional BODY. If not called recursively, and when *INITIALIZE-ON-CIRCUIT-LOAD* is T (the default), all
circuits are cleared with CLEAN-SLATE. BODY may be a (function) symbol, a filename, or a series of Lisp forms, as long as they create a
cell and a soma. A filename may be a full pathname, or, in order of precedence, reside in the directory of the file being loaded which contains
this form, or in directory given by the current value of *SURF-HOME*, *CIRCUIT-DIRECTORY* or *REFERENCE-DIRECTORY*."
  `(progn
    (when (and (not *within-topload*) *initialize-on-circuit-load*) (clean-slate))
    (let* ((*initialize-on-circuit-load* (unless *within-topload* *initialize-on-circuit-load*))
	   (*within-topload* t)
	   (form (progn ,@body)))
      (if (or (stringp form) (extract-function form))
	  (input-*circuit*s form *within-topload*)
	  (if (and *soma* *cell*)
	      (progn (setq *circuit-source* :forms)
		     (process-circuit-structure))
	     (when form (sim-error (format nil "~A is not a name string or function name that describes a circuit!" form))))))
    (unless *within-topload* (process-circuit-structure))
    nil))

(setf (macro-function 'topload) (macro-function 'circuit-load)) ; Backward compatibility

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-circuit-structure (&optional force circuit-element-to-update)
  "Processes the existing cells, somas and segments to complete the circuit's geometry. Normally called automatically, but may be
invoked from within a circuit definition to allow subsequent morphologically-based references to cell elements."
  (unless (and (not force)
	       (or (not *soma*) (not *cell*)
					;(not *circuit-loaded*)
		   *disable-process-circuit-structure*
		   *circuit-processed*))
    (setq *circuit-loaded* t
	  *make-segment-lists* t)
    (when (or (not *circuit*)
	      (numberp *circuit*) (symbolp *circuit*)
	      (and (stringp *circuit*) (= (length *circuit*) 0)))
      (setq *circuit* (format nil "~A"
			      (if (and *circuit* (numberp *circuit*) (symbolp *circuit*))
				  *circuit*
				  (cell-name *cell*))))
      (setq *simulation-name* (format nil "~A" *circuit*)))
    (clear-miscellaneous-element-parameters)
    (connect-loose-segments)
    (destroy-zero-length-segments)
    (when *test-for-loops* (loop-check))
    (setq *num-nodes* (loop for node being the hash-value of (NODE-HASH-TABLE) when (node-is-physical-cell-node node) sum 1))
    (collect-circuit-objects)
    (unless *kill-extra-messages* (display-message (format nil "Locating segments...")))
    (locate-all-nodes)
    (reorder-circuit)
    (print-branch-points)
    (choose-plot-data)
    (setq *circuit-processed* t)
    (set-circuit-elements-parameters nil circuit-element-to-update)
    (setup-all-extracellular-electrodes)))

(defun circuit-source-menu ()
  ;; Menu for setting *ONLY-LOAD-PASSIVE*, *CIRCUIT-SOURCE*, *CIRCUIT-FILE-TYPE*, and *INITIALIZE-ON-CIRCUIT-LOAD*. Returns T
  ;; when a circuit definition source is chosen.
  (let ((dummy1 *circuit-source*)
	(dummy2 (if *initialize-on-circuit-load* :initialize :add)))
    (choose-variable-values
     `((dummy1 "Get circuit definition from:" :choose (:Catalog_Function :Function :File) :vertical :toggle-p)
					; (*circuit-file-type* "File type of circuit file:" :choose (:neurolucida :lisp))
       (*only-load-passive* "Load passive model only" :boolean)
       ,(when *circuit-loaded*
	      `(dummy2 ,(format nil "Initialize simulator before loading circuit~%or add new circuit to existing circuit:") :choose (:initialize :add)))
					; (dummy1 "Cancel" :boolean)
       )
     :label "What Kind of Circuit Definition")
    (when dummy1 (setq *initialize-on-circuit-load* (case dummy2 (:initialize t) (t nil))
		       *circuit-source* dummy1))
    (true-p dummy1)))

(defun circuit-function-menu ()
  ;; Returns T unless CANCEL.
  (let (dummy1 (dummy2 (if (stringp *circuit-function*) *circuit-function* "")))
    (choose-variable-values
     '((dummy2 "Circuit function" :string)
       (dummy1 "Cancel" :boolean))
     :label "Compiled Circuit Specification")
    (unless dummy1
      (setq *circuit-function* dummy2
	    *circuit-parts* (list dummy2)))
    (not dummy1)))

(defun circuit-catalog-function-menu ()
  ;; Returns NIL if nothing selected.
  (setq *CIRCUIT-CATALOG-FUNCTIONS* (delete-duplicates *CIRCUIT-CATALOG-FUNCTIONS*)) ; Clean it up.
  (setq *circuit-parts*
	(choose-list-values-from-keys
	 ;; (key-values-from-symbol-list (append *CIRCUIT-CATALOG-FUNCTIONS* *CIRCUIT-FUNCTIONS*)) ; include the latter var for backward compatibility.
	 (key-values-from-symbol-list *CIRCUIT-CATALOG-FUNCTIONS*)
	 nil :direction :vertical :do-all-at-once t :text "Choose one or more functions" :label "Compiled Circuit Catalog")))

(defun load-circuit-menu ()
  (setq *initialize-on-circuit-load* (or *initialize-on-circuit-load* (not *circuit-loaded*)))
  (when (circuit-source-menu)
    (when (case *circuit-source*
	    (:FUNCTION (circuit-function-menu))
	    (:CATALOG_FUNCTION (circuit-catalog-function-menu))
	    (:FILE (unless (eq (circuit-file-browser) :CANCEL)
		     (setq *circuit-parts* (list *circuit-filename*)))))
      (input-*circuit*s *circuit-parts*))))

(defun set-*circuit-source* (circuit)
  ;; Tries to figure out what the CIRCUIT argument refers to - that is to either a file name or a compiled function. Sets the
  ;; variables *CIRCUIT-SOURCE*, *CIRCUIT-FILENAME*, *CIRCUIT-FILE-TYPE* and *CIRCUIT* accordingly. CIRCUIT can be either a symbol
  ;; or a string, and if it refers to a file, may be either a string with the complete pathname or a file in the
  ;; *CIRCUIT-DIRECTORY* directory (default is surf-hippo/circuits/).
  (when (and circuit
	     (not (and (stringp circuit) (zerop (length circuit)))))
    (let (circuit-car circuit-cdr)
      (typecase circuit
	(cons (setq circuit-car (car circuit)
		    circuit-cdr (cdr circuit)))
	(t (setq circuit-car circuit)))
      (setq *circuit-source*
	    (cond ((if (stringp circuit-car)
		       (fboundp (read-from-string circuit-car))
		       (or (functionp circuit-car) (fboundp circuit-car)))
		   ;; CIRCUIT points to a function.
		   (setq *circuit-function* circuit
			 *circuit* (string circuit-car)
			 *input-is-function* t
			 *circuit-filename* nil)
		   ;; *CIRCUIT-SOURCE* may be either :CATALOG_FUNCTION or :FUNCTION.
		   (if (member *circuit-function* *CIRCUIT-CATALOG-FUNCTIONS*) :catalog_function :FUNCTION))
		  ((and
		    (not circuit-cdr)	; Don't allow a CIRCUIT list for filename parsing.
		    (or
		     ;; Is CIRCUIT a valid filename by itself?
		     (find-and-set-circuit-file-variables (string circuit))
		     ;; Or is CIRCUIT a valid filename in the directory associated with *LOAD-TRUENAME*?
		     (and *load-truename* (find-and-set-circuit-file-variables (concatenate-strings (DIRECTORY-NAMESTRING *load-truename*) (string circuit))))
		     ;; Or is CIRCUIT a valid filename in the *CIRCUIT-DIRECTORY*?
		     (find-and-set-circuit-file-variables (concatenate-strings *circuit-directory* (string circuit)))
		     ;; Or is CIRCUIT a valid filename in the *SURF-HOME*?
		     (find-and-set-circuit-file-variables (concatenate-strings *surf-home* (string circuit)))
		     ;; Or is CIRCUIT a valid filename in the *REFERENCE-DIRECTORY*?
		     (find-and-set-circuit-file-variables (concatenate-strings *REFERENCE-DIRECTORY* (string circuit)))))
		   ;; CIRCUIT is a filename.
		   (setq *input-is-function* nil
			 *circuit-parts* (list *circuit-filename*)
			 *circuit-function* nil
			 *circuit-file-type* (anatomy-file-type *circuit-filename* ;circuit
								))
		   :FILE)
		  (t (sim-error (format nil "Cannot find a source for circuit ~a!~%" circuit))))))))

(defun anatomy-file-type (filename)
  (let ((filename (replace-tilde-in-path filename)))
    (when (probe-file filename)
      (with-open-stream (stream (open filename :direction :input))
	(setq *circuit-file-type*
	      (if (and (search "VERSION" (string-upcase (read-line stream)))
		       (search "FILE ID" (string-upcase (read-line stream))))
		  :neurolucida
		  (when
		      (string= ".lisp" (string-tail (namestring filename) 5))
		    :lisp)))))))
      
(defun find-and-set-circuit-file-variables (filename)
  ;; This is also done by the QUIT-WITH-FILE function called by the CIRCUIT-FILE-BROWSER.
  (let ((filename (replace-tilde-in-path filename)))
    (when (probe-file filename)
      (setq *circuit-filename* (namestring filename)
	    *circuit-file* (file-namestring filename) 
	    *circuit-directory* (directory-namestring filename) 
	    *circuit* (pathname-name filename)))))

(defun input-*circuit*s (&optional circuit suppress-process-circuit)
  ;; Reads in the circuit function(s) or file(s) and processes the circuit structure.
  (let ((*initialize-on-circuit-load* *initialize-on-circuit-load*)
	(circuit-parts (no-nils (coerce-to-list (or circuit *circuit-parts*)))))
    (when (and (not *within-topload*) *initialize-on-circuit-load*) (clean-slate))
    (when (and (> (length circuit-parts) 1)
	       (and (not *use-simple-names*) (not *ADD-CELL-NAME-TO-SEGS*)))
      (choose-variable-values
       `((*ADD-CELL-NAME-TO-SEGS* "Add cell names to all segment names" :boolean))
       :label (format nil "Processing circuit with at least ~A parts" (length circuit-parts))))
    (loop for circuit-part in circuit-parts
	  do (read-in-circuit circuit-part) (setq *initialize-on-circuit-load* nil)))
  (if *soma*
      (progn (when (or (and (not *within-topload*) (not *initialize-on-circuit-load*))
		       (> (length *loaded-circuit-parts*) 1))
	       (make-compound-circuit-name))
	     (unless suppress-process-circuit (process-circuit-structure)))
      ;; Signal an non-fatal error.
      (display-message (format nil "Input circuit called with a non-circuit defining ~A."
			       (case *circuit-source*
				 ((:CATALOG_FUNCTION :FUNCTION) "function")
				 (:FILE "file"))))))

(defvar *read-in-circuit-level* 0)

(defun read-in-circuit (&optional circuit &key nts-cell-name (origin-offset `(0.0 0.0 0.0)))
  ;; If CIRCUIT is supplied, then it is loaded. Otherwise, the current state of *CIRCUIT-SOURCE* is referenced and, as appropriate, *CIRCUIT-FUNCTION* or
  ;; *CIRCUIT-FILENAME* is loaded. NTS-CELL-NAME is valid when CIRCUIT names an ntscable created file - if not supplied, cell name is taken from the
  ;; specification in the file. ORIGIN-OFFSET applies to all cells in the circuit created by the current evaluation of READ-IN-CIRCUIT. 
  (setq *circuit-processed* nil *circuit-loaded* nil)
  (cond-every
   ((= 0 *read-in-circuit-level*) (setq *session-name* *circuit*))
   (circuit (set-*circuit-source* circuit))
   (*circuit-source*
    (display-message (format nil "~a in circuit ~a..." (or (and (not *circuit*) *initialize-on-circuit-load*) "Reading" "Adding") *circuit*))
    (let ((previous-cells (cells))
	  (*monitor-circuit-modifications*)
	  (*read-in-circuit-level* (1+ *read-in-circuit-level*))
	  error)
      (case *circuit-source*
	((:CATALOG_FUNCTION :FUNCTION)
	 (let ((function-sym (typecase *circuit-function*
			       (string (read-from-string *circuit-function*))
			       (t *circuit-function*))))
	   (when function-sym
	     (unless (member function-sym *CIRCUIT-CATALOG-FUNCTIONS*) (push function-sym *CIRCUIT-CATALOG-FUNCTIONS*))
	     (typecase function-sym
	       (cons (apply (car function-sym) (cdr function-sym)))
	       (t (funcall function-sym))) ; Call function that defines circuit.
	     (when (= 1 *read-in-circuit-level*) (push *circuit-function* *loaded-circuit-parts*)))))
	(:FILE
	 (if (and (open *circuit-filename* :direction :probe)
		  (case *circuit-file-type*
		    (:neurolucida (READ-NEUROLUCIDA-FILE *circuit-filename*))
		    (t			; (:lisp :ntscable :surf-functions)
		     (setq *nts-cell-type* "" *nts-cell-name* "")
		     (load (pathname *circuit-filename*)) ; LOAD THE CELL FILE.
		     (when *process-ntscable-list* 
		       (unless (typecase *nts-cell-type*
				 (string (> (length *nts-cell-type*) 0))
				 (t *nts-cell-type*))
			 (setq *nts-cell-type* (pathname-name *circuit-file*)))
		       (unless (typecase *nts-cell-name*
				 (string (> (length *nts-cell-name*) 0))
				 (t *nts-cell-name*))
			 (setq *nts-cell-name* (pathname-name *circuit-file*))))
		     t)))
	     (when (= 1 *read-in-circuit-level*) (push (pathname-name *circuit-file*) *loaded-circuit-parts*))
	     (setq error (format t "File ~a does not exist!~%" *circuit-file*)))))
      (unless error
	(when *process-ntscable-list*
	  (process-ntscable-list (or nts-cell-name *nts-cell-name*))
	  (setq *process-ntscable-list* nil))
	;; ADJUST ORIGINS OF NEW CELLS.
	(unless (eq origin-offset '(0.0 0.0 0.0))
	  (loop for cell in (cells) unless (member cell previous-cells)
		do (setf (cell-origin cell) (mapcar `+ (cell-origin cell) origin-offset))))
	(setq *circuit-loaded* (true-p (and *soma* *cell*))))))))

(defun make-compound-circuit-name ()
  (setq *multiple-source-circuit* t
	; *ADD-CELL-NAME-TO-SEGS* t
	)
  (let ((names (copy-list *loaded-circuit-parts*))
	(*automatic-run* t)
	(dummy1 nil))
    (choose-variable-values
     `((*circuit* "What do you want to call this group" :string)
       (dummy1 ,(format nil "Ignore above and generate name~%composed of all the parts") :boolean)
       ; (*ADD-CELL-NAME-TO-SEGS* "To be safe, add cell name to all segment names" :boolean)
       )
     :label (let ((string "Naming circuit made from functions: "))
	      (loop for name in names do (setq string (concatenate-strings string (format nil "~A" name) " ")))
	      string))
    (when (or dummy1 (= 0 (length *circuit*)))
      (setq *circuit* (CONCATENATE-ATOMS-TO-STRING-LIST *loaded-circuit-parts* :string-spacer "_")))))

(defun clear-miscellaneous-element-parameters ()
  ;; (loop for elt in (cell-elements) do nil)
  (loop for vsource in (vsources) do (setf (vsource-adjacent-nodes-and-g-axials vsource) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-circuit (circuit &optional ADD-CELL-NAME-TO-SEGS suppress-process-circuit)
  (let ((*automatic-run* t)
	(*initialize-on-circuit-load* nil)
	(*ADD-CELL-NAME-TO-SEGS* ADD-CELL-NAME-TO-SEGS))
    (input-*circuit*s circuit suppress-process-circuit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (fdefinition 'input-*circuit*) (fdefinition 'input-*circuit*s)) ;; Backward compatibility

(defun input-several-*circuit*s (circuit-list) ;; Backward compatibility  
  (let ((*circuit-parts* circuit-list))
    (input-*circuit*s)))



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


;;; SYS Source file: print.lisp
(in-package "SURF-HIPPO")

;; General output executive, printing the circuit, making the output lists and output files.

(defun get-surf-filename (pathname-directory extension &optional (trailer ""))
  "Generate a full pathname of the form:

           PATHNAME-DIRECTORY/filename.EXTENSION

where \"filename\" is either the current value of *SIMULATION-NAME*, when *ADD-SIMULATION-TO-FILENAMES* is T, otherwise the
current value of *TIME-STAMP*, in either case postpended with TRAILER. Any repeated \"/\" will be replaced with a single \"/\"."
  (replace-repeated-character-w-single
   (concatenate-strings (string pathname-directory) "/"
			(if *ADD-SIMULATION-TO-FILENAMES* *simulation-name* (format nil "~a" *time-stamp*))
			(string trailer) "." (string extension))
   "/"))

;; The function PRINT-ANALYSIS is now defined in analysis.lisp

(defun sim-output ()
  "Overall simulation output function, normally called automatically at the end of each simulation. Prepares output lists, prints simulation information,
writes data files, and plots results, as appropriate. SIM-OUTPUT may also be called during a simulation, where it will process all output data up to the
current simulation time. SIM-OUTPUT also calls (without arguments) any functions specified in the global variable *USER-OUTPUT-DATA-FUNCTIONS*."
  (analysis-output)			; Analyzes, if required.
  (after-simulation-print-circuit)
  (surf-plotter)			; Collects the plotting data and plots.
  (dump-simulation-files)		; Writes files, if required
  (USER-OUTPUT-DATA)
  (unless *kill-extra-messages* (display-message (format nil "Done.~%"))))

(defun simulation-output () (sim-output)) ; Backward comp

(defun user-output-data () (mapcar 'funcall *user-output-data-functions*))

(defun analysis-output ()
  (when *print-out-to-lisp* (print-simulation-stats))
  (when *print-out-to-info-window* (let (*DUMP-ANALYSIS-TO-FILE*) (OUTPUT-TEXT-tO-INFO-WIN 'print-simulation-stats))))

(defun print-numerical-details ()
  "Print out a number of numerical details relevant to the last or current simulation."
  (format t "Numerical Details:~%")
  (case *INTEGRATION-TIME-REFERENCE*
    (:LIST				; *use-time-list*
     (format t "Shadowed time steps~%"))
    (:fixed				; *use-fixed-step*
     (format t " - User fixed step = ~A~%" *user-step*))
    (:variable				; LTE variable step
     (format t " LTE/variable step: *USER-MAX-STEP* = ~A, *USER-MIN-STEP* = ~A~%" *user-max-step* *user-min-step*)
     (if (not *LTE-NODE-CRITERIUM*)
	 (format t "*LTE-NODE-CRITERIUM* is NIL~%")
	 (let* ((lte-node-criterium (coerce-to-list *lte-node-criterium*))
		(explicit-elements (coerce-to-list (element-node lte-node-criterium)))
		(keywords (no-nils (mapcar #'(lambda (keyword) (find keyword lte-node-criterium)) '(:all :synapses :channels :axons :vsources :isources :somas)))))
	   (format t " *LTE-NODE-CRITERIUM* includes ")
	   (when keywords (format t "the keyword~p ~s" (length keywords) keywords))
	   (when (and keywords explicit-elements) (format t " and "))
	   (when explicit-elements (format t "~A explicit element~:p" (length explicit-elements)))
	   (format t "~%")))
     (format t " *ABSOLUTE-VOLTAGE-ERROR* = ~A, *PICK-TIME-STEP-FUDGE* = ~A~%" *absolute-voltage-error* *pick-time-step-fudge*)

     (cond-every
      (*debug-max-dvdts* (statistics-on-list *MAX-DV-DIFF-NODES*))
      (*calculate-particle-error*
       (format t " *ABSOLUTE-PARTICLE-ERROR* = ~A~%" *absolute-particle-error*))
      (*lte-was-punted*
       (format t " The LTE estimater was ignored ~A ~a time~:p~%" (if (= *user-min-step* 0) "(integer min step reached)" "(*USER-MIN-STEP* reached)") *lte-was-punted*))
      (*count-error-step-change*
       (if *full-error-step-change*
	   (format t " Voltage lte estimate set iteration/step ~A/~a time~:p~%"
		   (length (intersection *sim-reverse-time-list* *VOLTAGE-ERROR-STEP-CHANGES*))
		   (length *VOLTAGE-ERROR-STEP-CHANGES*))
	   (format t " Voltage lte estimate set step ~a time~:p~%" (length *VOLTAGE-ERROR-STEP-CHANGES*)))
       (when *calculate-particle-error*
	 (if *full-error-step-change*
	     (format t " Particle lte estimate set iteration/step ~A/~a time~:p~A~%"
		     (length (intersection *sim-reverse-time-list* *particle-ERROR-STEP-CHANGES*))
		     (length *particle-ERROR-STEP-CHANGES*)
		     (if (> (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*) 0)
			 (format nil " (Particle LTE step < *MIN-STEP* ~a time~:p)" (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*))
			 ""))
	     (format t " Particle lte estimate set iteration/step ~a time~:p~A~%"
		     (length *particle-ERROR-STEP-CHANGES*)
		     (if (> (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*) 0)
			 (format nil " (Particle LTE step < *MIN-STEP* ~a time~:p)" (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*))
			 ""))))
       (when *calculate-conc-int-error*
	 (format t " Conc integrator lte estimate set iteration/step ~A/~a time~:p~%"
		 (length (intersection *sim-reverse-time-list* *conc-int-ERROR-STEP-CHANGES*))
		 (length *conc-int-ERROR-STEP-CHANGES*)))))))
  (format t " Particle lookup precision: ~amV [interpolation ~A]~%" *particle-look-up-table-precision* (if *interpolate-particle-arrays* "ON" "OFF")))

#|
(format t "~:[~; foo ~]" t)
(defun print-simulation-stats (&optional complete)
  "Print out the progress of the last or current simulation and a number of numerical details."
  (when (and *simulation-started* *circuit-loaded*) ; *simulation-finished*
    (when (or complete (not *kill-extra-messages*))
      (format t "~d ms simulation~A (~D/~D time points/iterations)~%"
	      (round *user-stop-time*)
	      (not (>= *simulation-max-time* *user-stop-time*)) " [Time reached: ~,1f ms]"
	      *total-num-time-points* *total-num-iterations*)
      (when (or complete *print-numerical-details*) (print-numerical-details))
      (format t "~&")
      (when (or complete *print-analysis*) (print-analysis)))))
|#


(defun print-simulation-stats (&optional complete)
  "Print out the progress of the last or current simulation and a number of numerical details."
  (when (and *simulation-started* *circuit-loaded*) ; *simulation-finished*
    (when (or complete (not *kill-extra-messages*))
      (format t "~d ms simulation~A (~D/~D time points/iterations)~%"
	      (round *user-stop-time*)
	      (if (not (>= *simulation-max-time* *user-stop-time*)) (format nil " [Time reached: ~,1f ms]" *simulation-max-time*) "")
	      *total-num-time-points* *total-num-iterations*)
      (when (or complete *print-numerical-details*) (print-numerical-details))
      (format t "~&")
      (when (or complete *print-analysis*) (print-analysis)))))

(defun print-cell-element-elements (element)
  (concatenate-string-list
   (no-nils (mapcar (lambda (model-type)
		      (let ((num-elts (length (cell-element-elements element model-type))))
			(when (and (> num-elts 0) (not (or (eq model-type 'cell) (eq model-type 'segment) (eq model-type 'soma))))
			  (format nil "~d ~a~p" num-elts (nice-symbol-string model-type) num-elts))))
		    *circuit-element-model-names*))
   :string-count-to-add-linefeed 7 :string-spacer ", "))

(defun dump-simulation-files (&optional force-data-out force-info-out description)
  (when (and *simulation-finished* (or *save-simulation-data-to-file* force-data-out))
    (write-element-data (unless *automatic-run* (when (go-ahead-menu "Prompt for each data list to save") :menu))))
  (when (or *save-simulation-info* force-info-out) (dump-info-file description)))

#|
(defun dump-data-file (&optional select-each-element selected-element-names-and-slots)
  ;; Writes out plot data to file, according to *FILE-OUTPUT-VARIABLE-LIST*.
  (let (*print-pretty*)
    (when *simulation-finished*
      (let* ((pathname-directory (get-surf-data-directory))
	     (data-filename (get-surf-filename pathname-directory "dat"))
	     (time-symbol (create-output-symbol *simulation-name* 'time))
	     (approved-file-output-variable-list
	      (if (or selected-element-names-and-slots select-each-element)
		  (let ((*automatic-run* (or *automatic-run* (not select-each-element))))
		    (choose-list-values-from-keys
		     (mapcar (lambda (var-info-list) (list (format nil "~a" (nth 0 var-info-list)) var-info-list)) *FILE-OUTPUT-VARIABLE-LIST*)
		     nil
		     :punt-if-only-one-entry nil
		     :selected-keys
		     (let (var-symb)
		       (loop for thing in *FILE-OUTPUT-VARIABLE-LIST*
			     when (setq var-symb
					(loop for name-and-slot in selected-element-names-and-slots
					      when (and (string= (car name-and-slot) (element-name (nth 1 thing)))
							(eq (cadr name-and-slot) (nth 2 thing)))
					      do (return (nth 0 thing))))
			     collect (format nil "~a" var-symb)))
		     :label "Choose Plotted Data To Dump To File"))
		  *file-output-variable-list*)))
	(when approved-file-output-variable-list
	  (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
	  (when (probe-file (ext:unix-namestring pathname-directory nil))
	    (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
	      (format stream ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	      ;; VAR-INFO-LIST has the following format: (var-symb circuit-element data-slot)
	      (mapcar (lambda (var-info-list) (write-lisp-list-to-stream (nth 0 var-info-list) (element-data (nth 1 var-info-list) (nth 2 var-info-list)) stream)
			      approved-file-output-variable-list))
	      ;; Now write out the time list.
	      (write-lisp-list-to-stream time-symbol (CURRENT-SIM-PLOT-TIME-LIST) stream)
	      ;; Now write out an archive variable name function.
	      (write-push-lisp-list-to-stream
	       '*archive-variable-list*
	       (list *simulation-name* time-symbol (mapcar (lambda (var-info-list) (list (car var-info-list) (nth 2 var-info-list))) approved-file-output-variable-list))
	       stream)
	      (format t "File ~a written~%" data-filename))
	    (setq *last-simulation-file-path* *simulation-name*)))))))
|#

(defun parse-*FILE-OUTPUT-VARIABLE-LIST*-for-menu-entries (element-names-and-slots)
  (let (var-symb)
    (loop for thing in *FILE-OUTPUT-VARIABLE-LIST*
	  when (setq var-symb
		     (loop for name-and-slot in element-names-and-slots
			   when (and (same-element-names (car name-and-slot) (element-name (nth 1 thing)))
				     (eq (cadr name-and-slot) (nth 2 thing))) ; Check the data type
			   do (return (nth 0 thing))))
	  collect (format nil "~a" var-symb))))

(defun approved-file-output-variable-list-menu (select-each-element element-names-and-slots)
  (let ((*automatic-run* (or *automatic-run* (not select-each-element))))
    (choose-list-values-from-keys
     (mapcar (lambda (var-info-list) (list (format nil "~a" (nth 0 var-info-list)) var-info-list)) *FILE-OUTPUT-VARIABLE-LIST*)
     (parse-*FILE-OUTPUT-VARIABLE-LIST*-for-menu-entries element-names-and-slots)
     :punt-if-only-one-entry nil :label "Choose Plotted Data To Dump To File")))

(defun write-element-lisp-data (data-filename approved-file-output-variable-list time-symbol suppress-comments)
  (write-lisp-header data-filename)
  (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
   (unless suppress-comments
     (let ((*standard-output* stream))
       (format t "#|~%")
       (print-circuit :terse)
       (format t "|#~%")))
   ;; VAR-INFO-LIST has the following format: (var-symb circuit-element data-slot)
   (mapcar (lambda (var-info-list) (write-lisp-list-to-stream (nth 0 var-info-list) (element-data (nth 1 var-info-list) (nth 2 var-info-list)) stream 10 1))
	   approved-file-output-variable-list)
   ;; Now write out the time list.
   (write-lisp-list-to-stream time-symbol (CURRENT-SIM-PLOT-TIME-LIST) stream 10 1)
   ;; Now write out an archive variable name function.
   (write-push-lisp-list-to-stream
    '*archive-variable-list*
    (list *simulation-name* time-symbol (mapcar (lambda (var-info-list) (list (car var-info-list) (nth 2 var-info-list))) approved-file-output-variable-list))
    stream)
   (format t "File ~a written~%" data-filename)))

(defun write-element-columns-data (pathname-directory approved-file-output-variable-list units-labels suppress-comments)
  (loop for var-info-list in approved-file-output-variable-list
	for units-label in units-labels
	do (let* ((data-symbol (nth 0 var-info-list))
		  (ELEMENT-name (nth 1 var-info-list))
		  (SLOT (nth 2 var-info-list))
		  (data-filename (format nil "~A~A.dat" pathname-directory (make-nice-filename data-symbol))))
	     (write-lists-multi-column
	      (list (CURRENT-SIM-PLOT-TIME-LIST) (element-data element-name slot))
	      :announce-write t
	      :comment (unless suppress-comments (format nil "~%;; ~A: ~A ~A time (milliseconds) and data (~A), x y format" *simulation-name* element-name slot units-label))
	      :filename data-filename))))

#|
(defun write-element-data (element filename &key data-type model-type state-index (output-format :lisp) suppress-comments extra-comment filename-extension)
  "Given ELEMENT or ELEMENTs of type MODEL-TYPE, write the plot data list in OUTPUT-FORMAT to FILENAME, according to
ELEMENT-DATA. Remaining arguments are as for STORE-XY-DATA."
    (loop for elt in (elements element model-type)
	  with data do (setq data (element-data elt data-type model-type state-index))
	  when data collect (list (CURRENT-SIM-PLOT-TIME-LIST) data) into xy-lists and collect (massage-element-plot-label elt) into labels
	  finally (store-XY-data xy-lists filename output-format
				 :filename-extension filename-extension :suppress-comments suppress-comments :extra-comment extra-comment :labels labels))))
|#

(defun write-element-data (&optional elements-and-slots &key (output-format :lisp) filename suppress-comments)
  "Writes ELEMENT-DATA from all elements referenced in ELEMENTS-AND-SLOTS. ELEMENTS-AND-SLOTS may be a list whose members are
either element references or sublists of element references and data types. If ELEMENTS-AND-SLOTS is :MENU, then a menu will be
generated. If ELEMENTS-AND-SLOTS is NIL or not supplied, then all data saved from last simulation will be written to
file. Remaining args are as for GRAB-AND-STORE-PLOT-DATA. For :LISP OUTPUT-FORMAT, variable symbols are created from the
simulation, element and data type name. These symbols are used when the file is loaded into Lisp. For :COLUMNS
OUTPUT-FORMATseparate files are written, whose names are constructed as the case for the variable symbols in the :LISP
OUTPUT-FORMAT case. In both cases the created variable symbols are listed in the global variable *FILE-OUTPUT-VARIABLE-LIST*. If
FILENAME is not supplied then the full pathname is obtained with the GET-SURF-FILENAME function (directory obtained with the
GET-SURF-DATA-DIRECTORY function), with file extension \"dat\"."
  (let* (*print-pretty*
	 (pathname-directory (get-surf-data-directory))
	 (use-menu (eq elements-and-slots :menu))
	 (data-filename (or filename (get-surf-filename pathname-directory "dat")))
	 (time-symbol (create-output-symbol *simulation-name* 'time))
	 (element-names-and-slots (unless use-menu
				    (mapcar (lambda (elt-slot) (list (element-name (if (consp elt-slot) (car elt-slot) elt-slot))
								     (or (when (consp elt-slot) (cdr elt-slot)) (default-data-type elt-slot))))
					    (coerce-to-list elements-and-slots))))
	 (approved-file-output-variable-list
	  (if (or use-menu element-names-and-slots) (approved-file-output-variable-list-menu use-menu element-names-and-slots) *file-output-variable-list*))
	 (units-labels (coerce-to-list (element-data-units (mapcar #'(lambda (var-info-list) (ELEMENT-name (nth 1 var-info-list))) approved-file-output-variable-list))))
	 (dummy3 output-format)
	 (dummy1 data-filename)
	 (dummy4 suppress-comments))
    (when approved-file-output-variable-list
      (when use-menu (choose-variable-values
		      `((dummy1 "Edit filename (for Lisp files, normally the ext is .lisp)" :string)
			(dummy3 "Output format" :choose (:lisp :columns))
			(dummy4 "Suppress file comments" :boolean)
			(dummy2 "CANCEL" :boolean))
		      :title (format nil "Write Data From ~A" *simulation-name*)))
      (unless dummy2
	(let ((suppress-comments dummy4)
	      (output-format dummy3))
	  (when approved-file-output-variable-list
	    (multiple-value-bind (data-filename pathname)
		(provide-pathname-and-filename dummy1 pathname-directory)
	      (when pathname
		(case output-format
		  (:lisp (write-element-lisp-data data-filename approved-file-output-variable-list time-symbol suppress-comments))
		  (:columns (write-element-columns-data pathname-directory approved-file-output-variable-list units-labels suppress-comments)))
		(setq *last-simulation-file-path* *simulation-name*))))))))
  nil)

#|
;; fix this
(defun dump-synapse-waveform-file ()
  (let ((*print-pretty* nil))
    (if *simulation-finished*
	(let* ((pathname (get-surf-data-directory))
	       (data-filename (get-surf-filename pathname "dat" "-syn-waves")))
	  (unix:unix-mkdir (ext:unix-namestring pathname nil) #o777)
	  (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
	    (format stream ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	    (format stream "#|~%~%")
	    (print-circuit)
	    (after-simulation-print-circuit stream)
	    (format stream "|#~%~%")
	    ;; VAR-INFO-LIST has the following format: (var-symb circuit-element data-slot)
	    (write-lisp-list-to-stream '*synapse-waveforms *synapse-waveforms stream))
	  (format t "File ~a written~%" data-filename))
	(setq *last-simulation-file-path* *simulation-name*))))
|#

(defun dump-info-file (&optional description-level)
  (let* ((pathname-directory (get-surf-data-directory))
	 (info-filename (get-surf-filename pathname-directory "info"))
	 (new-file (not (probe-file info-filename)))
	 (*print-pretty* nil))
    (unix-mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (with-open-stream (*standard-output* (open info-filename :direction :output :if-exists :append :if-does-not-exist :create))
	(when new-file (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%"))
	(format t "#|~%~%")
	(print-circuit description-level)
	(after-simulation-print-circuit)
	(format t "~%~%")
	(let (*DUMP-ANALYSIS-TO-FILE*) (print-simulation-stats))
	(format t "|#~%~%"))
      (format t "File ~a written~%" info-filename))
    (setq *last-simulation-file-path* *simulation-name*)
    nil))

(defun add-element-doc-to-info-file (elt)
  (let* ((pathname-directory (get-surf-data-directory))
	 (info-filename (get-surf-filename pathname-directory "info"))
	 (new-file (not (probe-file info-filename)))
	 (*print-pretty* nil))
    (unix-mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (with-open-stream (*standard-output* (open info-filename :direction :output :if-exists :append :if-does-not-exist :create))
	(when new-file (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%"))
	(format t "#|~%~%")
	(print-element elt nil t)
	(format t "~%~%")
	(format t "|#~%~%"))
      (format t "File ~a written~%" info-filename)
      (setq *last-simulation-file-path* *simulation-name*)
      nil)))

(defun dump-analysis-file (results &optional filename)
  (let* ((pathname-directory (get-surf-data-directory))
	 (filename (if filename
		       (format nil "~a/~a" pathname-directory filename)
		       (get-surf-filename pathname-directory "results"))))
    (unix-mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (unless (probe-file filename)
	(with-open-stream (*standard-output* (open filename :direction :output :if-does-not-exist :create))
	  (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	  (write-lisp-list-to-stream '*archive-session-results* nil t 10 0 t)
	  (format t "~%~%~%")))
      (with-open-stream (*standard-output* (open filename :direction :output :if-exists :append))
	(format t "(push ~% '(")
	(format-list (list *simulation-name* *simulation-results-addendum* results) 1 t)
	(format t ")~%          *archive-session-results*)~%~%"))
      (format t "File ~a written~%" filename))
    nil))

(defun write-comment-to-analysis-file (comment &optional filename)
  (let* ((pathname-directory (get-surf-data-directory))
	 (filename (if filename
		     (format nil "~a/~a" pathname-directory filename)
		     (get-surf-filename pathname-directory "results"))))
    (mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (unless (probe-file filename)
	(with-open-stream (*standard-output* (open filename :direction :output :if-does-not-exist :create))
	 (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	 (write-lisp-list-to-stream '*archive-session-results* nil t 10 0 t)
	 (format t "~%~%")))
      (with-open-stream (*standard-output* (open filename :direction :output :if-exists :append))
       (format t "#|~%~%~a~%~%|#~%~%" comment))
      (format t "File ~a written~%" filename))))

(defun dump-all-circuit-elements-file ()
  (let ((*automatic-run* t)) (dump-elements-file (loop for model in (models) nconc (copy-list (things-in-circuit (model-name model)))))))

(defun dump-DOCUMENTED-USER-VARIABLES-file ()
  "Write a loadable file of all documented user variables"
  (let* ((pathname-directory (get-surf-data-directory))
	 (elements-filename (get-surf-filename pathname-directory "vars"))
	 (*print-pretty* nil))
    (mkdir (namestring pathname-directory nil) #o777)
    (when (probe-file (namestring pathname-directory nil))
      (with-open-stream (*standard-output* (open elements-filename :direction :output :if-exists :append :if-does-not-exist :create))
	(format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	(format t "#|~%")
	(let (*enable-print-DOCUMENTED-USER-VARIABLES*) (print-circuit :terse))
	(after-simulation-print-circuit)
	(format t "|#~%")
	(format t "~%~%")
	(print-DOCUMENTED-USER-VARIABLES))
      (format t "File ~a written~%" elements-filename)
      (setq *last-simulation-file-path* *simulation-name*)
      nil)))

(defun dump-elements-file (&optional elements-or-select-each-element)
  "Write a loadable file with TYPE-DEF forms for selected (loaded) elements which are element types, and CREATE forms for selected
elements such as channels, synapses, or sources. Selected elements are determined by the ELEMENT-OR-SELECT-EACH-ELEMENTS argument
- this arg can be either a single element, a list of elements, non-NIL (generating a selection menu, or NIL which will select all
loaded elements."
  (let* ((pathname-directory (get-surf-data-directory))
	 (elements-filename (get-surf-filename pathname-directory "elts"))
	 (*print-pretty* nil)
	 (mod-elts
	  (cond
	    ((element (car (coerce-to-list elements-or-select-each-element)))
	     (loop for elt in (coerce-to-list elements-or-select-each-element)
		   when (element-model elt) collect (list (element-model elt) elt)))
	    (elements-or-select-each-element
	     (loop for type in (CHOOSE-ASSOCIATED-ELEMENTS-TYPES-FOR-DOCUMENTATION) nconcing
		   (let ((mod (type-symbol-model type)))
		     (choose-list-values-from-keys
		      (loop for elt being the hash-value of (model-hash-table mod)
			    when (and (instance-in-cell elt) (model-document-routine mod) )
			    collect (list (format nil "~a ~a" (element-name elt)(model-name mod))
					  (list mod elt)))
		      nil :punt-if-only-one-entry nil :label "Choose Elements To Dump To File"))))
	    (t
	     (loop for mod in (models) nconcing
		   (loop for elt being the hash-value of (model-hash-table mod)
			 when (model-document-routine mod) collect (list mod elt)))))))
    (when mod-elts
      (mkdir (namestring pathname-directory nil) #o777)
      (when (probe-file (namestring pathname-directory nil))
	(with-open-stream (*standard-output* (open elements-filename :direction :output :if-exists :append :if-does-not-exist :create))
	  (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	  (format t "#|~%")
	  (print-circuit :terse)
	  (after-simulation-print-circuit)
	  (format t "|#~%")
	  (loop for mod-elt in mod-elts	when (model-document-routine (nth 0 mod-elt)) do (funcall (model-document-routine (nth 0 mod-elt)) (nth 1 mod-elt)) (format t "~%")))
	(format t "File ~a written~%" elements-filename)
	(setq *last-simulation-file-path* *simulation-name*))
      nil)))

;; Not tested
(defun dump-object-to-file (object &optional filename-comp)
  (let* ((pathname-directory (get-surf-data-directory))
	 (data-filename (get-surf-filename pathname-directory "dat" filename-comp)))
    (when (mkdir (namestring pathname-directory nil) #o777)
      (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
	(write object :stream stream)))))

(defun write-lisp-list-to-stream (name list stream &optional (values-per-line 10) (indent 0) defvar-only)
  (format stream "(defvar ~s nil)~%" name)	; avoid warnings.
  (unless defvar-only
    (format stream "(setq ~s '(~%" name)
    (format-list list values-per-line stream nil indent)
    (dotimes (i indent) (format stream " "))
    (format stream "~%))~%"))
  (format stream "~%"))

(defun write-lisp-sym-and-value-to-stream (sym stream &optional (values-per-line 10) (indent 0))
  (let ((value (symbol-value sym)))
    (typecase value
      (cons (write-lisp-sym-list-to-stream sym (symbol-value sym) stream values-per-line indent))
      (atom (format stream "~%(setq ~s ~s)~%" sym value)))))

(defun write-lisp-sym-list-to-stream (name list stream &optional (values-per-line 10) (indent 0) (include-defvar t))
  (dotimes (i indent) (format stream " "))
  (when include-defvar (format stream "(defvar ~s)~%" name))	; avoid warnings.
  (dotimes (i indent) (format stream " "))
  (format stream "(setq ~s ~%" name)
  (formatted-list-dump list stream nil values-per-line (1+ indent))
  (format stream "~%")
  (dotimes (i indent) (format stream " "))
  (format stream " )~%~%"))

(defun write-push-lisp-list-to-stream (name list stream &optional (values-per-line 5))
  (format stream "(if (not (member ~% '(")
  (format-list list values-per-line stream)
  (format stream ")~% ~s :test 'equal))~%~%" name)
  (format stream " (push ~% '(")
  (format-list list values-per-line stream)
  (format stream ")~%   ~s))~%~%" name))

(defun format-thing-list-count (thing-list name &optional period-not-comma (correction 0))
  (let ((number-things-in-circuit (+ correction (loop for thing in thing-list when (element-in-circuit thing) sum 1))))
    (when (> number-things-in-circuit 0)
      (format t (concatenate-strings "~D " name "~:P" (if period-not-comma ".~%" ", ")) number-things-in-circuit))))

(defun format-hash-table-count (table name &optional period-not-comma (correction 0))
  (let ((number-things-in-circuit (+ correction (loop for entry being the hash-value of table when (element-in-circuit entry) sum 1))))
    (when (> number-things-in-circuit 0)
      (format t (concatenate-strings "~D " name "~:P" (if period-not-comma ".~%" ", ")) number-things-in-circuit))))

(defun format-type-symbol-count (type-symbol &optional period-not-comma)
  (let ((name (nice-symbol-string type-symbol))
	(number-things-in-circuit (num-type-instances-in-circuit (element-type type-symbol))))
    (when (> number-things-in-circuit 0)
      (format t (concatenate-strings "~D " name "~:P" (if period-not-comma ".~%" ", ")) number-things-in-circuit))))

;; There are some things which should be printed out about the circuit only after the simulation.
(defun after-simulation-print-circuit () nil)

;; Print light stimulus information
(defun print-light-stimulus ()
  (when *light-stimulus*
    (format t "Light stimulus is a ~a, strength ~a~A~%" *light-stimulus* *light-stimulus-strength*
	    (if (= *light-background* 0) "" (format nil " (background level ~A)" *light-background*)))
    (format t "  Start time: ~,1f ms ~a,~A~:[~; (Light input @ t=0 holds for all t<0)~].~%"
	    *light-stimulus-start-time*
	    (if *fast-full-field-spot* ""
		(format nil " at (X=~,1f um, Y=~,1f um)" *light-start-position-x* *light-start-position-y*))
	    (if (> *light-stimulus-stop-time* *user-stop-time*) " active for entire simulation" (format nil " Stop time: ~,1f ms" *light-stimulus-stop-time*))
	    *constant-light-input-from-negative-infinity*)
    (case *light-stimulus*
      ((:on-bar :off-bar :bar)
       (format t "  Stimulus orientation (width axis) ~,1f degrees~%" (rad-to-deg *light-theta*)))
      ((:moving-bar :on-moving-bar :off-moving-bar :moving-sine-grating :moving-bar-grating)
       (format t "  Stimulus trajectory orientation (width axis) ~,1f degrees~%" (rad-to-deg *light-theta*)))
      ((:moving-spot :on-moving-spot :off-moving-spot)
       (format t "  Stimulus trajectory orientation ~,1f degrees~%" (rad-to-deg *light-theta*))))
    (case *light-stimulus*
      (:apparent-motion (print-apparent-motion-parameters))
      ((:moving-bar :on-moving-bar :off-moving-bar) (print-moving-bar-parameters))
      ((:on-bar :off-bar :bar) (print-bar-parameters))
      (:moving-bar-grating (print-moving-bar-grating-parameters))
      (:moving-sine-grating (print-moving-sine-grating-parameters))
      ((:moving-spot :on-moving-spot :off-moving-spot) (print-moving-spot-parameters))
      (:annulus (print-spot-and-annulus-parameters))
      ((:on-spot :off-spot :spot) (print-spot-and-annulus-parameters)))))

(defun print-apparent-motion-parameters () (format t "  Bar A intensity = ~,1f, Bar B intensity = ~,1f~%" *bar-a-intensity* *bar-b-intensity*))

(defun print-bar-parameters () (format t "  Bar width ~,1f um, length ~,1f um.~%" *bar-width* *bar-length*))

(defun light-direction-end-string () (format t " in ~adirection of orientation.~%" (if *light-direction* "opposite " "")))

(defun print-moving-bar-parameters ()
  (print-bar-parameters)
  (format t "  Speed ~,1e um/ms," *light-speed*)
  (light-direction-end-string))

(defun print-moving-bar-grating-parameters ()
  (format t "~&  Bar width ~,1f um, length ~,1f um.~%" *bar-width* *bar-length*)
  (format t "~&  Grating speed ~,1fe um/ms," *light-speed*)
  (light-direction-end-string)
  (format t "~&  Grating spatial period = ~,1f~%" *grating-spatial-period*))

(defun print-moving-sine-grating-parameters ()
  (format t "  Grating speed [microns per millisecond]= ~,1e, " *light-speed*)
  (light-direction-end-string)
  (format t "  Grating spatial period = ~,1f ~%" *grating-spatial-period*))

(defun print-moving-spot-parameters ()
  (format t "  Spot speed ~,1e um/ms, " *light-speed*)
  (light-direction-end-string))

#|
(defun parse-*SYNAPSE-NAMES-TO-DO-FIRST* (&optional (*SYNAPSE-NAMES-TO-DO-FIRST* *SYNAPSE-NAMES-TO-DO-FIRST*))
  (flatten-list
   (loop for name in *SYNAPSE-NAMES-TO-DO-FIRST* collect
	 (or (element-name name 'synapse)
	     (element-name (cell-element-elements name 'synapse))))))
|#

(defun print-spot-and-annulus-parameters ()
  (if *fast-full-field-spot*
      (progn
;        (format t "   Fast RF Full Field Spot (TM) convolution used - reference synapse(s):~%")
;        (loop for name in (parse-*SYNAPSE-NAMES-TO-DO-FIRST*) do (format t "~A " name))
	(format t "    Full Field Spot~%"))
      (progn
	(format t "  Spot outside diameter ~,1f um, " *spot-outside-diameter*)
	(format t "inside diameter ~,1f um~%" *spot-inside-diameter*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-connectivity (source-cell-type destination-cell-type synapse-type)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((pre-syn-flag nil)
	 (source-cell-type (element source-cell-type 'cell-type)	 )
	 (destination-cell-type (element destination-cell-type 'cell-type))
	 ;;	 (source-cells (cell-type-cells source-cell-type))
	 ;;	 (destination-cells (cell-type-cells destination-cell-type))
	 (total-gbar 0.0d0))
    (declare (double-float total-gbar))
    (loop for syn in (synapses-of-type synapse-type)
	  when (cell-element-p (synapse-pre-synaptic-element syn))
	  do (setq pre-syn-flag t)
	  when (let ((synapse-cell (element-cell (synapse-cell-element syn))))
		 (and (eq (cell-type synapse-cell) destination-cell-type)
		      ;; (member synapse-cell destination-cells)
		      (or (and (not (CELL-ELEMENT-P (synapse-pre-synaptic-element syn)))
			       (equal source-cell-type destination-cell-type))
			  (and (CELL-ELEMENT-P (synapse-pre-synaptic-element syn))
			       (eq (cell-type (element-cell (synapse-pre-synaptic-element syn))) source-cell-type))
			  ;;   (member (element-cell (synapse-pre-synaptic-element syn))  source-cells)
			  )))
	  do (setq total-gbar (the df (+ total-gbar (the df (synapse-gbar syn))))))
    (unless (= 0.0 total-gbar)
      (if pre-syn-flag
	(format t "Total gbar of synapse type ~a (cell type ~A -> cell type ~a): ~,2euS~%"
		(synapse-type-name synapse-type) (cell-type-name source-cell-type)
		(cell-type-name destination-cell-type)
		(s-flt total-gbar))
	(format t "Total gbar of synapse type ~a (-> cell type ~a): ~,2euS~%"
		(synapse-type-name synapse-type)
		(cell-type-name destination-cell-type)
		(s-flt total-gbar))))))

#|
LG Correct print-total-connectivity 28.08.2016

(defun print-total-connectivity ()
  (let ((cell-types (cell-types)))
    (mapcar (lambda (destination-cell-type)
	      (mapcar (lambda (source-cell-type)
			(mapcar (lambda (synapse-type)
				  (print-connectivity source-cell-type destination-cell-type synapse-type))
				(synapse-types)))
		      cell-types))
	      cell-types))))

|#

(defun print-total-connectivity ()
  (let ((cell-types (cell-types)))
    (mapcar (lambda (destination-cell-type)
	      (mapcar (lambda (source-cell-type)
			(mapcar (lambda (synapse-type)
				  (print-connectivity source-cell-type destination-cell-type synapse-type))
				(synapse-types)))
		      cell-types))
	      cell-types)))

(defun print-spaced-simulation-header ()
  (print-simulation-header)
  (format t "~%~%~%~%"))

(defun print-simulation-header ()
  (format t "~%***********************************************~% ")
  (decode-time-stamp)
  (when (> (length *username*) 0) (format t "   User: ~A" *username*))
  (format t "~%***********************************************~% ")
  (when *circuit-loaded*
    (if (> (length *simulation-name*) 0)
      (format t "~%Simulation '~a' " *simulation-name*)
      (when (> (length *circuit*) 0) (format t "~%Circuit '~a' " *circuit*)))))

(defun print-VARIABLES-setq-form (vars &optional wrapper)
  (let ((vars (coerce-to-list VARS)))
    (when wrapper
      (format t "(~A~%" wrapper))
    (loop for var in vars
	  for count from 1
	  do
	  (format t "~:[~; ~]~:[(setq~;     ~] ~a ~s"
		  wrapper
		  (/= count 1)
		  (symbol-name var)
		  (quote-symbol-cons (symbol-value var)))
	  when (< count (length vars)) do (format t "~%")
	  finally (format t "~:[~;)~])~%" wrapper)))
  )
(defun get-documentable-user-variables (&optional (all-new-vars *DOCUMENT-all-new-VARIABLES*))
  (loop for var in (delete-duplicates (concatenate 'list (when all-new-vars (get-new-surf-variable-symbols))
						   (when (consp *DOCUMENTED-USER-VARIABLES*) *DOCUMENTED-USER-VARIABLES*)))
	when (boundp var) collect var))

(defun print-DOCUMENTED-USER-VARIABLES () (when *enable-print-DOCUMENTED-USER-VARIABLES* (print-VARIABLES (get-documentable-user-variables t))))

(defun print-circuit (&optional (description-level *SIMULATION-PRINT-DETAIL*))
  "Print out circuit details.
The level of detail is given by the optional DESCRIPTION-LEVEL [default *SIMULATION-PRINT-DETAIL*]."
  ;; (unless *simulation-finished* (UPDATE-SIMULATION-TIME-STAMP-AND-NAME))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((description-level
	 (cond (description-level description-level)
	       (*print-full-to-lisp* (if *include-segments-w-full-description* :FULL_With_SEGMENTS :full))
	       (*print-mini-to-lisp* :terse)
	       (t (or *simulation-print-detail* :none)))))
    (case description-level
      (:specific_elements (information-for-elements-menu))
      (t (print-simulation-header)
	 (unless (or (not *circuit-loaded*) *simulation-finished*) (format t "** Loaded circuit not simulated yet **~%"))
	 (if (= (length *circuit*) 0)
	     (format t "~%** No circuit loaded **~%")
	     (unless (or (eq description-level :none) (= (hash-table-count (CELL-HASH-TABLE)) 0))
	       (when (> (length *loaded-circuit-parts*) 1)
		 (format t (let ((string "[Composed from:"))
			     (if (> (length *loaded-circuit-parts*) 8)
				 (setq string (format nil "~A ~A ... ~A" string (first *loaded-circuit-parts*) (car (last *loaded-circuit-parts*))))
				 (loop for name in *loaded-circuit-parts* do (setq string (format nil "~A ~A" string name))))
			     (concatenate-strings string "]~%"))))
	       (cond
		 (*multiple-source-circuit* (format t "Multiple sources defined this circuit.~%"))
		 (*input-is-function* (format t "[Compiled function: ~a]~%" *circuit*))
		 ((> (length *circuit-file*) 0) (format t "[File: ~a]~%" (concatenate-strings *circuit-directory* *circuit-file*))))
	       (when (and *neuron-tree-consolidated* (not (eq description-level :terse)))
		 (format t "Trees consolidated with a max electrotonic length of ~A~%" *maximum-electrotonic-length*))
	       (format-hash-table-count (CELL-TYPE-HASH-TABLE) "cell type")
	       (when *circuit-loaded*
		 (format-hash-table-count (CELL-HASH-TABLE) "cell")
		 (format t "~a nodes. " (1- (hash-table-count (NODE-HASH-TABLE))))
		 (if *ignore-q10*
		     (format t "Temperature dependence ignored.")
		     (format t "Temperature ~,2f degrees(C)." (- *Temperature* 273.16)))
		 (format t " ~a ms simulation.~%" *user-stop-time*))
	       (if *circuit-loaded*
		   (format t "There ~A " (if (> (length (somas)) 1) "are" "is"))
		   (format t "Loaded elements "))
	       (when *circuit-loaded*
		 (format-hash-table-count (SOMA-HASH-TABLE) "soma")
		 (format-hash-table-count (AXON-HASH-TABLE) "axon")
		 (format-hash-table-count (VSOURCE-HASH-TABLE) "voltage source")
		 (format-hash-table-count (ISOURCE-HASH-TABLE) "current source"))
	       (format-hash-table-count (CHANNEL-TYPE-HASH-TABLE) "channel type")
	       (when *circuit-loaded*
		 (format-hash-table-count (CHANNEL-HASH-TABLE) "channel")
		 (when *channel* (format t "~%")))
	       (format-hash-table-count (PARTICLE-TYPE-HASH-TABLE) "voltage-dep particle type")
	       (when *circuit-loaded*
		 (format-hash-table-count (PARTICLE-HASH-TABLE) "voltage-dep particle")
		 (when *particle* (format t "~%")))
	       (format-hash-table-count (CONC-PARTICLE-TYPE-HASH-TABLE) "concentration-dep particle type")
	       (when *circuit-loaded*
		 (format-hash-table-count (CONC-PARTICLE-HASH-TABLE) "concentration-dep particle")
		 (when *conc-particle* (format t "~%")))
	       (format-hash-table-count (CONC-INT-TYPE-HASH-TABLE) "concentration integrator type")
	       (when *circuit-loaded*
		 (format-hash-table-count (CONC-INT-HASH-TABLE) "concentration integrator")
		 (when *conc-int* (format t "~%")))
	       (format-hash-table-count (PUMP-TYPE-HASH-TABLE) "pump type")
	       (when *circuit-loaded* (format-hash-table-count (PUMP-HASH-TABLE) "pump"))
	       (format-hash-table-count (BUFFER-TYPE-HASH-TABLE) "buffer type")
	       (when *circuit-loaded*
		 (format-hash-table-count (BUFFER-HASH-TABLE) "buffer")
		 (when (or *pump* *buffer*) (format t "~%")))
	       (format-hash-table-count (SYNAPSE-TYPE-HASH-TABLE) "synapse type")
	       (when *circuit-loaded*
		 (format-hash-table-count (SYNAPSE-HASH-TABLE) "synapse")
		 (when *synapse* (format t "~%")))
	       ;; (format-hash-table-count (ELECTRODE-HASH-TABLE) "electrode")
	       (FORMAT-THING-LIST-COUNT (electrodes) "electrode")
	       (format-hash-table-count (EXTRACELLULAR-ELECTRODE-HASH-TABLE) "extracellular electrode")
	       (format t "and ")
	       (if *segment*
		   (format-hash-table-count (SEGMENT-HASH-TABLE) "segment" t (- (length (ELECTRODEs))))
		   (format t "no segments."))
	       (unless (eq description-level :terse)
		 (let ((num-soma-segs (loop for cell in (cells) sum (length (soma-segments cell)))))
		   (when (> num-soma-segs 0) (format t "(~A segment~:p assigned to soma~p.)~%" num-soma-segs (length (somas))))))
	       (when (and (channels) (not *enable-channels*)) (format t " ** All channels blocked **~%"))
	       (when (and (synapses) (not *enable-synapses*)) (format t " ** All synapses blocked **~%"))
	       (format t "~% ~%")
	       (print-circuit-elements description-level)
	       (when (and *enable-light* (are-there-light-synapses)) (PRINT-LIGHT-stimulus))
	       (when (and (synapses) (or (eq description-level :medium) (eq description-level :terse)))
		 (count-active-synapses)
		 (PRINT-TOTAL-CONNECTIVITY))
	       (unless (or (eq description-level :terse) (eq description-level :none)) (print-DOCUMENTED-USER-VARIABLES))
	       (when *include-simulation-annotation* (format t "~A~%" *simulation-annotation*))
	       (format t "~%")))))
    nil))

(defvar *maximum-num-elements-for-terse-print-circuit-elements* 5)

(defun print-circuit-elements (description-level)
  (loop for name
	in (flatten-list
	    `(list cell-type isource-type isource vsource-type vsource
		   ,(when *circuit-loaded*
		      (case description-level
			((:medium :FULL_With_SEGMENTS :full)
			 `(cell synapse-type channel-type particle-type conc-particle-type conc-int-type pump-type buffer-type axon-type extracellular-electrode
				,(case description-level
				   ((:FULL_With_SEGMENTS :full)
				    `(synapse channel particle conc-particle conc-int pump buffer soma axon
					      ,(case description-level
						 (:FULL_With_SEGMENTS `segment)))))))))))
	do
	(let ((mod (element-model name)))
	  (when (and mod
		     (> (hash-table-count (model-hash-table mod)) 0)
		     (or (not (eq description-level :terse))
			 (< (hash-table-count (model-hash-table mod)) *maximum-num-elements-for-terse-print-circuit-elements*)))
	    (let ((print-routine (if (and (model-short-print-routine mod)
					  (or (eq description-level :medium)
					      (eq description-level :terse)))
				   (model-short-print-routine mod)
				   (model-print-routine mod)))
		  element-printed)
	      (when print-routine
		(loop for elt being the hash-value of (model-hash-table mod) do ; this is really obsolete??
		      (when (or (extracellular-electrode-p elt) (instance-in-cell elt))
			(setq element-printed t)
			(format t "  ")
			(funcall print-routine elt)
			(format t "~&")))
		(when element-printed (format t  "~%"))))))))

(defun print-model-parameters (model-name)
  (loop for mod in (models)
	when (and (model-print-routine mod) (equal model-name (model-name mod)))
	do (return (loop for elt being the hash-value of (model-hash-table mod) do (funcall (model-print-routine mod) elt)))))

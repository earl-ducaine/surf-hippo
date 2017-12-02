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


;;; SYS Source file: isource.lisp

;; the isource model

(in-package "SURF-HIPPO")

(defun isource-enabled (src) (not (isource-blocked src)))

(defun get-isource-current (source) (if (isource-blocked source) 0.0 (isource-current source)))

(defun isource-current-value (source)
  (let ((src (element source 'isource)))
    (when src (get-isource-current src))))

(defun edit-isource (source)
  (let* ((isrc (element source 'isource))
	 (name (element-name source 'isource))
	 (dummy1 (isource-resistance isrc))
	 dummy2 (dummy3 t)
	 (dummy4 (element-parameter isrc :enable-isource-drop))
	 (dummy5 (element-parameter isrc :enable-bridge-balance))
	 (dummy6 (or (element-parameter isrc :bridge-balance) 0.0))

	 (dummy11 (or (element-parameter isrc 'pulse-transition-constraint) :fixed-slope))
	 (dummy12 (or (element-parameter isrc 'pulse-transition-slope) *pwl-isource-di-dt*))
	 (dummy13 (or (element-parameter isrc 'pulse-transition-time) *minimum-source-transition-time*))
	 (dummy15 (isource-blocked isrc))
	 (autonomous-source-p (eq :autonomous (isource-type-class (isource-type isrc)))))
    (loop while (or dummy3 (= *pwl-isource-di-dt* 0))
	  do (setq dummy3 nil
		   dummy2 nil)
	  (choose-variable-values
	   `((dummy15 "Block this source" :boolean)
	     (dummy1 "Current Source Resistance [Mohms]" :float)
	     (dummy4 "Add isource internal resistance drop to node voltage" :boolean)
	     (dummy5 "Subtract bridge balance drop from node voltage" :boolean)
	     (dummy6 "Bridge balance [mohms]" :float)
	     ,(when autonomous-source-p `(dummy11 "Transition between pulse stimuli defined by:" :choose (:fixed-slope :fixed-transition-time)))
	     ,(when autonomous-source-p `(dummy12 "Pulse transition slope (nA/msec)" :float))
	     ,(when autonomous-source-p `(dummy13 "Pulse transition time (msec)" :float))
	     ,(when autonomous-source-p `(dummy2 "Edit stimulus" :boolean)))
	   :label (format nil "Setting up parameters of current source ~A" name)
	   :text (when (= *pwl-isource-di-dt* 0) (format nil "Slope cannot be 0!")))
	  (setf (isource-blocked isrc) dummy15)
	  (element-parameter isrc :enable-isource-drop dummy4)
	  (element-parameter isrc :enable-bridge-balance dummy5)
	  (element-parameter isrc :bridge-balance dummy6)
	  (element-parameter isrc 'pulse-transition-constraint dummy11)
	  (element-parameter isrc 'pulse-transition-slope dummy12)
	  (element-parameter isrc 'pulse-transition-time dummy13)
	  (when dummy2 (edit-source-stimulus source))
	  (setf (isource-resistance isrc) dummy1))))

(defun menu-for-isources ()
  (loop for name in (select-hash-values-menu (ISOURCE-HASH-TABLE) "Select Current Sources" :punt-if-only-one-entry t) do (edit-isource name)))

(defun document-isource (&optional (isrc *isource*) (circuit-dump *document-elements-for-circuit-dump*))
  (let* ((isource (element isrc 'isource))
	 (isource-name (element-name isrc))
	 (cell-name (cell-name (element-cell isource)))
	 (massaged-isource-name (if circuit-dump (massage-dumped-cell-elt-name isource-name cell-name) isource-name))
	 (target-name (element-name (element-cell-element isource)))
	 (massaged-target-name (if circuit-dump (massage-dumped-cell-elt-name target-name cell-name) target-name)))
    (when isource
      (format t "#|~%")
      (print-isource isource)
      (format t "|#~%")
      (print `(add-isource 
	       ,(quote-symbol-cons massaged-target-name)
	       :name ,(quote-symbol-cons massaged-isource-name)
	       :type ,(quote-symbol-cons (element-name (isource-type isource)))))
      (when (element-parameter isource 'waveform-function)
	(document-waveform-function isource massaged-isource-name))
      (when (isource-use-pulse-list isource)
	(document-pulse-list isource massaged-isource-name))))
  nil)

(defun print-isource (&optional (isrc *isource*))
  (let ((isrc (element isrc 'isource)))
    (when isrc
      "Prints out the data associated with a isource."
      (format t "~a (Rint ~aMohms, slope ~anA/msec): type ~A~%"
	      (massage-element-plot-label isrc nil t) (isource-resistance isrc)
	      *pwl-isource-di-dt*
	      (isource-type-name (isource-type isrc)))
      (unless (zerop (isource-reference-magnitude isrc)) (format t " Default magnitude ~AnA~%" (isource-reference-magnitude isrc)))
      (cond
       ((isource-blocked isrc)
	(format t "  Source turned off~%"))
       ((isource-use-pulse-list isrc)
	(cond-every
	 ((and (element-parameter isrc 'enable-individual-pulses) (extract-pulse-list isrc))
	  (loop for pulse in (extract-pulse-list isrc)
		do (format t "      ~,2e nA pulse from ~,2e to ~,2e msec~%" (s-flt (+ (isource-reference-magnitude isrc) (nth 2 pulse))) (nth 0 pulse) (nth 1 pulse))))
	 ((and (element-parameter isrc 'enable-pulse-train) (extract-pulse-train-args isrc))
	  (print-pulse-train-info "nA" (extract-pulse-train-args isrc)))))
       ((extract-waveform-function isrc)
	(print-spaces t 4)
	(document-function-args (extract-waveform-function isrc))
	(format t "~%"))
       ((source-waveform-array isrc)
	(format t "  Explicit waveform~%"))
       (t
	(format t "  No stimulus~%")))
      (when (element-parameter isrc 'targets)
	(format t "  Additional targets for current output are ~A~%" (element-parameter isrc 'targets))))))

(defun get-isource-simple-name ()
  (loop for candidate from (max 1 *isource-simple-name-counter*)
	until (not (isource-hash-table candidate))
	finally (return (setf *isource-simple-name-counter* candidate))))

(defun rename-isources-simple (&optional (isources (isources)))
  "Rename ISOURCES [default all isources in circuit] with simple integer names."
  (loop for ch in (coerce-to-list (element isources 'isource)) do (set-element-name ch (get-isource-simple-name))))

(defun create-isource (cell-element &key (type 'autonomous) name node-1-cell-element targets
		       enable-pulse-train pulse-train-args
		       (use-pulse-list t) pulse-list (enable-individual-pulses t)
		       waveform-spec (WAVEFORM-time-interval *default-waveform-step*))
  ;; All cell elements associated with TARGETS will receive the current output of the isource, identical to CELL-ELEMENT.
  (let* ((type (if (isource-type-p type) type (create-isource-type type)))
	 (source-name
	  (if (and *use-simple-names* (not name))
	      (GET-ISOURCE-SIMPLE-NAME) 
	      (or name (format nil "~a-~a-isrc" (element-name cell-element) (isource-type-name type)))))
	 (cell-element (element-cell-element cell-element)))
    (or (element source-name 'isource)
	(let* ((node1 (or (element-node node-1-cell-element)
			  (create-node "Ground" :cell (cell-name (element-cell cell-element)))))
	       (node2 (element-node cell-element))
	       (isrc (make-isource :name source-name :node-1 node1 :node-2 node2
				   :type type
				   :cell-element cell-element :use-pulse-list use-pulse-list
				   :node-1-pointp (true-p node-1-cell-element) :node-2-pointp t)))
	  (case (isource-type-class type)
	    (:driven )
	    (:autonomous
	     (element-parameter isrc 'enable-pulse-train enable-pulse-train)
	     (element-parameter isrc 'enable-individual-pulses enable-individual-pulses)
	     (element-parameter isrc 'pulse-train-args pulse-train-args)
	     (element-parameter isrc 'waveform-function waveform-spec)
	     (pulse-list isrc pulse-list)
	     (when waveform-spec (add-waveform isrc :WAVEFORM-time-interval WAVEFORM-time-interval :waveform-spec waveform-spec :use-menu nil))))
	  (setq *make-node-w/elements-array* t)
	  (setf (isource-current isrc) 0.0) ; ISOURCE-CURRENT is really a macro 
	  (push isrc (node-elements node2))
	  (when (true-p node-1-cell-element) (push isrc (node-elements node1)))
	  (push-element-parameter node2 :isources source-name)
	  (setf (ISOURCE-HASH-TABLE source-name) isrc)
	  (element-parameter isrc 'targets targets)
	  (setf
	   (isource-resistance isrc)
	   (let ((source-name-and-resistance (car (member source-name *source-resistance-lists* :test #'equal :key 'caar))))
	     (if (eq (cdar source-name-and-resistance) 'isource)
		 (cdr source-name-and-resistance)
		 (progn
		   (push (cons (cons source-name 'isource) (isource-resistance isrc)) *source-resistance-lists*)
		   (float *isource-electrode-resistance*)))))
	  (setq *isource* isrc)))))

;; Backward comp
(defun create-pwl-isource (cell-element &key type name node-1-cell-element (use-pulse-list t) pulse-list
					(enable-individual-pulses t) enable-pulse-train pulse-train-args
					waveform-spec (WAVEFORM-time-interval *default-waveform-step*))
  (create-isource cell-element :type type :name name :node-1-cell-element node-1-cell-element
		  :use-pulse-list use-pulse-list :pulse-list pulse-list :enable-individual-pulses enable-individual-pulses
		  :enable-pulse-train enable-pulse-train :pulse-train-args pulse-train-args
		  :waveform-spec waveform-spec :WAVEFORM-time-interval WAVEFORM-time-interval))

(defvar *debug-isource* nil)

(proclaim '(inline accumulate-node-isource-current))
(defun accumulate-node-isource-current (node isrc &optional node-is-isource-node-1)
  (when *debug-isource* (format t "Isource ~A: ~a, old node-current ~a, " isrc (isource-current isrc) (node-current node)))
  (unless (node-has-ideal-voltage-source node)
    (setf (node-current node) (- (node-current node) (* (if node-is-isource-node-1 -1 1) (the sf (isource-current isrc))))))
  (when *debug-isource* (format t "new node-current ~a~%" (node-current node))))

(proclaim '(inline eval-isource))
(defun eval-isource (isrc)
  (unless (isource-blocked isrc)
    ;; calculate the current send the values back where they go
    (accumulate-node-isource-current (isource-node-2 isrc) isrc)
    (when (isource-node-1-pointp isrc) (accumulate-node-isource-current (isource-node-1 isrc) isrc t))))

#|
(defun eval-all-isources ()
  (loop for isrc in *isource-list* do
	(eval-isource isrc)
	(loop for target in (element-parameter isrc 'targets) do (accumulate-node-isource-current (element-node target) isrc))))
|#

(defun isource-type-active-p (type)
  (let ((type (element-type type)))
    (when type
      (and 
       (isource-type-first-element type)
       (isource-type-iterator
	(src type)
	unless (isource-blocked src)
	do (return t)
	finally (return nil))))))

(defun setup-isources ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setq *isource-type-list*
	(no-nils
	 (loop for type in (isource-types)
	       when (isource-type-active-p type)
	       collect type))))

(defun eval-all-isources ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *isource-type-list* do (eval-isource-type type)))

(defun eval-isource-type (type)
  (isource-type-iterator
   (src type)
   unless (isource-blocked src)
   do (eval-isource src)
   (loop for target in (element-parameter src 'targets) do (accumulate-node-isource-current (element-node target) src))))

(defun set-all-isources ()
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *isource-type-list* do
	(case (isource-type-class type)
	  (:driven (set-driven-isource-type type))
	  (:autonomous (set-autonomous-isource-type type)))))

(defun set-driven-isource-type (type)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((activation-function (isource-type-activation-function type)))
    (isource-type-iterator
     (src type)
     do (set-driven-isource src activation-function)))
  nil)

(defun set-driven-isource (src &optional (activation-function (isource-type-activation-function (isource-type src))))
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type (function (isource) single-float) activation-function))
  (unless (isource-blocked src) (setf (isource-current src) (the sf (funcall (the function activation-function) src))))
  nil)
   
(defun set-autonomous-isource-type (type)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (isource-type-iterator
   (src type)
   do (set-autonomous-isource src))
  nil)

(defun set-autonomous-isource (src)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (when (and (not (isource-blocked src)) (or (isource-pwl-list src) (isource-waveform-array src)))
    (let ((time (the sf (if (isource-use-pulse-list src) (*input-time*) (- (*input-time*) (isource-delay src))))))
      (unless (= 0.0 (the sf (isource-period src)))
	(setq time (the sf (float-mod (the sf time) (the sf (isource-period src))))))
      (if (isource-use-pulse-list src)
	  (setf (isource-current src) (the sf (extract-pwl-value-single time src (isource-pwl-list src))))
	  (let ((array (the (simple-array single-float *) (isource-waveform-array src))))
	    (multiple-value-bind (delayed-effective-time-integer-part delayed-effective-time-fractional-part)
		(truncate (the sf (* (isource-waveform-time-interval-inverse src) time)))
	      (declare (single-float delayed-effective-time-fractional-part)
		       (fixnum delayed-effective-time-integer-part))
	      ;; (printvars delayed-effective-time-integer-part delayed-effective-time-fractional-part)
	      (setf (isource-current src)
		    (if (and (>= time 0.0)
			     (or (< delayed-effective-time-integer-part (1- (length array)))
				 (and (= delayed-effective-time-integer-part (length array))
				      (zerop delayed-effective-time-fractional-part))))
			(progn
			  (check-element-time-step-maximum (isource-waveform-time-interval-mrt src))
			  (the sf (interpolated-array-value array delayed-effective-time-integer-part delayed-effective-time-fractional-part)))
			0.0))))))))
      
(defun isource-reference-magnitude (isource &optional value)
  (when (numberp value) (element-parameter isource 'reference-magnitude (d-flt value)))
  (or (element-parameter isource 'reference-magnitude)
      (d-flt *isource-default-reference-magnitude*)))

(defun convert-isource-type-class-sym (sym)
  (case sym
    ((auto :auto :autonomous autonomous) :autonomous)
    ((driven :driven) :driven)
    (t sym)))

(defun create-isource-type (type-symbol &optional actual-type-symbol update-parameters)
  (let* ((type (unless actual-type-symbol (if (isource-type-p type-symbol) type-symbol (ISOURCE-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (isource-type-p type-symbol) (isource-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'isource-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless original-parameters (sim-error (format nil "No isource library entry for ~A." type-symbol)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol) (sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about isource type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type (setq type (if parent-type-symbol
				(create-ISOURCE-TYPE parent-type-symbol type-symbol update-parameters)
				(make-ISOURCE-TYPE :name type-symbol))))
      (when (and update-parameters parent-type-symbol)
	(update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      (setf (isource-type-class type) (convert-isource-type-class-sym (get-a-value 'class original-parameters)))
      (case (isource-type-class type)
	(:driven (cond-every
		  ((assoc 'activation-function original-parameters)
		   (setf (isource-type-activation-function type) (compile-or-extract-function-or-number (get-a-value 'activation-function original-parameters))))
		  ((assoc 'rf-function original-parameters)
		   (setf (isource-type-rf-function type) (compile-or-extract-function-or-number (get-a-value 'rf-function original-parameters))))))
	(:autonomous))
      (setf (ISOURCE-TYPE-HASH-TABLE type-symbol) type))
    (setq *isource-type* type)
    type))




  


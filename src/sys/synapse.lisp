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


;;; SYS Source file: synapse.lisp

(in-package "SURF-HIPPO")

(defun synapse-pre-synaptic-node (syn)
  (let ((pre-synaptic-elt (synapse-pre-synaptic-element syn)))
    (typecase pre-synaptic-elt
      (axon (axon-node pre-synaptic-elt))
      (soma (soma-node pre-synaptic-elt))
      (segment (segment-node-2 pre-synaptic-elt))
      (t (element-physical-node pre-synaptic-elt)))))
     
(defun synapse-cell (syn)
  (let ((cell-elt (synapse-cell-element syn)))
    (typecase cell-elt
      (soma (soma-cell cell-elt))
      (segment (segment-cell cell-elt))
      (t (the cell *cell*)))))

;; This must be called after fixnum-delay has been calculated in SETUP-VOLTAGE-SYNAPSES.
(defun STORE-MAX-DELAY-FOLLOWERS (voltage-synapses)
  (declare (optimize (speed 3)))
  (loop for syn in voltage-synapses when (eq syn (synapse-event-generator syn)) do ; Equivalent to VENT-GENERATOR-P.
	;; Note the UPDATE arg was not necessary in the original version.
	(element-parameter syn 'MAX-FIXNUM-DELAY-FOLLOWERS (loop for foll in (event-followers syn) maximize (synapse-fixnum-delay foll)))))

(defun light-synapse-type-p (type)
  (let ((type (element type 'synapse-type)))
    (when type
      (or (eq (synapse-type-control type) :light)
	  (eq (synapse-type-control type) :light-event)))))

(defun ONE-SYNAPSE-OF-TYPE-P (type) (synapse-type-first-element type))

(defun SYNAPSE-ACTIVE-P (synapse &optional fast)
  "T when a SYNAPSE satisfies the condition for being ACTIVE: the synapse or type is not blocked, the
synapse conductance is not 0.0, and for a light synapse, it is within the aperture. If SYNAPSE is a
pointer to a synapse, then the FAST flag may be used."
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let ((synapse (if fast synapse (element synapse 'synapse))))
    (and synapse
 	 (case (synapse-type-control (synapse-type synapse))
	   (:light (test-for-aperture synapse))
	   (:event (synapse-event-times synapse))
	   (t t)) 	
	 (not (or (synapse-blocked synapse)
		  (synapse-type-blocked (synapse-type synapse))
		  (= (synapse-gbar synapse) 0.0d0))))))
  
(defun synapse-type-active-p (type)
  (let ((type (element-type type 'synapse-type)))
    (and type
	 (not (synapse-type-blocked type))
	 (synapse-type-iterator
	  (syn type)
	  when (synapse-active-p syn t) do (return t)
	  finally (return nil)))))

(defun IS-THERE-ONE-ACTIVE-SYN-OF-TYPE (type) (synapse-type-active-p type))

(defun number-SYNAPSE-TYPE-SYNAPSES (type &optional only-active)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let* ((type (element-type type 'synapse-type))
	 (not-light (not (light-synapse-type-p type)))
	 (result 0))
    (declare (fixnum result))
    (synapse-type-iterator
     (syn type)
     do (when (or (not only-active)
		  (or not-light (synapse-wave-ref syn))
		  (synapse-active-p syn))
	  (incf result)))
    result))

(defun NB-ACTIVE-CHS-OF-TYPE (type)
  "Returns the number of synapses of the associated with TYPE and satisfying the conditions for being active."
  (number-SYNAPSE-TYPE-SYNAPSES type t))

(defun SYNAPSE-TYPE-SYNAPSES (type &optional only-active)
  (let* ((type (element type 'synapse-type))
	 (not-light (not (light-synapse-type-p type))))
    (when (and type (not (and only-active (synapse-type-blocked type))))
      (loop with syn = (synapse-type-first-element type)
	    while syn
	    when (or (not only-active)
		     (and (or not-light (synapse-wave-ref syn))
			  (not (synapse-blocked syn))
			  (not (= (synapse-gbar syn) 0.0d0))))
	    collect syn into syns
	    do (setq syn (synapse-next-element syn))
	    finally (return syns)))))

(defun last-synapse (syn) (if (and syn (synapse-next-element syn)) (last-synapse (synapse-next-element syn)) syn))
(defun last-synapse-of-type (type) (last-synapse (synapse-type-first-element type)))

(defun clear-synapses-of-type (type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setf (synapse-type-first-element type) nil)
  (loop for syn being the hash-value of (SYNAPSE-HASH-TABLE) when (eq type (synapse-type syn)) do (setf (synapse-next-element syn) nil)))

(defun get-synapses-of-type (type) (loop for syn being the hash-value of (SYNAPSE-HASH-TABLE) when (eq type (synapse-type syn)) collect syn))

(defun ACTIVE-SYNAPSES (type)
  "List of all active synapses of the given TYPE."
  (when (synapse-type-first-element type)
    (loop with var = (synapse-type-first-element type) while var
	  when (synapse-active-p var)  collect var into syns
	  do (setq var (synapse-next-element var))
	  finally (return syns))))

(defun document-synapse-type (type)
  (let ((type (element type 'synapse-type)))
    (when type
      (format t "(synapse-type-def~%")
      (format t " (~a~%" (element-name type))
      (format t "  (control . ~s)~%" (synapse-type-control type))
      (document-iv-parameters type)
      ;;	  (format t "        (reference-temp . ~a)~%" (synapse-type-reference-temp type))
      
      (loop for param in  (synapse-type-parameters type)
	    unless (or (let ((val (if (listp (cdr param)) (cadr param) (cdr param))))
			 (arrayp val))
		       (case (car param)
			 ;; don't document stuff that may be changed via the slot values (above), or are calculated at run time from other args.
			 ((parent-type parent-types
			   gbar-density gbar permeability permeability-density
			   control IV-RELATION-COEFFICIENTS iv-density IV-MODULATION 
			   EFFECTIVE-DEFAULT-EXTRACELLULAR-CONCENTRATION EFFECTIVE-DEFAULT-INTRACELLULAR-CONCENTRATION
			   EFFECTIVE-DEFAULT-VALENCE-FN EFFECTIVE-DEFAULT-VALENCE
			   delta-waveform LAST-WAVE-INPUT-TIME-INTEGER-PART e-rev ion-permeabilities 
			   q10 SYNAPSE-NODE-FLOATS SYNAPSE-NODE-FIXNUMS COLOR SYNAPSES IMPULSE linear-IMPULSE waveform 
			   spatial-rf STATIC-VOLTAGE-DEPENDENCE LIGHT-RESPONSES-ARRAY LIGHT-INPUTS-LIST-OF-ARRAYS LIGHT-INPUTS-BASE-GAIN 
			   LIGHT-INPUTS-BASE-SHIFT LIGHT-INPUTS-1ST RF-SHAPE)
			  t)))
	    do (if (not (proper-list-p param))
		 (format t "  ~s ~%" param)
		 (format t "  (~s . ~s) ~%" (car param) (cdr param)))
		 )
      (print-element-document-extras type)
      (format t "                   ))~%")
      (format t "~%"))))

#|
(defun document-synapse (elt &optional (include-events *include-events-in-element-documentation-code*))
  (let ((synapse (element elt 'synapse)))
    (when synapse
      (let* ((cell-elt-name (element-name (element-cell-element synapse)))
	     (pre-syn-ell-elt-name (element-name (synapse-pre-synaptic-element synapse)))
	     (type-name (element-name (element-type synapse)))
	     (create-form (format nil "(create-synapse ~s ~a~a)" cell-elt-name
				  (type-name-format-input type-name)
				  (if pre-syn-ell-elt-name (format nil " ~S" pre-syn-ell-elt-name) ""))))
	(if (and include-events (eq :event (synapse-type-control (synapse-type synapse))))
	  (format t "(events ~a `~a)~%" create-form (events synapse))
	  (format t "~a~%" create-form))))))
|#

(defun document-synapse (&optional (syn *synapse*) (include-events *include-events-in-element-documentation-code*) (circuit-dump *document-elements-for-circuit-dump*))
  (let* ((synapse (element syn 'synapse))
	 (target-name (element-name (element-cell-element synapse)))
	 (pre-synaptic-target-name (element-name (pre-synaptic-element synapse)))
	 (cell-name (cell-name (element-cell synapse))))
    (when synapse
      (let ((create-form `(create-synapse
			   ,(quote-symbol-cons (if circuit-dump (massage-dumped-cell-elt-name target-name cell-name) target-name))
			   ,(quote-symbol-cons (element-name (element-type synapse)))
			   ,@(when pre-synaptic-target-name
				   `(,(quote-symbol-cons (if circuit-dump (massage-dumped-cell-elt-name pre-synaptic-target-name cell-name) pre-synaptic-target-name)))))))
	(if (and include-events (eq :event (synapse-type-control (synapse-type synapse))))
	    `(events ,create-form ',(events synapse))
	    create-form)))))

(defun edit-receptive-field-center (syn)
  (let ((dummy1 (element-parameter syn 'LIGHT-input-x))
	(dummy2 (or (element-parameter syn 'LIGHT-input-x)
		    (case *light-stimulus-plane*
		      (:xy (first (where syn)))
		      (:xz (first (where syn))))))
	(dummy3 (element-parameter syn 'LIGHT-input-y))
	(dummy4 (or (element-parameter syn 'LIGHT-input-y)
		    (case *light-stimulus-plane*
		      (:xy (second (where syn)))
		      (:xz (- (third (where syn))))))))
    (choose-variable-values
     `((:comment ,(format nil "~A location is ~a" (synapse-name syn) (where syn)))
       (:comment ,(format nil "Light stimulus is now mapped from~%[X,Y] (light coordinates) -> [X,~a] (anatomical coordinates)"
			  (case *light-stimulus-plane* (:xy "Y") (:xz "-Z"))))
       (dummy1 "Assign explicit light plane X RF center" :boolean)
       (dummy2 "Explicit light plane X RF center" :number)
       (dummy3 "Assign explicit light plane Y RF center" :boolean)
       (dummy4 "Explicit light plane Y RF center" :number))
     :label (format nil "Edit Receptive Field Location for  ~a" (synapse-name syn)))
    (when dummy1 (set-synapse-rf-center-x SYN dummy2))
    (when dummy3 (set-synapse-rf-center-y SYN dummy4)))
  nil)

(defun edit-synapse (syn &optional called-from-type-menu)
  (let ((syn (element syn 'synapse)))
    (when syn
      (set-synapse-parameters syn)
      (let ((dummy1 (synapse-gbar/perm-reference-value syn))
	    ;; (or (element-parameter syn 'iv-reference) 0.0)
	    ;; (dummy2 (or (element-parameter syn 'iv-density) 0.0))
	    (dummy3 (synapse-inherit-parameters-from-type syn))
	    ;; (dummy4 (or (element-parameter syn 'iv-source) :density))
	    dummy5
	    (dummy6 (synapse-blocked syn))
	    (dummy7 (if (numberp (car (synapse-event-times syn))) (car (synapse-event-times syn)) 0.0))
	    dummy8 dummy9)
	(choose-variable-values
	 `((:comment ,(format nil "Current gbar/perm is ~,2e uS or cm3/s" (synapse-gbar syn)))
	   (dummy1 "Absolute conductance/permeability [uS or cm3/s]" :float)
	   ;; '(dummy2 "Conductance density [pS per sq uM]" :float)
	   (:comment "If type ignored, then channel defined with absolute cond/perm")
	   (dummy3 "Ignore parameters listed above and inherit from type" :boolean)
	   ,(when (eq :light (synapse-type-control (synapse-type syn))) `(dummy9 "Edit receptive field center" :boolean))
	   ,(when (eq :event (synapse-type-control (synapse-type syn))) `(dummy8 "Set activation time to single event or process:" :choose (:Single :process)))
	   ,(when (eq :event (synapse-type-control (synapse-type syn))) `(dummy7  "Synapse delay [ms] (when choosing single event)" :float))
	   (dummy6 "Block this synapse" :boolean)
	   ,(unless called-from-type-menu '(dummy5 "Edit Synapse type" :boolean)))
	 :label (format nil "Edit Synapse ~a" (synapse-name syn))
	 :text "Conductance values are at type conductance reference temperature")
	(when dummy9 (edit-receptive-field-center syn))
	(case dummy8
	  (:single (setf (synapse-event-times syn) (list dummy7)))
	  (:process (edit-synapse-event-times syn)))
	(setf (synapse-blocked syn) dummy6
	      (synapse-inherit-parameters-from-type syn) dummy3)
	(setf (synapse-gbar/perm-reference-value syn) (d-flt dummy1))
	;;      (unless (synapse-inherit-parameters-from-type syn)
	;;        (case dummy4
	;;          (:absolute (set-element-iv-reference syn dummy1))
	;;          (:density (set-element-iv-density syn dummy2))))
	(when dummy5 (menu-for-synapse-types (synapse-type syn))))
      (set-synapse-parameters syn))))

(defun edit-synapse-type (type) (menu-for-synapse-types type))

(defun convert-synapse-type-control-sym (sym)
  (case sym
    ((auto :auto :event event) :event)
    ((light-auto :light-auto :light-event light-event) :light-event)
    ((tonic :tonic) :tonic) 
    ((voltage :voltage) :voltage)
    (t sym)))


(defun create-synapse-type (type-symbol &optional actual-type-symbol update-parameters)
  "TYPE-SYMBOL is a symbol or synapse type; in the former case it must match the CAR of one of the lists contained in synapse type
model parameter library. Returns the synapse type structure, whether is was already made or not. If the type was already made, and
UPDATE-PARAMETERS is T, the type will be updated according to the current description loaded in parameter library."
  (let* ((type (unless actual-type-symbol (if (synapse-type-p type-symbol) type-symbol (SYNAPSE-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (synapse-type-p type-symbol) (synapse-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'synapse-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about synapse type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type (setq type (if parent-type-symbol
				(create-SYNAPSE-TYPE parent-type-symbol type-symbol update-parameters)
				(make-SYNAPSE-TYPE :name type-symbol))))
      (when parent-type-symbol (clear-up-iv-relation-references-from-parent-type type original-parameters))
      (when update-parameters
	(when parent-type-symbol (update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type))
	(clean-up-membrane-element-type-parameters-to-be-updated type original-parameters))
      ;; Add ORIGINAL-PARAMETERS to the existing type element parameters.
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))
      ;; (when (and parent-type-symbol (not update-parameters)) (push-element-parameter type 'parent-types parent-type-symbol))
      ;; Go through the type-parameters to set various synapse-type structure slots.
      (setf (synapse-type-use-defined-e-rev type) t)
      (cond-every
       ((assoc 'evaluation-function original-parameters)
	(setf (synapse-type-evaluation-function type) (compile-or-extract-function-or-number (get-a-value 'evaluation-function original-parameters))))
       ((assoc 'control original-parameters)
	(setf (synapse-type-control type) (convert-synapse-type-control-sym (get-a-value 'control original-parameters))))
       ((assoc 'conc-int-type-params original-parameters)
	(setf (synapse-type-conc-int-type-params type) (massage-conc-int-type-params (get-a-value 'conc-int-type-params original-parameters))))
       ((assoc 'refractory-period original-parameters)
	(setf (synapse-type-refractory-period type) (get-a-sf-value 'refractory-period original-parameters)))
       ((assoc 'SUPRA-THRESHOLD-DURATION-MIN original-parameters)
	(setf (synapse-type-SUPRA-THRESHOLD-DURATION-MIN type) (get-a-sf-value 'SUPRA-THRESHOLD-DURATION-MIN original-parameters)))
       ((assoc 'input-THRESHOLD original-parameters)
	(setf (synapse-type-input-THRESHOLD type) (get-a-df-value 'input-THRESHOLD original-parameters)))

       ;; Process parameters that are stored in the type-parameters a-list.

       ((assoc 'CONC-INT-TYPE-E-REV-PARAMS original-parameters)
	(element-parameter type 'CONC-INT-TYPE-E-REV-PARAMS (massage-conc-int-type-params (get-a-value 'CONC-INT-TYPE-E-REV-PARAMS original-parameters))))
       ((assoc 'IMPULSE-FUNCTION original-parameters)
	(element-parameter type 'impulse (s-flt-array (evaluate-funspec-in-a-list 'IMPULSE-FUNCTION original-parameters))))
       ((assoc 'linear-IMPULSE-FUNCTION original-parameters)
	(element-parameter type 'linear-impulse (s-flt-array (evaluate-funspec-in-a-list 'linear-IMPULSE-FUNCTION original-parameters))))

       ((assoc 'WAVEFORM-SPEC original-parameters)
	(element-parameter type 'waveform 
			   (s-flt-array (if (numeric-sequence-p (get-a-value 'waveform-spec original-parameters))
					  (get-a-value 'waveform-spec original-parameters)
					  (progn (element-parameter type 'waveform-spec (get-a-value 'waveform-spec original-parameters))
						 (evaluate-funspec-in-a-list 'waveform-spec original-parameters))))))
       
       ((assoc 'specified-waveform-breakpoints original-parameters)
	(element-parameter type 'specified-waveform-breakpoints (assoc 'specified-waveform-breakpoints original-parameters)))

       ((eq :light (synapse-type-control type)) (get-spatial-rf-array type))

       ((assoc '1st-order-Depressing-dynamics original-parameters)
	(element-parameter type '1st-order-Depressing-dynamics t)
	(element-parameter type 'tau-recovery (s-flt (or (get-a-value 'tau-recovery original-parameters) 1.0)))
	(element-parameter type 'release-fraction (or (s-flt (or (get-a-value 'release-fraction original-parameters) 1.0))))))

      (when (element-parameter type 'waveform)
      	(element-parameter type 'delta-waveform (delta-wave-array (element-parameter type 'waveform))))
      (element-parameter type 'waveform-time-interval (if (assoc 'WAVEFORM-time-interval original-parameters)
							(get-a-sf-value 'WAVEFORM-time-interval original-parameters)
							*default-waveform-step*))
      (setf (synapse-type-waveform-time-interval-inverse type) (/ 1.0 (element-parameter type 'waveform-time-interval)))
      (extract-conductance-type-parameters type original-parameters)
      (setf (SYNAPSE-TYPE-HASH-TABLE type-symbol) type))
    (setq *synapse-type* type)))

#|
(defun revamp-synapse-type-parameters (&optional type)
;;  "Update the parameters of synapse TYPE, if specified, else all synapse types, according to the current parameters in the synapse
;; type parameter libraries."
  (loop for type in (if type (list (element type 'synapse-type)) (synapse-types)) do (create-synapse-type type nil t)))
|#

(defun synapse-type-waveform-interval (type) (or (element-parameter type 'waveform-time-interval) *default-waveform-step*))

(defun add-waveform-to-synapse-type (type wave-spec waveform-time-interval) (add-waveform-to-synapse-type (type wave-spec waveform-time-interval)))

;; Supply WAVE-SPEC as either an explicit sequence or a function list. For example,
;;
;;   (synapse-type-g-wave 'GABA-Q '(alpha-array 1.0 2.0) 0.5)
;; or
;;
;;   (synapse-type-g-wave 'GABA-T '(0.0 0.2 0.5 3.0 2.3 1.2) 0.25)
;;
;; If WAVE-SPEC is a sequence, then the 'WAVEFORM-FUNCTION entry for the synapse type is NIL - otherwise the  
;; 'WAVEFORM-FUNCTION entry is set to WAVE-SPEC.
;;
;; NOTE June 11 2001. THIS FUNCTION IS BROKEN
(defun synapse-type-g-wave (type &optional wave-spec waveform-time-interval)
  (element-parameter type 'waveform-function nil)
  (let ((wave (s-flt-array (typecase wave-spec
			     (array wave-spec)
			     (cons (typecase (car wave-spec)
				     (number wave-spec)
				     (t (element-parameter type 'waveform-function wave-spec)
					(apply (car wave-spec) (cdr wave-spec)))))))))
    (element-parameter type 'waveform-time-interval waveform-time-interval)    
    (element-parameter type 'waveform wave)
    (element-parameter type 'delta-waveform (delta-wave-array wave)))
  nil)

(defun synapse-wavetype-from-function-type (synapse-type-function-sym)
  (let ((string (string synapse-type-function-sym)))
    (cond ((search "-FUNCTION" string :test 'string=) (read-from-string (string-remove-tail string (length "-function"))))
	  ((search "-SPEC" string :test 'string=) (read-from-string (string-remove-tail string (length "-spec"))))
	  (t (sim-error (format nil "synapse-wavetype-from-function-type doesn't like ~A~%" synapse-type-function-sym))))))

(defvar *synapse-type-functions* '(waveform-spec impulse-FUNCTION linear-impulse-FUNCTION spatial-rf-function waveform-function static-voltage-dependence-function))
(defvar *synapse-type-wavetypes* (loop for sym in *synapse-type-functions* collect (synapse-wavetype-from-function-type sym)))
    
(defun choose-synapse-type-functions (type)
  (let ((type (element-type type)))
    (when (synapse-type-p type)
      (choose-list-values-from-keys
       (loop for synapse-type-function in *synapse-type-functions* when (element-parameter type synapse-type-function)
	     collect (list (wave-name-from-type synapse-type-function) synapse-type-function))
       nil :label (format nil "Edit Various Waveforms for Synapse Type ~A" (synapse-type-name type))))))

(defun synapse-type-wave-from-synapse-type-function (type &optional synapse-type-function (new-wave nil new-wave-supplied-p))
  (let ((type (element-type type)))
    (when (synapse-type-p type)
      (let ((key (if synapse-type-function
		   (synapse-wavetype-from-function-type synapse-type-function)
		   (case (synapse-type-control type)
		     (:light 'impulse)
		     ((:voltage :event :light-event) 'waveform)))))
	(when key (if new-wave-supplied-p (element-parameter type key new-wave) (element-parameter type key)))))))

(defun edit-synapse-type-waveforms (type &optional (use-menu t))
  (let ((type (element type 'synapse-type)))
    (when type
      (let ((name (synapse-type-name type)))
	(loop for synapse-type-function in (choose-synapse-type-functions type) do
	      (let* ((synapse-type-function-params (element-parameter type synapse-type-function))
		     (args (cdr synapse-type-function-params))
		     (function-name (car synapse-type-function-params))
		     (wavename (wave-name-from-type synapse-type-function))
		     dummy1 dummy2)
		(loop until dummy2 do
		      (when use-menu
			(setq args (edit-function-args function-name args :extra-text (format nil "Editing args for ~a,~%synapse type ~A~%" wavename name)))
			(when (eq synapse-type-function 'waveform-function)
			  (let ((dummy3 (or (element-parameter type 'waveform-time-interval) 0.2)))
			  (choose-variable-values
			   '((dummy3 "Waveform time step:" :float))
			   :text (concatenate-string-list (list "If necessary, reconcile waveform time step according to"
								(format nil "the edited ~A function arguments:" function-name)
								(format nil "~s" args))
							  :string-count-to-add-linefeed 1)
			   :label "Reconciling Waveform Time Step")
			  (element-parameter type 'waveform-time-interval dummy3))))
		      (let* ((sequence (apply function-name args))
			     (new-wave (if (eq SYNAPSE-TYPE-FUNCTION 'SPATIAL-RF-FUNCTION) sequence (s-flt-array sequence))))
			(when (and use-menu (go-ahead-menu (format nil "Plot ~A~%for Synapse Type ~A"  wavename name)))
			  (plot-synapse-type-wave type synapse-type-function new-wave))
			(element-parameter type synapse-type-function (cons function-name args))
			(synapse-type-wave-from-synapse-type-function type synapse-type-function new-wave)
			(when (member synapse-type-function '(waveform-spec waveform-function))
			  (element-parameter type 'delta-waveform (delta-wave-array (element-parameter type 'waveform)))))
		      (setq dummy2 (go-ahead-menu (format nil "Done editing ~A~%for synapse type ~A" wavename name))))))))))
			    
(defun create-light-synapse (post-synaptic-element synapse-type-symbol &key rf-center-x rf-center-y add-rf-label-to-name)
  (let ((type (create-synapse-type synapse-type-symbol)))
    (if (not (case (synapse-type-control type)
	       ((:light :light-event) t)))
      (sim-error (format nil "~A referenced by CREATE-LIGHT-SYNAPSE is not a light/light-event type" synapse-type-symbol))
      (let ((syn (create-synapse
		  post-synaptic-element synapse-type-symbol nil nil
		  (when (and add-rf-label-to-name rf-center-x rf-center-y)
		    (format nil "Syn-~a-~a-~A-~A" (node-name (element-physical-node post-synaptic-element)) synapse-type-symbol rf-center-x rf-center-y)))))
	(cond-every (rf-center-x (set-synapse-rf-center-x syn rf-center-x))
		    (rf-center-y (set-synapse-rf-center-y syn rf-center-y)))
	syn))))

(defun get-synapse-simple-name ()
  (loop for candidate from (max 1 *synapse-simple-name-counter*) until (not (synapse-hash-table candidate))
	finally (return (setf *synapse-simple-name-counter* candidate))))

(defun rename-synapses-simple (&optional (synapses (synapses)))
  "Rename SYNAPSES [default all synapses in circuit] with simple integer names."
  (loop for syn in (coerce-to-list (element synapses 'synapse)) do (set-element-name syn (get-synapse-simple-name))))
    
;; This new version does not require a PRE-SYNAPTIC-ELEMENT.
(defun CREATE-SYNAPSE (post-synaptic-element type &optional pre-synaptic-element (add-pre-synaptic-element-name-to-name t) name)
  "Returns a synapse of TYPE, installed on the cell element associated with POST-SYNAPTIC-ELEMENT. Synapse types that are
controlled by the voltage of another node must include a PRE-SYNAPTIC-ELEMENT \(element or name associated with a soma, segment,
or axon\). If the POST-SYNAPTIC-ELEMENT (can be NIL) already has a synapse of the same type, and the PRE-SYNAPTIC-ELEMENT is
either different or not required for TYPE, then an alternate name will be created from the addition of a number at the end
of the standard synapse name. If the global variable *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* is T, then the user is prompted before
the additional synapse is created, otherwise, the synapse is created. The standard synapse name is either an integer, if
*USE-SIMPLE-NAMES* is T \(generated by GET-SYNAPSE-SIMPLE-NAME\), or given by NAME, if non-NIL, or composed from the TYPE
and the post-synaptic element, including also the name of the PRE-SYNAPTIC-ELEMENT if there is one and if
ADD-PRE-SYNAPTIC-ELEMENT-NAME-TO-NAME is T."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (unless *only-load-passive*
    (let* ((cell-element (element-cell-element post-synaptic-element))
	   (node (element-node cell-element))
	   (type (if (synapse-type-p type) type (create-synapse-type type)))
	   (type-parameters (synapse-type-parameters type))
	   (synapse-type-name (synapse-type-name type))
	   (name (cond (*use-simple-names* (get-synapse-simple-name))
		       (name name)
		       ((and pre-synaptic-element add-pre-synaptic-element-name-to-name)
			(format nil "Syn-~a-~A-~A" (node-name node) synapse-type-name (element-name pre-synaptic-element)))
		       ((not node) (1+ (hash-table-count (SYNAPSE-HASH-TABLE))))
		       (t (format nil "Syn-~a-~A" (node-name node) synapse-type-name))))
	   (pre-synaptic-element (element pre-synaptic-element))
	   (pre-synaptic-cell-element (if (axon-p pre-synaptic-element) pre-synaptic-element (element-cell-element pre-synaptic-element))) 
	   (pre-synaptic-node (typecase pre-synaptic-cell-element
				(axon (axon-node pre-synaptic-cell-element))
				(t (element-physical-node-fast pre-synaptic-cell-element))))
	   (current-syn (SYNAPSE-HASH-TABLE name)))
      (cond
       ;; Check for complete synapse information
       ((and (member (synapse-type-control type) '(:channel :voltage)) (not pre-synaptic-cell-element))
	(format t ";; CREATE-SYNAPSE aborted: Synapse ~a must have complete presynaptic info~%" name))
       ;; If *allow-duplicate-elements* is NIL, check for same type of synapse at the node.
       ((duplicate-element-check node type) nil)
       ;; Check for synapses with identical pre and post elements.
       ((and current-syn pre-synaptic-cell-element (not *allow-duplicate-synaptic-connections*)
	     (equal pre-synaptic-cell-element (synapse-pre-synaptic-element current-syn)))
	(format t "CREATE-SYNAPSE: synapse ~a already defined~%" name)
	current-syn)
       ;; Create synapse if there is not one of the given name, or an alternate name is allowed.
       ((or (not current-syn)
	    (confirm-alternate-name-creation-and-update-*PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*
	     (format nil "Make new name for duplicate synapse type ~A on ~A" synapse-type-name (if node (node-name node) "Undefined Dest."))))
	(let ((name (if current-syn (check-element-name name 'synapse) name)))
	  (when name
	    (let ((syn (make-synapse :name name :cell-element cell-element :pre-synaptic-element pre-synaptic-cell-element
				     :channel (when (get-element-parameter-fast 'channel type-parameters)
						(create-channel cell-element (get-element-parameter-fast 'channel type-parameters)
								:pre-synaptic-element pre-synaptic-cell-element))
				     :type type :inherit-parameters-from-type (synapse-type-inherit-parameters-from-type type))))
	      (setf (synapse-event-generator syn) syn) ; Only relevant for EVENT, LIGHT-EVENT, or VOLTAGE controlled synapse types.
	      (when node (push syn (node-elements node)))
	      (element-slot-fast (element-slot-fast type :last-element) :next-element syn)
	      (element-slot-fast type :last-element syn)
	      (when (and node (get-element-parameter-fast 'static-voltage-dependence type-parameters))
		(setf (node-has-v-dep-element node) t))
	      (when (and cell-element (synapse-type-conc-int-type-params type))
		(setf (synapse-conc-ints-params syn) (parse-conc-int-info-for-element type cell-element)))
	      (when (or (axon-p pre-synaptic-cell-element) (cell-element-p pre-synaptic-cell-element))
		(unless (eq node pre-synaptic-node) (push syn (node-elements pre-synaptic-node)))
		(typecase pre-synaptic-cell-element
		  (axon (setf (axon-target-synapse pre-synaptic-cell-element) syn))))
	      (set-synapse-parameters syn)
	      (setf (SYNAPSE-HASH-TABLE name) syn)
	      (setq *recheck-circuit-elements-parameters* t  *make-node-w/elements-array* t)
	      (setq *synapse* (SYNAPSE-HASH-TABLE name))))))))))

(defun synapse-in-elements-of (syn) (no-nils (list (element-physical-node syn) (element-physical-node (synapse-pre-synaptic-element syn)))))

(defun add-synapse-to-cells (cells type &optional (targeted-fraction 1.0) include-soma)
  (when (and *always-intialize-random-gen* targeted-fraction) (get-reference-random-state))
  (let ((*monitor-circuit-modifications* *monitor-circuit-modifications*))
    (when *monitor-circuit-modifications*
      (format t ";; (add-synapse-to-cells ~%;; ")
      (FORMATTED-LIST-DUMP (coerce-to-list (element-name cells)) t t)
      (format t "~%;; ~a" (element-name type))
      (when targeted-fraction (format t " ~a" targeted-fraction))
      (when include-soma (format t " t "))
      (format t "~%;; )~%"))
    (setq *monitor-circuit-modifications* nil)
    (loop for cell in (elements cells 'cell) do
          (create-element (random-nth-fraction (if include-soma (cell-elements cell) (segments cell)) targeted-fraction) type))))

(defun consolidate-synaptic-connections ()
  (loop for type in (synapse-types) do 
	(case (synapse-type-control type)
	  ((:voltage :channel)
	   (synapse-type-do
	    (syn type)
	    (let ((pre-synaptic-element (synapse-pre-synaptic-element syn)))
	      (unless (cell-element-p pre-synaptic-element)
		(sim-error (format nil "consolidate-synaptic-connections: pre-synaptic elt ~a (synapse ~a) invalid"
				   (element-name pre-synaptic-element) (synapse-name syn))))))))))

(defun get-current-synapse-type-names (&optional control)
  (loop for type in (synapse-types) when (and (synapses-of-type type) (or (not control) (eq control (synapse-type-control type))))
	collect (synapse-type-name type)))

(defun get-light-synapse-type-names () (GET-CURRENT-SYNAPSE-TYPE-NAMES :light))

(defun driven-synapses (element)
  "Return a list of synapses whose pre-synaptic cell element is that associated directly with ELEMENT."
  (let ((source-element (element-cell-element element)))
    (loop for synapse in (synapses element) when (equal source-element (synapse-pre-synaptic-element synapse)) collect synapse)))

(defun synaptic-targets (element)
  "Return a list of cell elements who are post-synaptic to the cell element associated directly with ELEMENT."
  (element-cell-element (driven-synapses element)))
	  
(defun impinging-synapses (element)
    "Return a list of synapses whose post-synaptic cell element is that associated directly with ELEMENT."
  (let ((source-element (element-cell-element element)))
    (loop for synapse in (synapses element) when (equal source-element (synapse-cell-element synapse)) collect synapse)))

(defun are-there-synapses-of-control (control &optional target-elt) ; The CONTROL arg is e.g. :EVENT, :TONIC, :LIGHT, :LIGHT-EVENT or :VOLTAGE.
  (loop for control in (coerce-to-list control)
	when (if target-elt
	       (loop for syn in (synapses target-elt) thereis (eq control (synapse-type-control (synapse-type syn))))
	       (loop for type in (synapse-types) thereis (and (eq control (synapse-type-control type)) (instance-in-cell type))))
	do (return t)))

(defun are-there-tonic-synapses (&optional target-elt) (are-there-synapses-of-control :tonic target-elt))
(defun are-there-event-synapses (&optional target-elt) (or (are-there-synapses-of-control '(:event :light-event) target-elt)))
(defun are-there-light-event-synapses (&optional target-elt) (are-there-synapses-of-control :light-event target-elt))
(defun are-there-light-synapses (&optional target-elt) (or (are-there-synapses-of-control '(:light :light-event) target-elt)))
(defun are-there-voltage-synapses (&optional target-elt) (are-there-synapses-of-control :voltage target-elt))

(defun get-synapse-types (&optional control)
  (let ((controls (coerce-to-list control)))
    (loop for type in (synapse-types) when (or (not controls) (member (synapse-type-control type) controls)) collect type)))

(defun synapse-types-of-control (control) (get-synapse-types control))
(defun synapses-of-control (control) (loop for type in (get-synapse-types control) nconcing (copy-list (synapses-of-type type))))

(defun synapses-of-type (type &optional cell-element)
  "Return a list of synapses of TYPE that are associated with members of CELL-ELEMENT [atom or list].  Members of CELL-ELEMENT may
refer explicitly to a cell type or specific cell, or may be associated with a cell element. If CELL-ELEMENT is NIL, then all
synapses of TYPE are returned."
  (let ((type (element type 'synapse-type)))
    (when type
      ;; Parse CELL-ELEMENT arg.
      (loop for elt in (coerce-to-list (element cell-element))
	    when (cell-type-p elt) collect elt into cell-types
	    when (cell-p elt) collect elt into cells
	    else collect (element-cell-element elt) into cell-elements
	    finally
	    (return
	     ;; Collect synapses associated with the cell types, cells, or cell elements of CELL-ELEMENT.
	     (loop with syn = (synapse-type-first-element type) while syn
		   when (or (not (or cell-types cells cell-elements))
			    (let ((synapse-cell (synapse-cell syn)))
			      (or (member (cell-type synapse-cell) cell-types)
				  (member synapse-cell cells)
				  (member (synapse-cell-element syn) cell-elements))))
		   collect syn into syns
		   do (setq syn (synapse-next-element syn))
		   finally (return syns)))))))

(defun synapse-types-of-ion-type (ion-type &optional only-loaded only-in-circuit exclude-conc-dependent-types)
  (if only-in-circuit
      (loop for type in (synapse-types)
	    when (and (element-in-circuit type)
		      (not (and exclude-conc-dependent-types (element-has-concentration-dependence type)))
		      (find ion-type (synapse-type-ion-permeabilities type) :key 'car))
	    collect type)
      (no-nils
       (delete-duplicates
	(loop for syn-list in (MODEL-PARAMETER-type-LIBRARY (type-symbol-model 'synapse-type))
	      when (and (find ion-type (get-a-value 'ion-permeabilities (cdr syn-list)) :key 'car)
			(not (and exclude-conc-dependent-types (get-a-value 'conc-particles (cdr syn-list)))))
	      collect (if only-loaded (element (car syn-list)) (create-synapse-type (car syn-list))))))))

(defun synapses-of-ion-type (ion-type) (loop for type in (synapse-types-of-ion-type ion-type) nconc (copy-list (synapse-type-synapses type))))
  
(defun are-there-synapses () (loop for type being the hash-value of (synapse-type-hash-table) thereis (synapse-type-first-element type)))
(defun get-current-synapse-types () (loop for type in (synapse-types) when (synapse-type-first-element type) collect type))
(defun get-current-synapse-type-names () (coerce-to-list (element-name (get-current-synapse-types))))

(defun get-current-synapse-types-menu (&optional (label "Synapse Types In Circuit"))
  (loop for name in (choose-list-values (get-current-synapse-type-names) nil :label label) collect (element name 'synapse-type)))

(defun print-synapses () (PRINT-MODEL-PARAMETERS "synapse"))

(defun print-synapse (syn)
  (let (*print-pretty* (syn (element syn 'synapse)))
    (when syn
      (format t "~a~A: type ~A, cell ~a, ~A ~A; ~a"
	      (massage-element-plot-label syn nil t)
	      (cond ((synapse-type-blocked (synapse-type syn)) " (type blocked)")
		    ((synapse-blocked syn) " (blocked)")
		    (t ""))
	      (element-name (element-type syn))
	      (cell-name (synapse-cell syn))
	      (string-downcase (string (type-of (element-cell-element syn)))) (element-name (element-cell-element syn))
	      (print-element-iv-relation-string syn))
      (case (synapse-type-control (synapse-type syn))
	((:light-event :event)
	 (let* ((events (synapse-event-times syn))
		(num-events (length events)))
	   (when events
	     (if (> num-events *MAXIMUM-SYNAPSE-PRINTED-EVENTS*)
		 (format t "~%    ~A event times (between ~a and ~Ams)" (length events) (min-of-list events) (max-of-list events))
		 (format t "~%    Event~P[ms]: ~{~a ~}" (length events) events))))))
      (when (cell-element-p (synapse-pre-synaptic-element syn))
	(format t "~%    Presynaptic element: ~a" (element-name (synapse-pre-synaptic-element syn))))
      (when (synapse-channel syn)
	(format t "~%    Channel: ~a" (channel-name (synapse-channel syn))))
      (format t "~%")
      (when *simulation-initialized*
	(format t "    Conductance ~,2e ~a, current ~,2e ~a @ ~,2e ms~%"
		(s-flt (synapse-conductance syn)) (synapse-conductance-units) (s-flt (get-synapse-current syn)) (synapse-current-units) *real-time*)))))

(defun synapse-conductance-units () (plot-list-info-units-label (find '*plot-synapse-conductances* *plot-lists-info* :key 'car)))
(defun synapse-current-units () (plot-list-info-units-label (find '*plot-synapse-currents* *plot-lists-info* :key 'car)))

(defun print-synapse-types () (PRINT-MODEL-PARAMETERS "synapse-type"))

(defun print-synapse-type (type &optional brief (always t))
  (let (*print-pretty* (type (element type 'synapse-type)))
    (when type
      (let ((syns (synapses-of-type type)))
	(when (or always syns)
	  (format t "Synapse Type ~a: " (synapse-type-name type))
	  (case (synapse-type-control type)
	    ((:light-event :LIGHT)
	     (format t "~%    Light-dependent~%    ")
	     (1d-impulse-info (element-parameter type 'IMPULSE-FUNCTION))
	     (format t "~%    Nonlinearity: ~A" (or (element-parameter type 'IMPULSE-NONLINEARITY) 'threshold))
	     (when (element-parameter type 'IMPULSE-NONLINEARITY-parameters)
	       (format t "Parameters: ~A" (element-parameter type 'IMPULSE-NONLINEARITY-parameters)))
	     (format t "~%")
	     (when (element-parameter type 'linear-IMPULSE-FUNCTION)
	       (format t "    Linear ")
	       (1d-impulse-info (element-parameter type 'linear-IMPULSE-FUNCTION)))
	     (format t "    ")
	     (if (element-parameter type 'SPATIAL-RF-FUNCTION)
	       (2d-spatial-rf-info (element-parameter type 'SPATIAL-RF-FUNCTION))
	       (format t "Light Spatial RF is 2D Impulse~%"))
	     (when (eq :light-event (synapse-type-control type))
	       (format t "    Light driven events~%")))
	    (:VOLTAGE (format t "~%    Voltage-dependent~%"))
	    (:EVENT (format t "~%    Autonomous events~%"))
	    (:tonic (format t "~%    Tonic activation~%")))
	  (when (or (element-parameter type 'waveform-function)
		    (element-parameter type 'waveform-spec)
		    (element-parameter type 'waveform))
	    (cond ((element-parameter type 'waveform-function) (format t "    Waveform function: ~s" (element-parameter type 'waveform-function)))
		  ((element-parameter type 'waveform-spec) (format t "    Waveform spec: ~s" (element-parameter type 'waveform-spec)))
		  ((element-parameter type 'waveform) (format t "    Explicit waveform")))
	    (when (element-parameter type 'waveform-time-interval)
	      (format t "  Waveform time interval: ~Ams" (element-parameter type 'waveform-time-interval)))
	    (format t "~%"))
		
	  (format t "    ")
	  (print-membrane-element-type-iv-parameters type)
	  (when (synapse-type-blocked type) (format t "    This synapse type is blocked!~%"))
	  (unless brief
	    (print-general-element-stuff type)
	    (when (element-parameter type '1st-order-Depressing-dynamics)
	      (format t " 1st-order-Depressing dynamics with Tau-recovery: ~,2e ms, Release fraction: ~,2e~%"
		      (element-parameter type 'tau-recovery) (element-parameter type 'release-fraction)))
	    (case (synapse-type-control type)
	      (:voltage
	       (format t "    Threshold: ~,2e mV, Refractory period: ~,2e ms, Supra-threshold delay: ~,2e ms~%"
		       (s-flt (synapse-type-input-threshold type))
		       (synapse-type-refractory-period type) (synapse-type-supra-threshold-duration-min type))))
	    (print-inheritance-info syns))
	  (print-num-elements-sourcefile type)
	  (format t "~%"))))))

(defun print-synapse-type-brief (type)
  (let* ((type (element type 'synapse-type))
	 (number-synapse-TYPE-synapses (number-synapse-TYPE-synapses type)))
    (format t "Synapse Type ~a: ~D child~a" (synapse-type-name type) number-synapse-TYPE-synapses
	    (if (= 1 number-synapse-TYPE-synapses) "" "ren"))
    (unless (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0)
      (format t "~%   GBAR modulation by ~,2e" (element-parameter type :IV-MODULATION)))
    (when (synapse-type-blocked type) (format t " (**BLOCKED**)"))
    (format t "~%")))

(defun print-create-synapses-for-cell (cell &optional (indent-output 0))
  (loop for synapse being the hash-value of (SYNAPSE-HASH-TABLE) when (eq (element-cell synapse) cell) do
	(when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
	(print-create-synapse synapse)))

(defun print-create-synapse (synapse) (document-synapse synapse *include-events-in-element-documentation-code*))

;; (defun block-all-synapse-types () (loop for syn in (synapse-types) do (setf (synapse-type-blocked syn) t)))
;; (defun block-synapse-types (types) (loop for type in types when (element type 'synapse-type) do (setf (synapse-type-blocked (element type 'synapse-type)) t)))
;; (defun unblock-all-synapse-types () (loop for syn in (synapse-types) do (setf (synapse-type-blocked syn) nil)))
;; (defun unblock-synapse-types (types) (loop for type in types when (element type 'synapse-type) do (setf (synapse-type-blocked (element type 'synapse-type)) nil)))

(defun unblock-or-block-synapse-types (types block)
  (loop for type in (coerce-to-list types) when (element type 'synapse-type) do (setf (synapse-type-blocked (element type 'synapse-type)) block)))

;; (defun block-synapse-types (types) (loop for type in (coerce-to-list types) when (element type 'synapse-type) do (setf (synapse-type-blocked (element type 'synapse-type)) t)))
(defun block-synapse-types (types) (unblock-or-block-synapse-types types t))

;; (defun block-all-synapse-types () (loop for syn in (synapse-types) do (setf (synapse-type-blocked syn) t)))
(defun block-all-synapse-types () (block-synapse-types (synapse-types)))

;; (defun unblock-synapse-types (types) (loop for type in (coerce-to-list types) when (element type 'synapse-type) do (setf (synapse-type-blocked (element type 'synapse-type)) nil)))
(defun unblock-synapse-types (types) (unblock-or-block-synapse-types types nil))

;; (defun unblock-all-synapse-types () (loop for syn in (synapse-types) do (setf (synapse-type-blocked syn) nil)))
(defun unblock-all-synapse-types () (unblock-synapse-types (synapse-types)))

(defun blocked-synapse-types () (loop for type in (synapse-types) when (and (instance-in-cell type) (synapse-type-blocked type)) collect type))

(defun block-all-synapses () (loop for syn in (synapses) do (setf (synapse-blocked syn) t)))
(defun unblock-all-synapses () (unblock-all-synapse-types) (loop for syn in (synapses) do (setf (synapse-blocked syn) nil)))

;; A fast version of ELEMENT-PARAMETER
(defun set-synapse-type-parameter-slot-fast (type key value params)
  (if (assoc key params)
      (rplacd (assoc key params) value)
      (setf (synapse-type-parameters type) (acons key value params))))

(defun set-synapse-types-parameters () (loop for type in (synapse-types) do (set-synapse-type-parameters type)))

(defun set-synapse-type-parameters (type)
  (fixup-type-modulation type)
  (fixup-1st-order-Depressing-params type)
  (UPDATE-g-TYPE-CELL-TYPE-IV-RELATION-COEFFICIENT type))
	
(defun set-synapses-parameters (&optional (update-fixed-e-rev t) type (update-synapse-gbars t))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for type in (or (coerce-to-list type) (synapse-types)) do
	(let ((IV-MODULATION (or (element-parameter type :IV-MODULATION) (element-parameter type 'pbar-modulation) 1.0)))
	  (synapse-type-do
	   (syn (element type 'synapse-type))
	   (set-synapse-parameters syn update-fixed-e-rev update-synapse-gbars IV-MODULATION))))
  nil)

(defun set-synapse-type-param (type param &optional value (update t))
  (atomize-list
   (loop for type in (coerce-to-list (element type 'synapse-type))
	 collect
	 (let ((return-value (when type
			       (case param
				 (IV-MODULATION (element-parameter type :IV-MODULATION (s-flt value)))
				 ((:iv-reference :permeability-ref permeability-ref :gbar-ref gbar-ref)
				  (if value
				    (setf (synapse-type-iv-reference type) (s-flt value))
				    (synapse-type-iv-reference type)))
				 ((:iv-source :gbar-source gbar-source)
				  (if value
				    (setf (synapse-type-iv-source type) value)
				    (synapse-type-iv-source type)))
				 ((:iv-density :gbar-density gbar-density)
				  (if value
				    (setf (synapse-type-iv-density type) (s-flt value))
				    (synapse-type-iv-density type)))
				 ((:e-rev e-rev) (if value
						   (setf (synapse-type-e-rev type) (s-flt value))
						   (synapse-type-e-rev type)))
				 ((:blocked blocked) (if value
						   (setf (synapse-type-blocked type) (= 1 value))
						   (synapse-type-blocked type)))))))    
	   (when (and value (not (eq value return-value)))
	     (setq *enable-synapse-membrane-parameter-update* t)
	     (if update (set-synapses-parameters nil type) (setq *recheck-circuit-elements-parameters* t)))
	   return-value))))

(proclaim '(inline get-synapse-conc-in))
(defun get-synapse-conc-in (syn &optional fast-conc-in-calculation)
  (the df (get-element-conc-ints-conc-in syn (synapse-conc-ints-params syn) fast-conc-in-calculation)))

;; mM
(proclaim '(inline get-synapse-conc-out))
(defun get-synapse-conc-out (syn &optional fast-conc-out-calculation)
  (the df (get-element-conc-ints-conc-out syn (synapse-conc-ints-params syn) fast-conc-out-calculation)))

(proclaim '(inline get-synapse-pre-synaptic-value))
(defun get-synapse-pre-synaptic-value (syn) (the df (node-voltage-n (synapse-pre-synaptic-node syn))))

(proclaim '(inline synapse-voltage))
(defun synapse-voltage (syn) (node-voltage-n (synapse-node syn)))

(proclaim '(inline synapse-voltage-n+1))
(defun synapse-voltage-n+1 (syn) (node-voltage-n+1 (synapse-node syn)))

;; Returns current in nA
(proclaim '(inline synapse-ohmic-current))
(defun synapse-ohmic-current (syn)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (* (synapse-conductance syn) (- (synapse-voltage syn) (synapse-e-rev syn))))

(proclaim '(inline synapse-ohmic-current-n+1))
(defun synapse-ohmic-current-n+1 (syn)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (* (synapse-conductance syn) (- (synapse-voltage-n+1 syn) (synapse-e-rev syn))))

(proclaim '(inline synapse-constant-field-current))
(defun synapse-constant-field-current (syn &key permeability conc-in conc-out valence)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (* (the df (or permeability (synapse-conductance syn)))
     (the df (constant-field-current-coefficient
	      syn
	      :conc-in-double conc-in :conc-out-double conc-out
	      :valence valence
	      :voltage-double (node-voltage-n (synapse-node syn))))))

(proclaim '(inline synapse-constant-field-current-dumb))
(defun synapse-constant-field-current-dumb (syn &key permeability conc-in conc-out valence voltage)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (* (the df (or permeability (synapse-conductance syn)))
     (the df (constant-field-current-coefficient
	      syn
	      :conc-in-double conc-in :conc-out-double conc-out :valence valence
	      :voltage-double (if voltage (d-flt voltage) (node-voltage-n (synapse-node syn)))))))

(proclaim '(inline synapse-constant-field-current-n-1))
(defun synapse-constant-field-current-n-1 (syn &key permeability conc-in conc-out valence)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (synapse-constant-field-current-dumb syn :permeability permeability
				       :conc-in conc-in
				       :conc-out conc-out
				       :valence valence
				       :voltage (node-voltage-n-1-double (synapse-node syn))))
  
(proclaim '(inline synapse-constant-field-current-n+1))
(defun synapse-constant-field-current-n+1 (syn &key permeability conc-in conc-out valence)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (synapse-constant-field-current-dumb syn :permeability permeability
				       :conc-in conc-in
				       :conc-out conc-out
				       :valence valence
				       :voltage (node-voltage-n+1 (synapse-node syn))))

(defun get-synapse-current (syn &key conc-in conc-out valence voltage)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  "Current in nA (double float) of SYN at time tn."
  (case (synapse-type-iv-relation (synapse-type syn))
    (:CONSTANT-FIELD
     (if (and conc-in conc-out valence voltage)
       (synapse-constant-field-current syn :permeability (synapse-conductance syn) :conc-in conc-in :conc-out conc-out :valence valence)
       (synapse-constant-field-current-dumb
	syn :permeability (synapse-conductance syn) :conc-in conc-in :conc-out conc-out :valence valence :voltage voltage)))
    (t (synapse-ohmic-current syn))))

(proclaim '(inline get-synapse-current-complete))
(defun get-synapse-current-complete (syn &key conc-in conc-out valence voltage)
;; Returns the synapse current nA of SYN at time N, a double float.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (case (synapse-type-iv-relation (synapse-type syn))
    (:CONSTANT-FIELD
     (synapse-constant-field-current :permeability (synapse-conductance syn) :conc-in conc-in :conc-out conc-out :valence valence))
    (t (synapse-ohmic-current syn))))

;; GET-SYNAPSE-CURRENT-N+1 Returns the synapse current nA of SYN at time N+1, a double float.
(defun get-synapse-current-n+1 (syn)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (case (synapse-type-iv-relation (synapse-type syn))
    (:CONSTANT-FIELD (synapse-constant-field-current-n+1 syn))
    (t (synapse-ohmic-current-n+1 syn))))

(defun get-synapse-e-rev (synapse) ;; mV
  (let ((type (synapse-type synapse)))
    (if (and (synapse-conc-ints-params synapse)
	     (not (synapse-type-variable-e-rev type))
	     (eq (synapse-type-iv-relation type) :constant-field))
	(element-e-rev-from-shells (synapse-conc-ints-params synapse))
	(synapse-e-rev synapse))))

;; Make this smarter for multiple cell type circuits
(defun init-synapses-e-rev ()
  (loop for type in *synapse-type-list* unless (synapse-type-variable-e-rev type)
	do
	(let* ((cell-types-in-circuit (list-of-all-things-in-circuit 'cell-type))
	       (fixed-e-rev (when (= (length cell-types-in-circuit) 1)
			      (current-element-type-e-rev type (car cell-types-in-circuit)))))
	  (synapse-type-do
	   (syn type)
	   (unless (or (synapse-blocked syn)	  
		       (synapse-conc-ints-params syn))
	     (update-element-fixed-e-rev syn fixed-e-rev type))))))

;; For adjusting simulation duration dependent parameters, and to clear input events for voltage synapses.
(defun init-synapses ()
  (loop for type in (synapse-types) do
	(synapse-type-do (syn type)
			 (setf (synapse-conductance syn) 0.0d0
			       (synapse-current syn) 0.0d0)
			 (when (eq :voltage (synapse-type-control type))
			   (setf (synapse-event-times syn) nil
				 (synapse-transformed-events syn) nil
				 (synapse-sub-threshold-time syn) 0.0))))
  (init-synapses-e-rev)
  (setf (*t[n]*) 0.0)
  (let ((*eval-all-synapses-this-iteration* t)) (eval-all-synapses))
  )

;; ***********************************
;;
;; Setting Up Synapses 
;;
;; ***********************************

(defun set-synapse-delay (syn delay) (setf (synapse-delay syn) (s-flt delay)))

#|
(defun SYNAPSE-DELAYS-UNCHANGED (syn)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (and (synapse-event-times syn)
       (synapse-last-events syn)
       (= (length (the cons (synapse-last-events syn))) (length (the cons (synapse-event-times syn))))
       (loop for old-event in (synapse-last-events syn)
	     for new-event in (synapse-event-times syn)
	     when (not (= (the sf old-event) (the sf new-event)))
	     do (return nil)
	     finally (return t))))
|#

(defun setup-synapses ()
  ;; Set up synapses with apriori conductance waveforms, e.g. for whom the SYNAPSE-TYPE :CONTROL is :EVENT, :LIGHT-EVENT
  ;; or :LIGHT.
  (setq *synapse-type-list*
	(when (and (are-there-synapses) *enable-synapses*)
	  (loop for type in (synapse-types) when (synapse-type-active-p type) do
		(setf (synapse-type-waveform-time-interval-inverse type) (/ 1.0 (synapse-type-waveform-interval type)
					; (element-parameter type 'waveform-time-interval)
									    ))
		(setf (synapse-type-waveform-time-interval-mrt type)
		      (waveform-interval-inverse-to-mrt-interval (synapse-type-waveform-time-interval-inverse type))))
	  (cond-every
	   (*setup-tonic-synapses* (setup-tonic-synapses))
	   (*setup-light-synapses* (setup-light-synapses)) ; Handles both LIGHT and LIGHT-EVENT types.
	   (*setup-event-synapses* (setup-event-synapses)) ; Handles both EVENT and LIGHT-EVENT types.
	   (*setup-voltage-synapses* (setup-voltage-synapses)))
	  (loop for type in (synapse-types) when (synapse-type-active-p type)
		do (update-membrane-element-type-params type) and collect type))))

(defun fixup-1st-order-Depressing-params (syn-type)
  (cond-every
   ((element-parameter syn-type 'release-fraction)
    (element-parameter syn-type 'release-fraction (s-flt (element-parameter syn-type 'release-fraction))))
   ((element-parameter syn-type 'tau-recovery)
    (element-parameter syn-type 'tau-recovery (s-flt (element-parameter syn-type 'tau-recovery))))))

#|
(defun remove-synapse-type-lists (type)
  (remove-element-parameters type '(SYNAPSE-NODE-FIXNUMS SYNAPSE-NODE-FLOATS synapse-pre-syn-node-floats)))

(defun revamp-synapse-type-lists (type synapses &optional inclusion))

(defun setup-tonic-synapses ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for type in (get-synapse-types :tonic)
	;; when (synapse-type-blocked type) do (remove-synapse-type-lists type) else do
	unless (synapse-type-blocked type) do
	(let (syns)
	  (synapse-type-iterator
	   (syn type)
	   when (not (or (synapse-blocked syn) (= (synapse-gbar syn) 0.0d0))) do (push syn syns))
	  ;; (revamp-synapse-type-lists type syns)
	  )))
|#

(defun setup-tonic-synapses ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  nil)

(defun update-CONDUCTANCE-DECAY-FACTOR (type)
  (element-parameter type 'CONDUCTANCE-DECAY-FACTOR
		     (when (and *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS*
				(*INTEGRATION-TIME-REFERENCE* :fixed) ; *use-fixed-step*
				(element-parameter type 'TAU-EXPO-DECAY))
		       ;; Il vaut : exp -dt/tau. LBG 7/6/99 Change set-element-parameter to element-parameter.
		       (s-flt (exp (- (/ *user-step* (element-parameter type 'TAU-EXPO-DECAY))))))))

(defun synapse-type-conductance-waveform-half-time (type)
  (* 0.5 (/ (length (element-parameter type 'waveform))
	    (synapse-type-waveform-time-interval-inverse type))))

(defun update-conductance-waveform-half-time (type)
  (element-parameter type 'conductance-waveform-half-time (synapse-type-conductance-waveform-half-time type))
  (element-parameter type 'conductance-waveform-half-time-mrt (convert-time-to-mrt-units (element-parameter type 'conductance-waveform-half-time)))
  (element-parameter type 'conductance-waveform-event-breakpoints-mrt
		     (cons (convert-time-to-mrt-units (element-parameter type 'conductance-waveform-half-time))
			   (loop for additional-waveform-breakpoint in (element-parameter type 'specified-waveform-breakpoints)
				 collect (convert-time-to-mrt-units additional-waveform-breakpoint)))))

;;; A "Lumped Synapse" is one of several synapses of the same EVENT or LIGHT-EVENT type on the same cell element.
(defun setup-event-synapses ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for type in (concatenate 'list (get-synapse-types :event) (get-synapse-types :light-event)) do
	(element-parameter type 'last-valid-wave-input-time-integer-part nil) ; Clear this value.
	(update-conductance-waveform-half-time type)
	;; when (synapse-type-blocked type) do (remove-synapse-type-lists type) else do
	unless (synapse-type-blocked type) do
	(let ((active-event-synapses (active-synapses type))
	      (conductance-waveform-event-breakpoints
	       (cons (element-parameter type 'conductance-waveform-half-time) (element-parameter type 'specified-waveform-breakpoints)))
	      (convert-light-response-to-events-function
	       (or (element-parameter type 'convert-light-response-to-events-function)
		   #'default-convert-light-response-to-events)))
	  (synapse-type-do
	   (syn type)
	   (unless (synapse-blocked syn)
	     (case (synapse-type-control type)
	       (:light-event (when (and *enable-light-event-update*
					(or (not *enable-event-generators*)
					    *convert-light-response-to-events-for-each-synapse*
					    (eq syn (synapse-event-generator syn)))) ; Equivalent to (EVENT-GENERATOR-P syn)
			       (setf (synapse-event-times syn) (funcall convert-light-response-to-events-function syn)))))
	     (UPDATE-CONDUCTANCE-DECAY-FACTOR type)
	     (when *adjust-breakpoints-for-event-synapses*
	       (queue-event-synapse-breakpoint-time syn conductance-waveform-event-breakpoints))
	   
	     (setf (synapse-fixnum-delay syn) (round (* (synapse-type-waveform-time-interval-inverse type) (synapse-delay syn))))
	     (setf (synapse-transformed-events syn) (sort-scale-and-shift-event-times syn))
	     
	     ;; (setf (synapse-last-events syn) (synapse-event-times syn))
	     
	     (when nil			; *EVALUATE-LUMPED-event-SYNAPSES*
	       (push-element-parameter (synapse-cell-element syn) 'event-syns syn)))  
	   
	   (when  nil			; *EVALUATE-LUMPED-event-SYNAPSES*
	     ;; The active event synapses are consolidated to the lumped synapses (representative synapse
	     ;; from each node).
	     (setq active-event-synapses (loop for cell-element in (cell-elements)
					       when (element-parameter cell-element 'event-syns) collect
					       (consolidate-lumped-synapse-events cell-element))))))))

(defvar *nb-event-syn-bps-w-ideal-vsource* 10
  "If even synapse node has an ideal voltage source, add this number of breakpoints over the duration
of the synapse conductance waveform to try to sample properly. Must be a fixnum.")

(defun queue-event-synapse-breakpoint-time (syn conductance-waveform-event-breakpoints)
  "Queue not only the event start times, but also the midpoint of the associated event waveform and any user-specified breakpoints, in order to better
catch event-driven changes in the circuit. If node has an ideal voltage source, add *NB-EVENT-SYN-BPS-W-IDEAL-VSOURCE* points over the duration of the
synapse conductance waveform to try to sample properly. EVENT-SYNAPSE-CONDUCTANCE-WAVEFORM-HALF-TIME must be a single-float."
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (let ((EVENT-SYNAPSE-CONDUCTANCE-WAVEFORM-HALF-TIME (the sf (car conductance-waveform-event-breakpoints))))
    (loop for time single-float in (synapse-event-times syn) do
	  (if (node-has-ideal-voltage-source (synapse-node syn))
	    (loop for small-step single-float from time
		  by (the sf (/ event-synapse-conductance-waveform-half-time
				(/ (the fn *nb-event-syn-bps-w-ideal-vsource*) 2)))
		  for count fixnum from 1 to (the fn *nb-event-syn-bps-w-ideal-vsource*)
		  do (queue-breakpoint-time small-step))
	    (loop for conductance-waveform-event-breakpoint in (cons 0.0 conductance-waveform-event-breakpoints) do
		  (queue-breakpoint-time (+ time conductance-waveform-event-breakpoint)))))))

(defun set-voltage-synapses-delay-to-connection-distance (&optional propagation-velocity)
  (loop for syn in (synapses-of-control :voltage) do
	(set-voltage-synapse-delay-to-connection-distance syn propagation-velocity)))

(defun set-voltage-synapse-delay-to-connection-distance (syn &optional propagation-velocity)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setf (synapse-delay syn)
	(the sf (/ (the sf (cartesian-vector-distance-float (element-absolute-location syn) (element-absolute-location (synapse-pre-synaptic-node syn))))
		   (the sf (or propagation-velocity
			       (element-parameter (synapse-type syn) 'propagation-velocity)
			       1.0)))))	; 1 um/ms
  nil)

;; LBG 7/6/99 Add STORE-MAX-DELAY-FOLLOWERS, update type 'GENERATORS, *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* preprocessing.
(defun SETUP-VOLTAGE-SYNAPSES ()
  (declare (optimize (safety 1) (speed 3) (space 1) (compilation-speed 0)))
  (loop for type in (get-synapse-types :voltage)
	unless (synapse-type-blocked type) do
	(let ((voltage-synapses (active-synapses type)))
          (loop for syn in voltage-synapses do 
                (setf (synapse-fixnum-delay syn)
		      (round (the sf (* (synapse-type-waveform-time-interval-inverse type) (synapse-delay syn))))))
	  (setup-event-generators-and-followers-of-type voltage-synapses)
	  (store-max-delay-followers voltage-synapses)
	  (element-parameter type 'GENERATORS (type-event-generators type))
	  (update-conductance-waveform-half-time type)

	  ;; Les transformed-events residuels de la simulation d'avant sont supprimes dans
	  ;; la fonction INIT-SYNAPSES, appelee dans INIT-NODE-ELEMENTS, elle-meme appelee dans
	  ;; INITIALIZE-SIMULATION.
	  ;;
	  ;; Remarque importante :
	  ;; synapse-conductance est mis a 0.0d0 avant le debut de la simulation dans INIT-SYNAPSES.
	  ;;
	  ;; Rajoute le 17/2/98 : pour optimisation du decay exponentiel. Ce parametre devrait etre
	  ;; un slot dans la structure synapse-type !!!!!!
	  (update-CONDUCTANCE-DECAY-FACTOR type)
	  ))
  nil)

(defun synapse-wave-ref-fun (syn) (synapse-wave-ref syn))

;; *************************************
;;
;; Plotting Stuff
;;
;; *************************************

(defun plot-synapse-type-waveform (type)
  (let ((type (element-type type)))
    (when (synapse-type-p type)
      (plot-timed-data (element-parameter type 'waveform) nil (synapse-type-waveform-interval type)
		       :y-label "" :x-label "ms" :prompt-for-overlay t
		       :y-min 0
		       :fix-to-unity-mag-if-so t :title (format nil "Synapse Type ~a Conductance Waveform" (element-name type))))))

(defun plot-synapse-types (&optional synapse-types)
  (dolist (type (or (coerce-to-list synapse-types) (select-hash-values-menu (synapse-TYPE-HASH-TABLE) "Select Synapse Types To Plot Waveforms")))
    (let ((type (element-type type)))
      (when (synapse-type-p type) (dolist (synapse-type-function *synapse-type-functions*) (plot-synapse-type-wave type synapse-type-function))))))

(defun plot-synapse-type-wave (type synapse-type-function &optional waveform)
  (let ((waveform (or waveform (synapse-type-wave-from-synapse-type-function type synapse-type-function)))
	(wavename (wave-name-from-type synapse-type-function))
	(name (synapse-type-name type)))
    (when waveform
      (case synapse-type-function
	(spatial-rf-function (3dplot waveform :theta 20 :phi 20 :title (format nil "Synapse Type ~A: Spatial RF" name)))
	(static-voltage-dependence-function
	 (PLOT-VOLTAGE-SEQUENCE waveform *particle-look-up-table-precision*
				*PARTICLE-LOOK-UP-TABLE-MIN-VOLTAGE* "V-Dep" (format nil "~A Static Voltage Dependence" name) :prompt-for-overlay t))
	(waveform-function (plot-timed-data waveform wavename (synapse-type-waveform-interval type)
					    :y-min 0
					    :y-label "" :x-label "ms" :prompt-for-overlay t :fix-to-unity-mag-if-so t 
					    :title (format nil "Synapse Type ~a ~a" name wavename)))))))

#|
(defun plot-synapse-impulse (&optional type &key overlay waveform-type)
  (loop for type
	in (coerce-to-list (element (or type (select-hash-values-menu
					      (SYNAPSE-TYPE-HASH-TABLE)
					      "Choose synapse type for plotting impulse response or event waveforms"))
				    'synapse-type))
	do
	(let ((wave (synapse-type-wave-from-synapse-type-function type synapse-type-function))
	      (time-interval (synapse-type-waveform-interval type)))
	  (when wave
	    (let ((label (case (synapse-type-control type)
			   (:light "Impulse Response")
			   ((:voltage :event) "Event Waveform"))))
	      (plot-timed-data wave (list label) time-interval
			       :y-label "" :x-label "ms" :prompt-for-overlay t
			       :title (format nil "Synapse Type ~a ~a" type label)))))))
|#

(defvar *count-active-and-triggered-synapses* t "When non-NIL, the function COUNT-ACTIVE-SYNAPSES, which normally prints out info at the end of each
simulation, also prints the number of synapses actually fired.")

(defun count-active-synapses ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (and *simulation-finished* *enable-synapses*)
    (let ((total-active 
	   (loop for control in '(:light :event :voltage :tonic) summing
		 (let ((num-active))
		   (loop for type in (get-synapse-types control)
			 do (setq num-active 0)
			 when (synapse-type-first-element type) do
			 (setq num-active (or (number-SYNAPSE-TYPE-SYNAPSES type t) 0))
			 (format t "~d active ~a synapse~p~A"
				 num-active
				 (synapse-type-name type)
				 num-active
				 (if *count-active-and-triggered-synapses*
				     (format nil " (~D triggered)"
					     (let ((triggered-syns 0))
					       (synapse-type-do
						(syn type)
						(when (get-events syn) (incf (the fn triggered-syns))))
					       triggered-syns))
				     ""))				  
			 (format t "~%")
			 summing num-active into inner-count fixnum finally (return inner-count)))
		 into count fixnum finally (return count))))
      (format t "~d total active synapse~p.~%" total-active total-active))))
 
(defvar *interpolate-synapse-waveforms* t)

(defun get-type-params-waveform (params) (get-a-value 'light-responses-ARRAY params))

(defun light-syn-rf-label (syn) (list (syn-rf-center-x syn) (syn-rf-center-y syn) (synapse-type syn)))

;; ***************************
;; Menus Related to Synapses
;; ***************************

(defun menu-for-removing-synapse-types ()
  (erase-element
   (choose-list-values (GET-CURRENT-SYNAPSE-TYPE-NAMES) nil :punt-if-only-one-entry nil
		       :do-all-at-once t :rank-margin 5 :direction :vertical :label "Choose Synapse Types to Remove")
   'synapse-type))

(defun menu-for-adding-synapse-types (&optional target-elt)
  ;; just to clean things up
  (setf (MODEL-PARAMETER-type-LIBRARY (type-symbol-model 'synapse-type))
	(delete-duplicates (MODEL-PARAMETER-type-LIBRARY (type-symbol-model 'synapse-type)) :from-end t :key 'car :test 'eql))

  (let ((no-light-synapses-yet (not (are-there-light-synapses))) dummy1)
    (choose-variable-values '((dummy1 "Synapse types control:" :x-choose (:light-event :light :voltage :event :channel)))
			    :label (if target-elt
				       (format nil "Synapse Classes to Remove or Add to ~a" (element-name target-elt))
				       "Adding Synapse Classes"))
    (loop for control in dummy1 do
	  (let* ((original-synapses (when target-elt
				      (loop for syn in (synapses target-elt) when (eq control (synapse-type-control (synapse-type syn))) collect syn)))
		 (new-synapse-type-names
		  (choose-list-values
		   (LIBRARY-CATALOG 'synapse :synapse-control control)
		   (loop for syn in original-synapses collect (synapse-type-name (synapse-type syn)))
		   :do-all-at-once t :rank-margin 5 :direction :vertical
		   :label (if target-elt
			      (format nil "~A Synapse Class to Remove or Add to ~a" control (element-name target-elt))
			      (format nil "Choose ~A Synapse Types To Add Randomly" control)))))
	    (when target-elt
	      (loop for syn in original-synapses
		    when (not (string-member (synapse-type-name (synapse-type syn)) new-synapse-type-names))
		    do (erase-element syn)
		    else do (setq new-synapse-type-names (string-remove (synapse-type-name (synapse-type syn)) new-synapse-type-names))))
	    (if target-elt
		(create-element target-elt new-synapse-type-names)
		(loop for type-symbol in new-synapse-type-names do
		      (setq *recheck-circuit-elements-parameters* t)
		      (create-element type-symbol)))
	    (when (and no-light-synapses-yet (are-there-light-synapses))
	      (menu-for-light-stimulus)
	      (setq no-light-synapses-yet nil))))))

(defun menu-for-synapse-parameters ()
  (let* (dummy1 dummy2 dummy5 dummy6 dummy7 dummy8
		(dummy9 (not *enable-synapses*))
		(menu-list `((dummy1 "Edit parameters of synapse types" :boolean)
			     ,(when *synapse* `(dummy9 "Block all synapses" :boolean))
			     ,(when *synapse* `(dummy5 "Remove synapse types" :boolean)))))
    (when (are-there-light-synapses)
      (setq menu-list
	    (append
	     menu-list
	     `((dummy2 "Edit light inputs" :boolean)
	       (*reuse-synapse-waveforms* ,(format nil "For light synapses use previous light convolutions~%(if simulation time unchanged)") :boolean)
	       (dummy7 "Map light synapse RF centers over defined area" :boolean)
	       (dummy6 "Plot light synapse RFs" :boolean)
	       (*SETUP-LIGHT-SYNAPSES* "Run setup on light synapses prior to simulation" :boolean)))))
    (when (are-there-voltage-synapses)
      (setq menu-list (append menu-list'((*SETUP-VOLTAGE-SYNAPSES* "Run setup on voltage synapses prior to simulation" :boolean)))))
    (when (are-there-event-synapses)
      (setq menu-list
	    (append
	     menu-list
	     '((*SETUP-EVENT-SYNAPSES* "Run setup on event synapses prior to simulation" :boolean)
	       ;; (*evaluate-lumped-event-synapses* "Evaluate all event syns of same type on a given node together" :boolean)
	       ))))
    (choose-variable-values menu-list :label "Setting Up Synapse Parameters")
    (when (not (xor *enable-synapses* dummy9))
      (loop for type in (synapse-types) do (setf (synapse-type-blocked type) dummy9))
      (setq *recheck-circuit-elements-parameters* t))
    (setq *enable-synapses* (not dummy9))
    (cond-every
     (dummy5 (menu-for-removing-synapse-types))
     (dummy2 (menu-for-light-stimulus))
     (dummy1 (MENU-FOR-SYNAPSE-TYPES))
     (dummy6 (plot-synapse-rfs))
     (dummy7 (MAP-LIGHT-INPUTS)))))

(defun menu-for-synapse-types (&optional types)
  (let ((parameters-included-in-main-menu '(e-rev iv-density refractory-period input-threshold supra-threshold-duration-min q10 reference-temp))    
	(types (if types (element types 'synapse-type) (menu-for-type 'synapse-type)))
	dummy1 dummy2 dummy3 dummy4 (dummy6 t) dummy7 dummy8 dummy9 dummy10 dummy11 dummy12
	dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19 dummy20 dummy21 dummy22
	dummy23 dummy24 dummy25 dummy26 dummy27 dummy28 dummy29 dummy30 dummy31 dummy32)
    (when types
      (loop for type in (coerce-to-list types) do
	    (let (menu-list)
	      (setf dummy1 (synapse-type-iv-density type)
		    dummy2 (synapse-type-e-rev type)
		    dummy3 (or (not *enable-synapses*) (synapse-type-blocked type))
		    dummy6 t dummy8 t
		    dummy14 (synapse-type-iv-reference type)
		    dummy15 nil dummy18 t dummy17 t
		    dummy16 (synapse-type-name type)
		    dummy19 (synapse-type-iv-source type)
		    dummy20 (if (synapse-type-use-defined-e-rev type) :fixed_erev :ion_perms)
		    dummy23 (synapse-type-q10 type)
		    dummy24 (synapse-type-reference-temp type)
		    dummy25 (or (element-parameter type :IV-MODULATION) 1.0)
		    dummy26 (element-parameter type '1st-order-Depressing-dynamics)
		    dummy27 (or (element-parameter type 'release-fraction) 1.0)
		    dummy28 (or (element-parameter type 'tau-recovery) 1.0)
		    dummy29 nil dummy30 nil
		    dummy31 (symbolp (synapse-type-name type))
		    dummy32 nil)
	      (when (eq (synapse-type-control type) :voltage)
		(setf dummy10 (synapse-type-input-threshold type)
		      dummy11 (synapse-type-refractory-period type)
		      dummy12 (synapse-type-supra-threshold-duration-min type)))
	      (loop while (or dummy8 dummy6 dummy17 dummy18 dummy21 dummy22 dummy29 dummy32) do
		    (setq dummy6 nil dummy8 nil dummy17 nil dummy18 nil dummy22 nil dummy13 nil dummy21 nil dummy29 nil dummy32 nil)
		    (setq menu-list
			  `((dummy14 ,(membrane-element-type-iv-relation-absolute-reference-menu-string type) :float)
			    (dummy1 ,(membrane-element-type-iv-density-menu-string type) :float)
			    (dummy19 ,(MEMBRANE-ELEMENT-TYPE-IV-SOURCE-MENU-STRING type) :choose (:absolute :density) :label-left)
			    (dummy23 "Gbar/Pbar Q10" :float)
			    (dummy24 "Gbar/Pbar Kinetics Reference Temperature [degs C]" :float)
			    (dummy25 ,(ELEMENT-TYPE-MENU-IV-RELATION-MOD-COMMENT) :float)
			    (dummy2 ,(format nil "Fixed Reversal Potential [mV] (actual ~,2fmV)" (current-element-type-e-rev type)) :float)
			    (dummy20 ,(MENU-E-REV-COMMENT type) :choose (:fixed_erev :ion_perms) :label-left)
			    (dummy21 "Edit ion permeabilities" :boolean)
			    (dummy3 ,(format nil "Block all ~a synapses" (synapse-type-name type)) :boolean)
			    (dummy32 ,(format nil
					      "Revamp synapse type definitions from current library.
This will *not* remove any properties which
are excluded from the current library definitions.") :boolean)))
		    (loop for param in (synapse-type-parameters type)
			  when (and (not (member (car param) parameters-included-in-main-menu)) (numberp (cdr param)))
			  do (return (push '(dummy13 "Edit other numeric parameters" :boolean) menu-list)))
		    (when (and (eq (synapse-type-control type) :light)
			       (assoc 'SPATIAL-RF-FUNCTION (synapse-type-parameters type)))
		      (push '(dummy22 "Plot light spatial receptive field" :boolean) menu-list))
		    (when nil ; (eq (synapse-type-control type) :event)
		      (push '(dummy27 "If using 1st-order-Depressing dynamics, release fraction" :float) menu-list)
		      (push '(dummy28 "If using 1st-order-Depressing dynamics, tau recovery [ms]" :float) menu-list)
		      (push '(dummy26 "Use 1st-order-Depressing dynamics" :boolean) menu-list))
		    (when (eq (synapse-type-control type) :voltage)
		      (push '(dummy10 "Input Threshold [mV]" :float) menu-list)
		      (push '(dummy11 "Refractory Period [ms]" :float) menu-list)
		      (push '(dummy12 "Supra-Threshold Minimun Duration [ms]" :float) menu-list))
		    (cond-every
		     ((or (assoc 'IMPULSE-FUNCTION (synapse-type-parameters type))
			  (assoc 'static-voltage-dependence (synapse-type-parameters type))
			  (assoc 'linear-IMPULSE-FUNCTION  (synapse-type-parameters type))
			  (assoc 'waveform-spec  (synapse-type-parameters type))
			  (assoc 'waveform-function  (synapse-type-parameters type)))
		      (push '(dummy6 "Edit various waveforms" :boolean) menu-list))
		     ((assoc 'waveform-time-interval (synapse-type-parameters type))
		      (setq dummy7 (get-a-value 'waveform-time-interval (synapse-type-parameters type)))
		      (push '(dummy7 "Waveform time interval" :number) menu-list)))
		    (when (and (not (or (assoc 'waveform-function (synapse-type-parameters type))
					(assoc 'waveform-spec (synapse-type-parameters type))))
			       (assoc 'waveform (synapse-type-parameters type)))
		      (push '(dummy30 "Plot explicit conductance waveform" :boolean) menu-list))
		    (push '(dummy15 "Modify individual synapses (e.g. delays, gbars, etc.)" :boolean) menu-list)
		    (push `(dummy29 "Dump definition to .elts file" :boolean) menu-list)
		    (push `(dummy16 ,(format nil "Edit name of type (used if saved to file):") :string) menu-list)
		    (choose-variable-values
		     menu-list
		     :text (ADD-LINEFEEDS-TO-STRING-LIST
			    (list (format nil "~A are ~s synapses.~A" (synapse-type-name type) (synapse-type-control type)
					  (if (independent-element-gbars-p type) " Some synapses have independent GBAR parameters." ""))
				  (ELEMENT-SOURCEFILE-STRING type nil)))
		     :label (format nil "Parameters Of Synapse Type ~A" (synapse-type-name type)))
		    (if dummy32 (create-synapse-type type nil t)
			(progn
			  (when dummy21 (EDIT-ION-PERMEABILITIES type))
			  (element-parameter type '1st-order-Depressing-dynamics dummy26)
			  (element-parameter type 'release-fraction dummy27)
			  (element-parameter type 'tau-recovery dummy28)
			  (setf (synapse-type-use-defined-e-rev type) (eq dummy20 :fixed_erev))
			  (unless (string= (format nil "~A" (synapse-type-name type)) dummy16)
			    (set-element-name type (if dummy31 (intern dummy16) dummy16)))
			  (when dummy13
			    (setf (synapse-type-parameters type)
				  (let ((parameters (synapse-type-parameters type))
					(parameters-included-in-main-menu parameters-included-in-main-menu))
				    #|
				    (loop for param in parameters
				       when (or (member (car param) parameters-included-in-main-menu) (not (numberp (cdr param)))) collect param
				       else collect
					 (let ((dummy1 (cdr param)))
					   (choose-variable-values `((dummy1 ,(string (car param)) :number))
								   :label (format nil "More Parameters Of Synapse Type ~A" (synapse-type-name type)))
					   (cons (car param) dummy1)))
				    |#
				    (loop for param in parameters
				       when (or (member (car param) parameters-included-in-main-menu) (not (numberp (cdr param)))) collect param into un-treated-params
				       else collect param into treated-params
					 finally 
					 (let ((dummy1 (cdr param)))
					   (choose-variable-values `((dummy1 ,(string (car param)) :number))
								   :label (format nil "More Parameters Of Synapse Type ~A" (synapse-type-name type)))
					   (cons (car param) dummy1)))

				    wh::*dummy-variables*


				    )))
			  (when dummy15
			    (loop for synname in 
				  (choose-list-values
				   (loop for syn in (synapses) when (eq type (synapse-type syn)) collect (synapse-name syn))
				   nil :label (format nil "Choose Synapse of Type ~A To Modify" (synapse-type-name type)))
				  do (edit-synapse (element synname 'synapse) t))
			    (setq dummy15 nil))		 
			  (setq dummy13 nil)
			  (setq *recheck-circuit-elements-parameters* 
				(or *recheck-circuit-elements-parameters*
				    (not (and (= (synapse-type-iv-reference type) dummy14)
					      (= (or (element-parameter type :IV-MODULATION) 1.0) dummy25)
					      (= (synapse-type-iv-density type) dummy1)
					      (= (synapse-type-e-rev type) dummy2)
					      (eq (synapse-type-blocked type) dummy3)
					      (eq (synapse-type-iv-source type) dummy19)))))
			  (setq *update-temperature-dependent-parameters*
				(or *update-temperature-dependent-parameters*
				    (not (= dummy23 (synapse-type-q10 type)))
				    (not (= dummy24 (synapse-type-reference-temp type)))))
			  (element-parameter type :IV-MODULATION dummy25)
			  (setf (synapse-type-e-rev type) dummy2
				(synapse-type-blocked type) dummy3
				(synapse-type-q10 type) dummy23 
				(synapse-type-reference-temp type) dummy24
				(synapse-type-iv-source type) dummy19
				(synapse-type-iv-reference type) dummy14
				(synapse-type-iv-density type) dummy1)
			  (cond-every
			   ((assoc 'waveform-time-interval (synapse-type-parameters type))
			    (setf (synapse-type-parameters type)
				  (acons 'waveform-time-interval dummy7
					 (remove (assoc 'waveform-time-interval (synapse-type-parameters type)) (synapse-type-parameters type)))))
			   (dummy22 (plot-synapse-rfs type))
			   (dummy30 (plot-synapse-type-waveform type))
			   (dummy6 (edit-synapse-type-waveforms type))
			   ((eq (synapse-type-control type) :voltage)
			    (setf (synapse-type-input-threshold type) (d-flt dummy10)
				  (synapse-type-refractory-period type) dummy11 
				  (synapse-type-supra-threshold-duration-min type) dummy12)))
			  (when dummy29 (dump-elements-file type))))
		    (setq *last-edited-synapse-type* (element-name type))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; old code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun create-synapse (post-synaptic-element synapse-type-symbol &optional pre-synaptic-element (add-pre-synaptic-element-name-to-name t) name)
  "CREATE-SYNAPSE A synapse that is controlled by the voltage of another node is specified with a pre-synaptic cell element or
name (soma, segment, or axon). If the POST-SYNAPTIC-ELEMENT already has a synapse of the same type, and the PRE-SYNAPTIC-ELEMENT
is either different or the synapse type has no PRE-SYNAPTIC-ELEMENT, then an alternate name will be created from the addition of a
number at the end of the standard synapse name. If the global variable *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* is T, then the user is
prompted before the additional synapse is created, otherwise, the synapse is created."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless *only-load-passive*
    (let* ((cell-element (element post-synaptic-element))
	   (type (create-synapse-type synapse-type-symbol))
	   (synapse-type-symbol (synapse-type-name type))
	   (node (typecase cell-element
		   (segment (segment-node-2 cell-element))
		   (soma (soma-node cell-element))))
	   (name (cond
		  (*use-simple-names* (1+ (hash-table-count (SYNAPSE-HASH-TABLE))))
		  (name name)
		  ((and pre-synaptic-element add-pre-synaptic-element-name-to-name)
		   (format nil  "Syn-~a-~A-~A" (node-name node) synapse-type-symbol (element-name pre-synaptic-element)))
		  (t (format nil "Syn-~a-~a" (node-name node) synapse-type-symbol))))
	   (pre-synaptic-element (element pre-synaptic-element))
	   (current-syn (SYNAPSE-HASH-TABLE name)))
      (cond ((and (case (synapse-type-control type)
		    ((:channel :voltage) t)
		    (t nil))
		  (not pre-synaptic-element))
	     (format *standard-output* ";; CREATE-SYNAPSE aborted: Synapse ~a must have complete presynaptic info~%" name))
	    ((and current-syn
		  pre-synaptic-element
		  (not *allow-duplicate-synaptic-connections*)
		  (equal pre-synaptic-element (synapse-pre-synaptic-element current-syn)))
	     (format t "CREATE-SYNAPSE: synapse ~a already defined~%" name)
	     current-syn)
	    ((or (not current-syn)
		 (not *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*)
		 (go-ahead-menu
		  (format nil "Make new name for duplicate synapse type ~A on ~A" synapse-type-symbol (node-name node)) "Rename Authorization" t t))
	     (let* ((name (if current-syn (check-element-name name 'synapse) name))
		    (syn (make-synapse :name name :cell-element cell-element
				       :pre-synaptic-element pre-synaptic-element
				       :channel (when (element-parameter type 'channel)
						  (create-channel cell-element (element-parameter type 'channel) :pre-synaptic-element pre-synaptic-element))
				       :inherit-parameters-from-type (synapse-type-inherit-parameters-from-type type))))
	       (unless (synapse-inherit-parameters-from-type syn)
		 (case (synapse-type-iv-source type)
		   (:absolute (set-element-iv-reference syn (synapse-type-iv-reference type)))
		   (:density (set-element-iv-density syn  (synapse-type-iv-density type)))))

	       (setf (synapse-type syn) type) 
	       (push syn (node-elements node))
	       (push syn (synapse-type-synapses type))

	       (when (element-parameter type 'static-voltage-dependence)
		 (setf (node-has-v-dep-element node) t))
	       (when (synapse-type-conc-int-type-params type)
		 (setf (synapse-conc-ints-params syn) (parse-conc-int-info-for-element type cell-element)))
	       (when (or (axon-p pre-synaptic-element) (cell-element-p pre-synaptic-element))
		 (unless (eq node (element-physical-node pre-synaptic-element))
		   (push syn (node-elements (element-physical-node pre-synaptic-element))))
		 (typecase pre-synaptic-element
		   (axon (setf (axon-target-synapse pre-synaptic-element) syn))))
	       (setf (SYNAPSE-HASH-TABLE name) syn)
	       (setq *recheck-circuit-elements-parameters* t
		     *make-node-w/elements-array* t)
	       syn))))))
|#

#|
(defun init-synapses-e-rev ()
  (loop for syn-type in *synapse-type-list* unless (synapse-type-variable-e-rev syn-type) do
	(let* ((cell-types-in-circuit (list-of-all-things-in-circuit 'cell-type))
	       (fixed-e-rev (when (= (length cell-types-in-circuit) 1)
			      (current-element-type-e-rev syn-type (car cell-types-in-circuit)))))
	  (loop for syn in (element-parameter syn-type 'active-synapses)
		unless (synapse-conc-ints-params syn) do (update-element-fixed-e-rev syn fixed-e-rev syn-type)))))
|#
#|
(defun revamp-synapse-type-lists (type synapses &optional inclusion)
  (when inclusion (setq synapses (loop for syn in synapses when (funcall inclusion syn) collect syn)))
  (if synapses
      (loop for synapse in synapses
	    collect (when (synapse-pre-synaptic-node synapse)
		      (node-double-floats (synapse-pre-synaptic-node synapse))) into pre-syn-node-floats-list
	    collect (node-double-floats (synapse-node synapse)) into node-floats-list
	    collect (node-fixnums (synapse-node synapse)) into node-fixnums-list
	    finally
	    (ELEMENT-PARAMETER type 'active-synapses synapses)
	    (ELEMENT-PARAMETER type 'SYNAPSE-NODE-FIXNUMS node-fixnums-list)
	    (ELEMENT-PARAMETER type 'SYNAPSE-NODE-FLOATS node-floats-list)
	    (ELEMENT-PARAMETER type 'synapse-pre-syn-node-floats pre-syn-node-floats-list))
      (remove-synapse-type-lists type)))
|#

#|
(defun sort-scale-and-shift-event-times (syn)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((scaling (synapse-type-waveform-time-interval-inverse (synapse-type syn)))
	(delay (the fn (synapse-fixnum-delay syn))))
    (sort (loop for event in (synapse-event-times syn)
		collect (the fn (+ delay (the fn (round (the sf (* (the sf event) scaling)))))))
	  '<)))
|#
#|
(proclaim '(inline initialize-synapse-node-constant-jacobian-and-current))
(defun initialize-synapse-node-constant-jacobian-and-current (syn node-double-floats)	
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type synapse syn)
	   (type node-df-array node-double-floats))
  (unless (node-has-ideal-voltage-source (synapse-node syn))
    (setf (node-aref-element-const-jacobian node-double-floats)
	  (the df (+ (node-aref-element-const-jacobian node-double-floats) (synapse-conductance syn)))
	  (node-aref-element-const-current node-double-floats)
	  (the df (- (node-aref-element-const-current node-double-floats)
		     (the df (* (synapse-conductance syn) (synapse-e-rev syn)))))))
  nil)	      

(defun initialize-synapse-node-constant-jacobians-and-currents ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (do ((index 0 (the fn (+ 1 index))))
      ((= index (the fn *CORE-NODE-ARRAY-LENGTH*)))
    (let ((node (aref (the (simple-array node (*)) *core-node-array*) index)))
      (setf (node-element-const-jacobian-double node) 0.0d0
            (node-element-const-current-double node) 0.0d0)))
  (loop for type in (synapse-types)
	when (case (synapse-type-control type)
	       (:light *enable-light*)
	       (t t))
	do
	(let* ((params (synapse-type-parameters type))
	       (active-synapses (get-a-value 'active-synapses params)))
	  (when active-synapses
	    (let ((SYNAPSE-NODE-FLOATS (get-a-value 'SYNAPSE-NODE-FLOATS params)))
	      (loop for syn in active-synapses
		    for synapse-node-float in synapse-node-floats
		    do
		    (initialize-synapse-node-constant-jacobian-and-current syn synapse-node-float)))))))
|#
#|
(defun set-syn-delays (SYNAPSE EVENTS)
  (case (synapse-type-control (synapse-type (element synapse 'synapse)))
    (:event (setf (synapse-event-times (element synapse 'synapse))
		(sort (loop for num in events collect (s-flt num)) '<)))
    (t (format t "Error setting events: Synapse ~A is not an event synapse!~%"
	       (element-name synapse 'synapse)))))
;; not used anywhere
(defun get-synapse-waveform (syn) (element-parameter syn 'waveform))
|#
;; Not done.....
#|
(defun evaluate-exponential-sum-synapse (syn evaluation-method-parameters last-valid-wave-input-time-integer-part)
  ;; Format of evaluation-method-parameters: (:2nd-order-integration '((tau-1 coefficient)(tau-2 coefficient)...(tau-n coefficient)))
  (loop for tau-coefficient in (cdr evaluation-method-parameters)
	summing (* (cadr tau-coefficient) (exp (/ *real-time* (car tau-coefficient)))))
  (let ((next-event-time (the fn (car (synapse-transformed-events syn)))))
    (unless (> next-event-time *integer-time*)
      (when (and (numberp last-valid-wave-input-time-integer-part)
		 (<= next-event-time (the fn last-valid-wave-input-time-integer-part)))
	(setf (synapse-transformed-events syn) (car (synapse-transformed-events syn)))
	(setq next-event-time (the fn (car (synapse-transformed-events syn)))))
      (when (<= next-event-time *integer-time*)
	(include-new-event)))))
|#

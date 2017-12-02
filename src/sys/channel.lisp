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


;;; SYS Source file: channel.lisp
					

(in-package "SURF-HIPPO")

(defun channel-cell (ch)
  (let ((cell-elt (channel-cell-element ch)))
    (typecase cell-elt
      (soma (soma-cell cell-elt)) 
      (segment (segment-cell cell-elt))
      (t (the cell *cell*)))))

(defun channel-pre-synaptic-node (ch)
  (let ((pre-synaptic-elt (channel-pre-synaptic-element ch)))
    (typecase pre-synaptic-elt
      (axon (axon-node pre-synaptic-elt))
      (soma (soma-node pre-synaptic-elt))
      (segment (segment-node-2 pre-synaptic-elt))
      (t (element-physical-node pre-synaptic-elt)))))

(defun CHANNEL-ACTIVE-P (channel &optional fast)
  "T when a CHANNEL satisfies the condition for being ACTIVE: the channel or type is not blocked, and the channel conductance is
not 0.0. If CHANNEL is a pointer to a channel, then the FAST flag may be used."
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let ((channel (if fast channel (element channel 'channel))))
    (and channel
	 (not (or (channel-blocked channel)
		  (channel-type-blocked (channel-type channel))
		  (= (channel-gbar channel) 0.0d0))))))

(defun channel-type-active-p (type)
  (let ((type (element-type type 'channel-type)))
    (and type
	 (not (channel-type-blocked type))
	 (channel-type-iterator
	  (ch type)
	  when (channel-active-p ch t) do (return t)
	  finally (return nil)))))

(defun number-CHANNEL-TYPE-CHANNELS (type &optional only-active)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let ((result 0))
    (declare (fixnum result))
    (channel-type-iterator
     (ch (element-type type))
     do (when (or (not only-active) (channel-active-p ch)) (incf result)))
    result))

(defun NB-ACTIVE-CHS-OF-TYPE (type)
  "Returns the number of channels of the associated with TYPE and satisfying the conditions for being active."
  (number-CHANNEL-TYPE-CHANNELS type t))

(defun CHANNEL-TYPE-CHANNELS (type &optional only-active)
  (let* ((type (element type 'channel-type)))
    (when (and type (not (and only-active (channel-type-blocked type))))
      (loop with ch = (channel-type-first-element type)
	    while ch
	    when (or (not only-active)
		     (and (not (channel-blocked ch))
			  (not (= (channel-gbar ch) 0.0d0))))
	    collect ch into chs
	    do (setq ch (channel-next-element ch))
	    finally (return chs)))))

(defun channel-particle-power (particle &optional channel-type)
  "Returns the number of phenomenological particles of type associated with PARTICLE for the channel type associated with
CHANNEL-TYPE, if supplied, or with PARTICLE, if PARTICLE refers to a specific particle instance."
  (let* ((particle (element particle 'particle))
	 (particle-type (element-type particle))
	 (channel-type (or (element-type (or (element channel-type 'channel) channel-type))
			   (when (particle-p particle) (channel-type (particle-channel particle))))))
    (loop for particle-type-power in (channel-type-particle-types-and-powers channel-type)
	  when (eq (car particle-type-power) particle-type)
	  do (return (cdr particle-type-power)))))

(defun set-channel-type-particle-types-and-powers (type particle-types-and-powers)
  ;; Given a channel TYPE, assign particle types and their respective powers by the list
  ;; PARTICLE-TYPES-AND-POWERS, whose format is:
  ;;
  ;;   '((prt-type number-of-particles) (prt-type number-of-particles) ...)
  ;;
  (let ((ch-type (element type 'channel-type)))
    (when ch-type
      (setf (channel-type-particle-types-and-powers ch-type)
	    (loop for particle-type-and-power in particle-types-and-powers
		  unless (element (car particle-type-and-power) 'particle-type)
		  do (sim-error (format nil "~A particle type hasn't been loaded!" (car particle-type-and-power)))
		  else collect (cons (element (car particle-type-and-power) 'particle-type) (cadr particle-type-and-power)))))))

(defun last-channel (ch)
  (if (and ch (channel-next-element ch))
      (last-channel (channel-next-element ch))
      ch))

(defun last-channel-of-type (type) (last-channel (channel-type-first-element type)))

(defun clear-channels-of-type (type)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setf (channel-type-first-element type) nil)
  (loop for ch being the hash-value of (CHANNEL-HASH-TABLE) when (eq type (channel-type ch)) do (setf (channel-next-element ch) nil)))

(defun get-channels-of-type (type) (loop for ch being the hash-value of (CHANNEL-HASH-TABLE) when (eq type (channel-type ch)) collect ch))

(defun channel-top-conc-int (ch)
  ;; The first conc-int associated with a channel.
  (caar (channel-conc-ints-params ch)))


(defun document-channel-type (type)
  (let ((type-name (element-name type 'channel-type))
	(type (element type 'channel-type)))
    (when type
      (format t "(channel-type-def~%")
      (format t " (~a~%" type-name)
      (document-iv-parameters type)
      (cond-every
       ((channel-type-particle-types-and-powers type)
	(format t "  (v-particles . (")	
	(loop for type-power in (channel-type-particle-types-and-powers type)
	      do (format t "(~a ~a)" (particle-type-name (car type-power)) (cdr type-power)))
	(format t "))~%"))
       ((channel-type-conc-particle-types-and-powers type)
	(format t "  (conc-particles . (")	
	(loop for type-power in (channel-type-conc-particle-types-and-powers type)
	      do (format t "(~a ~a)" (conc-particle-type-name (car type-power)) (cdr type-power)))
	(format t "))~%")))
      (print-element-document-extras type)
      (format t "                 ))~%")
      (format t "~%~%")
      (loop for type-power in (channel-type-particle-types-and-powers type)
	    do (document-particle-type (car type-power)))
      (loop for type-power in (channel-type-conc-particle-types-and-powers type)
	    do (document-conc-particle-type (car type-power))))))

(defun document-channel (&optional (ch *channel*) (circuit-dump *document-elements-for-circuit-dump*))
  (let* ((channel (element ch 'channel))
	 (target-name (element-name (element-cell-element channel)))
	 (pre-synaptic-target-name (element-name (pre-synaptic-element channel)))
	 (cell-name (cell-name (element-cell channel))))
    (when channel
      (print
       `(create-channel
	 ,(quote-symbol-cons (if circuit-dump (massage-dumped-cell-elt-name target-name cell-name) target-name))
	 ,(quote-symbol-cons (element-name (element-type channel)))
	 ,@(when pre-synaptic-target-name
		 `(,(quote-symbol-cons (if circuit-dump (massage-dumped-cell-elt-name pre-synaptic-target-name cell-name) pre-synaptic-target-name)))))))
    nil))

(defun edit-channel (ch &optional called-from-type-menu)
  (let ((ch (element ch 'channel)))
    (set-channel-parameters ch)
    (let ((dummy1 (channel-gbar/perm-reference-value ch))
	  ;; (dummy2 (or (element-parameter ch 'iv-density) 0.0))
	  (dummy3 (channel-inherit-parameters-from-type ch))
	  ;; (dummy4 (or (element-parameter ch 'iv-source) :density))
	  dummy5 (dummy6 (channel-blocked ch)) dummy8 dummy9)
      (choose-variable-values
       `((:comment ,(format nil "Current gbar/perm is ~,2e uS or cm3/s" (channel-gbar ch)))
	 (dummy1 "Absolute conductance/permeability [uS or cm3/s]" :float)
	 ;; (dummy2 "Conductance density [pS per sq uM]" :float)
	 (:comment "If type ignored, then channel defined with absolute cond/perm")
	 (dummy3 "Ignore parameters listed above and inherit from type" :boolean)
	 (dummy6 "Block this channel" :boolean)
	 ,(unless called-from-type-menu '(dummy5 "Edit Channel type" :boolean)))
       :label (format nil "Edit Channel ~a" (channel-name ch))
       :text "Conductance values are at type conductance reference temperature")
      (setf (channel-blocked ch) dummy6
	    (channel-inherit-parameters-from-type ch) dummy3)
      (setf (channel-gbar/perm-reference-value ch) dummy1)

      ;;      (unless (channel-inherit-parameters-from-type ch)
      ;;        (case dummy4
      ;;          (:absolute (set-element-iv-reference ch dummy1))
      ;;          (:density (set-element-iv-density ch dummy2))))
      
      (when dummy5 (menu-for-channel-types (channel-type ch))))
    (set-channel-parameters ch)))

(defun menu-e-rev-comment (type)
  ;; For channel and synapse types.
  (concatenate-strings
   (format nil "Use fixed reversal potential defined above or~%refer to ion permeabilities:")
   (loop for ion-perm in (element-ion-permeabilities type)
	 when (and (cadr ion-perm) (> (cadr ion-perm) 0))
	 collect (format nil " ~d% ~a " (round (* 100 (cadr ion-perm))) (string-capitalize (string (car ion-perm)))))))

(defun element-type-menu-iv-relation-mod-comment () (format nil "Gbar/Pbar global modulation:"))

(defun channel-type-menu-block-comment (type)
  (concatenate-strings
   (format nil "Block all ~a channels" (channel-type-name type))
   (unless *enable-channels* (format nil " [Global Block Applied]"))))

(defun channel-type-menu-prts-comment (type class)
  (let ((prt-names (element-names (element-type
				   (loop for prt-type-and-power in
					 (case class
					   (v-dep (channel-type-particle-types-and-powers type))
					   (conc-dep (channel-type-conc-particle-types-and-powers type)))
					 collecting (car prt-type-and-power))))))
    (format nil "Edit parameters for the ~Aparticle types~%~A"
	    (case class
	      (v-dep "")
	      (conc-dep "concentration "))
	    prt-names)))

(defun edit-channel-type (type) (when (element type 'channel-type) (menu-for-channel-types type)))

(defun edit-channels-of-type (type)
  (loop for chname in 
	(choose-list-values
	 (loop for ch in (channels) when (eq type (channel-type ch)) collect (channel-name ch))
	 nil :label (format nil "Choose Channel of Type ~A To Modify" (channel-type-name type)))
	do (edit-channel (element chname 'channel) t)))

(defun channel-type-particles-plot-options (type)
  (loop for particle-type-and-power in (channel-type-particle-types-and-powers type)
	unless (eq (particle-type-class (car particle-type-and-power)) :markov)
	do (return '(:steady_state :tau :alpha_&_beta))
	finally (return '(:alpha_&_beta :steady_state))))

(defun menu-for-channel-types (&optional types)
  (loop for type in (or (element (if (consp types) types (list types))) (menu-for-type 'channel-type)) do
	(let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy9 dummy10 dummy11 update-type
		     (dummy12 t) dummy13 dummy14 dummy15
		     dummy16 dummy17 dummy18 dummy19 dummy20 dummy21 dummy22 dummy23 dummy24 dummy25 dummy26)
	  (loop while (or dummy7 dummy10 dummy4 dummy9 dummy12 dummy20 dummy23 dummy24 dummy25 dummy18) do
		(setq dummy1 (channel-type-iv-density type)
		      dummy2 (channel-type-e-rev type) dummy3 (channel-type-blocked type)
		      dummy4 nil dummy5 nil dummy6 nil dummy7 nil
		      dummy8 nil dummy9 nil dummy10 nil dummy11 (channel-type-variable-e-rev type) dummy12 nil
		      dummy13 (channel-type-name type) dummy14 (channel-type-q10 type)
		      dummy15 (channel-type-reference-temp type)
		      dummy16 (channel-type-iv-reference type)
		      dummy17 (channel-type-iv-source type)
		      dummy18 nil
		      dummy19 (if (channel-type-use-defined-e-rev type) :fixed_erev :ion_perms)
		      dummy20 nil dummy21 nil
		      dummy22 (or (element-parameter type :IV-MODULATION) 1.0)
		      dummy23 nil dummy24 nil dummy25 nil
		      dummy26 (symbolp (channel-type-name type)))
		(let ((menu-list
		       `((dummy17 ,(MEMBRANE-ELEMENT-TYPE-IV-SOURCE-MENU-STRING type) :choose (:absolute :density) :label-left)
			 (dummy16 ,(membrane-element-type-iv-relation-absolute-reference-menu-string type) :float)
			 (dummy1 ,(membrane-element-type-iv-density-menu-string type) :float)
			 (dummy14 "Gbar/Pbar Q10" :float)
			 (dummy15 "Gbar/Pbar Kinetics Reference Temperature [degs C]" :float)
			 (dummy22 ,(element-type-menu-iv-relation-mod-comment) :float)
			 ,(unless (eq (channel-type-iv-relation type) :constant-field)
			    `(dummy2 ,(format nil "Fixed Reversal Potential [mV] (actual ~,2emV)" (current-element-type-e-rev type)) :float))
			 ,(unless (eq (channel-type-iv-relation type) :constant-field)
			    `(dummy19 ,(menu-e-rev-comment type) :choose (:fixed_erev :ion_perms) :label-left))
			 ,(unless (eq (channel-type-iv-relation type) :constant-field) `(dummy20 "Edit ion permeabilities" :boolean))
			 ,(when (channel-type-conc-int-type-params type) `(dummy21 "Edit concentration integrator parameters" :boolean))
			 ;; ,(when (element-in-circuit type) `(dummy12 "Plot somatic steady-state IV characteristic" :boolean))
			 ,(when (element-in-circuit type) `(dummy3 ,(channel-type-menu-block-comment type) :boolean))
			 (dummy13 ,(format nil "Edit name of type (used if saved to file):") :string)
			 (dummy24 "Dump definition to .elts file" :boolean)
			 (dummy25 ,(format nil
					   "Revamp channel type (and associated particle types) definitions
from current library. This will *not* remove any properties
which are excluded from the current library definitions.") :boolean)
			 (dummy23 "Restore any edited parameters above" :boolean))))
		  (cond-every
		   ((or (channel-type-particle-types-and-powers type) (channel-type-conc-particle-types-and-powers type))
		    (setq menu-list (concatenate 'list menu-list `((:comment "Gating Particles")))))
		   ((channel-type-particle-types-and-powers type)
		    (setq menu-list (concatenate 'list menu-list
						 `((dummy7 "Plot voltage-dep particles:" :x-choose ,(channel-type-particles-plot-options type) :label-left)
						   (dummy4 ,(channel-type-menu-prts-comment type 'v-dep) :boolean)))))
		   ((channel-type-conc-particle-types-and-powers type)
		    (setq menu-list (concatenate 'list menu-list
						 `((dummy10 "Plot conc-dep particles:" :x-choose (:steady_state :tau))
						   (dummy9 ,(channel-type-menu-prts-comment type 'conc-dep) :boolean)))))
		   ((and (not (eq (channel-type-iv-relation type) :constant-field)) (member 'ca (channel-type-ion-permeabilities type) :key 'car))
		    (setq menu-list (concatenate 'list menu-list '((dummy11 "Dynamically calculate e-rev based on [Ca++]" :boolean)))))
		   ((element-in-circuit type)
		    (push '(dummy18 "Modify individual channels (e.g. gbars, etc.)" :boolean) menu-list)))
		  (choose-variable-values
		   menu-list
		   :text (ADD-LINEFEEDS-TO-STRING-LIST
			  (list (when (independent-element-gbars-p type) "Some channels have independent GBAR parameters.") (ELEMENT-SOURCEFILE-STRING type nil)))
		   :label (format nil "Parameters Of Channel Type ~A" (channel-type-name type)))
		  (if dummy25 (create-channel-type type nil t)
		      (unless dummy23
			(when dummy20 (EDIT-ION-PERMEABILITIES type))
			(when dummy21 (edit-conc-int-shell-current-proportions type))
			(setf (channel-type-use-defined-e-rev type) (eq dummy19 :fixed_erev))
			(unless (string= (format nil "~A" (channel-type-name type)) dummy13)
			  (set-element-name type (if dummy26 (intern dummy13) dummy13)))
			(when dummy18 (edit-channels-of-type type))
			(setq update-type
			      (or update-type
				  (not (and (= (channel-type-iv-density type) dummy1)
					    (= (or (element-parameter type :IV-MODULATION) 1.0) dummy22)
					    (= dummy16 (channel-type-iv-reference type))
					    (eq dummy17 (channel-type-iv-source type))
					    (= (channel-type-e-rev type) dummy2)
					    (eq (channel-type-blocked type) dummy3)
					    (eq (channel-type-variable-e-rev type) dummy11)
					    (= (channel-type-q10 type) dummy14)
					    (= (channel-type-reference-temp type) dummy15)))))
			(element-parameter type :IV-MODULATION dummy22)
			(setf (channel-type-iv-density type) dummy1
			      (channel-type-iv-reference type) dummy16
			      (channel-type-iv-source type) dummy17
			      (channel-type-e-rev type) dummy2
			      (channel-type-blocked type) dummy3
			      (channel-type-variable-e-rev type) dummy11
			      (channel-type-q10 type) dummy14
			      (channel-type-reference-temp type) dummy15)
			(cond-every
			 (dummy12 (update-and-plot-iv type))
			 (dummy7 (plot-channel-v-particles type :what dummy7))
			 (dummy4 (menu-for-channel-type-particle-types type))
			 (dummy10 (plot-channel-conc-particles type :what dummy10))
			 (dummy9 (menu-for-channel-type-conc-particle-types type)))
			(setq *last-edited-channel-type* (element-name type))))
		  (when dummy24 (dump-elements-file type)))
		(when update-type (set-channels-parameters t type))))))
						     
(defun edit-conc-int-shell-current-proportions (type)
  (setf (channel-type-conc-int-type-params type)
	(loop for conc-int-type-param in (channel-type-conc-int-type-params type)
	      collect (let ((dummy1 (s-flt (* 100 (cadadr conc-int-type-param)))))
			(choose-variable-values
			 `((dummy1 ,(format nil "Percentage of current feeding shell ~A" (caadr conc-int-type-param)) :float))
			 :label (format nil "Current of ~A into concentration intergrator ~A" (element-name type) (car conc-int-type-param)))
			(list (car conc-int-type-param) (list (caadr conc-int-type-param) (/ dummy1 100.0d0))))))
  (channel-type-iterator
   (ch type) do (setf (channel-conc-ints-params ch) (parse-conc-int-info-for-element type (element-cell-element ch))))
  (set-conc-integrators-parameters))

(defun create-channel-type (type-symbol &optional actual-type-symbol)
  "TYPE-SYMBOL is a symbol or a channel type; in the former case it must match the CAR of one of the lists contained in channel
type model parameter library. Returns the channel type structure, whether is was already made or not. Even if it already exists, the
type will be updated according to the current description loaded in parameter library. This
will create particle types (v-dep and conc) according the entries in the V-PARTICLES and CONC-PARTICLES a-list entries."
;;  (typecase type-symbol
;;    (channel-type (setq type-symbol (intern (channel-type-name type-symbol))))
;;    (string (setq type-symbol (intern type-symbol))))
  (let* ((type (unless actual-type-symbol (if (channel-type-p type-symbol) type-symbol (CHANNEL-TYPE-HASH-TABLE type-symbol))))
	 (type-symbol (if (channel-type-p type-symbol) (channel-type-name type-symbol) type-symbol))
	 (model (type-symbol-model 'channel-type))
	 (library-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (type-parameters nil)
	 (parent-type-symbol (get-a-value 'parent-type library-parameters)))
    (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless library-parameters (sim-error (format nil "Don't know anything about channel type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type
	(setq type (if parent-type-symbol
		       (create-CHANNEL-TYPE parent-type-symbol type-symbol)
		       (make-CHANNEL-TYPE :name type-symbol))))
      (when parent-type-symbol (clear-up-iv-relation-references-from-parent-type type library-parameters))
    (update-element-parameters-with-new-parameters (element-parameters parent-type-symbol) type)
      (clean-up-membrane-element-type-parameters-to-be-updated type library-parameters)
      ;; Add LIBRARY-PARAMETERS to the existing type element parameters.
      (setq type-parameters (update-element-parameters-with-new-parameters library-parameters type))
      ;; (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      ;; Go through the type-parameters to set various channel-type structure slots.
    (setf (channel-type-particle-types-and-powers type)
	    (loop for v-part-type-and-order in 
		 (nconc (get-a-value 'v-particles type-parameters) ; 16.09.2016 Deprecate V-PARTICLES entry in type definition for PARTICLES, but maintain backward compatibility.
			(get-a-value 'particles type-parameters))
	       collecting
		  (cons (create-particle-type (car v-part-type-and-order))
			(cadr v-part-type-and-order)))
	    (channel-type-conc-particle-types-and-powers type)
	    (loop for ca-part-type-and-order in (get-a-value 'conc-particles type-parameters) collecting
		  (cons (create-conc-particle-type (car ca-part-type-and-order))
			(cadr ca-part-type-and-order)))
	    (channel-type-conc-int-type-params type)
	    (massage-conc-int-type-params (get-a-value 'conc-int-type-params type-parameters)))
      (extract-conductance-type-parameters type type-parameters)
      (cond-every
       ((assoc 'CONC-INT-TYPE-E-REV-PARAMS type-parameters)
	(element-parameter type 'CONC-INT-TYPE-E-REV-PARAMS (massage-conc-int-type-params (get-a-value 'CONC-INT-TYPE-E-REV-PARAMS type-parameters)))))
      (setf (CHANNEL-TYPE-HASH-TABLE type-symbol) type)
      (setq *channel-type* type)
      (set-channels-parameters t type)
    type))

#|
(defun revamp-channel-type-parameters (&optional type)
;;  "Update the parameters (including those for the associated particle types) of channel TYPE, if specified, else all channel
;; types, according to the current parameters in the channel type, particle type, and conc particle type parameter libraries."
  (loop for type in (if type (list (element type 'channel-type)) (channel-types)) do (create-channel-type type nil t)))
|#

(defun get-channel-simple-name ()
  (loop for candidate from (max 1 *channel-simple-name-counter*)
	until (not (channel-hash-table candidate))
	finally (return (setf *channel-simple-name-counter* candidate))))

(defun rename-channels-simple (&optional (channels (channels)))
  "Rename CHANNELS [default all channels in circuit] with simple integer names."
  (loop for ch in (coerce-to-list (element channels 'channel)) do (set-element-name ch (get-channel-simple-name))))

(defun create-channel (element type &key pre-synaptic-element conc-int-delta)
  "Create a channel of TYPE on the cell element associated with ELEMENT. CONC-INT-DELTA applies to channels associated with
concentration integrators, and specifies the fraction between 0 and 1 of the total channel current which will source the
appropriate integrator. When supplied, PRE-SYNAPTIC-ELEMENT specifies the cell element which controls the channel, otherwise taken
as the cell element associated with ELEMENT."
  (unless *only-load-passive*
    (let* (*print-pretty*
	   (cell-element (element-cell-element element))
	   (node (element-node cell-element))
	   (type (if (channel-type-p type) type (create-channel-type type)))
	   ;; (type-parameters (channel-type-parameters type))
	   (channel-type-name (channel-type-name type))
	   (channel-name (if *use-simple-names* (GET-CHANNEL-SIMPLE-NAME) (format nil "~A-~A" (node-name node) channel-type-name)))
	   (current-channel (CHANNEL-HASH-TABLE channel-name)))
      (cond
       ;; If *allow-duplicate-elements* is NIL, check for same type of channel at the node.
       ((duplicate-element-check node type) nil)
       ;; Additional tests
       (nil				; current-channel
	(unless *suppress-element-creation-messages*
	  (format t "CREATE-CHANNEL: channel ~a already defined~%" channel-name)))
       ;; Create channel if there is not one of the given name, or an alternate name is allowed.
       ((or (not current-channel)
	    (confirm-alternate-name-creation-and-update-*PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*
	     (format nil "Make new name for duplicate channel type ~A on ~A" channel-type-name (if node (node-name node) "Undefined Dest."))))
	(let ((channel-name (if current-channel (check-element-name channel-name 'channel) channel-name)))
	  (when channel-name
	    (let ((ch (make-channel :name channel-name :cell-element cell-element :type type
				    :inherit-parameters-from-type (channel-type-inherit-parameters-from-type type))))
	      (when pre-synaptic-element
		(let ((pre-syn-node (element-node (element-cell-element pre-synaptic-element))))
		  (setf (channel-pre-synaptic-element ch) pre-synaptic-element
			(node-has-v-dep-element pre-syn-node) t)))
	      (loop for particle-type-and-power in (reverse (channel-type-particle-types-and-powers type))
		    do (push (create-particle ch (car particle-type-and-power)) (channel-particles ch)))
	      (when (channel-type-conc-int-type-params type)
		(setf (channel-conc-ints-params ch) (parse-conc-int-info-for-element type cell-element)))
	      (loop for conc-particle-type-and-power in (reverse (channel-type-conc-particle-types-and-powers type))
		    do (push (create-conc-particle ch (car conc-particle-type-and-power)) (channel-conc-particles ch)))
	      (when conc-int-delta
		(when (> conc-int-delta 1.0)
		  (sim-error (format nil "The CONC-INT-DELTA for ~A is greater than one!~%" (element-name ch))))
		(element-parameter ch 'conc-int-delta conc-int-delta))
	      (push ch (node-elements node))
	      (element-slot-fast (element-slot-fast type :last-element) :next-element ch)
	      (element-slot-fast type :last-element ch)
	      (setf (CHANNEL-HASH-TABLE channel-name) ch)))
	  (set-channel-parameters (CHANNEL-HASH-TABLE channel-name))
	  (setq *enable-channel-type-membrane-parameter-update* t
		*enable-channel-membrane-parameter-update* t
		*recheck-circuit-elements-parameters* t
		*make-node-w/elements-array* t
		*channel* (CHANNEL-HASH-TABLE channel-name))
	  *channel*))))))

(defun create-channels (element types)
  ;; "Add channels of type specified in TYPES to cell-element associated with ELEMENT."
  (when (element-cell-element element)
    (dolist (channel-type (coerce-to-list types)) (create-channel (element-cell-element element) channel-type))))

(defun add-channels-ad-lib (targets type) (add-channel-to-targets targets type))  

(defun add-channel-to-targets (targets type)
  ;; "Adds channel of TYPE to cell elements associated with TARGETS."
  (when *monitor-circuit-modifications*
    (format t ";; (add-channel-to-targets ~%;;")
    (FORMATTED-LIST-DUMP (loop for target in targets collect (element-name target)) t t)
    (format t "~%;; ~a~%)" (element-name type)))
  (loop for target in targets 
	when (element-cell-element target) do (create-channel (element-cell-element target) type)))

(defun add-channels-to-targets (targets types)
  ;; Adds channels of TYPES to cell elements associated with TARGETS.
  (loop for type in types do (add-channel-to-targets targets type)))

(defun add-channel-type (type-symbol &optional percentage-of-channel-nodes)
  ;; Adds channel of name TYPE-SYMBOL to all cells.
  (add-channel-to-cells (cells) type-symbol percentage-of-channel-nodes))
 
(defun add-channel-to-cells (cells types &optional (percentage-of-channel-nodes 100))
  ;; Adds channel of TYPES to CELLS. Both TYPES and CELLS may be either an atom or a list.
  (when (and *always-intialize-random-gen* percentage-of-channel-nodes) (get-reference-random-state))
  (let ((*monitor-circuit-modifications* *monitor-circuit-modifications*))
    (when *monitor-circuit-modifications*
      (format t ";; (add-channel-to-cells ~%;; ")
      (FORMATTED-LIST-DUMP (element-name cells) t t)
      (format t "~%;; ~a" (element-name types))
      (when percentage-of-channel-nodes (format t " ~a" percentage-of-channel-nodes))
      (format t "~%;; )~%"))
    (setq *monitor-circuit-modifications* nil)
    (loop for cell-element in (cell-elements cells)
	  when (or (not percentage-of-channel-nodes) (= 100 percentage-of-channel-nodes) (< (random 100) percentage-of-channel-nodes))
	  do (create-channels cell-element types))))

(defun channel-type-particle-types (element)
  "Return a list of any particle types associated with the channel type associated with ELEMENT."
  (let ((element (element-type element)))
    (when (channel-type-p element)
      (loop for v-part-type-and-order in (channel-type-particle-types-and-powers element) collect (car v-part-type-and-order)))))

(defun channel-type-conc-particle-types (element)
  "Return a list of any concentration particle types associated with the channel type associated with ELEMENT."
  (let ((element (element-type element)))
    (when (channel-type-p element)
      (loop for ca-part-type-and-order in (channel-type-conc-particle-types-and-powers element) collect (car ca-part-type-and-order)))))

(defun get-current-channel-type-names ()
  (loop for type in (channel-types) when (channel-type-first-element type) collect (channel-type-name type)))

(defun get-current-channel-types ()
  (loop for type in (channel-types) when (channel-type-first-element type) collect type))

(defun get-current-channel-types-menu (&optional (label "Channel Types In Circuit"))
  (loop for name in (choose-list-values (get-current-channel-type-names) nil :do-all-at-once t :label label) collect (element name 'channel-type)))

(defun are-there-channels () (true-p *channel*))

(defun print-channels () (PRINT-MODEL-PARAMETERS "channel"))

(defun print-channel (ch)
  (let ((ch (element ch 'channel)))
    (format t "~a~A: type ~A, cell ~a, ~A ~A; ~A~A"
	    (massage-element-plot-label ch nil t)
	    (cond ((channel-type-blocked (channel-type ch)) " (type blocked)")
		  ((channel-blocked ch) " (blocked)")
		  (t ""))
	    (element-name (element-type ch)) (cell-name (node-cell (channel-node ch)))
	    (string-downcase (string (type-of (element-cell-element ch)))) (element-name (element-cell-element ch))
	    (print-element-iv-relation-string ch)
	    (if (element-parameter ch 'conc-int-delta)
	      (format nil " CONC-INT Delta: ~,2e" (element-parameter ch 'conc-int-delta)) ""))
    (when *simulation-initialized*
      (format t "~&    Conductance ~,2e ~a, current ~,2e ~a @ ~,2e ms"
	      (s-flt (channel-conductance ch)) (channel-conductance-units)
	      (s-flt (get-channel-current ch)) (channel-current-units)
	      *real-time*))))
  
(defun channel-conductance-units () (plot-list-info-units-label (find '*plot-channel-conductances* *plot-lists-info* :key 'car)))
  
(defun channel-current-units () (plot-list-info-units-label (find '*plot-channel-currents* *plot-lists-info* :key 'car)))

(defun print-channel-types () (PRINT-MODEL-PARAMETERS "channel-type"))

(defun channels-of-type (type &optional cell-element)
  "Return a list of channels of TYPE that are associated with members of CELL-ELEMENT [atom or list].  Members of CELL-ELEMENT may
refer explicitly to a cell type or specific cell, or may be associated with a cell element. If CELL-ELEMENT is NIL, then all
channels of TYPE are returned."
  (let ((type (element type 'channel-type)))
    (when type
      ;; Parse CELL-ELEMENT arg.
      (loop for elt in (coerce-to-list (element cell-element))
	    when (cell-type-p elt) collect elt into cell-types
	    when (cell-p elt) collect elt into cells
	    else collect (element-cell-element elt) into cell-elements
	    finally
	    (return
	      ;; Collect channels associated with the cell types, cells, or cell elements of CELL-ELEMENT.
	      (loop with ch = (channel-type-first-element type)
		    while ch
		    when
		    (or (not (or cell-types cells cell-elements))
			(let ((channel-cell (channel-cell ch)))
			  (or
			   (member (cell-type channel-cell) cell-types)
			   (member channel-cell cells)
			   (member (channel-cell-element ch) cell-elements))))
		    collect ch into chs
		    do (setq ch (channel-next-element ch))
		    finally (return chs)))))))

(defun channel-types-of-ion-type (ion-type &optional (only-loaded t) (only-in-circuit t) exclude-conc-dependent-types)
  (if only-in-circuit
      (loop for type in (channel-types)
	    when (and (element-in-circuit type)
		      (not (and exclude-conc-dependent-types (element-has-concentration-dependence type)))
		      (find ion-type (channel-type-ion-permeabilities type) :key 'car))
	    collect type)
      (no-nils
       (delete-duplicates
	(loop for ch-list in (MODEL-PARAMETER-type-LIBRARY (type-symbol-model 'channel-type))
	      when (and (find ion-type (get-a-value 'ion-permeabilities (cdr ch-list)) :key 'car)
			(not (and exclude-conc-dependent-types (get-a-value 'conc-particles (cdr ch-list)))))
	      collect (if only-loaded (element (car ch-list)) (create-channel-type (car ch-list))))))))

(defun channels-of-ion-type (ion-type)
  (loop for type in (channel-types-of-ion-type ion-type) nconc (copy-list (channel-type-channels type))))
  
(defun print-channel-type (type &optional (always t))
  (let ((type (element type 'channel-type)))
    (when type
      (let ((chs (channels-of-type type)))
	(when (or always chs)
	  (format t "Channel Type ~a: " (channel-type-name type))
	  (print-membrane-element-type-iv-parameters type)
	  (when (channel-type-blocked type) (format t "    This channel type is blocked!~%"))
	  (when (channel-type-particle-types-and-powers type)
	    (format t "    Particle Types: ")
	    (loop for particle-type-and-power in (channel-type-particle-types-and-powers type) do
		  (format t "~a (power ~a) " (particle-type-name (car particle-type-and-power)) (cdr particle-type-and-power)))
	    (format t "~%"))
	  (when (channel-type-conc-particle-types-and-powers type)
	    (format t "    Concentration Particle Types: ")
	    (loop for particle-type-and-power in (channel-type-conc-particle-types-and-powers type) do
		  (format t "~a (power ~a) " (conc-particle-type-name (car particle-type-and-power)) (cdr particle-type-and-power)))
	    (format t "~%"))
	  (when (channel-type-conc-int-type-params type)
	    (format t "    Concentration Integrator Parameters:~%")
	    (loop for conc-int-type-param in (channel-type-conc-int-type-params type) do
		  (format t "      Feeds conc int type ~A, shell ~A (~,1f%)~%"
			  (car conc-int-type-param) (caadr conc-int-type-param) (s-flt (* 100 (cadadr conc-int-type-param))))))
	  (print-inheritance-info chs)
	  (print-num-elements-sourcefile type)
	  (format t "~%"))))))

(defun print-channel-type-brief (type)
  (let* ((type (element type 'channel-type))
	 (number-CHANNEL-TYPE-CHANNELS (number-CHANNEL-TYPE-CHANNELS type)))
    (format t "Channel Type ~a: ~D child~a  " (channel-type-name type) number-CHANNEL-TYPE-CHANNELS
	    (if (= 1 number-CHANNEL-TYPE-CHANNELS) "" "ren"))
    (when (or (channel-type-particle-types-and-powers type) (channel-type-conc-particle-types-and-powers type))
      (format t "["))
    (when (channel-type-particle-types-and-powers type)
      (format t "Part-Types:")
      (loop for particle-type-and-power in (channel-type-particle-types-and-powers type) do
	    (format t " ~a" (particle-type-name (car particle-type-and-power)))))
    (when (channel-type-conc-particle-types-and-powers type)
      (when (channel-type-particle-types-and-powers type)
	(format t ", "))
      (format t "Conc-Part-Types:")
      (loop for conc-particle-type-and-power in (channel-type-conc-particle-types-and-powers type) do
	    (format t  " ~a" (conc-particle-type-name (car conc-particle-type-and-power)))))
    (when (or (channel-type-particle-types-and-powers type) (channel-type-conc-particle-types-and-powers type))
      (format t "]"))
    (unless (= (or (element-parameter type :IV-MODULATION) 1.0) 1.0)
      (format t "~%   GBAR modulation by ~,2e" (element-parameter type :IV-MODULATION)))
    (when (channel-type-blocked type) (format t " (**BLOCKED**)"))
    (format t "~%")
    ))

(defun print-create-channels-for-cell (cell &optional (indent-output 0))
  (loop for channel being the hash-value of (CHANNEL-HASH-TABLE) when (eq (element-cell channel) cell) do
	(when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
	(document-channel channel)))


(defun unblock-or-block-channel-types (types block)
  (loop for type in (coerce-to-list types) when (element type 'channel-type) do (setf (channel-type-blocked (element type 'channel-type)) block)))

;; (defun block-channel-types (types) (loop for type in (coerce-to-list types) when (element type 'channel-type) do (setf (channel-type-blocked (element type 'channel-type)) t)))
(defun block-channel-types (types) (unblock-or-block-channel-types types t))

;; (defun block-all-channel-types () (loop for syn in (channel-types) do (setf (channel-type-blocked syn) t)))
(defun block-all-channel-types () (block-channel-types (channel-types)))

;; (defun unblock-channel-types (types) (loop for type in (coerce-to-list types) when (element type 'channel-type) do (setf (channel-type-blocked (element type 'channel-type)) nil)))
(defun unblock-channel-types (types) (unblock-or-block-channel-types types nil))

;; (defun unblock-all-channel-types () (loop for syn in (channel-types) do (setf (channel-type-blocked syn) nil)))
(defun unblock-all-channel-types () (unblock-channel-types (channel-types)))

(defun blocked-channel-types () (loop for type in (channel-types) when (and (instance-in-cell type) (channel-type-blocked type)) collect type))

(defun block-all-channels () (loop for ch in (channels) do (setf (channel-blocked ch) t)))
(defun unblock-all-channels () (unblock-all-channel-types) (loop for ch in (channels) do (setf (channel-blocked ch) nil)))

(defun set-channel-types-parameters () (loop for type in (channel-types) do (set-channel-type-parameters type)))

(defun set-channel-type-parameters (type)
  (fixup-type-modulation type)
  (UPDATE-g-TYPE-CELL-TYPE-IV-RELATION-COEFFICIENT type))

(defun set-channels-parameters (&optional (update-fixed-e-rev t) type (update-channel-gbars t))
  ;; Note that SET-CHANNEL-PARAMETERS is defined in element_functions.lisp
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for type in (or (coerce-to-list type) (channel-types)) do
	(let ((IV-MODULATION (or (element-parameter type :IV-MODULATION) (element-parameter type 'pbar-modulation) 1.0)))
	  (channel-type-do
	   (syn (element type 'channel-type))
	   (set-channel-parameters syn update-fixed-e-rev update-channel-gbars IV-MODULATION))))
  nil)

(defun set-channel-type-parameters (type)
  (let ((type (element type 'channel-type)))
    (when type (channel-type-iterator (ch type) do (set-channel-parameters ch t))))
  nil)

;; *******************************
;;
;; Setup/Initialization Functions
;;
;; *******************************

(defun remove-channel-type-lists (type)
  (remove-element-parameters type '(channel-particles-array
				    channel-conc-particles-array
				    channel-type-particle-info-array
				    channel-array
				    channel-node-fixnums-array
				    channel-node-floats-array
				    channel-array-length
				    evaluation-arrays
				    ;; channel-particles
				    particle-info
				    node-fixnums
				    node-floats)))

(defun revamp-channel-type-lists (type channels &optional inclusion)
  (when inclusion (setq channels (loop for channel in channels when (funcall inclusion channel) collect channel)))
  (when channels
    (loop for ch in channels
	  ;; collect (loop for prt in (concatenate 'list (channel-conc-particles ch) (channel-particles ch))
	  ;;		collect (element-slot prt :double-floats))
	  ;; into channel-particles-list
	  collect (list
		   (loop for particle-type-and-power in
			 (no-nils (concatenate 'list
					       (channel-type-conc-particle-types-and-powers (channel-type ch))
					       (channel-type-particle-types-and-powers (channel-type ch))))
			 collect (cdr particle-type-and-power))
		   (and (or (channel-conc-ints-params ch) ; flag for saving conductance
			    (member ch *plot-channel-currents-structures*)
			    (member ch *plot-channel-conductances-structures*)
			    (vsource-on-node (channel-node ch))) t)) into channel-type-particle-info-list
	  collect (node-double-floats (channel-node ch)) into node-floats-list
	  collect (node-fixnums (channel-node ch)) into node-fixnums-list
	  finally
	  (element-parameter type 'particle-info channel-type-particle-info-list)
	  (element-parameter type 'node-fixnums node-fixnums-list)
	  (element-parameter type 'node-floats node-floats-list))
    nil))

(defun setup-channels ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setq *channel-type-list*
	(when *enable-channels*
	  (no-nils
	   (loop for type in (channel-types) do
		 (remove-channel-type-lists type)
		 (update-membrane-element-type-params type)
		 when (channel-type-active-p type)
		 collect type)))))

;; Make this smarter for multiple cell type circuits
(defun init-channels-e-rev ()
  (loop for type in *channel-type-list* do
	(unless (channel-type-variable-e-rev type)	
	  (let* ((cell-types-in-circuit (list-of-all-things-in-circuit 'cell-type))
		 (fixed-e-rev (when (= (length cell-types-in-circuit) 1) (current-element-type-e-rev type (car cell-types-in-circuit)))))
	    (channel-type-iterator
	     (ch type)
	     unless (or (channel-blocked ch) (channel-conc-ints-params ch)) do (update-element-fixed-e-rev ch fixed-e-rev type)))))
  nil)

(defun init-channels ()
  (loop for type in (channel-types)
	do (channel-type-iterator (ch type) do (setf (channel-conductance ch) 0.0d0
						     (channel-current ch) 0.0d0)))
  (when *enable-channels* (init-channels-e-rev) (eval-all-channels t)))

;; *******************************
;;
;; Evaluation functions
;;
;; *******************************

;; This is used in synapse-evaluation.lisp too
(defmacro static-v-dependence-value (node static-v-dependence)
  `(aref (the (simple-array single-float (,*PARTICLE-LOOK-UP-TABLE-LENGTH*)) ,static-v-dependence) (node-fixnum-aref-prt-v-index (node-fixnums ,node))))

(proclaim '(inline channel-voltage))
(defun channel-voltage (ch) (node-voltage-n (channel-node ch)))

(proclaim '(inline channel-voltage-n+1))
(defun channel-voltage-n+1 (ch) (node-voltage-n+1 (channel-node ch)))

(proclaim '(inline channel-ohmic-current))
(defun channel-ohmic-current (ch)
  ;; Returns current in nA
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (* (channel-conductance ch) (- (channel-voltage ch) (channel-e-rev ch))))

(proclaim '(inline channel-ohmic-current-n+1))
(defun channel-ohmic-current-n+1 (ch)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (* (channel-conductance ch) (- (channel-voltage-n+1 ch) (channel-e-rev ch))))

(proclaim '(inline channel-constant-field-current))
(defun channel-constant-field-current (&key permeability conc-in conc-out valence voltage)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (constant-field-equation-double voltage conc-in conc-out 1.0d0 valence permeability))

(proclaim '(inline channel-constant-field-current-dumb))
(defun channel-constant-field-current-dumb (ch &key permeability conc-in conc-out valence voltage)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (* (the df (or permeability (channel-conductance ch)))
     (the df (constant-field-current-coefficient
	      ch
	      :conc-in-double conc-in :conc-out-double conc-out :valence valence
	      :voltage-double (if voltage (d-flt voltage) (node-voltage-n (channel-node ch)))))))

(proclaim '(inline channel-constant-field-current-n-1))
(defun channel-constant-field-current-n-1 (ch &key permeability conc-in conc-out valence)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (channel-constant-field-current-dumb ch :permeability permeability
				       :conc-in conc-in :conc-out conc-out :valence valence
				       :voltage (node-voltage-n-1-double (channel-node ch))))
  
(proclaim '(inline channel-constant-field-current-n+1))
(defun channel-constant-field-current-n+1 (ch &key permeability conc-in conc-out valence)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (channel-constant-field-current-dumb ch :permeability permeability
				       :conc-in conc-in :conc-out conc-out :valence valence
				       :voltage (node-voltage-n+1 (channel-node ch))))

(proclaim '(notinline get-channel-current))
(defun get-channel-current (ch &key conc-in conc-out valence voltage)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  "Current in nA (double float) of CH at time tn."
  (case (channel-type-iv-relation (channel-type ch))
    (:CONSTANT-FIELD
     (if (and conc-in conc-out valence voltage)
	 (channel-constant-field-current :permeability (channel-conductance ch) :conc-in conc-in :conc-out conc-out :valence valence :voltage voltage)
	 (channel-constant-field-current-dumb ch :permeability (channel-conductance ch) :conc-in conc-in :conc-out conc-out :valence valence :voltage voltage)))
    (t (channel-ohmic-current ch))))


(proclaim '(inline get-channel-current-complete))
(defun get-channel-current-complete (ch &key conc-in conc-out valence voltage)
  ;; Returns the channel current nA of CH at time N, a double float.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (case (channel-type-iv-relation (channel-type ch))
    (:CONSTANT-FIELD
     (channel-constant-field-current :permeability (channel-conductance ch) :conc-in conc-in :conc-out conc-out :valence valence :voltage voltage))
    (t (channel-ohmic-current ch))))

(defun get-channel-current-n+1 (ch)
  ;; Returns the channel current nA of CH at time N+1, a double float.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (case (channel-type-iv-relation (channel-type ch))
    (:CONSTANT-FIELD (channel-constant-field-current-n+1 ch))
    (t (channel-ohmic-current-n+1 ch))))

(defun get-channel-e-rev (channel)	; mV
  (let ((type (channel-type channel)))
    (if (and (channel-conc-ints-params channel)
	     (not (channel-type-variable-e-rev type))
	     (eq (channel-type-iv-relation type) :constant-field))
      (element-e-rev-from-shells (channel-conc-ints-params channel))
      (channel-e-rev channel))))

(proclaim '(inline get-channel-conc-in)) 
(defun get-channel-conc-in (ch &optional fast-conc-in-calculation) ; mM
  (the df (get-element-conc-ints-conc-in ch (channel-conc-ints-params ch) fast-conc-in-calculation)))

(proclaim '(inline get-channel-conc-out))
(defun get-channel-conc-out (ch &optional fast-conc-out-calculation) ; mM
  (the df (get-element-conc-ints-conc-out ch (channel-conc-ints-params ch) fast-conc-out-calculation)))

(defvar *include-constant-field-current* t)

(defmacro particle-looper (prts prt-types-and-powers conductance)
  `(loop for prt in ,prts
	 for prt-type-and-power in ,prt-types-and-powers
	 do (setq ,conductance (* ,conductance (ch-power-double-macro (particle-aref-state-n+1 (particle-double-floats prt)) (cdr prt-type-and-power))))))

(defmacro conc-particle-looper (prts prt-types-and-powers conductance)
  `(loop for c-prt in ,prts
	 for c-prt-type-and-power in ,prt-types-and-powers
	 do (setq ,conductance
		  (* ,conductance (ch-power-double-macro (conc-particle-aref-state-n+1 (conc-particle-double-floats c-prt)) (cdr c-prt-type-and-power))))))

(defun save-current-during-eval-p (type &optional fast)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  ;; NG, 31/10/97, J'ai supprime tout les tests de ELEMENT-CONC-INT-TYPE-PARAMS.
  (true-p (or (element-conc-int-type-params type)
	    (let ((element-iv-parameters (element-iv-parameters type)))
	      (when element-iv-parameters
		(eq (membrane-element-type-iv-relation element-iv-parameters) :constant-field))))))

(defun save-ch-current-during-eval-p (type)
  (true-p (or (channel-type-conc-int-type-params type)
	      (let ((element-iv-parameters (channel-type-iv-parameters type)))
		(when element-iv-parameters
		  (eq (membrane-element-type-iv-relation element-iv-parameters) :constant-field))))))

(defun eval-all-channels (&optional (first-iteration-of-step t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *channel-type-list* do
	(let* ((type-params (channel-type-parameters type))
	       (static-voltage-dependence (get-a-value 'static-voltage-dependence type-params))
	       (constant-field (eq (channel-type-iv-relation type) :constant-field))
	       (conc-int-p (true-p (channel-type-conc-int-type-params type)))
	       (save-current-during-eval-p (save-ch-current-during-eval-p type))
	       (valence (when save-current-during-eval-p (get-a-value 'effective-default-valence type-params)))
	       (save-e-rev (and conc-int-p first-iteration-of-step (channel-type-variable-e-rev type)))
	       (fixed-conc-in (and save-current-during-eval-p
				   (not (get-a-value 'intracellular-conc-ints type-params))
				   (get-a-value 'effective-default-intracellular-concentration type-params)))
               (fast-conc-in-calculation (and save-current-during-eval-p
                                              (not fixed-conc-in)
                                              (= 1 (length (the cons (channel-type-conc-int-type-params type))))))
	       (fixed-conc-out (and save-current-during-eval-p
				    (not (get-a-value 'extracellular-conc-ints type-params))
				    (get-a-value 'effective-default-extracellular-concentration type-params)))
	       (fast-conc-out-calculation (and save-current-during-eval-p
					       (not fixed-conc-out)
					       (= 1 (length (the cons (channel-type-conc-int-type-params type))))))
	       ;; The 0.0's are dummies.
	       (zf/rt (if save-current-during-eval-p (* (the sf valence) *F/RT*) 0.0))
	       (zsqd-ff/rt (if save-current-during-eval-p (* (the sf valence) faraday zf/rt) 0.0))
	       (conductance-function (get-a-value 'conductance-function type-params)))
	  (declare (single-float zf/rt zsqd-ff/rt))
	  (channel-type-iterator
	   (ch type)
	   unless (channel-blocked ch)
	   do (let* ((conductance (channel-gbar ch))
		     (channel-node (channel-node ch))
		     (node-double-floats (node-double-floats channel-node)))
		(declare (double-float conductance))
		(particle-looper (channel-particles ch) (channel-type-particle-types-and-powers type) conductance)
		(conc-particle-looper (channel-conc-particles ch) (channel-type-conc-particle-types-and-powers type) conductance)
		(when conductance-function (setq conductance (* conductance (the sf (funcall (the function conductance-function) ch)))))
		(when static-voltage-dependence (setq conductance (* conductance (static-v-dependence-value channel-node static-voltage-dependence))))
		(setf (channel-conductance ch) conductance) ; uS or cm3/sec
		(when save-e-rev (setf (channel-e-rev ch) (element-e-rev-from-shells (channel-conc-ints-params ch))))
		(when save-current-during-eval-p
		  (setf (channel-current ch) ; nA
			(if CONSTANT-FIELD
			  (fast-constant-field-equation
			   (node-aref-particle-voltage node-double-floats)
			   (the df (or fixed-conc-in (get-channel-conc-in ch fast-conc-in-calculation)))
			   (the df (or fixed-conc-out (get-channel-conc-out ch fast-conc-out-calculation)))
			   zsqd-ff/rt zf/rt conductance)
			  (channel-ohmic-current ch)))
		  ;; (format t "time ~A current ~A (~A) ~%" *real-time* (channel-current ch) (get-channel-current-n+1 ch))
		  )
                (unless (node-has-ideal-voltage-source channel-node)
                  (if CONSTANT-FIELD
		    (accumulate-setf (node-aref-current node-double-floats) (channel-current ch))
		    (progn (deccumulate-setf (node-aref-current node-double-floats) (* conductance (channel-e-rev ch)))
			   (accumulate-setf (node-aref-jacobian node-double-floats) conductance))))))))
  nil)

;; *************************************
;;
;; Plotting Stuff
;;
;; *************************************

(defun plot-channel-types (&optional channel-types)
  (loop for type in (or (coerce-to-list channel-types)
			(select-hash-values-menu (CHANNEL-TYPE-HASH-TABLE) "Select Channel Types To Plot Particles"))
	do
	(plot-channel-v-particles type)
	(plot-channel-conc-particles type)))

(defun plot-channel-v-particles (type &key (new-plot nil) overlay (what '(:steady_state :tau)) query) ; WHAT can also be :ALPHA_&_BETA or :ALL
  (declare (ignore query))
  (let ((type (element type 'channel-type))
	(*create-new-plot-windows* new-plot))
    (when type (plot-particle-types (channel-type-particle-types-and-powers type) :what what :overlay overlay :new-plot new-plot :channel-type type))))

(defun channel-type-particle-plot-comment (channel-type)
  (let ((channel-type (element channel-type 'channel-type)))
    (when channel-type
      (case (channel-type-iv-relation channel-type)
	(:ohmic (format nil "~A~%~A"
			(case (channel-type-iv-source channel-type)
			  (:density (format nil "Gbar ~,2e pS/um2" (channel-type-iv-density channel-type)))
			  (:absolute (format nil "Gbar ~,2e uS" (channel-type-iv-reference channel-type))))
			(cond ((channel-type-variable-e-rev channel-type) "Conc-dep E-rev")
			      ((channel-type-use-defined-e-rev channel-type) (format nil "E-rev ~a mV" (channel-type-e-rev channel-type)))
			      (t "Ion-perm-dep E-rev"))))
	(:constant-field (format nil "~A~%Constant Field Model"
				 (case (channel-type-iv-source channel-type)
				   (:density (format nil "Perm Dens ~,2e 1.0e-6cm3/s/um2" (channel-type-iv-density channel-type)))
				   (:absolute (format nil "Perm ~,2e cm3/s" (channel-type-iv-reference channel-type))))))))))

(defun plot-channel-conc-particles (type &key (new-plot nil) overlay  (what '(:steady_state :tau))
					 (voltage-step 20.0) (voltage-min -70.0) (voltage-max 10.0)
					 (min-conc 1.0e-6) (max-conc 1.0e2) ; mM
					 increment-voltage)
  (loop for prt-type-power in (channel-type-conc-particle-types-and-powers (element type 'channel-type)) do
	(plot-conc-particle (car prt-type-power) :power (cdr prt-type-power)
			    :new-plot new-plot :overlay overlay :what what
			    :voltage-step voltage-step :voltage-min voltage-min :voltage-max voltage-max
			    :min-conc min-conc :max-conc max-conc :increment-voltage increment-voltage)))

(defun menu-for-channel-parameters ()
  (let ((dummy1 (not *channel*))
	dummy5)
    (unless dummy1
      (choose-variable-values `((dummy1 "Edit parameters of channel types" :boolean)
				,(when *channel* `(dummy5 "Remove channel types" :boolean)))
			      :label "Setting Up Channel Parameters"))
    (cond-every
     (dummy5 (menu-for-removing-channel-types))
     (dummy1 (MENU-FOR-CHANNEL-TYPES)))))

(defun menu-for-removing-channel-types ()
  (loop for type-symbol
	in (choose-list-values (namelist-of-all-things 'channel-type) nil
			       :punt-if-only-one-entry nil :do-all-at-once t :rank-margin 5 :direction :vertical
			       :label "Choose Channel Types to Remove"
			       :text "This will also remove associated particles and concentration particles.")
	do (erase-element type-symbol 'channel-type)))

(defun menu-for-removing-channels (target-elt)
  (loop for name in
	(choose-list-values
	 (loop for channel in (channels target-elt) collect (channel-name channel))
	 nil :label (format nil "Choose Channels to Remove from ~A" (element-name target-elt)))
	do (erase-element name 'channel)))

(defun menu-for-adding-channel-types (&optional target-elt)
  ;; just to clean things up
  (setf (MODEL-PARAMETER-type-LIBRARY (type-symbol-model 'channel-type))
	(delete-duplicates (MODEL-PARAMETER-type-LIBRARY (type-symbol-model 'channel-type)) :from-end t :key 'car :test 'eql))
  (if (and target-elt
	   (let ((dummy1 :add))
	     (choose-variable-values
	      '((dummy1 "Action vis-a-vis channels:" :choose (:add :remove)))
	      :label (format nil "Editing Channels for ~A" (element-name target-elt)))
	     (eq dummy1 :remove)))
    (menu-for-removing-channels target-elt)
    (let* ((original-channels (when target-elt (channels target-elt)))
	   (new-channel-type-names
	    (element-name
	     (menu-for-type 'channel-type
			    :exclude-types (loop for ch in original-channels collect (channel-type ch))
			    :text (when original-channels
				    (format nil "Channel types at this node:~%~A"
					    (concatenate-string-list (element-name (element-type original-channels))
								     :string-spacer ", " :string-count-to-add-linefeed 5)))
			    :label (if target-elt
				     (format nil "Channels to Add to ~a" (element-name target-elt))
				     (format nil "Choose Channel Types To Add Randomly"))))))
      (when nil				; target-elt
	(loop for channel in original-channels
	      when (not (string-member (channel-type-name (channel-type channel)) new-channel-type-names))
	      do (erase-element channel)
	      else do (setq new-channel-type-names (string-remove (channel-type-name (channel-type channel)) new-channel-type-names))))
      (if target-elt
	(create-channels target-elt new-channel-type-names)
	(loop for type-symbol in new-channel-type-names
	      do
	      (setq *recheck-circuit-elements-parameters* t)
	      (add-channel-type
	       type-symbol
	       (get-integer 100 100 0 "Add randomly to this percentage of nodes:" (format nil "Adding Channel Type ~a at Random" type-symbol))))))))

(defun channel-type-ss-current (type voltage &key gbar conc-in conc-out valence (include-particle-types :all))
  (let ((type (element type 'channel-type)))
    (when type
      (let* ((use-all-particles (eq include-particle-types :all))
	     (include-particle-types (unless use-all-particles
				       (loop for type in include-particle-types collect (element type))))
	     (voltage-index (voltage-to-voltage-index voltage))
	     (voltage (d-flt voltage))
	     (gbar (or gbar
		       (and (eq (channel-type-iv-source type) :absolute) (channel-type-iv-reference type))
		       1.0)))	
	(* (if (channel-type-particle-types-and-powers type)
	     (apply '* (loop for prt-type-power in (channel-type-particle-types-and-powers type)
			     when (or use-all-particles (member (car prt-type-power) include-particle-types))
			     collect (expt (aref (particle-type-inf-array (car prt-type-power)) voltage-index)
					   (cdr prt-type-power))))
	     1.0)
	   (if (channel-type-conc-particle-types-and-powers type)
	     (apply '* (loop for conc-prt-type-power in (channel-type-conc-particle-types-and-powers type)
			     when (or use-all-particles (member (car conc-prt-type-power) include-particle-types))
			     collect (conc-particle-type-ss (car conc-prt-type-power) conc-in
							    :power (cdr conc-prt-type-power) :voltage voltage)))
	     1.0)
	   gbar
	   (case (channel-type-iv-relation type)
	     (:CONSTANT-FIELD (constant-field-equation-double voltage (d-flt conc-in) (d-flt conc-out) 1.0d0 (s-flt valence) 1.0d0))
	     (t (- voltage (current-element-type-e-rev type)))))))))
  
(defun channel-ss-current (channel voltage &optional concentration)
  (let* ((channel (element channel))
	 (type (channel-type channel))
	 (concentration (or concentration 5.0d-5))
	 (voltage-index (voltage-to-voltage-index voltage)))
    (* (if (channel-type-particle-types-and-powers type)
	 (apply '* (loop for prt-type-power in (channel-type-particle-types-and-powers type)
			 collecting (expt (aref (particle-type-inf-array (car prt-type-power)) voltage-index)
					  (cdr prt-type-power))))
	 1.0)
       (if (channel-type-conc-particle-types-and-powers type)
	 (apply '* (loop for conc-prt-type-power in (channel-type-conc-particle-types-and-powers type)
			 collecting (conc-particle-type-ss (car conc-prt-type-power) concentration :power (cdr conc-prt-type-power) :voltage voltage)))
	 1.0)
       (channel-gbar channel)
       (case (channel-type-iv-relation type)
	 (:CONSTANT-FIELD (constant-field-current-coefficient channel :voltage voltage :conc-in concentration))
	 (t (- voltage (channel-e-rev channel)))))))



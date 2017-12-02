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


;;; SYS Source file: light-synapse-functions.lisp
;
; functions specific to light synapses
;

(in-package "SURF-HIPPO")

(defun light-controlled-p (element)
  "Returns T if ELEMENT is a LIGHT or LIGHT-EVENT controlled synapse or synapse type."
  (let ((element (element element)))
    (typecase element
      (synapse (case (synapse-type-control (synapse-type element))
		 ((:light :light-event) t)))
      (synapse-type (case (synapse-type-control element)
		      ((:light :light-event) t))))))
    
(defun menu-for-light-stimulus ()
  *light-stimulus*
  (choose-variable-values
   `((*enable-light* "Enable light inputs" :boolean)
     (*COMPUTE-ALL-LIGHT-INPUTS* "Explicitly convolve all synapse conductances" :boolean)
     (*light-stimulus* "Light stimulus type -" :choose ,*light-stimulus-types* :rank-margin 3)
     (*LIGHT-BACKGROUND* "Light background level" :float)
     (*light-stimulus-start-time* "Light stimulus start time" :float)
     (*light-stimulus-stop-time* "Light stimulus stop time" :float)
     (*CONSTANT-LIGHT-INPUT-FROM-NEGATIVE-INFINITY* "Light input @ t=0 holds for all t<0" :boolean)
     (*light-stimulus-plane* "Light stimulus XY plane maps to cell plane:" :choose (:xy :xz))
     (:comment "Mapping options are [X,Y] -> [X,Y] or [X,Y] -> [X,-Z]")
     (*light-start-position-x* "Light stimulus start center X (um - anatomical frame)" :float)
     (*light-start-position-y* "Light stimulus start center Y (um - anatomical frame)" :float)
     (*use-aperture* "Use aperture" :boolean)
     (*light-stimulus-strength* "Light stimulus strength" :float))
   :label "Setting Light Stimulus Parameters")
  (when *use-aperture* (light-aperture-menu))
  (when *enable-light* (light-stimulus-parameters-menu)))

(defun light-aperture-menu ()
  (choose-variable-values
   '((*aperture-radius* "Aperture radius [microns]" :float)
     (*aperture-center-x* "Aperture center X (um - anatomical frame)" :float)
     (*aperture-center-y* "Aperture center Y (um - anatomical frame)" :float))
   :Label "Setting Up Light Aperture"))

(defun light-stimulus-parameters-menu ()
  (let ((menu-list '())
	(dummy1 (rad-to-deg *light-theta*)))
    (case *light-stimulus*
      ((:on-spot :spot :off-spot :moving-spot)
       (push '(*spot-outside-diameter* "Spot diameter" :float) menu-list))
      (:annulus (setq menu-list (push-lists-onto-list '((*spot-outside-diameter* "Annulus outside diameter" :float)
							(*spot-inside-diameter* "Annulus inside diameter" :float))
						      menu-list))))
    (case *light-stimulus*
      ((:moving-bar :on-moving-bar :off-moving-bar
		    :on-bar :off-bar :bar :moving-bar-grating :moving-sine-grating :moving-spot :apparent-motion)
       (push '(dummy1 "Stimulus frame (Bar/grating long axis) orientation [deg]" :float) menu-list)))
    (case *light-stimulus*
      ((:moving-bar :moving-bar-grating :moving-sine-grating :moving-spot :off-moving-bar :on-moving-bar)
       (push (list `*light-speed* "Speed [microns per millisecond]" :float) menu-list)
       (push (list `*light-direction* "Movement in positive stimulus frame Y axis direction" :boolean) menu-list)))
    (case *light-stimulus*
      (:apparent-motion (setq menu-list (PUSH-LISTS-ONTO-LIST '((*bar-a-width* "Bar A width [uM]" :float)
								(*bar-a-length* "Bar A length [uM]" :float)
								(*bar-a-intensity* "Bar A intensity" :float)
								(*bar-a-start-time* "Bar A start [mS]" :float)
								(*bar-a-stop-time* "Bar A stop [mS]" :float)
								(*bar-a-position-x* "Bar A position X [uM]" :float)
								(*bar-a-position-y* "Bar A position Y [uM]" :float)
								(*bar-b-width* "Bar B width [uM]" :float)
								(*bar-b-length* "Bar B length [uM]" :float)
								(*bar-b-intensity* "Bar B intensity" :float)
								(*bar-b-start-time* "Bar B start [mS]" :float)
								(*bar-b-stop-time* "Bar B stop [mS]" :float)
								(*bar-b-position-x* "Bar B position X [uM]" :float)
								(*bar-b-position-y* "Bar B position Y [uM]" :float))
							      menu-list))))
    (case *light-stimulus*
      ((:moving-bar :moving-bar-grating :bar :on-bar :off-bar :off-moving-bar :on-moving-bar)
       (push `(*bar-width* "Bar width [microns]" :float) menu-list)
       (push `(*bar-length* "Bar length [microns]" :float) menu-list)))
    (case *light-stimulus*
      ((:spot :on-spot :off-spot)
       (push `(*fast-full-field-spot* "Full field spot" :boolean) menu-list)))
    (case *light-stimulus*
      (:moving-bar-grating
       (push `(*grating-spatial-period* "Spatial period of grating [microns]" :float) menu-list)))
    (when menu-list
      (choose-variable-values
       menu-list
       :label (format nil "Parameters For ~A Stimulus" *light-stimulus*)
       :text (case *light-stimulus*
	       (:apparent-motion
		(format nil "Positions of bars A and B are in~%shifted [~A,~A] and rotated stimulus frame"
			*light-start-position-x* *light-start-position-y*))))
      (setq *light-theta* (deg-to-rad dummy1))))
  nil)

#|
(defun set-light-synapses-delays (light-synapses)
  ;; bogus
;;  (jitter-light-synapses light-synapses)
  )

(defun jitter-light-synapses (light-synapses)
  (loop for syn in light-synapses
	do (setf (synapse-delays syn)
		 (list (+ (or (synapse-delay syn) 0.0)
			  (if *jitter-light-synapse* (random *maximum-synaptic-jitter*) 0.0))))
	(setf (synapse-fixnum-delay syn) (round (car (synapse-delays syn))))))
|#

(defun Clear-waveforms-from-light-synapse-types (type)
  ;; some stuff specific to light synapses
  (REMOVE-element-PARAMeters type
			     '(LIGHT-INPUTS-BASE-GAIN 
			       LIGHT-INPUTS-BASE-SHIFT 
			       LIGHT-INPUTS-LAST 
			       LIGHT-INPUTS-1ST
			       light-responses-ARRAY
			       light-inputs-list-of-arrays
			       WAVEFORMS-first-non-zero-element)))

(defvar *light-stimulus-strength nil)

(defun initialize-light-synapse-setup ()
  ;; Some book-keeping, also for backward compatibility *light-stimulus-strength* is still here. Use *light-stimulus-strength* for new code.
  (setq *light-stimulus-strength* (float (or *light-stimulus-strength* *light-stimulus-strength))
	*light-background* (float *light-background*))
  (S-FLT-LIGHT-VARIABLES)
  (unless (and *light-input-waveform* (= (length *light-input-waveform*) *int-user-stop-time*))
    (setq *light-input-waveform* (make-array (1+ *int-user-stop-time*) :element-type 'single-float)))
  (generate-light-origin-trajectory))

(defun active-light-synapses ()
  (flatten-list
   (loop for type in (synapse-types)
	 do (format t "~S~%" (synapse-type-control type))
	 when (and (or (eql (synapse-type-control type) :light)
		       (eql (synapse-type-control type) :light-event))
		   (not (synapse-type-blocked type)))
	 collect (synapse-type-synapses type t))))

(defun number-of-similar-waveforms ()
  (loop for type in (get-synapse-types :light)
	when (synapse-type-first-synapse type) do
	(let ((number-syns (length (number-synapse-type-synapses type))))
	  (if (get-a-value 'light-inputs-list-of-arrays (synapse-type-parameters type))
	      (let ((length-light-list (length (element-parameter type 'light-inputs-list-of-arrays))))
		(format t "Light synapse type ~A has ~a synapse~:p and ~A reference waveform~:p.~%"
			(synapse-type-name type)
			number-syns length-light-list))
	      (format t "Light synapse type ~A has ~A synapse~:p (light input not evaluated yet).~%"
		      (synapse-type-name type)
		      number-syns)))))

(defun any-newbie-synapses (syns)
  (loop for syn in syns
	thereis (eq :newbie (synapse-wave-ref syn))))

(defun clear-wave-ref (syns)
  (loop for syn in syns do (setf (synapse-wave-ref syn) nil)))

(defun setup-light-synapses ()
  ;; Pre-computes the conductance waveform for light-dependent synapses. Note that light-dependent synapses which are never hit by
  ;; the light stimulus have their synapse-waveform set to NIL. All time indices are taken in milliseconds.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (when (and *light-stimulus* *enable-light*)
    (let ((*reuse-synapse-waveforms* (and *reuse-synapse-waveforms* (= *user-stop-time* *last-user-stop-time*))))
      (initialize-light-synapse-setup)
      (loop
       for type in (flatten-list (get-synapse-types :light) (get-synapse-types :light-event))
       do (Clear-waveforms-from-light-synapse-types type)
       unless (synapse-type-blocked type) do ; Generate light-responses-ARRAY for the synapse types.
       (let ((synapses-of-type (synapses-of-type type)))
	 (when (and (not (synapse-type-blocked type))
		    (or (not (= (synapse-type-iv-reference type) 0.0))
			(when (synapse-type-iv-density type) (not (= (synapse-type-iv-density type) 0.0))))
		    (or (not *reuse-synapse-waveforms*)
			(any-newbie-synapses synapses-of-type)))
	   (clear-wave-ref synapses-of-type)
	   (let* ((params (synapse-type-parameters type))
		  (impulse (get-a-value 'IMPULSE params))
		  (IMPULSE-NONLINEARITY (or (get-a-value 'IMPULSE-NONLINEARITY params) 'threshold))
		  (nonlinearity-parameters (or (get-a-value 'impulse-nonlinearity-parameters params) 0.0))
		  (first-impulse (1st-non-zero-elt-index impulse))
		  (last-impulse (last-non-zero-elt-index impulse))
		  (linear-impulse (get-a-value 'linear-impulse params))
		  (active-light-synapses (active-synapses type)))
	     (when active-light-synapses
	       ;; (set-light-synapses-delays active-light-synapses)

	       (setup-event-generators-and-followers-of-type active-light-synapses)

	       (unless (or *KILL-ALL-OUTPUT* *kill-extra-messages*)
		 (format t "Convolving synapse type ~A conductance waveforms~%" (synapse-type-name type)))

	       (loop for syn in active-light-synapses when (synapse-event-generator syn) do
		     (cond
		      ((and (or (equal *light-stimulus* :on-spot) (equal *light-stimulus* :spot))
			    *fast-full-field-spot*
			    (get-a-value 'light-inputs-list-of-arrays params))
		       (setf (synapse-wave-ref syn) '(0 0 1.0)))
		      ((generate-light-input-array syn) ; Generate *LIGHT-INPUT-WAVEFORM*, returns T of light hits syn.
		       (compare-light-synapse-waveform syn))
		      (t (setf (synapse-wave-ref syn) nil))) ; This means that the syn is not hit by light.

		     (loop for follower in (event-followers syn) do
			   (setf (synapse-wave-ref follower) (synapse-wave-ref syn))))

	       (setq params (synapse-type-parameters type))
	       (when (get-a-value 'light-inputs-list-of-arrays params)
		 (let* ((light-inputs-list-of-arrays (get-a-value 'light-inputs-list-of-arrays params))
			(input-dimensions (list (length (the cons light-inputs-list-of-arrays))
						(length (the simple-ARRAY (car light-inputs-list-of-arrays)))))
			(light-responses-ARRAY (make-array input-dimensions :element-type 'single-float))
			(dummy-array
			 (when linear-impulse (make-array (the fn (cadr input-dimensions)) :element-type 'single-float))))
		   (push (cons 'light-responses-ARRAY light-responses-ARRAY) (synapse-type-parameters type))
		   (setq params (synapse-type-parameters type))
		   (loop for input-index fixnum from 0 
			 for light-input-1st across (the (simple-ARRAY FIXNUM (*)) (get-a-value 'LIGHT-INPUTS-1st params))
			 for light-input-last across (the (simple-ARRAY FIXNUM (*)) (get-a-value 'LIGHT-INPUTS-last params))
			 for light-input-array in light-inputs-list-of-arrays 
			 do
			 (convolve-input-array ; Convolve input-waveform w/ syn impulse response -> conductance waveform.
			  impulse light-input-array
			  :destination-array light-responses-ARRAY :2d-output-index input-index
			  :first-impulse first-impulse :last-impulse last-impulse
			  :first-input light-input-1st :last-input light-input-last
			  :nonlinearity IMPULSE-NONLINEARITY :nonlinearity-parameters nonlinearity-parameters)
			 (when linear-impulse
			   (convolve-input-array linear-impulse light-responses-ARRAY
						 :destination-array dummy-array :2d-input-index input-index)
			   (loop for time fixnum from 0 to (1- (the fn (cadr input-dimensions)))
				 do (setf (aref light-responses-ARRAY input-index time) (aref dummy-array time)))))
		   (element-parameter type 'light-responses-ARRAY light-responses-ARRAY)))
		  
	       (change-adjustable-light-synapse-type-arrays-to-simple type)
					; (revamp-synapse-type-lists type active-light-synapses 'synapse-wave-ref-fun)
	       (adjust-syns-wave-ref-shift type)))))))))

(defun adjust-syns-wave-ref-shift (type)
  (let ((base-shift-array (element-parameter type 'light-inputs-base-shift)))
    (synapse-type-iterator
     (syn type)
     when (synapse-wave-ref syn)
     do
     (setf (synapse-wave-ref syn)
	   (list (synapse-wave-index syn)
		 (+ (aref base-shift-array (synapse-wave-index syn))
		    (the fn (+ (synapse-wave-shift syn)
			       (synapse-fixnum-delay syn))))
		 (synapse-wave-gain syn))))
    nil))

(defun change-adjustable-light-synapse-type-arrays-to-simple (type)
  (when (get-a-value 'LIGHT-INPUTS-BASE-SHIFT (synapse-type-parameters type))
    (element-parameter
     type 'LIGHT-INPUTS-BASE-SHIFT
     (change-1d-array-to-simple (get-a-value 'LIGHT-INPUTS-BASE-SHIFT (synapse-type-parameters type)))))
  (when (get-a-value 'LIGHT-INPUTS-BASE-GAIN  (synapse-type-parameters type))
    (element-parameter
     type 'LIGHT-INPUTS-BASE-GAIN
     (CHANGE-1D-FLOAT-ARRAY-TO-SIMPLE (get-a-value 'LIGHT-INPUTS-BASE-GAIN  (synapse-type-parameters type))))))

#|
;;; The following allows *SYNAPSE-NAMES-TO-DO-FIRST* to contain either a list of synapse names or a list of node names. For the
;;; first synapse of type TYPE referenced directly or indirectly in this list, that synapse in the the type-parameters slot (acons
;;; synapse 'fast-rf-ref-synapse) for the associated synapse type.  Right now this function is only for light synapses and for
;;; fast rf bar simulations. Returns the reference synapse is successful, NIL if no synapse of the right type is found.
(defun parse-synapses-to-do-first (type)
  (loop for syn in (flatten-list (loop for thing in (mapcar 'element *SYNAPSE-NAMES-TO-DO-FIRST*)
				       collect (typecase thing
						 (synapse thing)
						 (t (cell-element-elements thing 'synapse)))))
	when (eq (synapse-type syn) type)
	collect syn))
|#

;; THIS COMMENT NEEDS TO BE UPDATED 11/7/94 LBG

;;; Go through all the light-input waves stored in the syn's type light-input-waves-array.
;;;        and all the base-shifts stored in the syn's type light-input-wave-base-shifts-array.

;;; For each test-wave and associated base-shift:

;;; IF (or (= (support test-wave) (support syninputwave))
;;;        (and (> (support test-wave) (support syninputwave))
;;;                 (not (= 0 (last-value syninputwave))))
;;;        (and (> (support syninputwave) (support test-wave))
;;;                 (not (= 0 (last-value test-wave))))

;;;    IF: new-light-input-wave(i) = gain * light-input-waves-array(n, i - shift - base-shift)

;;;           IF (> (support syninputwave) (support test-wave))

;;;                new-light-input-wave -> (get-a-value 'LIGHT-INPUTS-LIST-OF-ARRAYS type-parameters),
;;;                replacing the one at index n.
;;;                shift -> the associated base-shift
;;;                Set the returned shift value to 0

;;;                And new-light-input-wave first and last non-zero element indices into:
;;;                    (get-a-value 'light-input-WAVEFORMS-first-non-zero-element type-parameters)) and
;;;                    (get-a-value 'light-input-WAVEFORMS-last-non-zero-element type-parameters)) respectively,

;;            THEN return (T n shift gain)

;;; ELSE:

;;;  new-light-input-wave -> (get-a-value 'LIGHT-INPUTS-LIST-OF-ARRAYS type-parameters)

;; and new-light-input-wave first and last non-zero element indices into:
;; (get-a-value 'light-input-WAVEFORMS-first-non-zero-element type-parameters)) and
;; (get-a-value 'light-input-WAVEFORMS-last-non-zero-element type-parameters)) respectively,

;;; and return '(NIL n+1 0 1.0)
  
(defun compare-light-synapse-waveform (syn)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((input-1st (find-first-non-zero-element *light-input-waveform*))
	 (input-last (find-last-non-zero-element *light-input-waveform*))
	 (type (synapse-type syn))
	 (light-inputs-list-of-arrays (element-parameter type 'light-inputs-list-of-arrays)))
    (setf (synapse-wave-ref syn) nil)
    (when (and input-1st input-last)
      (unless (and (not *compute-all-light-inputs*)
		   light-inputs-list-of-arrays
		   (find-similar-synapse-waveform type syn *light-input-waveform* input-1st input-last
						  light-inputs-list-of-arrays))
	(setf (synapse-wave-ref syn) (list (length light-inputs-list-of-arrays) 0 1.0))
	(store-syn-type-input *light-input-waveform* (synapse-wave-ref syn) input-1st input-last type)))
    nil))

#|
(defun find-similar-synapse-waveform (type syn input input-1st input-last light-inputs-list-of-arrays)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type VEC-FLT input)
	   (fixnum input-1st input-last))
  (let* ((params (synapse-type-parameters type))
	 (linear-type (not (get-a-value 'impulse-nonlinearity-parameters params)))
	 (LIGHT-INPUTS-1st (get-a-value 'LIGHT-INPUTS-1st params))
	 (LIGHT-INPUTS-last (get-a-value 'LIGHT-INPUTS-last params))
	 (light-inputs-base-shift (get-a-value 'light-inputs-base-shift params))
	 (light-inputs-base-gain (get-a-value 'light-inputs-base-gain params))
	 (input-array-len (when light-inputs-list-of-arrays
			    (length (the vec-flt (car light-inputs-list-of-arrays)))))
	 (input-index 0)
	 corr-result)
    (declare (fixnum input-index))
    (loop 
     for light-input-1st across (the vec-fix LIGHT-INPUTS-1st) 
     for light-input-last across (the vec-fix LIGHT-INPUTS-last) 
     for base-shift across (the vec-fix light-inputs-base-shift)
     for light-input-array in light-inputs-list-of-arrays
     do
     (let ((waveform-shift (the fn (- input-1st light-input-1st)))
	   (waveform-gain
	    (the sf (/ (aref input input-1st)
				 (aref (the vec-flt light-input-array) light-input-1st)))))
       (do ((i input-1st (1+ (the fn i))))
	   ((or
	     (if (= 0 (aref (the vec-flt light-input-array) (the fn (- i waveform-shift))))
		 (not (= 0 (aref input i)))
		 (not (= waveform-gain
			 (the sf
			      (/ (aref input i)
				 (aref (the vec-flt light-input-array)
				       (the fn (- i waveform-shift))))))))
	     (when (and (or linear-type
			    (= waveform-gain (aref (the vec-flt light-inputs-base-gain)
						   input-index)))
			(or (= (the fn (- i waveform-shift)) (1- input-array-len))
			    (and (= i (the fn input-last))
				 (= (- i waveform-shift) light-input-last))))
	       (setq corr-result (list (the fn (- waveform-shift base-shift))
				       (the sf
					    (/ waveform-gain
					       (aref (the vec-flt light-inputs-base-gain)
						     input-index))))))
	     (= i input-last)))))

     when corr-result			; WE CAN USE PRIOR WAVE
     do (progn
	  (setf (synapse-wave-ref syn) (cons input-index corr-result))
	  (when (> light-input-1st input-1st)
	    (store-syn-type-input input (synapse-wave-ref syn) input-1st input-last type))
	  (return t))
     do (setq input-index (1+ input-index))
     finally (return nil))))
|#

(defun find-similar-synapse-waveform (type syn input input-1st input-last light-inputs-list-of-arrays)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type VEC-FLT input)
	   (fixnum input-1st input-last))
  (let* ((params (synapse-type-parameters type))
	 (linear-type (not (get-a-value 'impulse-nonlinearity-parameters params)))
	 (LIGHT-INPUTS-1st (get-a-value 'LIGHT-INPUTS-1st params))
	 (LIGHT-INPUTS-last (get-a-value 'LIGHT-INPUTS-last params))
	 (light-inputs-base-shift (get-a-value 'light-inputs-base-shift params))
	 (light-inputs-base-gain (get-a-value 'light-inputs-base-gain params))
	 (input-array-len (when light-inputs-list-of-arrays (length (the vec-flt (car light-inputs-list-of-arrays)))))
	 (input-array-len-1 (if input-array-len (1- input-array-len) 0))
	 corr-result)
    (declare (fixnum input-array-len-1))
    (do ((input-index 0 (1+ (the fn input-index)))
	 (light-input-array light-inputs-list-of-arrays (cdr light-input-array)))
	((or corr-result (null light-input-array)))
      (declare (fixnum input-index))
      (let* ((light-input-1st (aref (the vec-fix LIGHT-INPUTS-1st) input-index))
	     (light-input-last (aref (the vec-fix LIGHT-INPUTS-last) input-index))
	     (base-shift (aref (the vec-fix light-inputs-base-shift) input-index))
	     (base-gain (aref (the vec-flt light-inputs-base-gain) input-index))
	     (waveform-shift (the fn (- input-1st light-input-1st)))
	     (waveform-gain
	      (the sf (/ (aref input input-1st) (aref (the vec-flt (car light-input-array)) light-input-1st)))))
	(do* ((i input-1st (1+ (the fn i)))
	      (shifted-i (- i waveform-shift) (1+ (the fn shifted-i))))	       
	     ((or
	       (if (= 0.0 (aref (the vec-flt (car light-input-array)) shifted-i))
		   (not (= 0.0 (aref input i)))
		   (not (= waveform-gain
			   (the sf (/ (aref input i) (aref (the vec-flt (car light-input-array))  shifted-i))))))
	       (when (and (or linear-type (= waveform-gain base-gain))
			  (or (= shifted-i input-array-len-1) (and (= i input-last) (= shifted-i light-input-last))))
		 (setq corr-result (list (the fn (- waveform-shift base-shift)) (the sf (/ waveform-gain base-gain)))))
	       (= i input-last)))
	  (declare (fixnum i shifted-i)))
	(when corr-result		; WE CAN USE PRIOR WAVE
          (setf (synapse-wave-ref syn) (cons input-index corr-result))                                             
	  (when (> light-input-1st input-1st)
	    (store-syn-type-input input (synapse-wave-ref syn) input-1st input-last type)))))
    corr-result))

(defun store-syn-type-input (input wave-ref input-1st input-last type)
  ;; Load in the new waveform and associated info.
  (declare (optimize (safety 1) (speed 3) (space 1)) 
	   (type fixnum input-1st input-last)
	   (type VEC-FLT input))
  (let* ((params (synapse-type-parameters type))
	 (wavelength			; *int-user-stop-time*
	  (1+ *int-user-stop-time*))
	 (input-index (synapse-wave-ref-index wave-ref))
	 (shift (synapse-wave-ref-shift wave-ref))
	 (gain (synapse-wave-ref-gain wave-ref))
	 (new-number-of-waves (the fn (1+ input-index)))
	 (light-inputs-list-of-arrays
	  (if (> new-number-of-waves (length (get-a-value 'light-inputs-list-of-arrays params)))
	    (concatenate 'list (get-a-value 'light-inputs-list-of-arrays params)
			 (list (make-array (list wavelength) :element-type 'single-float)))
	    (get-a-value 'light-inputs-list-of-arrays params)))
	 (LIGHT-INPUTS-1st (adjust-or-make-array (get-a-value 'LIGHT-INPUTS-1st params) (list new-number-of-waves) 'fixnum))
	 (LIGHT-INPUTS-last (adjust-or-make-array (get-a-value 'LIGHT-INPUTS-last params) (list new-number-of-waves) 'fixnum))
	 (LIGHT-INPUTS-base-shift (adjust-or-make-array (get-a-value 'LIGHT-INPUTS-base-shift params) (list new-number-of-waves) 'fixnum))
	 (LIGHT-INPUTS-base-gain (adjust-or-make-array (get-a-value 'LIGHT-INPUTS-base-gain params) (list new-number-of-waves) 'single-float)))

    (declare (fixnum input-index shift)
	     (single-float gain))
    ;;    (format t "~A store: wavelength ~D input-index ~D shift ~D gain ~A  input-1st ~A ~A~%"
    ;;            (synapse-type-name type) wavelength input-index shift gain  input-1st LIGHT-INPUTS-base-shift)

    (element-parameter type 'LIGHT-INPUTS-1st LIGHT-INPUTS-1st)
    (element-parameter type 'LIGHT-INPUTS-last LIGHT-INPUTS-last)
    
    (element-parameter type 'LIGHT-INPUTS-base-shift LIGHT-INPUTS-base-shift)
    (element-parameter type 'LIGHT-INPUTS-base-gain LIGHT-INPUTS-base-gain)
    (setq params (synapse-type-parameters type))

    (setf (aref LIGHT-INPUTS-base-shift input-index) (- shift))
    (setf (aref LIGHT-INPUTS-1st input-index) input-1st)
    (setf (aref LIGHT-INPUTS-last input-index) input-last)
    (setf (aref LIGHT-INPUTS-base-gain input-index) gain)
    (element-parameter type 'light-inputs-list-of-arrays 
		       (loop for light-input-array in light-inputs-list-of-arrays
			     for count from 0
			     when (= count input-index)
			     do (do ((i 0 (1+ (the fn i)))) 
				    ((= (the fn i)  (length input)))
				  (setf (aref light-input-array i) (aref input i)))
			     collect light-input-array))
    
    ;; (format t "STORE.. shift ~D  syn1st: ~d  1st-1: ~F 1st: ~F~%" (- shift) INPUT-1ST (aref INPUT (1- INPUT-1ST)) (aref INPUT INPUT-1ST))

    nil))
  
(defun adjust-or-make-array (array dimensions array-element-type)
  (if array
      (when (loop for val1 in (array-dimensions array)
		  for val2 in dimensions
		  thereis (> val2 val1))
	(setq array (adjust-array array dimensions :element-type array-element-type)))
      (setq array (make-array dimensions :element-type array-element-type)))
  array)

(defun is-light-moving ()
  (case *light-stimulus* 
    ((:moving-spot  :on-moving-spot :off-moving-spot  :moving-bar :on-moving-bar :off-moving-bar
		    :moving-bar-grating 
		    :moving-sine-grating) t)))

(defun s-flt-light-variables ()
  (setq
   *light-input-offset-distance* (s-flt *light-input-offset-distance*)
   *light-input-offset-angle* (s-flt  *light-input-offset-angle*)
   *light-input-delay* (s-flt  *light-input-delay*)
   *light-speed* (s-flt *light-speed*)
   *bar-width* (s-flt  *bar-width*)
   *bar-length* (s-flt  *bar-length*)
   *light-theta* (s-flt  *light-theta*)
   *light-start-position-x* (s-flt  *light-start-position-x*)
   *light-start-position-y* (s-flt  *light-start-position-y*)
   *grating-temporal-period* (s-flt  *grating-temporal-period*)
   *grating-spatial-period* (s-flt  *grating-spatial-period*)
   *aperture-radius* (s-flt  *aperture-radius*)
   *aperture-center-x* (s-flt  *aperture-center-x*)
   *aperture-center-y* (s-flt  *aperture-center-y*)
   *bar-a-width* (s-flt  *bar-a-width*)
   *bar-a-length* (s-flt  *bar-a-length*)
   *bar-a-intensity* (s-flt  *bar-a-intensity*)
   *bar-a-start-time* (s-flt  *bar-a-start-time*)
   *bar-a-stop-time* (s-flt  *bar-a-stop-time*)
   *bar-a-position-x* (s-flt  *bar-a-position-x*)
   *bar-a-position-y* (s-flt  *bar-a-position-y*)
   *bar-b-width* (s-flt  *bar-b-width*)
   *bar-b-length* (s-flt  *bar-b-length*)
   *bar-b-intensity* (s-flt  *bar-b-intensity*)
   *bar-b-start-time* (s-flt  *bar-b-start-time*)
   *bar-b-stop-time* (s-flt  *bar-b-stop-time*)
   *bar-b-position-x* (s-flt  *bar-b-position-x*)
   *bar-b-position-y* (s-flt  *bar-b-position-y*)
   *synapse-g-leak-ratio*  (s-flt  *synapse-g-leak-ratio*)
   *light-stimulus-start-time* (s-flt	  *light-stimulus-start-time*)
   *light-stimulus-stop-time* (s-flt  *light-stimulus-stop-time*)
   *light-stimulus-strength* (s-flt  *light-stimulus-strength*)
   *light-background* (s-flt  *light-background*)
   *spot-outside-diameter* (s-flt  *spot-outside-diameter*)
   *spot-inside-diameter* (s-flt  *spot-inside-diameter*)
   ))

(defun generate-light-origin-trajectory ()
  ;; Generates the trajectory of moving light stimuli origin during the simulation, as a list of xy points. Result into
  ;; *LIGHT-ORIGIN-ARRAY*.
  (when (is-light-moving)
    (unless (= (length *light-origin-array*) (1+ *int-user-stop-time*))
      (setq *light-origin-array* (make-array (1+ *int-user-stop-time*))))
    (loop for time from 0.0 to (1- (length *light-origin-array*)) do
	  (let ((moving-time 0.0)
		(reverse-time 0.0)
		(x 0.0)
		(y 0.0))
	    (declare (single-float moving-time reverse-time x y time))
	    (setq moving-time
		  (cond ((> time *light-stimulus-stop-time*)
			 (- *light-stimulus-stop-time* *light-stimulus-start-time*))
			((< *light-stimulus-start-time* time *light-stimulus-stop-time*)
			 (- time *light-stimulus-start-time*))
			(t 0.0)))
	    (setq reverse-time
		  (if (eq *light-stimulus* :reversing-bar)
		    (if (< (mod moving-time (* 2.0 *grating-temporal-period*)) *grating-temporal-period*)
		      (mod moving-time (* 2.0 *grating-temporal-period*))
		      (- (* 2.0 *grating-temporal-period*) (mod moving-time (* 2.0 *grating-temporal-period*))))
		    moving-time))
	    (setq x (* (if *light-direction* (- *light-speed*) *light-speed*)
		       reverse-time
		       (cos (- pi-over-2 *light-theta*))))
	    (setq y (* (if *light-direction* *light-speed* (- *light-speed*))
		       reverse-time
		       )		; (sin (- (/ pi-single 2.0) *light-theta*))
		  )
	    (setf (aref *light-origin-array* (truncate time))
		  (list 0.0		; x
			y))))))

(defun SYNAPSE-FAST-RF-SHIFT (new-syn old-syn)
  ;; If a light synapse is identical to one whose input has been previously computed (OLD-SYN), and the stimulus is the same, just
  ;; shifted in time, then we can quickly compute the input to the new synapse (NEW-SYN).
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (type synapse new-syn old-syn))
  (let* ((delta-x (- (the sf (first (node-absolute-location (synapse-node new-syn))))
		     (the sf (first (node-absolute-location (synapse-node old-syn))))))
	 (delta-t (the fn (round (the sf (/ (the sf (* (if *light-direction* -1.0 1.0) delta-x)) *light-speed*))))))
    (setf (synapse-wave-ref new-syn) (list (synapse-wave-index old-syn) delta-t (synapse-wave-gain old-syn)))))

(defun test-for-aperture (syn)
  ;; If *use-aperture* AND synapse is not within aperture, returns NIL, else T.
  (if *use-aperture*
    (let* (				;(type (synapse-type syn))
	   (type-parameters (synapse-type-parameters (synapse-type syn)))
	   (light-input-offset-distance (s-flt (or (cdr-assoc 'LIGHT-OFFSET-DISTANCE type-parameters) 0.0)))
	   (light-input-offset-angle (s-flt (or (cdr-assoc 'LIGHT-OFFSET-ANGLE type-parameters) 0.0))))
      (declare (single-float light-input-offset-distance light-input-offset-angle))
      (< (cartesian-distance (+ (first (node-absolute-location (synapse-node syn))) (* light-input-offset-distance (cos light-input-offset-angle)))
			     (+ (second (node-absolute-location (synapse-node syn))) (* light-input-offset-distance (sin light-input-offset-angle)))
			     *aperture-center-x* *aperture-center-y*)
	 *aperture-radius*))
    T))

(defun get-spatial-rf-array (type)
  ;; Returns spatial RF array for the synapse type. The list associated with
  ;; 'SPATIAL-RF-FUNCTION specifies a function which returns a 2d array, e.g.:
  ;;
  ;;      '(GAUSSIAN-RF sigma-x sigma-y normalized-amp)
  ;;
  (let ((type-parameters (synapse-type-parameters type)) )
    (when (assoc 'SPATIAL-RF-FUNCTION type-parameters)
      (setf (synapse-type-parameters type)
	    (acons 'SPATIAL-RF
		   (evaluate-funspec-in-a-list 'SPATIAL-RF-FUNCTION type-parameters)
		   type-parameters))
      (when (position :grid-size-x (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
	(setf (synapse-type-parameters type)
	      (acons 'SPATIAL-RF-grid-size-x
		     (nth (1+ (position :grid-size-x (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters)))
			  (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
		     (synapse-type-parameters type))))
      (when (position :grid-size-y (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
	(setf (synapse-type-parameters type)
	      (acons 'SPATIAL-RF-grid-size-y
		     (nth (1+ (position :grid-size-y (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters)))
			  (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
		     (synapse-type-parameters type))))
      (when (position :grid-size (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
	(setf (synapse-type-parameters type)
	      (acons 'SPATIAL-RF-grid-size
		     (nth (1+ (position :grid-size (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters)))
			  (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
		     (synapse-type-parameters type)))))

    ;; This is for drawing the rf in the histology graphics
    (let ((type-parameters (synapse-type-parameters type)))
      (setf (synapse-type-parameters type)
	    (acons 'rf-shape
		   (case (if (assoc 'SPATIAL-RF-FUNCTION type-parameters)
			   (cadr (assoc 'SPATIAL-RF-FUNCTION type-parameters))
			   t)
		     (gaussian-rf
		      (let* ((SPATIAL-RF-FUNCTION (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
			     (x-sigma (nth 1 SPATIAL-RF-FUNCTION))
			     (y-sigma (nth 2 SPATIAL-RF-FUNCTION))
			     (grid-size (cdr-assoc 'SPATIAL-RF-grid-size type-parameters)))
			(list (list (* x-sigma -2.0) (* x-sigma 2.0)
				    (* x-sigma 2.0) (* x-sigma -2.0)
				    (* x-sigma -2.0)
				    )
			      (list (* y-sigma 2.0) (* y-sigma 2.0)
				    (* y-sigma -2.0) (* y-sigma -2.0)
				    (* y-sigma 2.0)))))
		     (simple-rf
		      (evaluate-funspec-in-a-list 'SPATIAL-RF-FUNCTION type-parameters '(:rf-shape-only t)))
		     (t
		      (let ((circle-shape-radius 10.0))
			(loop for angle from 0.0 to 360 by 60 
			      collect (* circle-shape-radius (cos-degrees angle)) into x
			      collect (* circle-shape-radius (sin-degrees angle)) into y
			      finally (return (list x y))))))
		   (synapse-type-parameters type))))
    (cdr-assoc 'SPATIAL-RF (synapse-type-parameters type))))

(defun get-spatial-rf-array (type)
  (let ((SPATIAL-RF-FUNCTION (element-parameter type 'SPATIAL-RF-FUNCTION)))
    (when SPATIAL-RF-FUNCTION
      (element-parameter type 'SPATIAL-RF (evaluate-funspec SPATIAL-RF-FUNCTION))
      (when (position :grid-size-x SPATIAL-RF-FUNCTION)
	(element-parameter type 'SPATIAL-RF-grid-size-x
			   (nth (1+ (position :grid-size-x SPATIAL-RF-FUNCTION)) SPATIAL-RF-FUNCTION)))
      (when (position :grid-size-y SPATIAL-RF-FUNCTION)
	(element-parameter type 'SPATIAL-RF-grid-size-y
			   (nth (1+ (position :grid-size-y SPATIAL-RF-FUNCTION)) SPATIAL-RF-FUNCTION)))
      (when (position :grid-size SPATIAL-RF-FUNCTION)
	(element-parameter type 'SPATIAL-RF-grid-size
			   (nth (1+ (position :grid-size SPATIAL-RF-FUNCTION)) SPATIAL-RF-FUNCTION))))
    ;; This is for drawing the rf in the histology graphics
    (element-parameter type 'rf-shape
		       (case (car SPATIAL-RF-FUNCTION)
			 (gaussian-rf
			  (let ((x-sigma (nth 1 SPATIAL-RF-FUNCTION))
				(y-sigma (nth 2 SPATIAL-RF-FUNCTION))
				(grid-size (element-parameter type 'SPATIAL-RF-grid-size)))
			    (list (list (* x-sigma -2.0) (* x-sigma 2.0)
					(* x-sigma 2.0) (* x-sigma -2.0)
					(* x-sigma -2.0))

				  (list (* y-sigma 2.0) (* y-sigma 2.0)
					(* y-sigma -2.0) (* y-sigma -2.0)
					(* y-sigma 2.0)))))
			 (simple-rf
			  (evaluate-funspec SPATIAL-RF-FUNCTION '(:rf-shape-only t)))
			 (t
			  (let ((circle-shape-radius 10.0))
			    (loop for angle from 0.0 to 360 by 60 
				  collect (* circle-shape-radius (cos-degrees angle)) into x
				  collect (* circle-shape-radius (sin-degrees angle)) into y
				  finally (return (list x y)))))))
    (element-parameter type 'SPATIAL-RF)))


(defun simple-rf (&key (amplitude 1.0) (sub-region-width 500) (sub-region-length 1500)
		       (grid-size 100) (number-sub-regions 3) (orientation 0) (phase 0)
		       (gabor nil) (gabor-sd 500.0) rf-shape-only)
  ;; Returns a rectangular single-float array with a rotated (:ORIENTATION in degrees, default 0) simple-type receptive field (RF)
  ;; whose amplitude is cosine modulated in the (rotated) x dimension and independent of the (rotated) y dimension. The wavelength
  ;; of the cosine is twice :SUB-REGION-WIDTH (default 500), and the number of cycles is given by one half :NUMBER-SUB-REGIONS
  ;; (default 3). The phase of the cosine is :PHASE degrees (default 0) with respect to the center of the RF. The cosine is
  ;; multiplied by :AMPLITUDE (default 1), and the (rotated) y length is given by :SUB-REGION-LENGTH (default 1500). The cosine
  ;; may be further modulated by a gaussian (standard deviation given by :GABOR-SD, in microns) along the (rotated) x dimension
  ;; when :GABOR is non-nil. The :GRID-SIZE, :SUB-REGION-WIDTH and :SUB-REGION-LENGTH arguments are in microns. For rotated RFs,
  ;; the array is sufficiently large to accomodate the entire RF, with the remainder of the array set to 0.0. Thus, for a RF with
  ;; a single excitatory center subregion, two flanking inhibitory subregions, a subregion width of 500 microns, a subregion
  ;; length of 1500 microns, and a grid size of 100 microns, rotated from the vertical by 20 degrees:
  ;;
  ;;     (SIMPLE-RF :orientation 20.0)
  ;;
  ;; It is probably a good idea to use "nice" arguments, i.e. dimensions that have integer ratios.  Otherwise, check the resulting
  ;; array to make sure that there are no unexpected edge effects or artifacts in the array. A quick check is to use:
  ;;
  ;;     (density-plot (SIMPLE-RF :orientation 20.0))
  ;;
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((cos-orientation (cos-degrees (float orientation)))
	 (sin-orientation (sin-degrees (float orientation)))
	 (rad-phase (* 2 pi-single (/ phase 360)))
	 (unrotated-width (* number-sub-regions (/ sub-region-width grid-size)))
	 (unrotated-length (/ sub-region-length grid-size))
	 (rf-w (round (+ (abs (* unrotated-width cos-orientation)) (abs (* unrotated-length sin-orientation)))))
	 (rf-l (round (+ (abs (* unrotated-width sin-orientation)) (abs (* unrotated-length cos-orientation)))))
	 (spatial-rf-array (unless rf-shape-only (make-array (list rf-w rf-l) :initial-element 0.0)))
	 (gabor-var (float (* gabor-sd gabor-sd))))
    (cond
     (rf-shape-only
      (simple-rf-shape number-sub-regions sub-region-width sub-region-length cos-orientation sin-orientation))
     (t (loop for index-x from 0 to (the fn (1- rf-w)) by 1
	      for x from
	      (the sf (* (the fn (1- rf-w)) (the sf (* grid-size -0.5)))) by grid-size
	      do
	      (loop for index-y from 0 to (the fn (1- rf-l)) by 1
		    for y from (the sf (* (the fn (1- rf-l)) (the sf (* grid-size -0.5))))
		    by grid-size
		    do
		    (let ((rot-x (the sf (+ (the sf (* x cos-orientation))
					    (the sf (* y sin-orientation)))))
			  (rot-y (the sf (- (the sf (* x sin-orientation))
					    (the sf (* y cos-orientation))))))
		      (setf
		       (aref spatial-rf-array index-x index-y)
		       (the sf
			 (*
			  amplitude
			  (the sf
			    (*
			     (the sf
			       (if (<= (abs rot-y) (the sf (/ sub-region-length 2.0))) 1.0 0.0))
			     (the sf
			       (*
				(the sf
				  (if gabor (the sf (gaussian rot-x 0.0 gabor-var)) 1.0))
				(the sf
				  (*
				   (the sf
				     (if (< (abs
					     (the sf
					       (/ rot-x (the sf (/ sub-region-width 2.0)))))
					    number-sub-regions) 1.0 0.0))
				   (cos (the sf
					  (- (the sf
					       (* (the sf (* 2.0 pi-single))
						  (the sf
						    (/ rot-x
						       (the sf (* 2.0 sub-region-width))))))
					     rad-phase)))))))))))))))
	spatial-rf-array))))

(defun simple-rf-shape (number-sub-regions sub-region-width sub-region-length cos-orientation sin-orientation)
  (loop for sub-region from 1 to number-sub-regions
	nconc
	(let ((x-shift (* (- (- sub-region (* 0.5 number-sub-regions)) 0.5) sub-region-width)))
	  (loop for x-unrotated-field in (list (* -0.5 sub-region-width)
					       (* 0.5 sub-region-width)
					       (* 0.5 sub-region-width)
					       (* -0.5 sub-region-width)
					       (* -0.5 sub-region-width))
		for y-unrotated-field in (list (* 0.5 sub-region-length)
					       (* 0.5 sub-region-length)
					       (* -0.5 sub-region-length)
					       (* -0.5 sub-region-length)
					       (* 0.5 sub-region-length))
		collect (list (+ (* (+ x-shift x-unrotated-field) cos-orientation)
				 (* y-unrotated-field sin-orientation))
			      (- (* y-unrotated-field cos-orientation)
				 (* (+ x-shift x-unrotated-field) sin-orientation)))))))

;;; Each of the arrays returned by GAUSSIAN-RF and DOG-RF are 5 * (sigma / GRID-SIZE um) on a side, and assume a GRID-SIZE um
;;; square grid, where the key argument :GRID-SIZE default is 10um. Thus (GAUSSIAN-RF 20 40 t) returns a 10 by 20 array.

(defun GAUSSIAN-RF (sigma-x sigma-y &key normalized-amp (grid-size 10))
  (let* ((gaussian-x-size (+ 1 (* 2 (truncate (/ (* 5 sigma-x) (* 2 grid-size)))))) ;Want odd # of elements
	 (gaussian-y-size (+ 1 (* 2 (truncate (/ (* 5 sigma-y) (* 2 grid-size))))))
	 (sigma-x-sq (float (* sigma-x sigma-x)))
	 (sigma-y-sq (float (* sigma-y sigma-y)))
	 (gaussian-x-normalizer (if normalized-amp
				    (/ 1 (gaussian 0.0 0.0 sigma-x-sq))
				    1.0))
	 (gaussian-y-normalizer (if normalized-amp
				    (/ 1 (gaussian 0.0 0.0 sigma-y-sq))
				    1.0))
	 (spatial-rf-array (make-array (list gaussian-x-size gaussian-y-size)))
	 (x-midpoint (* 0.5 (- gaussian-x-size 1)))
	 (y-midpoint (* 0.5 (- gaussian-y-size 1)))
	 (gaussian-xy-normalizer (the sf (* gaussian-x-normalizer gaussian-y-normalizer))))
    (declare (single-float sigma-x-sq sigma-y-sq gaussian-x-normalizer gaussian-y-normalizer)
	     (fixnum gaussian-x-size gaussian-y-size))
    (dotimes (x gaussian-x-size)
      (dotimes (y gaussian-y-size)
	(declare (fixnum x y))
	(let* ((x-arg (the sf (* grid-size (- x x-midpoint))))
	       (y-arg (the sf (* grid-size (- y y-midpoint))))
	       (gauss-x (the sf (gaussian x-arg 0.0 sigma-x-sq)))
	       (gauss-y (the sf (gaussian y-arg 0.0 sigma-y-sq)))
	       (gauss-xy (the sf (* gaussian-xy-normalizer gauss-x gauss-y))))
	  (declare (single-float gauss-x gauss-y gauss-xy))
	  ;;Make the gaussian somewhat circularly symmetric, despite the finite array.
	  (setf (aref spatial-rf-array x y) gauss-xy))))
    spatial-rf-array))

(defun DOG-RF (center-sigma-x center-sigma-y surround-sigma-x surround-sigma-y center-surround-ratio
	       &key (angle 0.0) normalized-amp (grid-size 10.0) total-vol (gaussian-rf-width-in-sigmas *gaussian-rf-width-in-sigmas*))
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let ((center-sigma-x (float center-sigma-x))
	(center-sigma-y (float center-sigma-y))
	(surround-sigma-x (float surround-sigma-x))
	(surround-sigma-y (float surround-sigma-y))
	(center-surround-ratio (float center-surround-ratio))
	(grid-size (float grid-size))
	(angle (float angle)))
    (declare (single-float center-sigma-x center-sigma-y surround-sigma-x surround-sigma-y center-surround-ratio angle))
    (when total-vol (setq total-vol (float total-vol)))
    (let* ((gaussian-x-size (+ 1 (* 2 (truncate
				       (/ (* gaussian-rf-width-in-sigmas surround-sigma-x)
					  (* 2 grid-size)))))) ;Want odd # of elements
	   (gaussian-y-size (+ 1 (* 2 (truncate
				       (/ (* gaussian-rf-width-in-sigmas surround-sigma-y)
					  (* 2 grid-size)))))) ;Want odd # of elements
	   (center-sigma-x-sq (float (* center-sigma-x center-sigma-x)))
	   (center-sigma-y-sq (float (* center-sigma-y center-sigma-y)))
	   (surround-sigma-x-sq (float (* surround-sigma-x surround-sigma-x)))
	   (surround-sigma-y-sq (float (* surround-sigma-y surround-sigma-y)))
	   (gaussian-x-normalizer (if normalized-amp
				      (/ 1 (- (gaussian 0.0 0.0 center-sigma-x-sq)
					      (gaussian 0.0 0.0 surround-sigma-x-sq)))
				      1.0))
	   (gaussian-y-normalizer (if normalized-amp
				      (/ 1 (- (gaussian 0.0 0.0 center-sigma-y-sq)
					      (gaussian 0.0 0.0 surround-sigma-y-sq)))
				      1.0))
	   (spatial-rf-array (make-array (list gaussian-x-size gaussian-y-size)))
	   (x-midpoint (* 0.5 (- gaussian-x-size 1)))
	   (y-midpoint (* 0.5 (- gaussian-y-size 1)))
	   (gaussian-xy-normalizer (the sf (* gaussian-x-normalizer gaussian-y-normalizer))))
      (declare (single-float center-sigma-x-sq center-sigma-y-sq
			     surround-sigma-x-sq surround-sigma-y-sq
			     gaussian-x-normalizer gaussian-y-normalizer))
      (declare (fixnum gaussian-x-size gaussian-y-size))

      (dotimes (x gaussian-x-size)
	(dotimes (y gaussian-y-size)
	  (declare (fixnum x y))
	  (let* ((x-arg (* grid-size (- x x-midpoint)))
		 (y-arg (* grid-size (- y y-midpoint)))
		 (center-gauss-x (gaussian x-arg 0.0 center-sigma-x-sq))
		 (center-gauss-y (gaussian y-arg 0.0 center-sigma-y-sq))
		 (surround-gauss-x (gaussian x-arg 0.0 surround-sigma-x-sq))
		 (surround-gauss-y (gaussian y-arg 0.0 surround-sigma-y-sq))
		 (center-gauss-xy (* center-gauss-x center-gauss-y))
		 (surround-gauss-xy (/ (* surround-gauss-x surround-gauss-y) (the sf center-surround-ratio)))
		 (gauss-xy (the sf (* gaussian-xy-normalizer
				      (the sf (- center-gauss-xy surround-gauss-xy))))))
	    (declare (single-float 
		      center-gauss-x center-gauss-y
		      surround-gauss-x surround-gauss-y
		      gauss-xy))
	 
	    ;;Make the gaussian somewhat circularly symmetric, despite the finite array.
	    (setf (aref spatial-rf-array x y) gauss-xy))))
      (if total-vol
	  (add-offset-2d-array-float spatial-rf-array (/ (- (the sf total-vol) (the sf (array-vol spatial-rf-array grid-size)))
							 (* (square grid-size) (square gaussian-y-size))))
	  spatial-rf-array))))

(proclaim '(inline custom-STIMULUS))
(defun custom-stimulus (x-synapse-rf y-synapse-rf delayed-time)
  0.0)

#|
(proclaim '(type single-float
 *bar-a-width* *bar-a-length* *bar-a-intensity* *bar-a-start-time* *bar-a-stop-time* *bar-a-position-x* *bar-a-position-y*
 *bar-b-width* *bar-b-length* *bar-b-intensity* *bar-b-start-time* *bar-b-stop-time* *bar-b-position-x* *bar-b-position-y*))
|#

(proclaim '(inline APPARENT-MOTION-STIMULUS))
(defun apparent-motion-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame delayed-time)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame delayed-time))
  (if
      (or
       ;; Test bar A
       (and (> (the sf delayed-time) (the sf *bar-a-start-time*))
	    (> (the sf *bar-a-stop-time*) (the sf delayed-time))
	    (<= (abs (the sf (- syn-rf-x-stimulation-frame *bar-a-position-x*)))
		(the sf (/ (the sf *bar-a-length*) 2.0)))
	    (<= (abs (the sf (- syn-rf-y-stimulation-frame *bar-a-position-y*)))
		(the sf (/ (the sf *bar-a-width*) 2.0))))
       ;; Test bar B
       (and (> (the sf delayed-time) (the sf *bar-b-start-time*))
	    (> (the sf *bar-b-stop-time*) (the sf delayed-time))
	    (<= (abs (the sf (- syn-rf-x-stimulation-frame *bar-b-position-x*)))
		(the sf (/ (the sf *bar-b-length*) 2.0)))
	    (<= (abs (the sf (- syn-rf-y-stimulation-frame *bar-b-position-y*)))
		(the sf (/ (the sf *bar-b-width*) 2.0)))))
    1.0 0.0))

(proclaim '(inline annulus-STIMULUS))
(defun annulus-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  (declare (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
	   (optimize (safety 1) (speed 3) (space 1)))
  (let ((distance-from-stimulus (cartesian-distance syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)))
    (declare (single-float distance-from-stimulus))
    (if (and (> (the sf (/ (the sf *spot-outside-diameter*) 2.0))
		distance-from-stimulus)
	     (< (the sf (/ (the sf *spot-inside-diameter*) 2.0))
		distance-from-stimulus))
      1.0 0.0)))

(proclaim '(inline ON-bar-STIMULUS))
(defun on-bar-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  ;; This determines whether the bar is presently overhead the syn. Cooridinates are in the bar frame. For a moving bar the
  ;; stimulation frame coordinates have been translated according to the bar's movement from its original position (in the bar
  ;; frame's y-direction).
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
  (if (and (< syn-rf-x-stimulation-frame (/ *bar-length* 2.0))
	   (<= (- syn-rf-x-stimulation-frame)(/ *bar-length* 2.0))
	   (< syn-rf-y-stimulation-frame (/ *bar-width* 2.0))
	   (<= (- syn-rf-y-stimulation-frame) (/ *bar-width* 2.0)))
    1.0 0.0))

(proclaim '(inline OFF-bar-STIMULUS))
(defun off-bar-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
  (if (and (< syn-rf-x-stimulation-frame (/ *bar-length* 2.0))
	   (<= (- syn-rf-x-stimulation-frame)(/ *bar-length* 2.0))
	   (< syn-rf-y-stimulation-frame (/ *bar-width* 2.0))
	   (<= (- syn-rf-y-stimulation-frame) (/ *bar-width* 2.0)))
      -1.0 0.0))

(proclaim '(inline on-spot-STIMULUS))
(defun on-spot-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  ;; This determines whether the spot is presently overhead the syn. Cooridinates are in the spot frame. For a moving spot the
  ;; stimulation frame coordinates have been translated according to the spot's movement from its original position (in the spot
  ;; frame's y-direction).
  (declare (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
	   (optimize (safety 1) (speed 3) (space 1)))
  (if (> (the sf (/ (the sf *spot-outside-diameter*) 2.0))
	 (cartesian-distance syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
    1.0 0.0))

(proclaim '(inline off-spot-STIMULUS))
(defun off-spot-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  (declare (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
	   (optimize (safety 1) (speed 3) (space 1)))
  (if (> (the sf (/ (the sf *spot-outside-diameter*) 2.0))
	 (cartesian-distance syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
      -1.0 0.0))

(proclaim '(inline bar-grating-STIMULUS))
(defun bar-grating-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  ;; This determines whether the moving bar grating is presently overhead the syn. Note that this function first translates the
  ;; syn coordinates according to the bar original position, and then rotates the coordinates according to the tilt of the rotated
  ;; bar (the bar frame). Coordinates are then translated according to the bar's movement from its original position (in the bar
  ;; frame's y-direction).
  (declare (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
  (if (and (or (> (abs (rem syn-rf-y-stimulation-frame *grating-spatial-period*))
		  (+ *grating-spatial-period* (/ *bar-width* -2.0)))
	       (< (abs (rem syn-rf-y-stimulation-frame *grating-spatial-period*))
		  (/ *bar-width* 2)))
	   (< (abs syn-rf-x-stimulation-frame) (/ *bar-length* 2.0)))
    1.0 0.0))

(proclaim '(inline sine-grating-STIMULUS))
(defun sine-grating-stimulus (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
  ;; This determines whether the moving sine grating is presently overhead the syn. Note that this function first translates the
  ;; syn coordinates according to the bar original position, and then rotates the coordinates according to the tilt of the rotated
  ;; bar (the bar frame). Coordinates are then translated according to the bar's movement from its original position (in the bar
  ;; frame's y-direction).
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
	   (ignore syn-rf-x-stimulation-frame))
  (sin (the sf (* (the sf (* 2.0 pi-single))
		  (the sf (/ syn-rf-y-stimulation-frame (the sf *grating-spatial-period*)))))))

(proclaim '(inline light-stimulus-at-xyt))
(defun light-stimulus-at-xyt (syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame delayed-time)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum delayed-time)
	   (single-float syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
  (the sf (+ (the sf *light-background*)
	     (the sf (* (the sf *light-stimulus-strength*)
			(the sf
			  (case *light-stimulus* 
			    (:reversing-bar 0.0)
			    (:apparent-motion
			     (apparent-motion-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame
						       (s-flt delayed-time)))
			    ((:bar :moving-bar :on-bar :on-moving-bar)
			     (on-bar-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
			    ((:Off-bar :off-moving-bar)
			     (off-bar-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
			    (:moving-bar-grating
			     ;; (bar-grating-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
			     0.0)
			    (:moving-sine-grating
			     ;; (sine-grating-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)
			     0.0)
			    ((:on-spot :spot :moving-spot :on-moving-spot)
			     (if *fast-full-field-spot* 1.0
				 (on-spot-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)))
			    ((:off-spot :off-moving-spot)
			     (if *fast-full-field-spot* -1.0
				 (off-spot-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame)))
			    (:annulus (annulus-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame))
			    (:custom
			     ;; (custom-stimulus syn-rf-x-stimulation-frame syn-rf-y-stimulation-frame delayed-time)
			     0.0)
			    (t 0.0))))))))

(proclaim '(inline spatial-rf-grid-size-x))
(defun spatial-rf-grid-size-x (spatial-rf-function type-parameters)
  (float (or (GET-KEY-VALUE-FROM-LIST spatial-rf-function :grid-size)
	     (cdr-assoc 'SPATIAL-RF-grid-size-x type-parameters)
	     (cdr-assoc 'SPATIAL-RF-grid-size type-parameters)
	     10.0)))

(proclaim '(inline spatial-rf-grid-size-y))
(defun spatial-rf-grid-size-y (spatial-rf-function type-parameters)
  (float (or (GET-KEY-VALUE-FROM-LIST spatial-rf-function :grid-size)
	     (cdr-assoc 'SPATIAL-RF-grid-size-y type-parameters)
	     (cdr-assoc 'SPATIAL-RF-grid-size type-parameters)
	     10.0)))

(proclaim '(inline syn-rf-center-x))
(defun syn-rf-center-x (syn)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (s-flt (or (element-parameter syn 'LIGHT-input-x)
	     (the sf (+ (the sf (first (node-absolute-location (synapse-node syn))))
			(* (or (element-parameter (synapse-type syn) 'LIGHT-OFFSET-DISTANCE) 0.0)
			   (cos (or (element-parameter (synapse-type syn) 'LIGHT-OFFSET-angle) 0.0))))))))

(proclaim '(inline syn-rf-center-y))
(defun syn-rf-center-y (syn)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (s-flt (or (element-parameter syn 'LIGHT-input-y)
	     (the sf (+ (the sf (case *light-stimulus-plane*
				  (:xy (second (node-absolute-location (synapse-node syn))))
				  (:xz (- (third (node-absolute-location (synapse-node syn)))))))
			(* (or (element-parameter (synapse-type syn) 'LIGHT-OFFSET-DISTANCE) 0.0)
			   (sin (or (element-parameter (synapse-type syn) 'LIGHT-OFFSET-angle) 0.0))))))))

(defmacro syn-rf-geometry-values ((syn) &body forms)
  `(let* ((type-parameters (synapse-type-parameters (synapse-type ,syn)))
	  (syn-parameters (synapse-parameters ,syn))
	  (spatial-rf-array (cdr-assoc 'SPATIAL-RF type-parameters))
	  (adjust-to-rf-area (cdr-assoc 'adjust-to-rf-area type-parameters))
	  (light-input-delay (s-flt (or (cdr-assoc 'LIGHT-input-delay type-parameters) 0.0)))
	  (spatial-rf-function (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters))
	  (spatial-rf-grid-size-x (spatial-rf-grid-size-x spatial-rf-function type-parameters))
	  (spatial-rf-grid-size-y (spatial-rf-grid-size-y spatial-rf-function type-parameters))
	  (spatial-rf-x-size (if spatial-rf-array (array-dimension spatial-rf-array 0) 1))
	  (spatial-rf-y-size (if spatial-rf-array (array-dimension spatial-rf-array 1) 1))
	  (spatial-rf-grid-area
	   (if spatial-rf-array (* spatial-rf-grid-size-x spatial-rf-grid-size-y spatial-rf-x-size spatial-rf-y-size) 1.0))
	  (syn-rf-center-x (the sf (syn-rf-center-x ,syn)))
	  (syn-rf-center-y (the sf (syn-rf-center-y ,syn)))
	  (scaled-x-rf-offset (* spatial-rf-grid-size-x (/ (- spatial-rf-x-size 1.0) -2.0)))
	  (scaled-y-rf-offset (* spatial-rf-grid-size-y (/ (- spatial-rf-y-size 1.0) -2.0))))
     (declare
      (fixnum spatial-rf-x-size spatial-rf-y-size)
      (single-float syn-rf-center-x syn-rf-center-y scaled-x-rf-offset scaled-y-rf-offset))
     (progn . ,forms)))
  
(defun generate-light-input-array (syn) 
  ;; ********* CHECK THIS COMMENT ********* Returns nil if *light-input-waveform* is all zeros, ie the light never hits this
  ;; synapse, else fills *light-input-waveform* with the light input over time for the light-dependent synapse syn. Thresholds the
  ;; result of the spatial integration at each time point. References *LIGHT-ORIGIN-ARRAY*. Integration over synapse RF -
  ;; spatial-rf array has a sigma-x by sigma-y um grid size, while the synapse and light origin coordinates are in units of 1um.
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (syn-rf-geometry-values
   (syn)
   (progn
     (let ((moving (is-light-moving)) (fix-delay (round light-input-delay))
	   (cos-theta (cos *light-theta*)) (sin-theta (sin *light-theta*))
	   (fix-start (round *light-stimulus-start-time*)) (fix-stop (round *light-stimulus-stop-time*))
	   (syn-rf-x-start (the sf (- (the sf (+ syn-rf-center-x scaled-x-rf-offset)) (the sf *light-start-position-x*))))
	   (syn-rf-y-start (the sf (- (the sf (+ syn-rf-center-y scaled-y-rf-offset)) (the sf *light-start-position-y*))))
	   (spatial-rf-coeff 1.0)
	   non-zero-result input-value)
       (declare (single-float cos-theta sin-theta spatial-rf-coeff))
       ;; Integrate the spatial RF with the stimulus for every time point of the simulation for
       ;; which the synapse is activated.
       (do ((time fix-delay (the fn (1+ time)))) ((= time *int-user-stop-time*))	
	 (declare (fixnum time fix-delay fix-start fix-stop))
	 (setq
	  input-value
	  (let ((d-time (the fn (- time (the fn fix-delay))))) ; Adjust time for syn delay.
	    (let ((x-light-origin (if moving (first (aref (the (simple-array cons *) *light-origin-array*) d-time)) 0.0))
		  (y-light-origin (if moving (second (aref (the (simple-array cons *) *light-origin-array*) d-time)) 0.0))
		  (sum 0.0))
	      (declare (single-float x-light-origin y-light-origin sum))
	      ;; Integrate over X
	      (do ((x-rf-index 0 (+ 1 (the fn x-rf-index)))
		   (syn-rf-x syn-rf-x-start (the sf (+ syn-rf-x spatial-rf-grid-size-x))))
		  ((= (the fn x-rf-index) (the fn spatial-rf-x-size)))
		;; Integrate over Y
		(do ((y-rf-index 0 (+ 1 (the fn y-rf-index)))
		     (syn-rf-y syn-rf-y-start (the sf (+ syn-rf-y spatial-rf-grid-size-y))))
		    ((= (the fn y-rf-index) (the fn spatial-rf-y-size)))
		  (declare (fixnum x-rf-index y-rf-index) (single-float syn-rf-x syn-rf-y))
		  (when spatial-rf-array
		    (setq spatial-rf-coeff (the sf (aref spatial-rf-array x-rf-index y-rf-index))))
		  (unless (= 0.0 spatial-rf-coeff)
		    (setq sum 
			  (the sf
			    (+ (the sf sum)
			       (the sf
				 (* spatial-rf-coeff
				    (the sf
				      (if (or (< d-time (the fn fix-start)) (> d-time (the fn fix-stop)))
					(the sf *light-background*)
					(light-stimulus-at-xyt
					 (the sf (- (the sf (+ (the sf (* syn-rf-x cos-theta))
							       (the sf (* syn-rf-y sin-theta))))
						    x-light-origin))
					 (the sf (- (the sf (- (the sf (* syn-rf-y cos-theta))
							       (the sf (* syn-rf-x sin-theta))))
						    y-light-origin))
					 d-time)))))))))))
	      (the sf (* (if adjust-to-rf-area spatial-rf-grid-area 1.0) sum)))))
	 (unless (= (the sf input-value) 0.0) (setq non-zero-result t))
	 (setf (aref (the (simple-array single-float *) *light-input-waveform*) time)
	       (the sf input-value)))	    
       non-zero-result))))

(defun map-light-inputs (&key (x-max 500)(x-min -500)(y-max 500)(y-min -500) type-name (use-menus t))
  (loop for type-name in
	(if use-menus
	  (choose-list-values (GET-CURRENT-SYNAPSE-TYPE-NAMES) (list type-name) :label "Choose Synapse Types to Map")
	  (list type-name))
	do
	(let ((dummy1 x-max) (dummy2 x-min) (dummy3 y-max) (dummy4 y-min))
	  (when use-menus
	    (choose-variable-values
	     '((dummy1 "X Maximum" :number) (dummy2 "X Minimum" :number)
	       (dummy3 "Y Maximum" :number) (dummy4 "Y Minimum" :number))
	     :label (format nil "Map Synapse Type ~a RF Centers Randomly" type-name))
	    (setq x-max dummy1 x-min dummy2 y-max dummy3 y-min dummy4))
	  (loop for syn in (synapses)
		when (and (if type-name (string= type-name (synapse-type-name (synapse-type syn))) t)
			  (eq (synapse-type-control (synapse-type syn)) :light))
		do
		(set-synapse-rf-center-x syn (+ x-min (* (random 1.0) (- x-max x-min))))
		(set-synapse-rf-center-y syn (+ y-min (* (random 1.0) (- y-max y-min))))))))

(defun synapse-rf-center-x (syn) (element-parameter syn 'light-input-x))
(defun synapse-rf-center-y (syn) (element-parameter syn 'light-input-y))
(defun set-synapse-rf-center-x (syn rf-center-x) (element-parameter syn 'light-input-x rf-center-x))
(defun set-synapse-rf-center-y (syn rf-center-y) (element-parameter syn 'light-input-y rf-center-y))

(defun plot-synapse-rfs (&optional type)
  (loop for type in (element (or (coerce-to-list type)
				 (choose-list-values (coerce-to-list (element-name (get-synapse-types :light))) nil
						     :label "Choose light synapse type for plotting RF profiles"))
			     'synapse-type)
	do
	(let* ((type-parameters (synapse-type-parameters type))
	       (spatial-rf-array (cdr-assoc 'SPATIAL-RF type-parameters))
	       (spatial-rf-grid-size-x (float (or (cdr-assoc 'SPATIAL-RF-grid-size-x type-parameters)
						  (cdr-assoc 'SPATIAL-RF-grid-size type-parameters)
						  10.0)))
	       (spatial-rf-grid-size-y (float (or (cdr-assoc 'SPATIAL-RF-grid-size-y type-parameters)
						  (cdr-assoc 'SPATIAL-RF-grid-size type-parameters)
						  10.0)))
	       (spatial-rf-x-size (if spatial-rf-array (array-dimension spatial-rf-array 0) 1))
	       (spatial-rf-y-size (if spatial-rf-array (array-dimension spatial-rf-array 1) 1))
	       (light-input-offset-distance (cdr-assoc 'LIGHT-OFFSET-DISTANCE type-parameters))
	       (light-input-offset-angle (cdr-assoc 'LIGHT-OFFSET-ANGLE type-parameters))
	       (spatial-rf-args (cdr-assoc 'SPATIAL-RF-FUNCTION type-parameters)))
	  (when spatial-rf-array
	    (let ((dummy1 :3dplot) win)
	      (choose-variable-values
	       '((dummy1 "Plot type" :choose (:3dplot :density)))
	       :label (format nil "Plot Type for ~a RF" (synapse-type-name type)))
	      (setq win
		    (case dummy1
		      (:3dplot (3dplot spatial-rf-array :phi 10.0 :theta 10.0 :scale 1.0
				       :grid-size-x spatial-rf-grid-size-x :grid-size-y spatial-rf-grid-size-y
				       :title (format nil "Synapse Type ~a RF (3d)" (synapse-type-name type))))
		      (:density (density-plot spatial-rf-array
					      :x-inc spatial-rf-grid-size-x
					      :y-inc spatial-rf-grid-size-y
					      :title (format nil "Synapse Type ~a RF (Dens)" (synapse-type-name type))))))
	      (add-comment win
			   (format nil "Synapse Type ~a RF args:~% ~A" (synapse-type-name type) spatial-rf-args)
			   :position :upper-right))))))
			  
(defun plot-light-input (&optional syns)
  (loop for syn in (element (or (coerce-to-list syns)
				(choose-list-values (coerce-to-list (element-name (active-light-synapses))) nil
						    :do-all-at-once nil :rank-margin 5 :direction :vertical :label "Choose Light Synapses For Light Plotting"))
			    'synapse)
	when (and (synapse-wave-ref syn)
		  (or (eq :light-event (synapse-type-control (synapse-type syn)))
		      (eq :light (synapse-type-control (synapse-type syn))))
		  (element-parameter (synapse-type syn) 'LIGHT-INPUTS-LIST-OF-ARRAYS))
	collect
	(let* ((light-input-list-of-arrays (element-parameter (synapse-type syn) 'LIGHT-INPUTS-LIST-OF-ARRAYS))
	       (index (synapse-wave-index syn))
	       (shift (synapse-wave-shift syn))
	       (gain (synapse-wave-gain syn)))
	  (loop for time from 0 to *user-stop-time*
		when (> *user-stop-time* (+ time shift) -1)
		collect	(* gain (aref (nth index light-input-list-of-arrays) (+ time shift))) into out
		else collect *light-background* into out
		finally (return out)))
	into lights
	and collect (element-name syn) into labels
	finally (when lights (plot-timed-data lights labels 1.0 :x-max *user-stop-time* :x-min 0.0
					      :y-label "Light" :title (format nil "~A: Light Input" *simulation-name*)))))

(defun get-light-response-input-waveform (syn)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((light-responses-ARRAY (element-parameter (synapse-type syn) 'light-responses-ARRAY))
	 (light-length (the fn (array-dimension (the (simple-array single-float (* *)) light-responses-ARRAY) 1)))
	 (index (synapse-wave-index syn))
	 (row-major-aref-offset (the fn (* light-length index)))
	 (shift (synapse-wave-shift syn))
	 (gain (synapse-wave-gain syn)))
    (declare (fixnum shift row-major-aref-offset)
	     (single-float gain))
    (loop for time fixnum from 0 to *user-stop-time*
	  when (> *int-user-stop-time* (+ time shift) -1)
	  collect (* gain (row-major-aref (the (simple-array single-float (* *)) light-responses-ARRAY) (+ row-major-aref-offset time shift)))
	  into out
	  else collect 0.0 into out
	  finally (return out))))





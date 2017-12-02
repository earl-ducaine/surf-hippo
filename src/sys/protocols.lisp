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


;;; SYS Source file: protocols.lisp
(in-package "SURF-HIPPO")

(defmacro add-comment-to-all-new-output-windows (comment comment-position borderp font append-to-old-comment &body body)
  (let ((original-*output-windows* (gensym))
	(internal-comment (gensym))
	(result (gensym)))
    `(let ((,original-*output-windows* *output-windows*)
	   (,internal-comment ,comment)
	   (,result (progn . ,body)))
       (when ,internal-comment
	 (add-comment (set-difference *output-windows* ,original-*output-windows*)
		      ,internal-comment :font ,font :position ,comment-position :borderp ,borderp :append-to-old-comment
		      ,append-to-old-comment))
       ,result)))

(defmacro apply-function-to-all-new-output-windows (function &body body)
  `(let ((internal-functions (coerce-to-list ,(or function 'identity)))
	 (original-*output-windows* *output-windows*)
	 (result (progn . ,body)))
     (dolist (internal-function internal-functions)
       (mapcar internal-function (set-difference *output-windows* original-*output-windows*)))
     result))

(defun std-setup (&optional (element *soma*))
  "Plot voltage and add an isource to the somas associated with ELEMENT [default *SOMA*]."
  (loop for soma in (coerce-to-list (element-soma element)) do
	(add-isource soma)
	(enable-element-plot soma)))

(defun replace-and-return-elements-and-caps (new-capacitance)
  (let ((double-new-capacitance (coerce new-capacitance 'double-float))
	(single-new-capacitance (coerce new-capacitance 'single-float)))
    (loop for elt in (cell-elements)
	  collect (list elt (element-capacitance elt)) into element-original-caps
	  do (element-capacitance elt new-capacitance)
	  finally (return element-original-caps))))

(defun init-with-steady-state-linear-voltage-clamp (vsource &optional v-holding)
  ; (setq *use-node-voltage-initializations* t)
  (steady-state-linear-voltage-clamp vsource v-holding)
  ; (set-*node-voltage-initializations*)
  )

(defun find-steady-state (&optional (steady-state-method :one-step) (run-regular-capacitance *RUN-REG-CAP-FOR-SS*))
  (if (and (eq steady-state-method :one-step)
	   (= 1 (length (vsources)))
	   (soma-p (element-cell-element *vsource*)))
      (init-with-steady-state-linear-voltage-clamp *vsource*)
      (init-with-reduced-capacitance run-regular-capacitance)))

(defun init-with-reduced-capacitance (run-regular-capacitance)
  (let (*find-steady-state*		; So not called recursively.
	*enable-channels* *ADVANCE-event-ELEMENTS* *advance-sources* *automatic-run* *kill-extra-messages*
	*plot-standard-windows* *plot-steady-state*
	(element-original-caps (replace-and-return-elements-and-caps *minimal-capacitance*))
	(*kill-all-output* t)
	(*user-stop-time* (+ *user-start-time* *low-cap-pseudo-steady-state-time*))
	; (*use-fixed-step* nil)
	(*INTEGRATION-TIME-REFERENCE* :variable)
	)
    (format t "Getting pseudo-steady-state (low capacitance)...~%")
    (setq *use-node-voltage-initializations* t
	  *node-voltage-initializations*		; Maybe this is a good start
	  (loop for vsource in (vsources)
		collect (list (element-physical-node vsource) (GET-VSOURCE-VOLTAGE vsource 0.0))))
    (goferit)
    (loop for elt-cap in element-original-caps
	  do (typecase (car elt-cap)
	       (soma (setf (soma-capacitance (car elt-cap)) (cadr elt-cap)))
	       (segment (setf (segment-capacitance (car elt-cap)) (cadr elt-cap)))))
    (when (or (not *plot-steady-state*) (let (*automatic-run*) (go-ahead-menu "Go ahead and set init-values...")))
      (set-*node-voltage-initializations*)
      (setq *user-stop-time* (+ *user-start-time* *pseudo-steady-state-time*))
      (setq *use-node-voltage-initializations* t)
      (when run-regular-capacitance
	(format t "Getting pseudo-steady-state (normal capacitance)...~%")
	(goferit)
	(when (or (not *plot-steady-state*) (let (*automatic-run*) (go-ahead-menu "Go ahead and set init-values...")))
	  (set-*node-voltage-initializations*))))))

(defun vclamp-soma-conductance (clamp-potential-1
				clamp-potential-2
				&key (dt 0.1) new-plot (plot-data t) (plot-uncorrected-wave t)
				comment
				(cell *cell*) (r-electrode 0.0) (ideal-vsource t)
				(timeit t) return-g-waves pause-between-clamps
				(correction-coeff 1.0) (DISABLE-VSOURCES t) interclamp-function run-reg-cap-for-ss)
  "Calculates the input soma conductance as a function of time using voltage clamp at two holding potentials CLAMP-POTENTIAL-1 and
CLAMP-POTENTIAL-2 [mV]. An electrode resistance may be included with R-ELECTRODE [Mohms]. The voltage clamp \(ideal if
IDEAL-VSOURCE is T\) is applied to the soma.  Any other voltage sources in the circuit are disabled during the simulation when
DISABLE-VSOURCES is T."
  (when (and (vsources) DISABLE-VSOURCES) (disable-element (vsources)))
  (let* ((new-plot (and new-plot plot-data))
	 (*use-node-voltage-initializations* t)
	 (soma (cell-soma (element-cell cell)))
	 (*plot-standard-windows* plot-data)
	 (*run-reg-cap-for-ss* run-reg-cap-for-ss)
	 (*vsource-resistance* r-electrode)
	 (*CREATE-NEW-SIMULATION-PLOTS* new-plot)
	 (*create-new-plot-windows* new-plot)
	 (*overlay-plots* (not new-plot))
	 (*CREATE-NEW-SIMULATION-PLOTS* (not new-plot))
	 (*user-max-step* 1.0)		;maybe conservative
	 (*plot-synapse-currents-p* (and plot-data *plot-synapse-currents-p*))
	 (*plot-synapse-conductances-p* (and plot-data *plot-synapse-conductances-p*))
	 (*accomodate-overlays* plot-data)
	 (vsource (or (element-vsources soma)
		      (add-vsource soma :ideal ideal-vsource))))
    (enable-element vsource)
    (enable-element-plot vsource 'current)
    (loop for vsource-reference-magnitude in (list clamp-potential-1 clamp-potential-2) do
	  (setq *vsource-default-reference-magnitude* (float vsource-reference-magnitude))
	  (typecase interclamp-function
	    (cons (apply 'funcall interclamp-function))
	    (function (funcall interclamp-function)))
	  (format t "Voltage clamp at ~,2fmV.~%"  (float vsource-reference-magnitude))
	  (init-with-steady-state-linear-voltage-clamp *vsource*)
	  (if timeit (gotimed) (goferit))
	  (when plot-data (setq *overlay-plots* t
				*create-new-plot-windows* nil
				*CREATE-NEW-SIMULATION-PLOTS* nil))
	  collect (element-data-dted vsource dt) into currents

	  when pause-between-clamps do (break) ; (go-ahead-menu "Go ahead and run 2nd clamp...")

	  finally
	  (setq *create-new-plot-windows* new-plot)
	  (return
	    (let* ((g-wave-data
		    (clean-up-list
		     (cons (when (and (not (= 0 r-electrode)) plot-uncorrected-wave)
			     (g-wave-new (nth 0 currents) nil clamp-potential-1
					 (nth 1 currents) nil clamp-potential-2))
			   (loop for coeff in (if (= 0 r-electrode) '(1.0) (list correction-coeff))
				 collect (g-wave-new (nth 0 currents) nil clamp-potential-1
						     (nth 1 currents) nil clamp-potential-2
						     (* coeff r-electrode))))))
		   (g-wave-labels		 
		    (clean-up-list
		     (cons (when (and (not (= 0 r-electrode)) plot-uncorrected-wave) "Soma estimated G uncorrected")
			   (if (= 0 r-electrode) '("Soma Gin")
			       (loop for coeff in (if (= 0 r-electrode) '(1.0) (list correction-coeff))
				     collect (format nil "Soma estimated G w/R-correction ~A Mohms"
						     (* coeff r-electrode))))))))
	      (when plot-data
		(plot-timed-data
		 g-wave-data g-wave-labels dt
		 :comment comment
		 :delta-t-start *user-start-time*
		 :title (concatenate
			 'string
			 (cell-name (soma-cell soma))
			 (if (= 0 r-electrode)
			     " G-wave Simulation"
			     (format nil " G-wave Simulation - R-electrode ~a Mohms" r-electrode)))
		 :x-max *user-stop-time* :y-label "uS" :y-min 0.0 :x-min *user-start-time*
		 :overlay t :timed-data t))
	      (erase-element vsource)
	      (when (and (vsources) DISABLE-VSOURCES) (enable-element (vsources)))
	      (when return-g-waves (cons g-wave-labels g-wave-data)))))))

(defun clamp-soma-conductance (clamp-command-1 clamp-command-2
					       &key (dt 0.1) new-plot (plot-data t) (plot-uncorrected-wave t)
					       (timeit t) pause-between-clamps
					       (vclamp-p t) (cell *cell*) (r-electrode 0.0) (ideal-vsource t) return-g-waves
					       (correction-coeff 1.0) (DISABLE-SOURCES t) interclamp-function run-reg-cap-for-ss)
  "Calculates the input soma conductance as a function of time using voltage clamp or current clamp protocols, depending on
VCLAMP-P, at two clamp command levels CLAMP-COMMAND-1 and CLAMP-COMMAND-2 [mV or nA, as appropriate]. An electrode resistance may
be included with R-ELECTRODE [Mohms]. The voltage clamp \(ideal if IDEAL-VSOURCE is T\) is applied to the soma.  Any other voltage
sources in the circuit are disabled during the simulation when DISABLE-SOURCES is T."
  (if vclamp-p
    (when (and (vsources) DISABLE-SOURCES) (disable-element (vsources)))
    (when (and (isources) DISABLE-SOURCES) (disable-element (isources))))
  (let* ((new-plot (and new-plot plot-data))
	 (*use-node-voltage-initializations* t)
	 (soma (cell-soma (element-cell cell)))
	 (*plot-standard-windows* plot-data)
	 (*vsource-resistance* r-electrode)
	 (*accomodate-overlays* t)
	 (*CREATE-NEW-SIMULATION-PLOTS* new-plot)
	 (*create-new-plot-windows* new-plot)
	 (*overlay-plots* (not new-plot))
	 (*CREATE-NEW-SIMULATION-PLOTS* (not new-plot))
	 (*user-max-step* 0.2)		; Maybe conservative
	 (*plot-synapse-currents-p* (and plot-data *plot-synapse-currents-p*))
	 (*plot-synapse-conductances-p* (and plot-data *plot-synapse-conductances-p*))
	 (*accomodate-overlays* plot-data)
	 (source (when vclamp-p (or (element-vsources soma) (add-vsource soma :ideal ideal-vsource)))))
    (enable-element source)
    (enable-element-plot (if vclamp-p source *soma*))
    (loop for clamp-command in (list clamp-command-1 clamp-command-2) do
	  (if vclamp-p
	    (setq *vsource-default-reference-magnitude* (float clamp-command))
	    (add-constant-current-to-element *soma* clamp-command))
	    
	  (typecase interclamp-function
	    (cons (apply 'funcall interclamp-function))
	    (function (funcall interclamp-function)))
	  (format t "Clamp at ~,2f.~%"  (float clamp-command))
	  (if vclamp-p
	    (init-with-steady-state-linear-voltage-clamp *vsource*)
					; Mohms * nA = mV
	    (init-with-steady-state-linear-voltage-clamp *soma* (* clamp-command (cell-r-in (element-cell *soma*))))) 
	  
	  (if timeit (gotimed) (goferit))
	  (unless vclamp-p (clear-constant-currents))
	  
	  (when plot-data (setq *overlay-plots* t *create-new-plot-windows* nil *CREATE-NEW-SIMULATION-PLOTS* nil))
	  collect (element-data-dted (if vclamp-p source *soma*) dt) into source-data

	  when pause-between-clamps do (break) ; (go-ahead-menu "Go ahead and run 2nd clamp...")

	  finally
	  (setq *create-new-plot-windows* new-plot)
	  
	  (return
	   (let* ((g-wave-data
		   (clean-up-list
		    (cons (when (and (not (= 0 r-electrode)) plot-uncorrected-wave)
			    (g-wave-new (nth 0 source-data) (not vclamp-p) clamp-command-1
					(nth 1 source-data) (not vclamp-p) clamp-command-2))
			  (loop for coeff in (if (= 0 r-electrode) '(1.0) (list correction-coeff))
				collect (g-wave-new (nth 0 source-data) (not vclamp-p) clamp-command-1
						    (nth 1 source-data) (not vclamp-p) clamp-command-2
						    (* coeff r-electrode))))))
		  (g-wave-labels		 
		   (clean-up-list
		    (cons (when (and (not (= 0 r-electrode)) plot-uncorrected-wave) "Soma estimated G uncorrected")
			  (if (= 0 r-electrode) (list "Soma Gin")
			      (loop for coeff in (if (= 0 r-electrode) '(1.0) (list correction-coeff))
				    collect (format nil "Soma estimated G w/R-correction ~A Mohms"
						    (* coeff r-electrode))))))))
	     (when plot-data
	       (plot-timed-data
		g-wave-data g-wave-labels dt :delta-t-start *user-start-time*
		:title (format nil "~A~A"
			       (cell-name (soma-cell soma))
			       (if (= 0 r-electrode)
				 " G-wave Simulation"
				 (format nil " G-wave Simulation - R-electrode ~a Mohms" r-electrode)))
		:x-max *user-stop-time* :y-label "uS" :y-min 0.0 :x-min *user-start-time* :overlay t :timed-data t))

	     (erase-element source)
	     (when (and (vsources) DISABLE-SOURCES) (enable-element (vsources)))
	     (when return-g-waves (cons g-wave-labels g-wave-data)))))))


#|
1.0e-9A => nA
------------  => 1.0e-6S => uS
1.0e-3V => mV
|#

;; For voltage clamp, the units for the output list is the unit of WAVEs (current) divided by the unit of CLAMPs
;; (volts). R-ELECTRODE should be in units of the inverse of the output units. Therefore, for example, if the WAVEs are in nA and
;; the CLAMPs are in mV, then r-electrode should be in Mohms, and the output will be in uS.

;; VOLTAGExP is true when WAVEx is a voltage trace (=> CLAMPx is current), which is the case when WAVEx is from a current clamp
;; recordings, and vica versa.

(defun g-wave-new (wave1 voltage1p clamp1 wave2 voltage2p clamp2 &optional (r-electrode 0.0)) ; Mohms
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((clamp1 (s-flt clamp1))
	(clamp2 (s-flt clamp2))
	(r-electrode (s-flt r-electrode)))
    (loop for val1 single-float in wave1 for val2 single-float in wave2 collecting
	  (/
	   ;; Delta I
	   (- (if voltage1p clamp1 val1) (if voltage2p clamp2 val2))
	   ;; Delta V
	   (- (- (if voltage1p val1 clamp1)
		 (if voltage1p 0.0 (* r-electrode val1)))
	      (- (if voltage2p val2 clamp2)
		 (if voltage2p 0.0 (* r-electrode val2))))))))

(defun vc-g-erev (v-holdings &key (data-dt 0.1)
			     comment
			     print-v-g-rest
			     (relative-voltage-error 0.01) (min-rel-g-for-e-rev 0.1)
			     debug break-btwn-runs
			     e-rev-plot-min e-rev-plot-max
			     new-plot (plot-data t))
  (let ((new-plot (and new-plot plot-data))
	(*CREATE-NEW-SIMULATION-PLOTS* new-plot)
	(*create-new-plot-windows* new-plot)
	(*overlay-plots* (or *overlay-plots* (not new-plot)))
	(vsrc (or *vsource* (add-vsource *soma*)))
	(*absolute-voltage-error* (s-flt relative-voltage-error))
	(*accomodate-overlays* plot-data)
	(*plot-vsource-currents-p* t)
	(*SAVE-DATA-STEP* 1)
	(*use-node-voltage-initializations* t)
	*beep-after-surf*)	
    (element-parameter vsrc 'set-default-to-start-of-pulses t)
    (enable-element-plot vsrc 'current)
    (loop for v-holding in v-holdings
	  for run-count from 1
	  do (when plot-data (setq *overlay-plots* t
				   *create-new-plot-windows* nil
				   *CREATE-NEW-SIMULATION-PLOTS* nil))
	  when break-btwn-runs do (break)
	  do (steady-state-vclamp v-holding vsrc)
	  collect (element-data-dted vsrc data-dt) into currents
	  collect v-holding into command-voltages
	  do (setq *overlay-plots* t)
	  finally
	  (setq *create-new-plot-windows* new-plot)
	  (multiple-value-bind (g-wave g-wave-time-list g-wave-during-e-rev e-rev-wave e-rev-time-list cc-volts-wave)
	      (vc-g-wave-linear-regression
	       (let ((min-length (loop for current-wave in currents minimize (length current-wave))))
		 (loop for current-wave in currents collect (list-head current-wave min-length)))
	       command-voltages
	       :print-v-g-rest print-v-g-rest
	       :debug debug :delta-t data-dt :min-rel-g-for-e-rev min-rel-g-for-e-rev)
	    (when plot-data
	      (plot-timed-data cc-volts-wave '(nil) g-wave-time-list 
			       :x-max *user-stop-time* :x-label "ms" :y-label "mV"
			       :comment comment :title (format nil "~A: Reconstructed CC Response" *simulation-name*))
	      (plot-timed-data g-wave-during-e-rev '(nil) e-rev-wave 
			       :comment comment :title (format nil "~A: Gin-Erev Characteristic" *simulation-name*)
			       :scatter t :connect-data-points nil
			       :x-origin e-rev-plot-min :x-min e-rev-plot-min :x-max e-rev-plot-max :y-min 0
			       :x-label "mV" :y-label "uS")
	      (plot-timed-data g-wave '(nil)  g-wave-time-list 
			       :comment comment :title (format nil "~A: Gin" *simulation-name*)
			       :x-label "ms" :y-label "uS"
			       :x-min 0 :x-max *user-stop-time* :y-min 0)
	      (plot-timed-data e-rev-wave '(nil) e-rev-time-list
			       :comment comment :title (format nil "~A: Erev" *simulation-name*)
			       :y-label "mV" :x-label "ms"
			       :x-min 0 :x-max *user-stop-time*
			       :scatter t :connect-data-points nil))))))

;; Returns g values in uS.
(defun vc-g-wave-linear-regression (current-waves ; nA
				    voltages ; mV
				    &key (r-electrode 0.0) ; Mohms
				    print-v-g-rest
				    (min-rel-g-for-e-rev 0.001)
				    (time-list *sim-reverse-plot-time-list*)
				    delta-t debug)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float min-rel-g-for-e-rev r-electrode))
  (let* ((clamp-voltages (loop for voltage in voltages collect (s-flt voltage)))
	 (length-of-waves (length (the cons (car current-waves))))
	 (time-list (if delta-t (list-of-nums length-of-waves 0.0 (float delta-t)) time-list))
	 (g-wave '())
	 (g-wave-during-e-rev '())
	 (g-wave-time-list '())
	 (e-rev-wave '())
	 (e-rev-time-list '())
	 (cc-volts-wave '()) 
	 (g-rest 0.0)
	 (v-rest 0.0))
    (declare (single-float g-rest v-rest))
    (loop for count fixnum from 0 to (1- length-of-waves)
	  for time in time-list do
	  (loop for current-wave in current-waves
		for clamp-voltage single-float in clamp-voltages
		
		collect (the sf (car current-wave)) into currents
		collect (cdr current-wave) into new-current-waves
		collect (- clamp-voltage (* (the sf r-electrode) (the sf (car current-wave)))) into corrected-voltages

		finally (setq current-waves new-current-waves)
		(multiple-value-bind (slope intercept r)
		    (lin-reg (list currents corrected-voltages))
					; (declare (single-float slope intercept r))
		  (when debug (format t "Time ~A: slope ~A, intercept ~A~%" (nth count time-list) slope intercept))
		  (let* ((g-value (if (eq :infinity slope) 0.0
				      (when (numberp slope) (/ 1.0 slope))))
			 (save-e-rev
			  (not (or (= 0 count)
				   (not g-value)
				   (not (numberp intercept))
				   (= g-value g-rest)
				   (= intercept v-rest)
				   (if (<= min-rel-g-for-e-rev 0.0)
				       (< g-value g-rest)
				       (< (abs (- 1 (/ g-value g-rest))) min-rel-g-for-e-rev))))))
		    (when (= 0 count)
		      (unless (and g-value (numberp intercept))
			(sim-error
			 (format nil
				 "Resting Value for G cannot be derived from an ~A slope and ~A intercept!"
				 slope intercept)))
		      (setq g-rest g-value 
			    v-rest intercept)
		      (when print-v-g-rest (format t "V-rest: ~AmV, G-rest ~AuS~%" v-rest g-rest)))
		    (when (and g-value (numberp intercept))
		      (push g-value g-wave)
		      (push intercept cc-volts-wave)
		      (push time g-wave-time-list)
		      (when  save-e-rev
			(push g-value g-wave-during-e-rev)
			(push (progn
				(when debug (format t "I-rev ~A~%" (/ (- slope (/ 1.0 g-rest)) (- v-rest intercept))))
				(+ v-rest ; intercept
				   (* (/ 1.0 g-rest) ; slope
				      (/ (- v-rest intercept)
					 (- slope (/ 1.0 g-rest))))))
			      e-rev-wave)
			(push time e-rev-time-list)))))))
    (values (reverse g-wave)
	    (reverse g-wave-time-list)
	    (reverse g-wave-during-e-rev)
	    (reverse e-rev-wave)
	    (reverse e-rev-time-list)
	    (reverse cc-volts-wave))))

(defun steady-state-vclamp (v-holding &optional (vsource *vsource*))
  "Run a steady-state voltage clamp simulation with the optional VSOURCE set to V-HOLDING [mV]."
  (unless vsource (sim-error "STEADY-STATE-VCLAMP has no VSOURCE."))
  (turn-on vsource)
  (let ((*use-node-voltage-initializations* t)
	(OLD-SET-DEFAULT-TO-START-OF-PULSES (element-parameter vsource 'SET-DEFAULT-TO-START-OF-PULSES)))
    (element-parameter vsource 'SET-DEFAULT-TO-START-OF-PULSES t)
    (pulse-list vsource (list (list 0.0 *user-stop-time* v-holding)))
    (init-with-steady-state-linear-voltage-clamp vsource v-holding)
    (goferit)
    (element-parameter vsource 'SET-DEFAULT-TO-START-OF-PULSES OLD-SET-DEFAULT-TO-START-OF-PULSES)
    (turn-off vsource)
    nil))

(defun steady-state-iclamp (clamp-command &key (cc-element *soma*) (vsource *vsource*) (init-with-ss-vc t))
  (let ((cc-element (cell-element cc-element)))
    (when init-with-ss-vc
      (unless vsource (sim-error "STEADY-STATE-ICLAMP called with INIT-WITH-SS-VC, but no VSOURCE."))
      (turn-on vsource)
      (init-with-steady-state-linear-voltage-clamp cc-element (+ (cell-type-param *cell* 'vl)
								 (* clamp-command (cell-r-in (element-cell cc-element)))))
      (turn-off vsource))
    (add-constant-current-to-element cc-element clamp-command)
    (goferit)
    (clear-element-constant-current cc-element)
    nil))

(defun get-vc/cc-g-erev-vc-p-list (vc-p-list number-of-clamps all-vcs-p)
  (if vc-p-list 
    (if (consp vc-p-list) vc-p-list (boolean-list t number-of-clamps))
    (boolean-list all-vcs-p number-of-clamps)))

(defun get-vc/cc-g-erev-vsource (vc-p-list all-vcs-p init-cc-with-ss-vc clamp-vsource clamp-element)
  (when (or (no-nils vc-p-list) all-vcs-p init-cc-with-ss-vc) ; Do we need a vsrc?
    (if (vsource-p clamp-vsource)
      clamp-vsource
      (or (car (element-vsources clamp-element))
	  (add-vsource clamp-element)
	  *vsource*))))

(defun vc/cc-g-erev-run-interclamp-function (interclamp-function)
  (typecase interclamp-function
    (cons (if (consp (car interclamp-function))
	    (loop for thing in interclamp-function do
		  (typecase thing
		    (cons (apply 'funcall thing))
		    (function (funcall thing))))
	    (apply 'funcall interclamp-function)))
    (function (funcall interclamp-function))))

(defun vc/cc-g-erev-run-single-clamp (clamp-command clamp-element vc-p vsrc init-cc-with-ss-vc plot-voltage plot-vc-current)
  (if vc-p
    (progn
      (unless plot-voltage (disable-element-plot clamp-element))
      (enable-element-plot vsrc 'current)
      (let ((*plot-standard-windows* (or plot-voltage plot-vc-current)))
	(steady-state-vclamp clamp-command vsrc)))
    (progn
      (enable-element-plot clamp-element)
      (disable-element-plot vsrc 'current)
      (steady-state-iclamp clamp-command :cc-element clamp-element :vsource vsrc :init-with-ss-vc init-cc-with-ss-vc))))

(defvar *plot-vc/cc-scatter-as-multipoint-p* t)

(defun vc/cc-g-erev (clamp-commands vc-p-list
		     &key (data-dt 0.1)
		     plot-total-conductance ; :all
		     (plot-vc-current t) (plot-g-waves t) (plot-e-rev-wave t)
		     (plot-g-erev-phase-plot t) plot-reconstruction print-v-g-rest return-waves
		     (lock-analysis-windows t)
		     data-filter-function
		     clamp-vsource (clamp-element *soma*)
		     conc-int-vclamp-init-lists ; A list of values for *conc-int-initializations*, according to each of the clamp protocols implied in :CLAMP-COMMANDS.
		     g-rest		; in uS
		     v-rest		; in mV
		     all-vcs-p
		     (init-cc-with-ss-vc t)
		     interclamp-function
		     comment append-to-old-comment
		     (min-rel-g-for-e-rev 0.1) (absolute-voltage-error *absolute-voltage-error*) ; (relative-voltage-error 0.01)
		     debug break-btwn-runs
		     e-rev-plot-min e-rev-plot-max plot-voltage new-plot-windows (plot-data t)
		     add-trace-analysis-to-plot
		     ;; DISSECT-G-WAVE parameters
		     dissect-g-wave
		     (eex 20.0) (einh -70.0) ; mV
		     (float-eex t)(float-einh t))
  (add-comment-to-all-new-output-windows
   comment *default-comment-position* t nil append-to-old-comment
   (apply-function-to-all-new-output-windows
    (if lock-analysis-windows 'lock-window 'identity)  
    (let* ((clamp-element (cell-element clamp-element))
	   (clamp-vsource (element clamp-vsource))
	   (new-plot-windows (and new-plot-windows plot-data))
	   (number-of-clamps (length clamp-commands))
	   (vc-p-list (get-vc/cc-g-erev-vc-p-list vc-p-list number-of-clamps all-vcs-p))
	   (*CREATE-NEW-SIMULATION-PLOTS* new-plot-windows)
	   (*create-new-plot-windows* new-plot-windows)
	   (top-level-overlay-all-plots (or *overlay-plots* ; (not new-plot-windows)
					    ))
	   (*overlay-plots* top-level-overlay-all-plots)
	   (*CREATE-NEW-SIMULATION-PLOTS* (not new-plot-windows))

	   (*simulation-plot-window-comment* comment)
	   (*simulation-plot-window-comment-position* *default-comment-position*) ; borderp font append-to-old-comment
	   
	   (vsrc (get-vc/cc-g-erev-vsource vc-p-list all-vcs-p init-cc-with-ss-vc clamp-vsource clamp-element))
	   (*absolute-voltage-error* (s-flt absolute-voltage-error)) ; (s-flt relative-voltage-error)
	   (*accomodate-overlays* plot-data) (*plot-vsource-currents-p* t) *conc-int-initializations* *use-conc-int-initializations*
	   plot-titles-by-simulation-name ; If NIL, plot window titles will be derived from *CIRCUIT*.
	   (*SAVE-DATA-STEP* 1) (*use-node-voltage-initializations* t) *beep-after-surf* total-conductance-wave)
      (element-parameter vsrc 'set-default-to-start-of-pulses t)
      (when plot-voltage (enable-element-plot clamp-element)) ;; (turn-off *isource*)
      ;; A "run" is a single simulation at a single clamp holding command.
      (loop for clamp-command in clamp-commands
	    for vc-p in vc-p-list
	    for run-count from 1 do

	    (when (> run-count 1) (setq *overlay-plots* t))
	    (when (nth (1- run-count) conc-int-vclamp-init-lists)
	      (setq *conc-int-initializations* (nth (1- run-count) conc-int-vclamp-init-lists)
		    *use-conc-int-initializations* t))
	    (when plot-data (setq *create-new-plot-windows* nil *CREATE-NEW-SIMULATION-PLOTS* nil))
	    (when break-btwn-runs (break))
	    
	    (if (and plot-total-conductance (= run-count 1))
		(progn (SETUP-PLOT-TOTAL-CONDUCTANCES plot-total-conductance) (setq *PLOT-TOTAL-CONDUCTANCES-P* nil))
		(clear-plot-total-conductances))

	    (vc/cc-g-erev-run-interclamp-function interclamp-function)

	    (vc/cc-g-erev-run-single-clamp clamp-command clamp-element vc-p vsrc init-cc-with-ss-vc plot-voltage plot-vc-current)

	    collect (let ((data-wave (element-data-dted (if vc-p vsrc clamp-element) data-dt)))
		      (if data-filter-function (sequence-to-list (funcall data-filter-function data-wave)) data-wave))
	    into data-waves

	    do
	    (when (and plot-total-conductance (= run-count 1))
	      (setq total-conductance-wave (convert-data-time-lists (reverse (car *total-conductances-data*)) (current-sim-plot-time-list) data-dt)))
	    (setq *overlay-plots* t)

	    finally

	    (setq data-waves (NORMALIZE-LIST-LENGTHS data-waves))

	    (let ((*overlay-plots* top-level-overlay-all-plots))
	      (setq *use-node-voltage-initializations* nil)
	      (setq *create-new-plot-windows* new-plot-windows)
	      (multiple-value-bind (g-wave g-wave-time g-wave-during-e-rev e-rev-wave e-rev-wave-time cc-volts-wave g-rest)
		  (vc/cc-g-wave-linear-regression
		   data-waves clamp-commands vc-p-list
		   :derive-g-wave t :derive-cc-volts-wave t :derive-e-rev-wave t
		   :print-v-g-rest print-v-g-rest :g-rest g-rest :v-rest v-rest :debug debug :delta-t data-dt :min-rel-g-for-e-rev min-rel-g-for-e-rev)
	    
		(when plot-data
		  (when data-filter-function
		    (plot-vc/cc-g-erev-filtered-input data-waves clamp-commands data-dt add-trace-analysis-to-plot plot-titles-by-simulation-name comment))
		  (let ((g-waves (if total-conductance-wave (list g-wave total-conductance-wave) g-wave))
			(g-wave-labels (if total-conductance-wave '("Measured Gin" "Total Gin") '(nil))))
		    (when dissect-g-wave 
		      (multiple-value-bind (gex-wave ginh-wave)
			  (dissect-g-wave-from-gtot-erev
			   g-wave-during-e-rev g-wave-time e-rev-wave e-rev-wave-time g-rest eex einh :float-eex float-eex :float-einh float-einh)
			(setq g-waves (no-nils (list g-wave total-conductance-wave gex-wave ginh-wave))
			      g-wave-labels (no-nils `("Measured Gin" ,(when total-conductance-wave "Total Gin") "Gex" "Ginh")))))
		    (plot-vc/cc-g-erev-output (when plot-g-waves g-waves)
					      g-wave-labels g-wave-time (when plot-g-erev-phase-plot g-wave-during-e-rev) e-rev-wave
					      (when plot-e-rev-wave e-rev-wave-time) (when plot-reconstruction cc-volts-wave)
					      comment e-rev-plot-min e-rev-plot-max *user-stop-time*
					      :add-trace-analysis-to-plot add-trace-analysis-to-plot
					      :plot-titles-by-simulation-name plot-titles-by-simulation-name)))
		(when return-waves (return (values g-wave g-wave-time g-wave-during-e-rev e-rev-wave e-rev-wave-time cc-volts-wave data-waves))))))))))

(defun vc/cc-g-erev-w-trials (clamp-commands vc-p-list &key (trials 1) (data-dt 0.1) all-vcs-p
					     (derive-g-wave t) (derive-cc-volts-wave t) (derive-e-rev-wave t)
					     (plot-data t) (plot-only-one-g-erev-result t) (lock-analysis-windows t) new-plot-windows 
					     plot-total-conductance ; :ALL
					     (plot-vc-current t) (plot-g-waves t) (plot-e-rev-wave t) (plot-g-erev-phase-plot t) plot-reconstruction
					     print-v-g-rest return-waves
					     interclamp-function data-filter-function
					     clamp-vsource (clamp-element *soma*)
					     ;; List of lists for *CONC-INT-INITIALIZATIONS*, for each of the clamp protocols implied in :CLAMP-COMMANDS.
					     conc-int-vclamp-init-lists  
					     g-rest ; in uS
					     v-rest ; in mV
					     (init-cc-with-ss-vc t)
					     comment append-to-old-comment
					     (min-rel-g-for-e-rev 0.1) (absolute-voltage-error *absolute-voltage-error*)
					     debug break-btwn-runs
					     e-rev-plot-min e-rev-plot-max plot-voltage 
					     add-trace-analysis-to-plot
					     ;; DISSECT-G-WAVE parameters
					     dissect-g-wave (FLOAT-EEX t) (FLOAT-EINH t)
					     (eex 20.0) (einh -70.0)) ; mV
  (add-comment-to-all-new-output-windows
   comment *default-comment-position* t nil append-to-old-comment
   (apply-function-to-all-new-output-windows
    (when lock-analysis-windows 'lock-window)
    (let* ((clamp-element (cell-element clamp-element))
	   (clamp-vsource (element clamp-vsource))
	   (new-plot-windows (and new-plot-windows plot-data))
	   (number-of-clamps (length clamp-commands))
	   (vc-p-list (get-vc/cc-g-erev-vc-p-list vc-p-list number-of-clamps all-vcs-p))
	   (*CREATE-NEW-SIMULATION-PLOTS* new-plot-windows)
	   (*create-new-plot-windows* new-plot-windows)
	   (top-level-overlay-all-plots *overlay-plots*)
	   (*overlay-plots* top-level-overlay-all-plots)
	   (*CREATE-NEW-SIMULATION-PLOTS* (not new-plot-windows))
	   (*simulation-plot-window-comment* comment)
	   (*simulation-plot-window-comment-position* *default-comment-position*)
	   (vsrc (get-vc/cc-g-erev-vsource vc-p-list all-vcs-p init-cc-with-ss-vc clamp-vsource clamp-element))
	   (*absolute-voltage-error* (s-flt absolute-voltage-error)) ; (s-flt relative-voltage-error)
	   (*accomodate-overlays* plot-data) (*plot-vsource-currents-p* t) *conc-int-initializations* *use-conc-int-initializations*
	   plot-titles-by-simulation-name ; If NIL, plot window titles will be derived from *CIRCUIT*.
	   (*SAVE-DATA-STEP* 1) (*use-node-voltage-initializations* t) *beep-after-surf* g-actual-wave)
      (element-parameter vsrc 'set-default-to-start-of-pulses t)
      (when plot-voltage (enable-element-plot clamp-element));; (turn-off *isource*)
      ;; A "run" is a single simulation at a single clamp holding command.
      (let* ((*overlay-plots* top-level-overlay-all-plots)
	     (*create-new-plot-windows* new-plot-windows)
	     (all-data-waves
	      (loop for trial from 1 to trials collect
		    (loop for clamp-command in clamp-commands
			  for vc-p in vc-p-list
			  for run-count from 1 do
			  (setup-vc/cc-g-erev-run run-count plot-data plot-total-conductance conc-int-vclamp-init-lists interclamp-function break-btwn-runs)
			  (vc/cc-g-erev-run-single-clamp clamp-command clamp-element vc-p vsrc init-cc-with-ss-vc plot-voltage plot-vc-current)
			  collect (let ((data-wave (element-data-dted (if vc-p vsrc clamp-element) data-dt)))
				    (if data-filter-function (sequence-to-list (funcall data-filter-function data-wave)) data-wave))
			  into data-waves
			  do
			  (when (and plot-total-conductance (= run-count 1))
			    (setq g-actual-wave
				  (convert-data-time-lists (reverse (car *total-conductances-data*)) (current-sim-plot-time-list) data-dt)))
			  (setq *overlay-plots* t)
			  finally (return data-waves)))))
	(let* ((trials (length all-data-waves))
	       (levels (length (car all-data-waves)))
	       (*overlay-plots* t))
	  (loop for combo in (combos-of-levels trials levels) ; Each combo is a list with a trial entry for each level - (trial# trial# ...)
		for combo-count from 1 collect
		(let ((data-waves (loop for level from 0 to (1- levels)	collect (nth level (nth (nth level combo) all-data-waves))))
		      (*overlay-plots* t))
		  (multiple-value-bind (g-wave g-wave-time g-wave-during-e-rev e-rev-wave e-rev-wave-time cc-volts-wave g-rest)
		      (vc/cc-g-wave-linear-regression
		       data-waves clamp-commands vc-p-list
		       :derive-g-wave derive-g-wave :derive-cc-volts-wave derive-cc-volts-wave :derive-e-rev-wave derive-e-rev-wave
		       :print-v-g-rest print-v-g-rest :g-rest g-rest :v-rest v-rest :debug debug :delta-t data-dt :min-rel-g-for-e-rev min-rel-g-for-e-rev)
		    (when (and plot-data (or (not plot-only-one-g-erev-result) (= combo-count 1)))
		      (when data-filter-function
			(plot-vc/cc-g-erev-filtered-input data-waves clamp-commands data-dt add-trace-analysis-to-plot plot-titles-by-simulation-name comment)))
		    (let ((g-waves (if g-actual-wave (list g-wave g-actual-wave) g-wave))
			  (g-wave-labels (if g-actual-wave '("Measured Gin" "Total Gin") '(nil))))
		      (when dissect-g-wave 
			(multiple-value-bind (gex-wave ginh-wave)
			    (dissect-g-wave-from-gtot-erev
			     g-wave-during-e-rev g-wave-time e-rev-wave e-rev-wave-time g-rest eex einh :float-eex float-eex :float-einh float-einh)
			  (setq g-waves (no-nils (list g-wave g-actual-wave gex-wave ginh-wave))
				g-wave-labels (no-nils `("Measured Gin" ,(when g-actual-wave "Total Gin") "Gex" "Ginh")))))
		      (when (or (not plot-only-one-g-erev-result) (= combo-count 1))
			(plot-vc/cc-g-erev-output (when plot-g-waves g-waves)
						  g-wave-labels g-wave-time (when plot-g-erev-phase-plot g-wave-during-e-rev) e-rev-wave
						  (when plot-e-rev-wave e-rev-wave-time) (when plot-reconstruction cc-volts-wave)
						  comment e-rev-plot-min e-rev-plot-max *user-stop-time*
						  :g-rest g-rest
						  :add-trace-analysis-to-plot add-trace-analysis-to-plot
						  :plot-titles-by-simulation-name plot-titles-by-simulation-name))
		      (list g-waves g-wave-time g-wave-during-e-rev e-rev-wave e-rev-wave-time cc-volts-wave data-waves))))
		into all-vc/cc-g-wave-linear-regression-waves
		finally
		(plot-mean-and-std-dev-of-all-vc/cc-g-wave-linear-regression-waves all-vc/cc-g-wave-linear-regression-waves)
		(when return-waves
		  (return (if (= trials 1)
			    ;; G-WAVE G-WAVE-TIME G-WAVE-DURING-E-REV E-REV-WAVE E-REV-WAVE-TIME CC-VOLTS-WAVE DATA-WAVES
			    (values-list (car all-vc/cc-g-wave-linear-regression-waves))
			    (values all-vc/cc-g-wave-linear-regression-waves)))))))))))

(defun plot-mean-and-std-dev-of-all-vc/cc-g-wave-linear-regression-waves (all-vc/cc-g-wave-linear-regression-waves)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  ;; '(g-waves g-wave-time g-wave-during-e-rev e-rev-wave e-rev-wave-time cc-volts-wave data-waves)
  (let ((all-g-waves (mapcar #'(lambda (trial-data) (nth 0 trial-data)) all-vc/cc-g-wave-linear-regression-waves)))
    (when all-g-waves
      (let* ((time-sequence (nth 1 (nth 0 all-vc/cc-g-wave-linear-regression-waves)))
	     (trials (length all-vc/cc-g-wave-linear-regression-waves))
	     (g-totals (make-array (list trials) :element-type 'double-float))
	     (g-dissection-p (member (length (nth 0 all-g-waves)) '(3 4)))
	     (g-ex-index (if (= (length (nth 0 all-g-waves)) 3) 1 2))
	     (g-inh-index (if (= (length (nth 0 all-g-waves)) 3) 2 3))
	     (g-exs (make-array (list trials) :element-type 'double-float))
	     (g-inhs (make-array (list trials) :element-type 'double-float))
	     reverse-g-total-mean reverse-g-total-std-dev
	     reverse-g-ex-mean reverse-g-ex-std-dev
	     reverse-g-inh-mean reverse-g-inh-std-dev)
	 
	(loop while (cdr (nth 0 (nth 0 all-g-waves)))
	      ;; g-waves = '(g-wave g-actual-wave gex-wave ginh-wave)
	      do (loop for g-waves in all-g-waves
		       for all-g-index fixnum from 0
		       do (setf (aref g-totals all-g-index) (d-flt (car (nth 0 g-waves))))
		       when g-dissection-p
		       do (setf (aref g-exs all-g-index) (d-flt (car (nth g-ex-index g-waves)))
				(aref g-inhs all-g-index) (d-flt (car (nth g-inh-index g-waves))))
		       do (setf (nth 0 g-waves) (cdr (nth 0 g-waves)))
		       when g-dissection-p
		       do (setf (nth g-ex-index g-waves) (cdr (nth g-ex-index g-waves))
				(nth g-inh-index g-waves) (cdr (nth g-inh-index g-waves)))
		       finally
		       (push (mean-df g-totals) reverse-g-total-mean) (push (STD-DEV-dF-ARRAY g-totals) reverse-g-total-std-dev)
		       (when g-dissection-p
			 (push (mean-df g-exs) reverse-g-ex-mean) (push (STD-DEV-dF-ARRAY g-exs) reverse-g-ex-std-dev)
			 (push (mean-df g-inhs) reverse-g-inh-mean) (push (STD-DEV-dF-ARRAY g-inhs) reverse-g-inh-std-dev))))
	(plot-timed-data (no-nils (list reverse-g-total-mean reverse-g-total-std-dev
					reverse-g-ex-mean reverse-g-ex-std-dev
					reverse-g-inh-mean reverse-g-inh-std-dev))
			 (flatten-no-nils-list 'MEAN-G-TOT 'STD-DEV-G-TOT (when g-dissection-p '(MEAN-G-ex STD-DEV-G-ex MEAN-G-inh STD-DEV-G-inh)))
			 (reverse time-sequence)
			 :title "Mean and Std-Dev of Measured Conductances")))))

(defun setup-vc/cc-g-erev-run (run-count plot-data plot-total-conductance conc-int-vclamp-init-lists interclamp-function break-btwn-runs)
  (when break-btwn-runs (break))
  (when (> run-count 1) (setq *overlay-plots* t))
  (when (nth (1- run-count) conc-int-vclamp-init-lists)
    (setq *conc-int-initializations* (nth (1- run-count) conc-int-vclamp-init-lists)
	  *use-conc-int-initializations* t))
  (when plot-data (setq *create-new-plot-windows* nil *CREATE-NEW-SIMULATION-PLOTS* nil))
  (if (and plot-total-conductance (= run-count 1))
    (progn (SETUP-PLOT-TOTAL-CONDUCTANCES plot-total-conductance) (setq *PLOT-TOTAL-CONDUCTANCES-P* nil))
    (clear-plot-total-conductances))
  (vc/cc-g-erev-run-interclamp-function interclamp-function))
					     
;; Given n trials of a clamp protocol with m levels, return a list of all the possible combinations of the m levels from all trials.
(defun combos-of-levels (trials levels)
  ;; '((0 0 0) (1 0 0) (2 0 0)
  ;;   (0 1 0) (1 1 0) (2 1 0)
  ;;   (0 2 0) (1 2 0) (2 2 0) 
  ;;   (0 2 0) (1 2 0) (2 2 0)
  (loop for count from 0.0 to (1- (expt trials levels))
	collect (loop for level from 0.0 to (1- levels)
		      collect  (floor (mod (/ count (expt trials level)) trials)))))
#|
count            0 1 2 3 4 5 6 7 8 ...
trial  2  mod 2  0 1 0 1 0 1 0 1 ....
          mod 4  0 0 1 1 0 0 1 1 ....
          mod 8  0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 ....
           .
           .
           .


(format t "~B" 10)
|#

(defun plot-vc/cc-g-erev-filtered-input (data-waves clamp-commands data-dt add-trace-analysis-to-plot plot-titles-by-simulation-name comment)
  (plot-timed-data data-waves clamp-commands data-dt :x-label "ms"
		   :comment (if add-trace-analysis-to-plot
			      (concatenate-string-list (no-nils (cons comment (surf-plot-trace-analysis-strings data-waves clamp-commands data-dt))) :lf-count 1)
			      comment)
		   :title (format nil "~A: Filtered Clamp Data" (get-circuit-or-simulation-name plot-titles-by-simulation-name))))

(defun plot-vc/cc-g-erev-output (g-waves g-wave-labels g-wave-time g-wave-during-e-rev e-rev-wave e-rev-wave-time cc-volts-wave
					 comment e-rev-plot-min e-rev-plot-max stop-time
					 &key time-inc plot-titles-by-simulation-name add-trace-analysis-to-plot
					 g-rest
					 print-erev-measures (print-erev-measures-min-g 0.0)
					 (g-waves-in-uS-p t)) ; Otherwise, in nS
  (let ((*default-scatter-size* 2))
    (cond-every
     (cc-volts-wave
      (plot-timed-data cc-volts-wave nil g-wave-time
		       :comment (if add-trace-analysis-to-plot
				  (concatenate-string-list (cons comment (surf-plot-trace-analysis-strings (list cc-volts-wave) '(nil) g-wave-time)) :lf-count 1)
				  comment)
		       :x-inc time-inc :x-max stop-time :x-label "ms" :y-label "mV"
		       :title (format nil "~A: Reconstructed CC Response" (get-circuit-or-simulation-name plot-titles-by-simulation-name))))
     (g-wave-during-e-rev
      (let ((plot-win (plot-timed-data g-wave-during-e-rev nil e-rev-wave
				       :title (format nil "~A: Gin-Erev Characteristic" (get-circuit-or-simulation-name plot-titles-by-simulation-name))
				       :x-label "mV" :y-label (if g-waves-in-uS-p "uS" "nS") :comment comment
				       :plot-POLYLINE-as-multipoint *plot-vc/cc-scatter-as-multipoint-p*
				       :scatter (not *plot-vc/cc-scatter-as-multipoint-p*) :connect-data-points *plot-vc/cc-scatter-as-multipoint-p*
				       :x-origin e-rev-plot-min :x-min e-rev-plot-min :x-max e-rev-plot-max :y-min 0)))
	(when print-erev-measures
	  (let ((max-g (max-of-list g-wave-during-e-rev)))
	    (loop for g-during-e-rev in g-wave-during-e-rev
		  for e-rev in e-rev-wave
		  when (> g-during-e-rev print-erev-measures-min-g) maximize e-rev into max-e-rev and minimize e-rev into min-e-rev
		  when (= g-during-e-rev max-g) collect e-rev into e-ref-at-max-g
		  finally
		  (add-comment plot-win (format nil "E-rev min/max ~,2f/~,2fmV (Gmin ~,2f)~%E-rev ~AmV@Gmax ~,2f"
						min-e-rev max-e-rev print-erev-measures-min-g (atomize-list e-ref-at-max-g) max-g)
			       :position :upper-right))))))
     (g-waves (plot-timed-data g-waves g-wave-labels g-wave-time :title (format nil "~A: Gin" (get-circuit-or-simulation-name plot-titles-by-simulation-name))
			       :comment (if add-trace-analysis-to-plot
					  (concatenate-string-list (cons comment (surf-plot-trace-analysis-strings g-waves g-wave-labels g-wave-time)) :lf-count 1)
					  comment)
			       :x-label "ms" :y-label (if g-waves-in-uS-p "uS" "nS") :x-inc time-inc :x-min 0 :x-max stop-time :y-min 0)
	      (when g-rest (mark-plotline *twin* 0 stop-time g-rest g-rest :line-style dashed-line)))
     ((and e-rev-wave e-rev-wave-time)
      (plot-timed-data e-rev-wave nil e-rev-wave-time
		       :title (format nil "~A: Erev" (get-circuit-or-simulation-name plot-titles-by-simulation-name)) :comment comment
		       :y-label "mV" :x-label "ms" :x-inc time-inc :x-min 0 :x-max stop-time
		       :plot-POLYLINE-as-multipoint *plot-vc/cc-scatter-as-multipoint-p*
		       :scatter (not *plot-vc/cc-scatter-as-multipoint-p*) :connect-data-points *plot-vc/cc-scatter-as-multipoint-p*)))))

(defun apparent-reversal-potential (v-rest g-rest slope intercept)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float v-rest g-rest slope intercept))
  (if (zerop g-rest)
      intercept
      (+ v-rest				; intercept
	 (* (/ 1.0 g-rest)		; slope
	    (/ (- v-rest intercept)
	       (- slope (/ 1.0 g-rest)))))))

(defun significant-conductance-modulation (g-value g-rest min-rel-g-for-e-rev)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float g-value g-rest min-rel-g-for-e-rev))
  (not (or (= g-value g-rest)
	   (and (not (zerop g-rest) )
		(if (<= min-rel-g-for-e-rev 0.0)
		    (> g-value g-rest)
		    (< (- (/ g-value g-rest) 1) min-rel-g-for-e-rev))))))

(defun vc/cc-g-wave-linear-regression (data-waves clamp-commands vc-p-list
				       &key (r-electrode 0.0) ; Mohms
				       g-rest ; uS
				       v-rest ; mV
				       derive-g-wave derive-cc-volts-wave derive-e-rev-wave
				       print-v-g-rest (min-rel-g-for-e-rev 0.001) (time-list *sim-reverse-plot-time-list*) delta-t debug)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float min-rel-g-for-e-rev r-electrode))
  (let* ((length-of-waves (length (the cons (car data-waves))))
	 (time-list (if delta-t (list-of-nums length-of-waves 0.0 (float delta-t)) time-list))
	 (clamp-cmds (s-flt-list clamp-commands))
	 (NUMBER-OF-CLAMP-LEVELS (length clamp-cmds))
	 (vc-p-list (if (consp vc-p-list) vc-p-list (loop for cmd in clamp-cmds collect (true-p vc-p-list))))
	 (v-and-g-rest-supplied (and (numberp g-rest) (numberp v-rest)))
	 (g-rest (if v-and-g-rest-supplied (s-flt g-rest) 0.0))
	 (v-rest (if v-and-g-rest-supplied (s-flt v-rest) 0.0))
	 (clamp-datum 0.0)
	 (currents (make-array (list number-of-clamp-levels) :element-type 'double-float))
	 (corrected-voltages (make-array (list number-of-clamp-levels) :element-type 'double-float))
	 g-wave g-wave-time cc-volts-wave g-wave-during-e-rev e-rev-wave e-rev-wave-time)
    (declare (single-float clamp-datum g-rest v-rest) (fixnum NUMBER-OF-CLAMP-LEVELS))
    (loop for count fixnum from 0 to (1- length-of-waves)
	  for time in time-list do
	  (loop for data-wave in data-waves
		for clamp-cmd single-float in clamp-cmds
		for clamp-level-index fixnum from 0
		for vc-p in vc-p-list
		do (setq clamp-datum (the sf (car data-wave)))
		when vc-p do (setf (aref currents clamp-level-index) (d-flt clamp-datum)
				   (aref corrected-voltages clamp-level-index) (d-flt (- clamp-cmd (* (the sf r-electrode) clamp-datum))))
		else do (setf (aref currents clamp-level-index) (d-flt clamp-cmd)
			      (aref corrected-voltages clamp-level-index) (d-flt (- clamp-datum (* (the sf r-electrode) clamp-cmd))))
		collect (cdr data-wave) into new-data-waves
		finally
		(setq data-waves new-data-waves)
		(when debug (format t "currents: ~A,  corrected-voltages ~A, " currents corrected-voltages))
		(multiple-value-bind (slope intercept r slope-annotation)
		    (lin-reg-core currents corrected-voltages :equal-xs nil ; (not vc-p)
				  :equal-ys nil) ; These are *corrected* voltages
		  (declare (single-float slope intercept r) (fixnum slope-annotation))
		  (when debug (format t "slope: ~A, intercept: ~A, r: ~A ~A~%" slope intercept r (lin-reg-slope-annotation-string slope-annotation)))
		  (if (not (or (lin-reg-slope-infinite-p slope-annotation) (lin-reg-slope-ok-p slope-annotation)))
		      (when (and (not v-and-g-rest-supplied) (zerop count))
			(sim-error (format nil "Resting Value for G from initial response cannot be derived from an ~A slope and ~A intercept!" slope intercept)))
		      (let ((g-value (if (lin-reg-slope-infinite-p slope-annotation) 0.0 (/ 1.0 (the sf slope))))) ; slope = 0 => :infinity
			(declare (single-float g-value))
			(when (and (not v-and-g-rest-supplied) (zerop count))
			  (setq g-rest g-value v-rest intercept)
			  (when print-v-g-rest (format t "V-rest: ~AmV, G-rest ~AuS~%" v-rest g-rest)))
			(when derive-g-wave (push g-value g-wave))
			(when derive-cc-volts-wave (push intercept cc-volts-wave))
			(when (or derive-g-wave derive-cc-volts-wave) (push time g-wave-time))
		      
			(when (and derive-e-rev-wave (significant-conductance-modulation g-value g-rest min-rel-g-for-e-rev)) ; Save Erev and associated data?
			  (when debug (format t "I-rev ~A~%" (/ (- slope (/ 1.0 g-rest)) (- v-rest intercept))))
			  (push g-value g-wave-during-e-rev)
			  (push (apparent-reversal-potential v-rest g-rest slope intercept) e-rev-wave)
			  (push time e-rev-wave-time)))))))
    (values (nreverse g-wave)
	    (nreverse g-wave-time)
	    (nreverse g-wave-during-e-rev)
	    (nreverse e-rev-wave)
	    (nreverse e-rev-wave-time)
	    (nreverse cc-volts-wave)
	    g-rest)))

(defun dissect-g-wave-from-gtot-erev (g-wave-during-e-rev g-wave-time e-rev-wave e-rev-wave-time g-rest eex einh &key (FLOAT-EEX t) (FLOAT-EINH t))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((g-rest (s-flt (or g-rest (car g-wave-during-e-rev))))
	(eex (s-flt eex))
	(einh (s-flt einh))
	(gex 0.0)
	(ginh 0.0))
    (declare (single-float gex ginh g-rest eex einh))
    (loop for time single-float in g-wave-time
	  when (and (car e-rev-wave-time) (= time (the sf (car e-rev-wave-time)))) do
	  (let* ((g-tot (car g-wave-during-e-rev))
		 (e-rev (car e-rev-wave))
		 (eex (if (and (> e-rev eex) float-eex) e-rev eex))
		 (einh (if (and (< e-rev einh) float-einh) e-rev einh)))
	    (declare (single-float g-tot e-rev eex einh))
	    (setq ginh (/ (* (- g-tot g-rest) (- e-rev eex))
			  (- einh eex)))
	    (setq gex (- (- g-tot g-rest) ginh))
	    (setq g-wave-during-e-rev (cdr g-wave-during-e-rev)
		  e-rev-wave          (cdr e-rev-wave)
		  e-rev-wave-time     (cdr e-rev-wave-time)))
	  else do (setq ginh 0.0 gex 0.0)
	  collect ginh into ginh-wave
	  collect gex into gex-wave
	  finally (return (values gex-wave ginh-wave)))))

;; For /home/mit-retina/code/lab-analysis.lisp
(defun retina-dissect-g-wave-from-gtot-erev (g-wave g-wave-time e-rev-wave e-rev-wave-time g-rest eex einh &key start-time stop-time (FLOAT-EEX t) (FLOAT-EINH t))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((g-rest (s-flt (or g-rest (car g-wave))))
	(eex (s-flt eex))
	(einh (s-flt einh))
	(gex 0.0)
	(ginh 0.0))
    (declare (single-float gex ginh g-rest eex einh))
    (loop for g-time single-float in g-wave-time
	  for g-tot single-float in g-wave do
	  (if (and (or (not start-time) (<= (the sf start-time) g-time))
		   (or (not stop-time) (<= g-time (the sf stop-time)))
		   (car e-rev-wave-time) (= g-time (the sf (car e-rev-wave-time))))
	    (let* ((e-rev (the sf (car e-rev-wave)))
		   (eex (if (and (> e-rev eex) float-eex) e-rev eex))
		   (einh (if (and (< e-rev einh) float-einh) e-rev einh)))
	      (declare (single-float e-rev eex einh))
	      (setq e-rev-wave (cdr e-rev-wave)
		    e-rev-wave-time (cdr e-rev-wave-time))
	      (setq ginh (/ (* (- g-tot g-rest) (- e-rev eex))
			    (- einh eex)))
	      (setq gex (- (- g-tot g-rest) ginh)))
	    (setq ginh 0.0 gex 0.0))
	  collect g-time into output-time
	  collect g-tot into g-tot-wave
	  collect g-rest into g-rest-wave
	  collect ginh into ginh-wave
	  collect gex into gex-wave
	  finally (return (values gex-wave ginh-wave g-rest-wave g-tot-wave output-time)))))

		 
(defun vclamp (&key (duration *user-stop-time*)
		    (relative-voltage-error 0.01) ; check this - need fairly high accuracy to avoid noisy difference of interpolations 
		    use-loaded-stimulus
		    (step1 *vsource-default-reference-magnitude*) (step1-duration 0.0)
		    (step2 *vsource-default-reference-magnitude*) (step2-duration 0.0)
		    (step3 *vsource-default-reference-magnitude*)  step3-duration
		    (time-resolution 0.1) ; for interpolation
		    (accomodate-overlays t) (overlay-all-plots *overlay-plots*)
		    (create-new-plot-windows *create-new-plot-windows*)
		    interpolate-traces
		    plot-interpolated-currents		    
		    comment)
  (disable-element (isources))
  (setq *plot-isource-currents-p* nil *plot-vsource-currents-p* t *plot-vsource-currents* 'all)
  (let ((*overlay-plots* overlay-all-plots)
	(*save-data-step* 1)
	(*accomodate-overlays* accomodate-overlays)
	(*simulation-plot-window-comment* comment)
	(*create-new-plot-windows* create-new-plot-windows)
	(*user-stop-time* duration)
	(step3-duration (or step3-duration *user-stop-time*))
	(*absolute-voltage-error* relative-voltage-error)
	(vsources (or (vsources) (create-element 'vsource (somas))))
	active-clamp-currents passive-clamp-currents)
    (unless use-loaded-stimulus 
      (loop for vsource in vsources
	    do (pulse-list
		vsource
		(list (list 0.0 step1-duration step1)
		      (list step1-duration (+ step1-duration step2-duration) step2)
		      (list (+ step1-duration step2-duration) (+ step2-duration step3-duration) step3)))))
    (goferit)				; Active simulation
    (setq active-clamp-currents
	  (loop for vsource in vsources
		collect
		(if interpolate-traces
		    (convert-data-time-lists (retrieve-single-data vsource 'current) (current-sim-plot-time-list) time-resolution)
		    (retrieve-single-data vsource 'current))))
    (let (*enable-channels*
	  (*overlay-plots* t)
	  (*use-time-list* t)
	  (*auto-refresh-last-sim-reverse-time-list* t))
      (goferit)				; Passive simulation
      (setq passive-clamp-currents
	    (loop for vsource in vsources
		  collect
		  (if interpolate-traces
		      (convert-data-time-lists (retrieve-single-data vsource 'current) (current-sim-plot-time-list) time-resolution)
		      (retrieve-single-data vsource 'current)))))
		
    (let ((*create-new-plot-windows* create-new-plot-windows)
	  (currents (loop for active-vsource-current in active-clamp-currents
			  for passive-vsource-current in passive-clamp-currents
			  collect (list active-vsource-current passive-vsource-current)))
	  (labels (loop for vsource in (element-name vsources)
			nconc (list (format nil "~A Interpolated Active Current" vsource)
				(format nil "~A Interpolated Passive Current" vsource)))))
      (when plot-interpolated-currents
	(plot-timed-data (car currents) labels (if interpolate-traces time-resolution (current-sim-plot-time-list)) 
			 :y-label "nA" :overlay t :comment *simulation-plot-window-comment* :title "Interpolated Vsource Currents"))
      (loop for vsource in (element-name vsources)
	    for isolated-active-current in (loop for active-vsource-current in active-clamp-currents
						 for passive-vsource-current in passive-clamp-currents
						 collect (mapcar '- active-vsource-current passive-vsource-current))
	    do 
	    (plot-timed-data (list isolated-active-current) (list (format nil "~A Isolated Active Current" vsource))
			     (if interpolate-traces time-resolution (current-sim-plot-time-list))
			     :overlay t :y-label "nA" :title "Isolated Active Vsource Currents" :comment *simulation-plot-window-comment*)))))

(defun f/I (&key (duration *user-stop-time*) step stimulus-min stimulus-max (stimulus-start 0.0) (stimulus-stop DURATION)
	      (isource *isource*) stimulus-function (intervals 5) include-ss 
	      (spike-elt *soma*) (spike-threshold -20.0) (supra-threshold-duration-min 0.1)
	      (plot-fi t) plot-gains plot-rasters scatter return-values
	      steps-to-plot stimulus-units title comment)
  "Classical spike frequency vs stimulus protocol.
Trial DURATION [default *USER-STOP-TIME*], STIMULUS-START [default 0ms] and STIMULUS-STOP [default DURATION] are in ms. Stimulus magnitudes given by STEP if a list, otherwise from
STIMULUS-MIN to STIMULUS-MAX, incremented by STEP. Spikes detected by SPIKE-TIMES (see definition for parameters) on SPIKE-ELT [default *SOMA*].

A non-NIL STIMULUS-FUNCTION [default NIL] is called to setup up the simulation conditions before each trial, as follows, where stim-mag is determined as above:

  (funcall STIMULUS-FUNCTION stim-mag STIMULUS-START STIMULUS-STOP)

Otherwise a current step stimulus is used, in nA, with ISOURCE [default *ISOURCE*; if NIL a temporary current source is added to SPIKE-ELT]: 

  (pulse-list ISOURCE (list STIMULUS-START STIMULUS-STOP stim-mag))

Plot windows can include the standard simulation plots as setup prior to calling F/I, for every trial or when the stimulus magnitude is in STEPS-TO-PLOT [default NIL]. Instantaneous gain in
Hz/Stimulus Magnitude are plotted when PLOT-GAINS is T. If plotting is enabled, then all traces will go to a single plot when INDIVIDUAL-PLOTS is NIL [default], otherwise each plot will go to a
new window. STIMULUS-UNITS [if NIL, will be set to \"nA\" for current stimulation] is for plot annotation.

Spike frequencies are the instantaneous frequencies for the first INTERVALS [default 5] and the last spike interval (e.g. steady state), when INCLUDE-SS is T [default NIL]. 

When RETURN-VALUES is T [default NIL], f/I returns (values SPKS-STIMS NTH-INT-FQ-STIMS NTH-INT-GAIN-STIM-MIDPOINTS):

      SPKS-STIMS, Spike times and associated stimuli
       -> '(((spike_time spike_time...) stimulus_1)
            ((spike_time spike_time...) stimulus_2)
            ...)
      NTH-INT-FQ-STIMS, Instantaneous frequencies in Hz for the specified intervals, and associated stimuli
       -> (((freq_1 freq_2 ...)(stimulus_1 stimulus_2  ...))  <- steady-state interval (if INCLUDE-SS)
           ((freq_1 freq_2 ...)(stimulus_1 stimulus_2  ...))   <- 1st interval
           ...               
           ((freq_1 freq_2 ...)(stimulus_1 stimulus_2  ...)))  <- INTERVALS [default 5th] interval
      NTH-INT-GAIN-STIM-MIDPOINTS, Gains in Hz/Stimulus Magnitude for the specified intervals, and associated stimulus midpoints
       -> (((gain_1 gain_2 ...)(stim_midpoint_1 sti_midpoint_2  ...))  <- steady-state interval (if INCLUDE-SS)
           ((gain_1 gain_2 ...)(stim_midpoint_1 sti_midpoint_2  ...))  <- 1st interval
           ...               
          ((gain_1 gain_2 ...)(stim_midpoint_1 sti_midpoint_2  ...)))  <- INTERVALS [default 5th] interval
"
  (unless (or (consp step) (and step stimulus-min stimulus-max)) (sim-error "f/I needs full stimulus specs!!"))
  (let* ((*user-stop-time* duration) (*kill-extra-messages* t)
	 (stim-steps (if (consp step) step (list-of-nums-start-end :start stimulus-min :end stimulus-max :increment step)))
	 (max-stimulus (max-of-list stim-steps))
	 (temp-isource (Unless (or stimulus-function isource) (setq isource (add-isource spike-elt))))
	 (stim-units (or stimulus-units (if (and (not STIMULUS-FUNCTION) (isource-p isource)) "nA" ""))))
    (enable-element-plot spike-elt)
    (loop for stim-mag in stim-steps do
	 (if stimulus-function (funcall STIMULUS-FUNCTION stim-mag STIMULUS-START STIMULUS-STOP) (pulse-list isource (list stimulus-start stimulus-stop stim-mag)))
	 (let* ((*plot-standard-windows* (if (consp steps-to-plot) (member stim-mag steps-to-plot) *plot-standard-windows*))
		(*simulation-plot-window-comment* (concatenate 'string *simulation-plot-window-comment* (format nil " ~,2e ~A step" stim-mag stim-units))))
	   (goferit))
       collect (list (spike-times spike-elt :spike-threshold spike-threshold :supra-threshold-duration-min supra-threshold-duration-min) stim-mag) into spks-stims
       finally
	 (erase-element temp-isource)
	 (macrolet ((push-int-and-stim (spks-stim interval dest)
		      `(when (or (numberp ,interval) (>= (length (car ,spks-stim)) 2))
			 (let ((interval-start (if (zerop interval) ; Steady state
						   (when INCLUDE-SS (car (last (car ,spks-stim) 2)))
						   (nth (1- ,interval) (car ,spks-stim))))
			       (interval-end (if (zerop interval) ; Steady state
						 (when INCLUDE-SS (car (last (car ,spks-stim))))
						 (nth ,interval (car ,spks-stim))))
			       (stim-mag (cadr ,spks-stim)))
			   (when (and interval-start interval-end) (push (list (- interval-end interval-start) stim-mag) ,dest))))))  ; Interval in ms, stimulus mag
	   (let ((out-list-array (make-array (1+ intervals) :initial-element nil)))
	     (loop for spks-stim in (reverse spks-stims) do ; Collect steady state and the intervals.
		  (loop for interval from 0 to intervals do (push-int-and-stim spks-stim interval (aref out-list-array interval)))
		finally
		  (let* ((int-stims-lists (mapcar #'(lambda (int-stims) (remove-if #'(lambda (int-stim) (zerop (car int-stim))) int-stims)) (no-nils (array-to-list out-list-array)))) ;; No 0 intervals
			 (plot-labels (mapcar #'(lambda (val) (format nil "~@(~:R~)" val)) (list-of-nums intervals 1 1))))
		    (when INCLUDE-SS (setq plot-labels (cons "Steady State" plot-labels)))
		    (when plot-rasters (raster-plots :event-data-lists (mapcar 'car spks-stims)
						    :event-element-labels (mapcar #'(lambda (stim) (format nil "~,2e" stim)) (mapcar 'cadr spks-stims))
						    :raster-source-label stim-units :title (or title (format nil "Raster Characteristic: ~A" *circuit*)))) 
		    (loop for nth-int-stims in int-stims-lists with intervals with stims
		       do (setq intervals (mapcar 'car nth-int-stims)
				stims (mapcar 'cadr nth-int-stims))
		       when (and intervals stims)
		       collect (list (differentiate-wave (mapcar #'(lambda (interval) (/ 1000.0 interval)) intervals) stims) (midpoints stims)) into nth-int-gain-stim-midpoints
		       and collect (list (mapcar #'(lambda (interval) (/ 1000.0 interval)) intervals) stims) into NTH-INT-FQ-STIMS
		       finally 
			 (when plot-fi (plot-xy-data NTH-INT-FQ-STIMS plot-labels :transpose-data t :x-label (format nil "Stimulus [~A]" stim-units) :y-label "[Hz]"
						     :x-min 0.0 :y-min 0.0 :x-max max-stimulus :y-label-v-position :two-thirds-up :y-are-fns t :width 400 :height 400 :scatter scatter
						     :upper-right-comment comment :title (or title (format nil "f/I Characteristic: ~A" *circuit*))))
			 (when plot-gains (plot-xy-data nth-int-gain-stim-midpoints plot-labels
							:x-label (format nil "Stimulus [~A]" stim-units) :y-label (format nil "Gain~%[Hz/~A]" stim-units)
							:x-min 0.0 :x-max max-stimulus :y-min 0.0 :y-origin 0 :y-label-v-position :two-thirds-up :y-are-fns t :transpose-data t
							:x-symbol-width 10 :y-symbol-width 10 :scatter  scatter :width 400 :height 400
							:upper-right-comment comment :title (format nil "g/I Characteristic: ~A" (or title *circuit*))))
			 (when return-values (return-from f/I (values spks-stims  NTH-INT-FQ-STIMS nth-int-gain-stim-midpoints)))))))))))

(defun fi-compare (labels-and-conditions &key (stimulus-min 0.1) (stimulus-max 0.6) (step 0.01) (duration 100) (intervals 5) (kill-all-output *kill-all-output*) restore-condition)
  "Run multiple condition F/I.
LABELS-AND-CONDITIONS is a list of conditions and associated labels:

     '((CONDITION-1-LABEL CONDITION-1-DEFINITION) ... (CONDITION-N-LABEL CONDITION-N-DEFINITION))

where each CONDITION-x-DEFINITION is a form or list of forms that are called prior to each F/I protocol. RESTORE-CONDITION is a form or forms that is called at the end of the protocol.
Remaining args from F/I."
  (let ((*kill-all-output* kill-all-output)
	(frequency-data '())
	(gain-data '())
	(all-labels '()))
    (loop for label-and-condition in labels-and-conditions do
	 (funcalls (nth 1 label-and-condition))
	 (MULTIPLE-VALUE-BIND (SPKS-STIMS NTH-INT-FQ-STIMS NTH-INT-GAIN-STIM-MIDPOINTS)
	     (F/I :STEP step :stimulus-min stimulus-min :STIMULUS-MAX STIMULUS-MAX :intervals intervals :DURATION DURATION :plot-fi nil :RETURN-VALUES T)
	   (loop for interval-index from 0 to (1- intervals) 
	      do (push (nth interval-index NTH-INT-FQ-STIMS) frequency-data)
		(push (nth interval-index NTH-INT-GAIN-STIM-MIDPOINTS) gain-data)
		(push (format nil "~A: Interval ~A" (nth 0 label-and-condition) (1+ interval-index)) all-labels)))
       finally
	 (PLOT-xy-data frequency-data all-labels :x-label "nA" :y-label "Hz" :x-min 0 :x-max stimulus-max :y-are-fns t :transpose-data t :scatter nil :width 500 :height 500  :title "f/I")
	 (plot-xy-data gain-data all-labels :x-label "nA" :y-label "Gain (Hz/nA)" :x-min 0 :x-max stimulus-max :y-are-fns t :transpose-data t :scatter nil :width 500 :height 500 :title "Gain")))
  (funcalls restore-condition))


(defun vc-iv (&key (vsource-elt *soma*) (duration 1000) (v-hold-min -90) (v-hold-max 20) (v-hold-step 2) return-traces plot-iv (target-time :end))
  "Voltage clamp IV of loaded cell with with voltage source at VSOURCE-ELT [default *SOMA*].
Trial DURATION [default 1000] in milliseconds is normally set to reach steady-state. Trial voltage steps defined by V-HOLD-MIN [default -90],
V-HOLD-MAX [default 20], and V-HOLD-STEP [default 2], in mV. Removes any existing sources. Current data is taken at TARGET-TIME [default :end]."  
  (erase-element (sources))
  (let ((*integration-time-reference* :variable)
	(data-time (case target-time
		     (:end duration)
		     (t target-time)))
	(*overlay-plots* nil)
	(*accomodate-overlays* t)
	(iv-vsource (add-vsource vsource-elt)))
    (enable-element-plot vsource-elt)
    (enable-element-plot iv-vsource)
    (loop for stimulus from v-hold-min to v-hold-max by v-hold-step
       do
	 (vsource-reference-magnitude iv-vsource stimulus)
	 (goferit duration)
	 (setq *overlay-plots* t)
       collect (element-value iv-vsource :target-time data-time) into currents
       collect (element-value vsource-elt :target-time data-time) into voltages
       finally
	 (when plot-iv (plot-xy-data (list voltages currents) nil :y-label "nA" :x-label "mV" :width 800 :height 500 :scatter t :title "IV-CURVE" :draw-grid t))
	 (erase-element iv-vsource)
	 (when return-traces (return (values voltages currents))))))

(defun vc-iv-compare (labels-and-conditions &key (v-hold-min -90) (v-hold-max 20) (v-hold-step 2) (duration 100) (target-time :end) kill-all-output restore-condition)
  "Run multiple condition VC-IV.
LABELS-AND-CONDITIONS is a list of conditions and associated labels:

     '((CONDITION-1-LABEL CONDITION-1-DEFINITION) ... (CONDITION-N-LABEL CONDITION-N-DEFINITION))

where each CONDITION-x-DEFINITION is a form or list of forms that are called prior to each VC-IV protocol. RESTORE-CONDITION is a form or forms that is called at the end of the protocol.
Remaining args from VC-IV."
  (let ((*kill-all-output* kill-all-output)
	(vc-waves '())
	(condition-labels '()))
    (loop for label-and-condition in labels-and-conditions do
	 (funcalls (nth 1 label-and-condition))
	 (multiple-value-bind (voltages currents)
	     (vc-iv :v-hold-min v-hold-min :v-hold-max v-hold-max :v-hold-step v-hold-step :duration duration :target-time target-time :return-traces t)
	   (push (list voltages currents) vc-waves)
	   (push (nth 0 label-and-condition) condition-labels))
       finally (plot-xy-data vc-waves condition-labels :y-label "nA" :x-label "mV" :y-min -1 :y-max 2 :x-min -90 :width 800 :height 500 :scatter t :title "IV-CURVE" :draw-grid t))
    (funcalls restore-condition)))

(defun clamp-steps (&key start-clamp stop-clamp step (duration *user-stop-time*)
		      (source (or *isource* *vsource*))
		      (clamp-start-time 10.0) (clamp-stop-time (* 0.9 duration)) ; milliseconds
		      (holding-potential (vsource-reference-magnitude source)) ; mV
		      individual-plots lock-plots (show-plot-windows t)
		      comment include-comment (extra-comment "") 
		      timeit return-source-data)
  "Run a series of clamp simulations where SOURCE [default \(or *ISOURCE* *VSOURCE*\)].
Trial DURATION [default *USER-STOP-TIME*] in milliseconds. Clamp steps taken from STEP, if it is a list, otherwise ranging from START-CLAMP to
STOP-CLAMP by STEP [nA or mV, as appropriate, relative to the source's reference magnitude]. Each clamp step begins at CLAMP-START-TIME [default
10ms] and ends at CLAMP-STOP-TIME [default \(* 0.9 DURATION\)], both in milliseconds. 

When INDIVIDUAL-PLOTS is nil, otherwise output is overlaid on a single plot. Plot window\(s\) can have the string EXTRA-COMMENT added, and they will
be locked if LOCK-PLOTS is set. A non-nil value of INCLUDE-COMMENT adds a comment describing the clamp source values to the plot output, unless a
non-nil string COMMENT which will overrule all other comments. SHOW-PLOT-WINDOWS enables plot window updating and refreshing after each simulation -
for dense plots, it is more efficient to do this only at the end of the runs. When run with a voltage SOURCE, the function
INIT-WITH-STEADY-STATE-LINEAR-VOLTAGE-CLAMP is called for every step, using HOLDING-POTENTIAL [mV, default given by the VSOURCE-REFERENCE-MAGNITUDE
of the voltage SOURCE]. When RETURN-SOURCE-DATA the function returns as values the source data, the source cell-element voltage, and the time for each step."
  (let* ((steps (when (consp step) step))
	 (source (element source))
	 (vclamp-p (vsource-p source))
	 (*CREATE-NEW-SIMULATION-PLOTS* (or lock-plots *CREATE-NEW-SIMULATION-PLOTS*))
	 (current-output-windows *output-windows*))
    (when source
      (when return-source-data
	(enable-element-plot source)
	(enable-element-plot (element-cell-element source)))
      (loop for clamp in (or steps (loop for clamp from start-clamp to stop-clamp by step collect clamp))
	 for count from 0 do
	   (pulse-list source (list (list clamp-start-time clamp-stop-time clamp)))
	   (when vclamp-p (init-with-steady-state-linear-voltage-clamp source holding-potential))
	   (let ((*CREATE-NEW-SIMULATION-PLOTS* individual-plots)
		 (*resurrect-plots* show-plot-windows)
		 (*update-plots* show-plot-windows)
		 (*ACCOMODATE-OVERLAYS* t)
		 (*overlay-plots* (and (> count 0) (not individual-plots)))
		 (*simulation-plot-window-comment*
		  (concatenate 'string
			       *simulation-plot-window-comment*
			       (when (and (> (length *simulation-plot-window-comment*) 0) include-comment)
				 (format nil "~%"))
			       (when include-comment
				 (if comment
				     (format nil "~A" comment)
				     (if individual-plots
					 (format nil "~,2f~A step ~A" clamp (source-units-string source) extra-comment)
					 (if steps
					     (format nil "Steps ~,2f to ~,2f~A ~A" (first steps) (car (last steps)) (source-units-string source) extra-comment)
					     (format nil "~,2f to ~,2f~A steps ~A" start-clamp stop-clamp (source-units-string source) extra-comment))))))))
	     (if timeit (gotimed) (goferit)))
	 when return-source-data collect *sim-plot-time-list* into time
	 and collect (element-data source) into source-data
	 and collect (element-data (element-cell-element source)) into source-cell-element-data
	 finally
	   (when lock-plots (lock-new-windows current-output-windows))
	   (when return-source-data (return (values source-data source-cell-element-data time)))))))

(defun find-holding-current (recording-location holding-potential &key (simulation-duration 1000))
  "Returns the assumed steady-state current necessary for maintaining a membrane voltage of HOLDING-POTENTIAL [mV] at the cell element associated with
RECORDING-LOCATION. Any existing voltage sources at RECORDING-LOCATION are removed. A voltage clamp simulation is made with the voltage source at
RECORDING-LOCATION for a duration of SIMULATION-DURATION [ms, default 1000]. The final current [nA] of the voltage source is then returned, and the
voltage source is removed." 
  (let* ((*user-stop-time* simulation-duration)	; Long enough to find a steady-state
	 (recording-location (cell-element recording-location)))
    (erase-elements (vsources recording-location))
    (let ((vsource (add-vsource recording-location :reference-magnitude holding-potential)))
      (enable-element-plot vsource)
      (enable-element-plot recording-location)
      (goferit)
      (let ((steady-state-clamp-current (car (last (element-data *vsource*)))))
	;; (printvars steady-state-clamp-current)
	(erase-elements 'vsource 'isource) ; Just to clean things up
	steady-state-clamp-current))))

(defun setup-constant-current-for-holding-potential (recording-location holding-potential
						     &key
						     (set-and-use-node-voltage-initialization t)
						     (simulation-duration 1000))
  "Add a constant current source at the cell element associated with RECORDING-LOCATION, sufficient to establish a steady-state HOLDING-POTENTIAL
[mV]. Calls FIND-HOLDING-CURRENT to determine the current with the parameter SIMULATION-DURATION [ms, default 1000]. When
SET-AND-USE-NODE-VOLTAGE-INITIALIZATION is T [default], all the node voltages are saved at the end of the FIND-HOLDING-CURRENT simulation by calling
SET-*NODE-VOLTAGE-INITIALIZATIONS*, and the variable *USE-NODE-VOLTAGE-INITIALIZATIONS* is set T." 
  (add-constant-current recording-location (find-holding-current recording-location holding-potential :simulation-duration simulation-duration))
  (when set-and-use-node-voltage-initialization
    (set-*node-voltage-initializations*))
  (setq *USE-NODE-VOLTAGE-INITIALIZATIONS* set-and-use-node-voltage-initialization))
  

;; Check for true linear dep of cap current, vis-a-vis finite dV/dt of vsources!!
(defun linear-corrected-vclp (&key
			      new-plots
			      (vsource *vsource*)
			      (step-start-amplitude (or (element-parameter vsource 'step-start-amplitude) 10.0))
			      (step-start (or (element-parameter vsource 'step-start) 0.0))
			      (step-duration (or (element-parameter vsource 'step-duration) 50.0))
			      step-base-potential
			      (disable-synapses-for-control t)
			      (simulation-duration *user-stop-time*)
			      (min-time-for-peak-valley (or (element-parameter vsource 'min-time-for-peak-valley) 0.0))
			      (max-time-for-peak-valley (or (element-parameter vsource 'max-time-for-peak-valley) *user-stop-time*))
			      (reference-magnitude (or (element-parameter vsource 'reference-magnitude) 0.0)) 
			      (total-steps (or (element-parameter vsource 'total-steps) 1))
			      (initial-step-integer-coefficient (or (element-parameter vsource 'initial-step-integer-coefficient) 0))
			      base-pulse-list ; reference-original-pulse-list
			      (relative-voltage-error *absolute-voltage-error*) ; 0.01 is ok
			      INCLUDE-local-CAP-CURRENT-IN-VSOURCE
			      (simulation-print-detail :none)
			      (plot-peaks-and-valleys (element-parameter vsource 'plot-peaks-and-valleys))
			      (plot-times-for-peaks-and-valleys (element-parameter vsource 'plot-times-for-peaks-and-valleys))
			      (delta-t 0.05)
			      (pulse-transition-time (or (element-parameter vsource 'pulse-transition-time) 0.1))
			      use-menu test-linearity)
  (when vsource
    (setq *user-stop-time* (float simulation-duration) *plot-vsource-currents-p* t)
    (when base-pulse-list (element-parameter vsource 'pulse-list base-pulse-list))
    (let* ((original-pulse-list (element-parameter vsource 'pulse-list))
	   (dummy1 step-start-amplitude)
	   (dummy2 step-start)
	   (dummy3 step-duration)
	   (dummy4 total-steps)
	   (dummy5 initial-step-integer-coefficient)
	   (dummy6 reference-magnitude)
	   (dummy7 relative-voltage-error)
	   (dummy8 plot-peaks-and-valleys)
	   (dummy9 delta-t)
	   (dummy10 *user-stop-time*)
	   (dummy11 pulse-transition-time)
	   (dummy12 test-linearity)
	   dummy13 dummy14
	   (dummy15 min-time-for-peak-valley)
	   (dummy16 max-time-for-peak-valley)
	   (dummy17 new-plots)
	   dummy18
	   (dummy19 plot-times-for-peaks-and-valleys)
					; (dummy29 reference-original-pulse-list)
	   )
      (when use-menu
	(choose-variable-values
	 '((dummy14 "Edit voltage source / base clamp pulse sequence stimulus" :boolean)
	   (dummy20 "Add current vsource pulse sequence to protocol" :boolean)
	   (dummy1 "Step start amplitude [mV]" :float)
	   (dummy2 "Step start time [ms]" :float)
	   (dummy3 "Step duration [ms]" :float)
	   (dummy4 "Total number of steps" :integer)
	   (dummy5 "Initial step coefficient [integer]" :integer)
	   (dummy6 "Reference holding potential" :float)
	   (dummy7 "Relative voltage error [mV]" :float)
	   (dummy8 "Plot peaks and valleys" :boolean)
	   (dummy19 "Plot times for peaks and valleys" :boolean)
	   (dummy15 "Min time for peaks and valleys" :float)
	   (dummy16 "Max time for peaks and valleys" :float)
	   (dummy9 "Delta-t for non-linear current caculation [ms]" :float)
	   (dummy10 "Duration [ms]" :float)
	   (dummy11 "Clamp pulse transition time [ms]" :float)
	   (dummy12 "Test linearity" :boolean)
	   (dummy17 "Make new plots" :boolean)
	   (dummy18 "If not new plots, overlay on existing plots" :boolean)
	   (dummy13 "Cancel" :boolean))
	 :label "Compensated Voltage Clamp Sequence")
	(when dummy14
	  (edit-element vsource)
	  (when (element-parameter vsource 'pulse-list)
	    (setq base-pulse-list (element-parameter vsource 'pulse-list)
		  original-pulse-list (element-parameter vsource 'pulse-list))))
	(setq min-time-for-peak-valley dummy15
	      max-time-for-peak-valley dummy16
	      new-plots dummy17
	      step-start-amplitude dummy1
	      step-start dummy2 
	      step-duration dummy3 
	      total-steps dummy4 
	      initial-step-integer-coefficient dummy5 
	      reference-magnitude dummy6 
	      relative-voltage-error dummy7 
	      plot-peaks-and-valleys dummy8
	      plot-times-for-peaks-and-valleys dummy19
	      delta-t dummy9 
	      *user-stop-time* dummy10 
	      pulse-transition-time dummy11 
	      test-linearity dummy12))
      (unless dummy13
	(let* (				; (*use-fixed-step* nil)
	       (*save-data-step* 1)
	       (*simulation-print-detail* simulation-print-detail)
	       (base-pulse-list (or base-pulse-list
				    (and dummy20 (element-parameter vsource 'pulse-list))
				    ; (list (list 0.0 simulation-duration reference-magnitude))
				    ))
	       (*vsource-default-reference-magnitude* (float reference-magnitude))
	       (step-base-potential (or step-base-potential reference-magnitude))
	       (*absolute-voltage-error* relative-voltage-error)
	       (*vsource-intrinsic-current* t)
	       (delta-t (float delta-t))
	       (*INCLUDE-local-CAP-CURRENT-IN-VSOURCE* INCLUDE-local-CAP-CURRENT-IN-VSOURCE)
	       ref-vsource-pulse-current ref-vsource-zero-input-current)

	  (element-parameter vsource 'pulse-transition-time pulse-transition-time)
	  (element-parameter vsource 'pulse-transition-constraint :fixed-transition-time)  
	  (enable-element-plot vsource 'current)

	  (let ((*enable-synapses* (not disable-synapses-for-control))
		*enable-channels*)
	    (pulse-list vsource (cons (list step-start (+ step-start step-duration) step-base-potential) base-pulse-list))
	    (let (*plot-standard-windows*) (GOFERIT))
	    (setq ref-vsource-zero-input-current (element-data-dted vsource delta-t))
	    (pulse-list vsource (cons (list step-start (+ step-start step-duration) (+ step-base-potential step-start-amplitude)) base-pulse-list))
	    (let (*plot-standard-windows*) (GOFERIT))
	    (setq ref-vsource-pulse-current (mapcar '- (element-data-dted vsource delta-t) ref-vsource-zero-input-current))
	    (let ((*CREATE-NEW-SIMULATION-PLOTS* new-plots) (*overlay-plots* dummy18) (*ACCOMODATE-OVERLAYS* t)
		  (*enable-channels* (not test-linearity)) 
		  (dted-list (list-of-nums (/ *user-stop-time* delta-t) 0.0 delta-t))
		  adjusted-current max min max-time min-time)
	      (loop for step-integer-coefficient from initial-step-integer-coefficient
		    for count from 1 to total-steps
		    do (pulse-list vsource
				   (cons (list step-start
					       (+ step-start step-duration)
					       (+ step-base-potential (* step-integer-coefficient step-start-amplitude)))
					 base-pulse-list))
		    (GOFERIT)
		    (setq *overlay-plots* t
			  max nil
			  min nil
			  adjusted-current (adjust-current (element-data-dted vsource delta-t) ref-vsource-pulse-current
							   ref-vsource-zero-input-current step-integer-coefficient delta-t))
		    collect adjusted-current into currents

		    do (loop for val in adjusted-current
			     for time in dted-list
			     when (<= min-time-for-peak-valley time max-time-for-peak-valley)
			     do (when (or (not max) (> val max)) (setq max val max-time time)))
		    collect max into maxs
		    collect max-time into max-times

		    do (loop for val in adjusted-current
			     for time in dted-list
			     when (<= min-time-for-peak-valley time max-time-for-peak-valley)
			     do (when (or (not min) (< val min)) (setq min val min-time time)))
		    collect min into mins
		    collect min-time into min-times

		    collect (+ step-base-potential (* step-integer-coefficient step-start-amplitude)) into steps
		    collect (format nil "~A mV step" (+ step-base-potential (* step-integer-coefficient step-start-amplitude)))
		    into labels
		    finally
		    (let ((*overlay-plots* dummy18))
		      (plot-timed-data currents labels delta-t :y-label "nA" :x-max *user-stop-time* :title (format nil "~A: Voltage Clamp Sequence" *simulation-name*))
		      (when plot-times-for-peaks-and-valleys
			(plot-timed-data (list max-times min-times) (list "Peak Times" "Valley Times") steps
					 :y-label "ms" :x-label "Command Voltage (mV)"
					 :y-min 0.0 :title (format nil "~A - Times for Peaks and Valleys" *simulation-name*)))
		      (when plot-peaks-and-valleys
			(plot-timed-data (list maxs mins) (list "Peaks" "Valleys") steps
					 :y-label "nA" :x-label "Command Voltage (mV)" ; :x-max (car steps) :x-inc step-value
					 :title (format nil "~A - Peaks and Valleys" *simulation-name*))))))))
	(mapcar (lambda (key-value) (element-parameter vsource (car key-value) (cadr key-value)))
		`((pulse-list ,original-pulse-list)
		  (min-time-for-peak-valley ,min-time-for-peak-valley)
		  (max-time-for-peak-valley ,max-time-for-peak-valley)
		  (plot-peaks-and-valleys ,plot-peaks-and-valleys)
		  (plot-times-for-peaks-and-valleys ,plot-times-for-peaks-and-valleys)
		  (step-start-amplitude ,step-start-amplitude)
		  (step-start ,step-start)
		  (step-duration ,step-duration)
		  (reference-magnitude ,reference-magnitude)
		  (total-steps ,total-steps)
		  (initial-step-integer-coefficient ,initial-step-integer-coefficient))))))
  nil)


#|
(defun linear-corrected-vclp (&key
			      new-plots
			      (vsource *vsource*)
			      (step-start-amplitude (or (element-parameter vsource 'step-start-amplitude) 10.0))
			      (step-start (or (element-parameter vsource 'step-start) 0.0))
			      (step-duration (or (element-parameter vsource 'step-duration) 50.0))
			      step-base-potential
			      (disable-synapses-for-control t)
			      (simulation-duration *user-stop-time*)
			      (min-time-for-peak-valley (or (element-parameter vsource 'min-time-for-peak-valley) 0.0))
			      (max-time-for-peak-valley (or (element-parameter vsource 'max-time-for-peak-valley) *user-stop-time*))
			      (holding-potential (or (element-parameter vsource 'holding-potential) 0.0)) 
			      (total-steps (or (element-parameter vsource 'total-steps) 1))
			      (initial-step-integer-coefficient (or (element-parameter vsource 'initial-step-integer-coefficient) 0))
			      
			      base-pulse-list ; reference-original-pulse-list
			      (relative-voltage-error *absolute-voltage-error*) ; 0.01 is ok
			      INCLUDE-local-CAP-CURRENT-IN-VSOURCE
			      (simulation-print-detail :none)
			      (plot-peaks-and-valleys (element-parameter vsource 'plot-peaks-and-valleys))
			      (plot-times-for-peaks-and-valleys (element-parameter vsource 'plot-times-for-peaks-and-valleys))
			      (delta-t 0.05)
			      (pulse-transition-time (or (element-parameter vsource 'pulse-transition-time) 0.1))
			      use-menu test-linearity)
  (when vsource
    (setq *user-stop-time* (float simulation-duration) *plot-vsource-currents-p* t)
    (element-parameter vsource 'set-default-to-start-of-pulses t)
    (when base-pulse-list (element-parameter vsource 'pulse-list base-pulse-list))
    (let* ((original-pulse-list (element-parameter vsource 'pulse-list))
	   dummy13 dummy14
	   dummy18)
      (when use-menu
	(wh::choose-variable-menu
	 '((dummy14 "Edit voltage source / base clamp pulse sequence stimulus" :boolean)
	   (dummy20 "Add current vsource pulse sequence to protocol" :boolean)
	   (step-start-amplitude "Step start amplitude [mV]" :float)
	   (step-start "Step start time [ms]" :float)
	   (step-duration "Step duration [ms]" :float)
	   (total-steps "Total number of steps" :integer)
	   (initial-step-integer-coefficient "Initial step coefficient [integer]" :integer)
	   (holding-potential "Holding potential" :float)
	   (relative-voltage-error "Relative voltage error [mV]" :float)
	   (plot-peaks-and-valleys "Plot peaks and valleys" :boolean)
	   (plot-times-for-peaks-and-valleys "Plot times for peaks and valleys" :boolean)
	   (min-time-for-peak-valley "Min time for peaks and valleys" :float)
	   (max-time-for-peak-valley "Max time for peaks and valleys" :float)
	   (delta-t "Delta-t for non-linear current caculation [ms]" :float)
	   (*user-stop-time* :float)
	   (pulse-transition-time "Clamp pulse transition time [ms]" :float)
	   (test-linearity "Test linearity" :boolean)
	   (new-plots "Make new plots" :boolean)
	   (dummy18 "If not new plots, overlay on existing plots" :boolean)
	   (dummy13 "Cancel" :boolean))
	 :label "Compensated Voltage Clamp Sequence")
	(when dummy14
	  (edit-element vsource)
	  (when (element-parameter vsource 'pulse-list)
	    (setq base-pulse-list (element-parameter vsource 'pulse-list)
		  original-pulse-list (element-parameter vsource 'pulse-list))))
	)
      (unless dummy13
	(let* (				; (*use-fixed-step* nil)
	       (*save-data-step* 1)
	       (*simulation-print-detail* simulation-print-detail)
	       (base-pulse-list (or base-pulse-list
				    (and dummy20 (element-parameter vsource 'pulse-list))
				    (list (list 0.0 simulation-duration holding-potential))))
	       (*vsource-default-reference-magnitude* (float holding-potential))
	       (step-base-potential (or step-base-potential holding-potential))
	       (*absolute-voltage-error* relative-voltage-error)
	       (*vsource-intrinsic-current* t)
	       (delta-t (float delta-t))
	       (*INCLUDE-local-CAP-CURRENT-IN-VSOURCE* INCLUDE-local-CAP-CURRENT-IN-VSOURCE)
	       ref-vsource-pulse-current ref-vsource-zero-input-current)

	  (element-parameter vsource 'pulse-transition-time pulse-transition-time)
	  (element-parameter vsource 'pulse-transition-constraint :fixed-transition-time)  
	  (enable-element-plot vsource 'current)

	  (let ((*enable-synapses* (not disable-synapses-for-control))
		*enable-channels*)
	    (pulse-list vsource (cons (list step-start (+ step-start step-duration) step-base-potential) base-pulse-list))
	    (let (*plot-standard-windows*) (GOFERIT))
	    (setq ref-vsource-zero-input-current (element-data-dted vsource delta-t))
	    (pulse-list vsource (cons (list step-start (+ step-start step-duration) (+ step-base-potential step-start-amplitude)) base-pulse-list))
	    (let (*plot-standard-windows*) (GOFERIT))
	    (setq ref-vsource-pulse-current (mapcar '- (element-data-dted vsource delta-t) ref-vsource-zero-input-current))
	    (let ((*CREATE-NEW-SIMULATION-PLOTS* new-plots) (*overlay-plots* dummy18) (*ACCOMODATE-OVERLAYS* t)
		  (*enable-channels* (not test-linearity)) 
		  (dted-list (list-of-nums (/ *user-stop-time* delta-t) 0.0 delta-t))
		  adjusted-current max min max-time min-time)
	      (loop for step-integer-coefficient from initial-step-integer-coefficient
		    for count from 1 to total-steps
		    do (pulse-list vsource
				   (cons (list step-start
					       (+ step-start step-duration)
					       (+ step-base-potential (* step-integer-coefficient step-start-amplitude)))
					 base-pulse-list))
		    (GOFERIT)
		    (setq *overlay-plots* t
			  max nil
			  min nil
			  adjusted-current (adjust-current (element-data-dted vsource delta-t) ref-vsource-pulse-current
							   ref-vsource-zero-input-current step-integer-coefficient delta-t))
		    collect adjusted-current into currents

		    do (loop for val in adjusted-current
			     for time in dted-list
			     when (<= min-time-for-peak-valley time max-time-for-peak-valley)
			     do (when (or (not max) (> val max)) (setq max val max-time time)))
		    collect max into maxs
		    collect max-time into max-times

		    do (loop for val in adjusted-current
			     for time in dted-list
			     when (<= min-time-for-peak-valley time max-time-for-peak-valley)
			     do (when (or (not min) (< val min)) (setq min val min-time time)))
		    collect min into mins
		    collect min-time into min-times

		    collect (+ step-base-potential (* step-integer-coefficient step-start-amplitude)) into steps
		    collect (format nil "~A mV step" (+ step-base-potential (* step-integer-coefficient step-start-amplitude)))
		    into labels
		    finally
		    (let ((*overlay-plots* dummy18))
		      (plot-timed-data currents labels delta-t :y-label "nA" :x-max *user-stop-time* :title (format nil "~A: Voltage Clamp Sequence" *simulation-name*))
		      (when plot-times-for-peaks-and-valleys
			(plot-timed-data (list max-times min-times) (list "Peak Times" "Valley Times") steps
					 :y-label "ms" :x-label "Command Voltage (mV)"
					 :y-min 0.0 :title (format nil "~A - Times for Peaks and Valleys" *simulation-name*)))
		      (when plot-peaks-and-valleys
			(plot-timed-data (list maxs mins) (list "Peaks" "Valleys") steps
					 :y-label "nA" :x-label "Command Voltage (mV)" ; :x-max (car steps) :x-inc step-value
					 :title (format nil "~A - Peaks and Valleys" *simulation-name*))))))))
	(mapcar (lambda (key-value) (element-parameter vsource (car key-value) (cadr key-value)))
		`((pulse-list ,original-pulse-list)
		  (min-time-for-peak-valley ,min-time-for-peak-valley)
		  (max-time-for-peak-valley ,max-time-for-peak-valley)
		  (plot-peaks-and-valleys ,plot-peaks-and-valleys)
		  (plot-times-for-peaks-and-valleys ,plot-times-for-peaks-and-valleys)
		  (step-start-amplitude ,step-start-amplitude)
		  (step-start ,step-start)
		  (step-duration ,step-duration)
		  (holding-potential ,holding-potential)
		  (total-steps ,total-steps)
		  (initial-step-integer-coefficient ,initial-step-integer-coefficient))))))
  nil)
|#

(defun adjust-current (current-data pulse-data zero-input-data pulse-coefficient delta-t)
  (declare (optimize (safety 0) (speed 3) (space 0)))
;;  (plot-timed-data (list current-data pulse-data zero-input-data) nil nil :delta-t delta-t)
  (loop for val in current-data
	for pulse in pulse-data
	for zero-c in zero-input-data
	collect (the sf (- (the sf val) (the sf (+ (the sf zero-c)
						   (the sf (* (the fn pulse-coefficient) (the sf pulse)))))))))

(defun conductance-from-current-trace (reversal-potential &optional current-values voltage-values label)
  (let ((voltage-current-values
	 (if (and current-values voltage-values)
	     (list voltage-values current-values)
	     (extract-plot-window-data nil t))))
    (loop for voltage in (car voltage-current-values)
	  for current in (cadr voltage-current-values)
	  unless (= 0 (- voltage reversal-potential))
	  collect (/ current (- voltage reversal-potential)) into gs
	  collect voltage into voltages
	  finally (plot-timed-data gs (list "") voltages
				   :x-label "mV" :y-label "uS" :title  (or label (format nil "Extracted Conductance w/E-rev ~AmV" reversal-potential))))))

;; After M. Hausser
(defun voltage-jumps (activated-synapse synapse-activation-time start-jumps jump-increment number-jumps
					v-holding v-jump &key new-plot (vsource *vsource*) (clamp-duration *user-stop-time*))
  (enable-element-plot vsource)
  (block-all-synapses)
  (setf (synapse-event-times activated-synapse) (list synapse-activation-time))
  (let* ((*save-data-step* 1)
	 (*user-stop-time* clamp-duration)
	 (control-clamp-current nil)
	 (synapse-clamp-current nil)
	 (*accomodate-overlays* t)
	 (*CREATE-NEW-SIMULATION-PLOTS* new-plot)
	 (*create-new-plot-windows* new-plot)
	 (*overlay-plots* (not new-plot))
	 (*CREATE-NEW-SIMULATION-PLOTS* (not new-plot)))
    (loop for jump-time from start-jumps by jump-increment
	  for jump-number from 1 to number-jumps
	  do
	  (pulse-list vsource (list (list 0 jump-time v-holding) (list jump-time clamp-duration v-jump)))

	  (setf (synapse-block activated-synapse) nil)
	  (goferit)
	  (setq synapse-clamp-current (element-data-dted vsource 0.1))

	  (setf (synapse-block activated-synapse) t)
	  (goferit)
	  (setq control-clamp-current (element-data-dted vsource 0.1))

	  
	  collect
	  (loop for val1 in control-clamp-current
		for val2 in synapse-clamp-current
		sum (- val1 val2))

	  into charges
	  collect jump-time into jump-times
	  
	  finally
	  (format t "~A~%~A~%" charges jump-times)
	  (plot-xy-data (list jump-times charges)))))

(defun find-rm (cell rin)
    "Finds an approximate value for membrane resistivity, under the constraint that the input resistance of CELL is ideally RIN
[Mohms]. Shrinking ranges of Rm are explored, with exponential increments \(argument of 10\) ranging from 1 to .001 \(by
decade). Rin associated with returned value is less than than RIN."
  (let ((start-exp 0)
	out
	this-rin
	last-rin
	(cell (or cell *cell*)))
    (set-segments-inherit-parameters-from-type cell)
    (loop for exp-increment in '(1 0.1 0.01 .001) 
	  do (setq out 
		   (loop for rm-exp from start-exp by exp-increment do
			 (set-cell-type-param (element-type cell) 'rm (expt 10.0 rm-exp))
			 (SET-AND-UPDATE-CELL-TYPE-LINEAR-MEMBRANE)
			 (setq this-rin (cell-z-cable-in-cell cell))
			 (format t "RIN: ~a, RM: ~a~%" this-rin (expt 10.0 rm-exp))
			 when (and last-rin (< last-rin rin this-rin))
			 do (setq start-exp (- rm-exp exp-increment))
			 (return (expt 10.0 (- rm-exp exp-increment)))
			 else do (setq last-rin this-rin)))
	  finally (return out))))

#| 
;; Original from retina code:
;; DERIVE-G-WAVE Given two sets of i(t) and v(t) taken under different clamp conditions, derive
;; gex(t) and gin(t) dependent on assumed values of erest, eex, einh, and grest for a lumped cell 3
;; conductance model (resting conductance, excitatory and inhibitory synapses - capacitance ignored
;; for current clamp recordings). 
(defun derive-g-wave (a-wave voltageap clampa
			     b-wave voltagebp clampb
			     g-total-wave &key
			     (grest 2.0) ; nS
			     (erest -60.0)
			     (einh -60.0)
			     (eex 0.0)
			     (filename1 "")
			     (filename2 "")
			     (title ""))
  (let* ((dummy1 grest)(dummy2 erest)(dummy3 einh)(dummy4 eex)(dummy5 nil))
    (loop until dummy5 do
	  (choose-variable-values
	   `((dummy1 "g resting [nS]:" :float)
	     (dummy2 "e rest [mV]:" :float)
	     (dummy3 "e inh [mV]" :float)
	     (dummy4 "e ex [mV]" :float)
	     (dummy5 "Quit analysis" :boolean))
	   :text title :label (format nil "g-wave dissection parameters for files ~s and ~s" filename1 filename2))
	  (unless dummy5
	    (setq grest dummy1 
		  erest dummy2
		  einh dummy3 
		  eex dummy4)
	    (let (gex-wave ginh-wave grest-wave)
	      (loop for a-val in a-wave
		    for b-val in b-wave
		    do
		    (let* ((ia (if voltageap clampa a-val))
			   (va (if voltageap a-val clampa))
			   (ib (if voltagebp clampb b-val))
			   (vb (if voltagebp b-val clampb))
			   (va1 (- va eex))
			   (va2 (- va einh))
			   (vb1 (- vb eex))
			   (vb2 (- vb einh))
			   (gva3 (* grest (- va erest)))
			   (gvb3 (* grest (- vb erest)))
			   (x (/ gva3 va1))
			   (y (/ gvb3 vb1))
			   (ginh (/ (+ (/ ib vb1) (- (/ ia va1)) (- y) x)
				    (- (/ vb2 vb1) (/ va2 va1))))
			   (gex (+ (/ ia va1) (- (* ginh (/ va2 va1))) (- x))))
		      (push grest grest-wave)
		      (push ginh ginh-wave)
		      (push gex gex-wave)))
	      (add-comment
	       (plot-timed-data (list (reverse gex-wave) (reverse ginh-wave) g-total-wave grest-wave)
				'("g-ex" "g-inh" "g-total" "g-rest")
				(loop for time from 0.0 to 1960 by (/ 1960.0 (length grest-wave)) collect time)
				:y-label "ns" :title title)
	       (format nil "g-rest: ~,2fns  e-rest: ~,2fmv~%e-inh: ~,2fmv  e-ex: ~,2fmv" grest erest einh eex)))))))
|#

#|
; first extract goes for inactivation data, second for conductance curve from activation
(plot-xy-data (list (let* ((xy (car (extract-plot-window-data)))
		     (max-y (- (loop for ys in (nth 1 xy) minimize ys ))))
		(list (nth 0 xy) (loop for ys in (nth 1 xy) collect (/ (- ys) max-y))))
		    (let* ((xy (car (extract-plot-window-data)))
		     (max-y (loop for ys in (nth 1 xy) maximize ys)))
		(list (nth 0 xy) (loop for ys in (nth 1 xy) collect (/ ys max-y))))))


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;; OLD  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun cc-steps (start-current stop-current step
			       &key (isource *isource*)
			       (current-start-time 10.0)
			       (current-stop-time (* 0.9 *user-stop-time*))
			       individual-plots
			       comment
			       (extra-comment "")
			       (overlay-plots t)
			       (show-plot-windows t)
			       timeit
			       (include-comment t))
  (let ((steps (when (consp step) step)))
    (loop for current in (or steps
			     (loop for current from start-current to stop-current by step collect current))
	  for count from 0
	  do
	  (pulse-list isource (list current-start-time current-stop-time current))
	  (let ((*CREATE-NEW-SIMULATION-PLOTS* individual-plots)
		(*resurrect-plots* show-plot-windows)
		(*update-plots* show-plot-windows)
		(*ACCOMODATE-OVERLAYS* t)
		(*overlay-plots* (and (> count 0) overlay-plots (not individual-plots)))
		(*simulation-plot-window-comment*
		 (when include-comment
		   (if comment
		       (format nil "~A" comment)
		       (if (and overlay-plots (not individual-plots))
			   (if steps
			       (format nil "Steps ~,2f to ~,2fnA ~A" (first steps) (car (last steps)) extra-comment)
			       (format nil "~,2f to ~,2fnA steps ~A" start-current stop-current extra-comment))
			   (format nil "~,2fnA step ~A" current extra-comment ))))))
	    (if timeit (gotimed) (goferit))))))


;; backward compatibility
(defun source-clamp-steps (start-clamp stop-clamp step
				       &key (source (or *isource* *vsource*))
				       (clamp-start-time 10.0)
				       (clamp-stop-time (* 0.9 *user-stop-time*))
				       (holding-potential -70) ; mV
				       individual-plots
				       comment
				       (extra-comment "")
				       (overlay-plots t)
				       (show-plot-windows t)
				       timeit
				       return-source-data
				       (include-comment t))
  (clamp-steps :start-clamp start-clamp
	       :stop-clamp stop-clamp
	       :step step
	       :source source
	       :clamp-start-time clamp-start-time
	       :clamp-stop-time clamp-stop-time
	       :holding-potential holding-potential
	       :individual-plots individual-plots
	       :comment comment
	       :extra-comment extra-comment
	       ; :overlay-plots overlay-plots
	       :show-plot-windows show-plot-windows
	       :timeit timeit
	       :return-source-data return-source-data
	       :include-comment include-comment))

(defun old-vclamp-soma-conductance (clamp-potential-1
				    clamp-potential-2
				    &key (dt 0.1) new-plot (plot-data t) (plot-uncorrected-wave t)
				    (r-electrode 0.0) (ideal-vsource t)
				    (timeit t) return-g-waves pause-between-clamps
				    (correction-coeff 1.0)
				    interclamp-function run-reg-cap-for-ss (find-steady-state t))
  (erase-elements (vsources))
  (let* ((new-plot (and new-plot plot-data))
	 (*plot-standard-windows* plot-data)
	 (*run-reg-cap-for-ss* run-reg-cap-for-ss)
	 (*vsource-resistance* r-electrode)
	 (*CREATE-NEW-SIMULATION-PLOTS* new-plot)
	 (*create-new-plot-windows* new-plot)
	 (*overlay-plots* (not new-plot))
	 (*CREATE-NEW-SIMULATION-PLOTS* (not new-plot))
	 (*use-node-voltage-initializations* find-steady-state)
	 (*find-steady-state* find-steady-state)
	 (*user-max-step* 0.2)		;maybe conservative
	 (*plot-synapse-currents-p* (and plot-data *plot-synapse-currents-p*))
	 (*plot-synapse-conductances-p* (and plot-data *plot-synapse-conductances-p*))
	 (*accomodate-overlays* plot-data))
    
    (loop for soma in (somas) do (add-vsource soma :ideal ideal-vsource))

    (enable-element-plot (vsources) 'current)
    
    (loop for vsource-reference-magnitude in (list clamp-potential-1 clamp-potential-2) do
	  (setq *vsource-default-reference-magnitude* (float vsource-reference-magnitude))
	  (typecase interclamp-function
	    (cons (apply 'funcall interclamp-function))
	    (function (funcall interclamp-function)))
	  (format t "Voltage clamp at ~,2fmV.~%"  (float vsource-reference-magnitude))
	  (if timeit (gotimed) (goferit))
	  (when plot-data
	    (setq *overlay-plots* t
		  *create-new-plot-windows* nil
		  *CREATE-NEW-SIMULATION-PLOTS* nil))
	  collect (loop for soma in (somas) collect (element-data-dted (car (node-vsources soma)) dt))
	  into all-currents

	  when pause-between-clamps do (break) ; (go-ahead-menu "Go ahead and run 2nd clamp...")

	  finally
	  (setq *create-new-plot-windows* new-plot)
	  
	  (return
	    (let ((data
		   (loop for soma in (somas)
			 for current-list-index from 0
			 collect
			 (let* ((currents (list (nth current-list-index (car all-currents))
						(nth current-list-index (cadr all-currents))))
				(g-wave-data
				 (clean-up-list
				  (cons (when (and (not (= 0 r-electrode)) plot-uncorrected-wave)
					  (g-wave-new (nth 0 currents) nil clamp-potential-1
						      (nth 1 currents) nil clamp-potential-2))
					(loop for coeff in (if (= 0 r-electrode) '(1.0) (coerce-to-list correction-coeff))
					      collect
					      (g-wave-new (nth 0 currents) nil clamp-potential-1
							  (nth 1 currents) nil clamp-potential-2
							  (* coeff r-electrode))))))
				(g-wave-labels		 
				 (clean-up-list
				  (cons (when (and (not (= 0 r-electrode)) plot-uncorrected-wave) "Soma estimated G uncorrected")
					(loop for coeff in (if (= 0 r-electrode) '(1.0) (coerce-to-list correction-coeff))
					      collect (format nil "Soma estimated G w/R-correction ~A Mohms" (* coeff r-electrode)))))))
			   (when plot-data
			     (plot-timed-data g-wave-data g-wave-labels dt
					      :delta-t-start *user-start-time*
					      :title (concatenate-strings
						      (cell-name (soma-cell soma))
						      (if (= 0 r-electrode)
							" G-wave Simulation"
							(format nil " G-wave Simulation - R-electrode ~a Mohms" r-electrode)))
					      :y-min 0.0 :x-min *user-start-time* :x-max *user-stop-time*
					      :overlay t :timed-data t :y-label "uS"))
			   (cons g-wave-labels g-wave-data)))))
	      (erase-elements (vsources))
	      (when return-g-waves data))))))

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


;;; SYS Source file: vsource.lisp
;
; the voltage source model
;

(in-package "SURF-HIPPO")

;; temporary hacks

(defvar *vsrnodevolt* nil)
(defvar *vsrvolt* nil)

;;;;;;;;;;;

(defun vsource-enabled (src) (not (vsource-blocked src)))

(defun document-vsource (&optional (vsrc *vsource*) (circuit-dump *document-elements-for-circuit-dump*))
  (let* ((vsource (element vsrc 'vsource))
	 (vsource-name (element-name vsrc))
	 (cell-name (cell-name (element-cell vsource)))
	 (massaged-vsource-name (if circuit-dump (massage-dumped-cell-elt-name vsource-name cell-name) vsource-name))
	 (target-name (element-name (element-cell-element vsource)))
	 (massaged-target-name (if circuit-dump (massage-dumped-cell-elt-name target-name cell-name) target-name)))
    (when vsource
      (format t "#|~%")
      (print-vsource vsource)
      (format t "|#~%")
      (print `(add-vsource 
	       ,(quote-symbol-cons massaged-target-name)
	       :name ,(quote-symbol-cons massaged-vsource-name)
	       :type ,(quote-symbol-cons (element-name (vsource-type vsource)))))
      (when (element-parameter vsource 'waveform-function)
	(document-waveform-function vsource massaged-vsource-name))
      (when (vsource-use-pulse-list vsource)
	(document-pulse-list vsource massaged-vsource-name))))
  nil)

(defun print-vsource (&optional (vsrc *vsource*))
  (let ((vsrc (element vsrc 'vsource)))
    (when vsrc
      (if (element-parameter vsrc 'ideal-vsource)
	  (format t "~a (~A) [Ideal],"
		  (massage-element-plot-label vsrc nil t)
		  (node-name (vsource-node vsrc)))
	  (format t "~a (~A), Internal resistance ~aKohms,"
		  (massage-element-plot-label vsrc nil t)
		  (node-name (vsource-node vsrc))
		  (* 1000 (vsource-resistance vsrc))))
      (format t " Default magnitude ~AmV~%" (vsource-reference-magnitude vsrc))
       
      (case (element-parameter vsrc 'pulse-transition-constraint)
	(:fixed-slope
	 (format t "  Pulse transtion fixed by slope of ~A mV/msec~%"
		 (or (element-parameter vsrc 'pulse-transition-slope) 1000.0)))
	(:fixed-transition-time
	 (format t "  Pulse transtion fixed by transition time of ~A msec~%"
		 (or (element-parameter vsrc 'pulse-transition-time) 0.1))))
      (cond
	((and *vsource-intrinsic-current* (not (element-parameter vsrc 'ideal-vsource)))
	 (format t "  Current calculated directly from voltage drop across source resistance.~%"))
	(*INCLUDE-VSOURCE-CURRENT-LINEAR-and-non-local-COMPONENT*
	 (format t "  Current calculated across individual source node elements.~%"))
	(t
	 (format t "  Current calculated across individual local source node elements, w/o node cap.~%")))
      (cond
	((vsource-blocked vsrc)
	 (format t "  Source turned off~%"))
	((not (source-stimulus-p vsrc))
	 (format t "  No stimulus~%"))
	((vsource-use-pulse-list vsrc)
	 (cond-every
	  ((and (element-parameter vsrc 'enable-individual-pulses) (extract-pulse-list vsrc))
	   (loop for pulse in (extract-pulse-list vsrc)
		 do (format t "       ~,2e mV pulse from ~,2e to ~,2e msec~%" (s-flt (+ (vsource-reference-magnitude vsrc) (nth 2 pulse))) (nth 0 pulse) (nth 1 pulse))))
	  ((and (element-parameter vsrc 'enable-pulse-train) (extract-pulse-train-args vsrc))
	   (print-pulse-train-info "mV" (extract-pulse-train-args vsrc)))))
	((extract-waveform-function vsrc)
	 (print-spaces t 4)
	 (document-function-args (extract-waveform-function vsrc)))
	(t (format t "  Explicit waveform~%"))))))

(defun get-vsource-simple-name ()
  (loop for candidate from (max 1 *vsource-simple-name-counter*)
	until (not (vsource-hash-table candidate))
	finally (return (setf *vsource-simple-name-counter* candidate))))

(defun rename-vsources-simple (&optional (vsources (vsources)))
  "Rename VSOURCES [default all vsources in circuit] with simple integer names."
  (loop for ch in (coerce-to-list (element vsources 'vsource)) do (set-element-name ch (get-vsource-simple-name))))

(defun create-vsource (cell-element &key name (ideal t) waveform-spec (WAVEFORM-time-interval *default-waveform-step*) 
		       pulse-train-args enable-pulse-train
		       (enable-individual-pulses t) pulse-list (use-pulse-list t))
  (when (node-vsources cell-element) (sim-warning (format nil "create-vsource: node ~a already has vsource, ignoring" (element-name cell-element))))
  (let ((source-name (if (and *use-simple-names* (not name))
			 (GET-VSOURCE-SIMPLE-NAME)
			 (or name (format nil "~a-vsrc" (element-name cell-element)))))
	(cell-element (element-cell-element cell-element)))
    (or	(element source-name 'vsource)
	(let* ((node (element-node cell-element))
	       (vsrc (make-vsource :name source-name :node node ; :plot-current t
				   :cell-element cell-element :use-pulse-list use-pulse-list
				   :resistance (max *minimum-vsource-resistance* *vsource-resistance*))))
	  (element-parameter vsrc 'ideal-vsource ideal)
	  (element-parameter vsrc 'enable-pulse-train enable-pulse-train)
	  (element-parameter vsrc 'enable-individual-pulses enable-individual-pulses)
	  (element-parameter vsrc 'pulse-train-args pulse-train-args)
	  (element-parameter vsrc 'waveform-function waveform-spec)
	  (pulse-list vsrc pulse-list)
	  (setq *make-node-w/elements-array* t)
	  (push vsrc (node-elements node))
	  (setf (VSOURCE-HASH-TABLE source-name) vsrc)
	  (when waveform-spec (add-waveform vsrc :WAVEFORM-time-interval WAVEFORM-time-interval :waveform-spec waveform-spec :use-menu nil))
	  (setq *vsource* vsrc)))))

;; backward comp
(defun create-pwl-vsource (cell-element &key (WAVEFORM-time-interval *default-waveform-step*) name
					pulse-train-args enable-pulse-train (ideal t) (enable-individual-pulses t) pulse-list (use-pulse-list t) waveform-spec)
  (create-vsource cell-element :name name :waveform-spec waveform-spec :WAVEFORM-time-interval WAVEFORM-time-interval
		  :pulse-train-args pulse-train-args :enable-pulse-train enable-pulse-train
		  :ideal ideal :enable-individual-pulses enable-individual-pulses :pulse-list pulse-list :use-pulse-list use-pulse-list))
    
(defun edit-vsource (source)
  (let* ((vsrc (element source 'vsource))
	 (name (element-name source 'vsource))
	 (dummy1 (* 1e6 (vsource-resistance vsrc)))
	 (dummy11 (or (element-parameter vsrc 'pulse-transition-constraint) :fixed-slope))
	 (dummy12 (or (element-parameter vsrc 'pulse-transition-slope) *pwl-vsource-dv-dt*))
	 (dummy13 (or (element-parameter vsrc 'pulse-transition-time) *minimum-source-transition-time*))
	 (dummy14 (element-parameter vsrc 'ideal-vsource))
	 (dummy15 (vsource-blocked vsrc))
	 (dummy16 (element-parameter vsrc 'reference-magnitude); *vsource-default-reference-magnitude*
	   )
	 dummy2)
    (choose-variable-values
     `((dummy14 "Ideal voltage source" :boolean)
       (dummy15 "Block this source" :boolean)
       (dummy1 ,(format nil "Voltage Source Internal Resistance [ohms, > 0]~%(only for non-ideal sources)") :float)
       (dummy11 "Transition between pulse stimuli defined by:" :choose (:fixed-slope :fixed-transition-time))
       (dummy12 "Pulse transition slope (mV/msec)" :float)
       (dummy13 "Pulse transition time (msec)" :float)
       (*steady-state-linear-vclamp-enable* "Initialize with one-step steady state linear method." :boolean)
       (dummy16 "Default reference value (mV)" :float)
       (:comment "Non-ideal source current calculation")
					; (*vsource-current-reference-time* "Reference time for current calculation:" :choose (:dt-midpoint :last-time-point))
       (*vsource-intrinsic-current*
	,(format nil "~A~%~A" "Current calculated from delta V across source resistance" "(otherwise calculated across individual elements):")
	:boolean)
       (*INCLUDE-VSOURCE-CURRENT-LINEAR-and-non-local-COMPONENT*
	,(format nil "~A~%~A" "If current calculation across individual elements," "include linear and non-local component of voltage current:")
	:boolean)
       (*INCLUDE-local-CAP-CURRENT-IN-VSOURCE*
	,(format nil "~A~%~A" "If current calculation across individual elements," "include source node capacitance (above flag must be set):")	  
	:boolean)
       (dummy2 "Edit stimulus" :boolean))
     :label (format nil "Setting up parameters of voltage source ~A" name))
    (setf (vsource-blocked vsrc) dummy15)
    (setf (vsource-resistance vsrc) (max (* 1e-6 dummy1) *minimum-vsource-resistance*))
    (when dummy2 (edit-source-stimulus source))
    (element-parameter vsrc 'reference-magnitude dummy16)
    (element-parameter vsrc 'ideal-vsource dummy14)
    (element-parameter vsrc 'pulse-transition-constraint dummy11)
    (element-parameter vsrc 'pulse-transition-slope dummy12)
    (element-parameter vsrc 'pulse-transition-time dummy13)))

(defun ideal-vsource (vsource)
  "Make VSOURCE ideal."
  (element-parameter vsource 'ideal-vsource t))

(defun non-ideal-vsource (vsource &optional resistance)
  "Make VSOURCE non-ideal. When RESISTANCE is included, set the internal resistance of VSOURCE to this
value [in Mohms]. If successful, returns the internal resistance of VSOURCE."
  (let ((vsource (element vsource 'vsource)))
    (when vsource
      (element-parameter vsource 'ideal-vsource nil)
      (when (numberp resistance) (setf (vsource-resistance vsource) (s-flt (max resistance *minimum-vsource-resistance*))))
      (vsource-resistance vsource))))

(defun menu-for-vsources ()
  (loop for name in (select-hash-values-menu (VSOURCE-HASH-TABLE) "Select Voltage Sources" :punt-if-only-one-entry t) do (edit-vsource name))) 

(defun vsource-on-node (node) (loop for vsrc in (vsources) thereis (eq (vsource-node vsrc) node)))

(defun get-vsource-voltage (src time)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((time (if (/= 0.0 (vsource-period src))
		  (mod (d-flt time) (vsource-period src))
		  (d-flt time))))
    (declare (double-float time))
    (let ((output
	   (cond ((vsource-use-pulse-list src) (extract-pwl-value time src))
		 ((vsource-waveform-array src)
		  (let ((array (the (simple-array single-float *) (vsource-waveform-array src))))
		    (multiple-value-bind (vsrc-time-int-part vsrc-time-fract-part)
			(truncate (* (vsource-waveform-time-interval-inverse src) (- time (vsource-delay src))))
		      (declare (double-float vsrc-time-fract-part)
			       (fixnum vsrc-time-int-part))
		      (cond ((< vsrc-time-int-part (the fn (1- (length array))))
			     (check-element-time-step-maximum (vsource-waveform-time-interval-mrt src))
			     ;; (let ((get-vsr-voltage (interpolated-array-value-double array vsrc-time-int-part vsrc-time-fract-part)))
			     ;;   (printvars time vsrc-time-int-part vsrc-time-fract-part get-vsr-voltage *real-time*))
	     
			     (interpolated-array-value-double array vsrc-time-int-part vsrc-time-fract-part))
			    ((= vsrc-time-int-part (the fn (1- (length array))))
			     (d-flt (aref array vsrc-time-int-part))
			     ;; (interpolated-value-double (aref array vsrc-time-int-part)
			     ;;			 (or (element-parameter vsrc 'reference-magnitude) *vsource-default-reference-magnitude*)
			     ;;		 vsrc-time-fract-part)
			     )
			    (t
			     (d-flt (if t ; (= (length array) (truncate (* (vsource-waveform-time-interval-inverse SRC) *user-stop-time*)))
					(aref array (1- (length array)))
					(or (element-parameter src 'reference-magnitude) *vsource-default-reference-magnitude*))))))))
		 (t (the double-float (vsource-reference-magnitude src))))))
      ;; (printvars time output)
      output)))

(defun get-vsource-voltage (src time)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((time (if (/= 0.0 (vsource-period src))
		  (mod (d-flt time) (vsource-period src))
		  (d-flt time))))
    (declare (double-float time))
    (cond ((vsource-use-pulse-list src) (extract-pwl-value time src))
	  ((vsource-waveform-array src)
	   (let ((array (the (simple-array single-float *) (vsource-waveform-array src))))
	     (multiple-value-bind (vsrc-time-int-part vsrc-time-fract-part)
		 (truncate (* (vsource-waveform-time-interval-inverse src) (- time (vsource-delay src))))
	       (declare (double-float vsrc-time-fract-part)
			(fixnum vsrc-time-int-part))
	       (cond ((< vsrc-time-int-part (the fn (- (length array) 1)))
		      ;; (check-element-time-step-maximum (vsource-waveform-time-interval-mrt src))
		      (interpolated-array-value-double array vsrc-time-int-part vsrc-time-fract-part))
		     (nil		; (= vsrc-time-int-part (the fn (- (length array) 1)))
		      (let ((out (d-flt (interpolated-value (aref array vsrc-time-int-part)
							    (s-flt (or (element-parameter src 'reference-magnitude) *vsource-default-reference-magnitude*))
							    (s-flt vsrc-time-fract-part)))))
			(printvars vsrc-time-int-part out)
			out))
		     (t	;; (format t "going for the last value of vsrc...~%")
		      (* 1.0d0 (aref array (1- (length array))))
		      ;; (or (element-parameter src 'reference-magnitude) *vsource-default-reference-magnitude*)
		      )))))
	  (t (the double-float (vsource-reference-magnitude src))))))

(defun vsource-reference-magnitude (vsource &optional value)
  (when (numberp value) (element-parameter vsource 'reference-magnitude (d-flt value)))
  (or (element-parameter vsource 'reference-magnitude)
      (d-flt *vsource-default-reference-magnitude*)))

;; When this is t, not good. (*INPUT-TIME*) is either at the time step midpoint, or at the prediction time. 
(defvar *use-*input-time*-for-vsources* nil)
(defun vsource-ref-time ()
  (the sf (if *use-*input-time*-for-vsources*
	      (*input-time*)
	      (the sf (* *mrt* 0.5 (+ *time-step* *sim-time-n* *sim-time-n*))))))

(proclaim '(inline vsource-current-ref-time))
(defun vsource-current-ref-time ()
  ; (*t[n+1]*)
  (*input-time*)
  )

(defun ideal-vsources-p () (loop for src in (vsources) thereis (element-parameter src 'ideal-vsource)))

#|
(defun check-fixed-voltage-nodes ()
  (let (reorder-matrix)
    (loop for src in (vsources) do
	  (setq reorder-matrix
		(or reorder-matrix
		    (element-parameter src 'ideal-vsource)
		    (node-has-ideal-voltage-source (vsource-node src))
		    (xor
		     (node-has-ideal-voltage-source (vsource-node src))
		     (and 
		      (element-parameter src 'ideal-vsource)
		       (not (vsource-blocked src))))
		    
		    ;;                    (or (xor (node-has-ideal-voltage-source (vsource-node src))
		    ;;                             (element-parameter src 'ideal-vsource))
		    ;;                        (and (element-parameter src 'ideal-vsource)
		    ;;                             (xor (vsource-enabled src) (element-parameter src 'enabled))))
		    ;;

		    ))
	  (setf (node-has-ideal-voltage-source (vsource-node src)) (and (not (vsource-blocked src)) (element-parameter src 'ideal-vsource))))
    (printvars reorder-matrix)
    (when reorder-matrix
      (setq *make-segment-lists* t)
      (process-circuit-structure t))
    reorder-matrix)
  (unless (or (ideal-vsources-p) (= *num-nodes* *CORE-NODE-ARRAY-LENGTH*))
    (format t "Reprocessing changed circuit structure...~%")
    (process-circuit-structure t)))
|#

(defun check-fixed-voltage-nodes ()
  (let ((need-to-reorder-matrix *need-to-reorder-matrix*))
    (loop for src in (vsources) do
	  (let ((src-node-in-core-node-array-p (find (element-node src) *core-node-array*))
		(ideal-vsource-p (element-parameter src 'ideal-vsource)))
	    (setq need-to-reorder-matrix
		  (or need-to-reorder-matrix
		      (and src-node-in-core-node-array-p
			   ideal-vsource-p
			   (not (vsource-blocked src)))
		      (and (not src-node-in-core-node-array-p)
			   (or (not ideal-vsource-p)
			       (vsource-blocked src))))))
	  (setf (node-has-ideal-voltage-source (vsource-node src)) (and (not (vsource-blocked src)) (element-parameter src 'ideal-vsource))))
    (when (or need-to-reorder-matrix (not (or (ideal-vsources-p) (= *num-nodes* *CORE-NODE-ARRAY-LENGTH*))))
      (format t "Reprocessing changed circuit structure...~%")
      (process-circuit-structure t))
    (setq *need-to-reorder-matrix* nil)))

(proclaim '(inline eval-vsource))
(defun eval-vsource (src)
  ;; Calculates the new voltage in time for a pwl voltage source. Evaluates vsource contribution to node in the same manner as a channel. 
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (unless (vsource-blocked src)
    (let* ((vsource-ref-time (the sf (vsource-ref-time)))
	   (vsource-ref-time-w-delay (the sf (if (vsource-use-pulse-list src) vsource-ref-time (- vsource-ref-time (vsource-delay src))))))
      (when (> vsource-ref-time-w-delay 0.0)
	(let ((conductance (the df (/ 1.0d0 (vsource-resistance src)))) ; uS
	      (e-rev (get-vsource-voltage src vsource-ref-time)))
	  (setf ;; (vsource-voltage src) e-rev
		(node-current (vsource-node src))
		(the df (+ (node-current (vsource-node src)) (the df (* conductance (- e-rev)))))
		(node-jacobian (vsource-node src))
		(the df (+ (node-jacobian (vsource-node src)) conductance)))
	  (setf (vsource-voltage src) (s-flt (get-vsource-voltage src (vsource-current-ref-time))))
	  (when *debug-time-trace* (format t "   VSRC: time ~A - v-e-rev: ~A, crnt: ~A ~%" vsource-ref-time e-rev (the df (* conductance (- e-rev)))))))))
  nil)

(defun eval-fixed-voltage-node-vsource (src)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (unless (vsource-blocked src)
    (let* ((vsource-ref-time (the sf (vsource-ref-time)))
	   (vsource-ref-time-w-delay (if (vsource-use-pulse-list src) vsource-ref-time (- vsource-ref-time (vsource-delay src)))))
      (when (> vsource-ref-time-w-delay 0.0)
	(setf (vsource-voltage src) (s-flt (get-vsource-voltage src (vsource-current-ref-time)))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation of ideal voltage sources - see numerical.doc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-adjacent-nodes-and-g-axials (vsource)
  (flet ((distal-seg-nodes-and-g-axials (cell-element)
	   (loop for seg in (distal-segments cell-element) collect (list (cell-element-physical-node-fast seg) (segment-g-axial seg)))))
    (let ((cell-element (vsource-cell-element vsource)))
      (setf (vsource-adjacent-nodes-and-g-axials vsource) (distal-seg-nodes-and-g-axials cell-element))
      (when (segment-p cell-element)
	(push (list (cell-element-physical-node-fast (proximal-cell-element cell-element)) (segment-g-axial cell-element))
	      (vsource-adjacent-nodes-and-g-axials vsource))))))

(defun eval-fixed-voltage-nodes ()
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (loop for src in *fixed-vsource-list* do
	(let* ((node (vsource-node src))
	       (vsource-ref-time (the sf (vsource-ref-time)))
	       (vsource-ref-time-w-delay (the sf (if (vsource-use-pulse-list src) vsource-ref-time (- vsource-ref-time (vsource-delay src))))))
	  (when (> vsource-ref-time-w-delay 0.0)
	    (let ((voltage (get-vsource-voltage src (vsource-current-ref-time))))
	      (setf (vsource-voltage src) (s-flt voltage))
	      ;; (printvars voltage)
	      (set-element-parameter-fast node 'fixed-voltage voltage (node-parameters node)) ; (element-parameter node 'fixed-voltage voltage)
	      (setf (node-voltage-n+1 node) (d-flt voltage))))
	  (let ((src-voltage (node-voltage-n+1 node)))
	    (loop for node-g-axial in (vsource-adjacent-nodes-and-g-axials src) do
		  (let ((destination-node (car node-g-axial))
			(g-axial (the df (cadr node-g-axial))))
		    (setf (node-current destination-node) (- (node-current destination-node) (* g-axial src-voltage))
			  (node-jacobian destination-node) (+ (node-jacobian destination-node) g-axial)))))))
  nil)

#|
(defun eval-fixed-node-voltages ()
  (loop for src in *fixed-vsource-list* do
	(let* ((node (vsource-node src))
	       (vsource-ref-time (the sf (vsource-ref-time)))
	       (vsource-ref-time-w-delay (the sf (if (vsource-use-pulse-list src) vsource-ref-time (- vsource-ref-time (vsource-delay src))))))
	  (when (> vsource-ref-time-w-delay 0.0)
	    (let ((voltage (get-vsource-voltage src (vsource-current-ref-time))))
	      (setf (vsource-voltage src) (s-flt voltage))
	      (element-parameter node 'fixed-voltage voltage)
	      (setf (node-voltage-n+1 node) voltage)))
	
	  (let ((src-voltage (node-voltage-n+1 node)))
	    (loop for destination-elt in
		  (typecase (vsource-cell-element src)
		    (segment
		     (cons (proximal-cell-element (vsource-cell-element src))
			   (distal-segments (vsource-cell-element src))))
		    (soma (trunk-segments (soma-cell (vsource-cell-element src)))))
				  
		  do
		  (let ((destination-node (element-physical-node destination-elt))
			(g-axial (the df (segment-g-axial destination-elt))))
		    (setf (node-current destination-node)
			  (- (node-current destination-node) (* g-axial src-voltage))
			  (node-jacobian destination-node)
			  (+ (node-jacobian destination-node) g-axial)))))))	    
  nil)
|#

(defun eval-all-non-ideal-vsources ()
  (loop for src in *non-ideal-vsource-list* do (eval-vsource src))
  nil)

(defun advance-vsource (src)
  (setf (vsource-last-voltage src) (vsource-voltage src))
  nil)

(defun advance-vsources ()
  (loop for src in *fixed-vsource-list* do (advance-vsource src))
  (loop for src in *non-ideal-vsource-list* do (advance-vsource src)))
  
(defun init-vsource (src)
  (let ((voltage (s-flt (get-vsource-voltage src 0))))
    (setf (vsource-last-voltage src) voltage
	  (vsource-voltage src) voltage)
    nil))

(defun INIT-VSOURCEs ()
  (loop for src in *non-ideal-vsource-list* do (init-vsource src))
  (loop for src in *fixed-vsource-list*
	do (set-adjacent-nodes-and-g-axials src)
	(init-vsource src)))

(defvar *INCLUDE-local-CAP-CURRENT-IN-VSOURCE* t)
(defvar *debug-vsource-current* nil)
(defvar *include-vsource-current-linear-and-non-local-component* t)
(defvar *vsource-voltage-reference-n* t)
(defvar *vsource-intrinsic-current* t)
(defvar *debug-vsource-voltage* nil)
(defvar *vsource-current-reference-time* :dt-midpoint)

(defun get-soma-current-for-vsource (vsrc-node-voltage vsrc-node-soma vsrc-node-dvdt)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float vsrc-node-voltage vsrc-node-dvdt))
  (if *include-vsource-current-linear-and-non-local-component*
      (let* ((soma-shunt-current (if (soma-include-shunt vsrc-node-soma)
				     (* (soma-g-shunt vsrc-node-soma) vsrc-node-voltage)
				     0.0d0))
	     (soma-capacitance-current (* (soma-capacitance vsrc-node-soma) vsrc-node-dvdt))
	     (soma-leak-current (* (soma-g-leak vsrc-node-soma)
				   (- vsrc-node-voltage
				      (soma-v-leak vsrc-node-soma)
				      )))
	     (soma-dendrite-current (soma-dendrite-current vsrc-node-soma))
	     (soma-total-current (+ soma-shunt-current
				    soma-capacitance-current
				    soma-leak-current
				    soma-dendrite-current)))
	(when *debug-vsource-current* (printvars vsrc-node-soma soma-shunt-current soma-capacitance-current soma-dendrite-current soma-leak-current soma-total-current))
	soma-total-current)
      0.0d0))

(defun get-seg-current-for-vsource (vsrc-node-voltage vsrc-node-segment vsrc-node-dvdt vsrc-node)
  (declare ; (optimize (safety 1) (speed 3) (space 0))
	   (double-float vsrc-node-voltage vsrc-node-dvdt))
  (if *include-vsource-current-linear-and-non-local-component*
      (let* ((proximal-element-voltage (node-voltage-n+1 (element-node (proximal-cell-element vsrc-node-segment))))
	     (proximal-axial-current (* (segment-g-axial vsrc-node-segment) (- vsrc-node-voltage proximal-element-voltage)))
	     (distal-axial-current
	      (loop for distal-segment in (distal-segments vsrc-node-segment)
		    sum (* (segment-g-axial distal-segment) (- vsrc-node-voltage (node-voltage-n+1 (element-node distal-segment))))))
	     (segment-capacitance-current ; (element-capacitance-current vsrc-node-segment)
	        (* (segment-capacitance vsrc-node-segment) (the df vsrc-node-dvdt))
	       )
	     (segment-leak-current (* (segment-g-leak vsrc-node-segment) (- vsrc-node-voltage (segment-v-leak vsrc-node-segment))))
	     (segment-total-current (+ segment-leak-current segment-capacitance-current proximal-axial-current distal-axial-current)))
	(when *debug-vsource-current*
	  (printvars vsrc-node-segment segment-leak-current segment-capacitance-current proximal-axial-current distal-axial-current segment-total-current))
	segment-total-current)
      0.0d0))

(defun get-isource-current-for-vsource (vsrc-node-isource)
  (let ((current (the df (* 1.0d0 (- (get-isource-current vsrc-node-isource))))))
    (when *debug-vsource-current* (printvars vsrc-node-isource current))
    current))

(defun get-ch-current-for-vsource (vsrc-node-ch)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((ch-current (the df (get-channel-current vsrc-node-ch))))
    (when *debug-vsource-current* (printvars vsrc-node-ch ch-current))
    ch-current))

(defun get-syn-current-for-vsource (vsrc-node-syn vsrc-node-voltage)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float vsrc-node-voltage))
  (let ((syn-current (the df (* (synapse-conductance vsrc-node-syn)
				(- vsrc-node-voltage (synapse-e-rev vsrc-node-syn))))))
    (when *debug-vsource-current* (printvars vsrc-node-syn syn-current))
    syn-current))

(defun collect-node-currents-for-vsource (vsrc-node vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt)
  ;; Returns DF.
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (double-float vsrc-node-voltage vsrc-node-dvdt)
	   (single-float vsrc-voltage))
  (let* ((vsrc-cell-element (element-cell-element vsrc))
	 (vsrc-node-is-soma-p (soma-p vsrc-cell-element)))
    (+
     (d-flt (typecase vsrc-cell-element
	      (soma (get-soma-current-for-vsource vsrc-node-voltage vsrc-cell-element vsrc-node-dvdt))
	      (segment	(get-seg-current-for-vsource vsrc-node-voltage vsrc-cell-element vsrc-node-dvdt vsrc-node))))
     (loop for vsrc-node-elt in (node-elements vsrc-node) summing
	   (the df (typecase vsrc-node-elt
		     (isource (get-isource-current-for-vsource vsrc-node-elt))
		     ;; (soma (get-soma-current-for-vsource vsrc-node-voltage vsrc-node-elt vsrc-node-dvdt))
		     (channel (get-ch-current-for-vsource vsrc-node-elt))
		     (synapse (get-syn-current-for-vsource vsrc-node-elt vsrc-node-voltage))
		     ;; (segment (* (if vsrc-node-is-soma-p 0 1) (get-seg-current-for-vsource vsrc-node-voltage vsrc-node-elt vsrc-node-dvdt vsrc-node)))
		     (t 0.0d0))) into result double-float
	   finally (return result)))))

(defun debug-vsource-messages (vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt current)
  (when *debug-vsource-voltage*
    (format t "time: ~A vsource voltage: ~A, node-voltage: ~A~%" *real-time* vsrc-voltage vsrc-node-voltage))
  (when *debug-vsource-current*
    (format t "time ~A vsource current (vsrc voltage ~,2f, node-volt ~,2f ~a):~% "
	    *real-time*	(vsource-last-voltage vsrc)
	    vsrc-node-voltage (if vsrc-node-dvdt
				  (format nil "dvdt ~,2f" vsrc-node-dvdt)
				  (format nil "dvdt not computed")))
    (format t " total current: ~,2f ~%~%" (s-flt current))))

(defun get-vsource-current-vsrc-voltage (vsrc)
  (if (vsource-blocked vsrc)
      (s-flt (element-value (cell-element vsrc)))
      (the sf (case *vsource-current-reference-time*
		(t			; :dt-midpoint
		 (vsource-voltage vsrc)
					; (* 0.5 (+ (vsource-voltage vsrc) (vsource-last-voltage vsrc)))
					;(vsource-voltage vsrc)
					;(vsource-last-voltage vsrc)
		 )
					; (:last-time-point (vsource-last-voltage vsrc))
		))))
(defun get-vsource-current-vsrc-node-voltage (vsrc-node)
  (the df (case *vsource-current-reference-time*
	    (t				; :dt-midpoint
	     ; (node-voltage-n+1 vsrc-node)
	     (node-voltage-n vsrc-node)
	     )
					; (:last-time-point (node-voltage-n vsrc-node))
	    )))

(defun get-vsource-current-vsrc-node-dvdt (vsrc-node)
  (the df (if (and *include-vsource-current-linear-and-non-local-component*
		   *INCLUDE-local-CAP-CURRENT-IN-VSOURCE*)
	      (case *vsource-current-reference-time*
		(t			; :dt-midpoint
					; (s-flt (node-dvdt-n vsrc-node))
		 (node-dvdt-n vsrc-node))
					; (:last-time-point (* 0.5 (+ (node-dvdt-n vsrc-node) (node-dvdt-n-1 vsrc-node))))
		)
	      0.0d0)))

(defun get-vsource-current (vsrc)
  ;; Called after a successful time step, from SAVE-VSOURCE-DATA.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (if (not (vsource-blocked vsrc))
      (let* ((vsrc-node (vsource-node vsrc))
	     (vsrc-voltage (the sf (get-vsource-current-vsrc-voltage vsrc)))
	     (vsrc-node-voltage (the df (get-vsource-current-vsrc-node-voltage vsrc-node))))
	(if (and *vsource-intrinsic-current* (not (get-element-parameter-fast 'ideal-vsource (vsource-parameters vsrc))))
	    (* 1.0d0 (/ (- vsrc-voltage vsrc-node-voltage) (vsource-resistance vsrc)))
	    (let* ((vsrc-node-dvdt (the df (get-vsource-current-vsrc-node-dvdt vsrc-node)))
		   (current (the df (collect-node-currents-for-vsource vsrc-node vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt))))
	      (when (or *debug-vsource-voltage* *debug-vsource-current*)
		(debug-vsource-messages vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt current))
	      (s-flt current))))
      0.0))

(defun vsource-current-value (vsource) (get-vsource-current vsource))

(defun vsource-voltage-value (vsource) (get-vsource-current-vsrc-voltage vsource))

#|
;;; not used
(defun vsource-return-current (vsrc)
  (let ((vsource-supplied-current 0)(voltage (node-voltage-n+1 (vsource-node vsrc))))
    (dolist (elt (node-elements (vsource-node vsrc)))
      (setq vsource-supplied-current
	    (+ vsource-supplied-current
	       (cond ((eq (named-structure-symbol elt) 'isource)
		      (isource-current elt))
		     ((eq (named-structure-symbol elt) 'segment)
		      (let (v1 v2 v-diff i1 i2)
			(setf v1 (get-segment-voltage-1 elt))
			(setf v2 (get-segment-voltage-2 elt))
			(setf v-diff (- v1 v2))
					; calculate the current
			(setf i1 (* v-diff (segment-g-axial elt)))
			(setf i2 (+ (- i1) (* (segment-g-leak elt)
					      (- v2 (segment-v-leak elt)))))
			(if (eq (vsource-node vsrc) (segment-node-1 elt))
			    (- i1) (- i2))
			))
    
		     ((eq (named-structure-symbol elt) 'soma)
		      (- (+ (* (soma-g-shunt elt) voltage)
			    (* (soma-g-leak elt)
			       (- voltage (soma-v-leak elt))))))
		     ((eq (named-structure-symbol elt) 'channel)
		      (channel-current elt))
		     (t 0)))))
    vsource-supplied-current))


(defun vsource-return-charge ())

(defun vsource-return-conductance ())
|#

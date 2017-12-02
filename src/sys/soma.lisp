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


;;; SYS Source file: soma.lisp
(in-package "SURF-HIPPO")

; The soma model.

; There is no soma-type since the soma parameters are defined by the cell-type.

(defun soma-membrane-resistivity (soma)
  "The membrane resistivity of SOMA, in ohms-cm2." 
  (let* ((soma (if (soma-p soma) soma (element soma 'soma)))
	 (rm (unless (soma-inherit-parameters-from-type soma)
	       (get-a-value `rm (soma-parameters soma)))))
    (the sf (or rm (cell-type-rm-soma (cell-type (soma-cell soma)))))))
	    
(defun soma-membrane-resistivity (soma) (soma-rm soma))

(defun soma-specific-capacitance (soma)
  "The membrane specific capacitance of SOMA, in uF/cm2."
  (let* ((soma (if (soma-p soma) soma (element soma 'soma)))
	 (cm (unless (soma-inherit-parameters-from-type soma)
	       (get-a-value `cm (soma-parameters soma)))))
    (the sf (or cm (cell-type-cm-soma (cell-type (soma-cell soma)))))))
	      
(defun soma-v-leak (soma)
  "The reversal potential of the leak resistance of SOMA, in mV." 
  (let* ((soma (if (soma-p soma) soma (element soma 'soma)))
	 (v-leak (unless (soma-inherit-parameters-from-type soma)
		   (get-a-value `v-leak (soma-parameters soma)))))
    (the sf (or v-leak (cell-type-v-leak-soma (cell-type (soma-cell soma)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Soma segments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun soma-segments (&optional (target *cell*))
  "Returns a list of segments which are conceptually assigned to the actual soma for the cell associated with TARGET."
  (when *cell* (element-parameter (cell-soma (or (element-cell target) *cell*)) 'segments)))
  
(defun soma-segment-p (element)
  "Predicate for whether the segment associated with ELEMENT is a soma segment."
  (element-parameter (element-cell-element element) 'soma-segment))
  
(defun add-soma-segment (soma segment &optional initialize)
  "Assign the segments associated with SEGMENT to the SOMA. If INITIALIZE is T [default NIL], then first remove any already defined soma segments."
  (let* ((soma (element soma 'soma))
	 (prior-soma-segments (soma-segments soma))
	 (seg-or-segs (segments segment)))
    (when (and soma seg-or-segs)
      (when initialize (mapcar #'(lambda (seg) (element-parameter seg 'soma-segment nil)) prior-soma-segments))
      (loop for seg in seg-or-segs do (element-parameter seg 'soma-segment t))
      (element-parameter soma 'segments
			 (if initialize
			     seg-or-segs
			     (delete-duplicates (concatenate 'list seg-or-segs prior-soma-segments)))))))

(defun remove-soma-segment (soma seg)
  (let ((soma (element soma 'soma))
	(seg (element seg 'segment)))
    (element-parameter seg 'soma-segment nil)
    (element-parameter soma 'segments (remove seg (element-parameter soma 'segments)))))

(defun soma-segments-g-leak (&optional soma-or-cell)
  ;; in uS
  (loop for seg in (soma-segments soma-or-cell) summing (segment-g-leak seg)))

(defun soma-segments-cap (&optional soma-or-cell)
  ;; in nF
  (loop for seg in (soma-segments soma-or-cell) summing (segment-capacitance seg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zero-soma-p (&optional (target *cell*))
  (let ((soma (cell-soma (element-cell target))))
    (unless soma (sim-error (format nil "No soma associated with ~A!" target)))
    (zerop (soma-diameter soma))))

(defun print-soma (&optional (soma *soma*))
  (format t "~a (Cell: ~a~A): ~A~%"
	  (MASSAGE-ELEMENT-PLOT-LABEL soma nil t)
	  (cell-name (soma-cell soma))
	  (if (trunk-segments) (format nil "; Trunk segs: ~{~A ~}" (element-names (trunk-segments))) "")
	  (if *simulation-initialized* (format nil "Membrane voltage ~,2e mV @ ~,2e ms" (recorded-element-voltage soma) *real-time*) ""))
  (if (zero-soma-p soma)
      (format t "    Model soma has zero diameter~A.~%" (if (soma-segments soma) " - refer to soma segments" ""))
      (progn
	(format t "    ~aV-leak ~d mV"
		(if (and (> (soma-g-shunt soma) 1e-6) (soma-include-shunt soma))
		    (format nil " G-shunt ~,2e uS, " (if (> (soma-g-shunt soma) 1e-6)  (soma-g-shunt soma) 0)) "")
		(soma-v-leak soma))
	(format t ", G-leak ~,2e uS, Cap ~,2e nF, Diameter ~,2e um, Area ~,2e um2, Volume ~,2e um3~%"
		(s-flt (soma-g-leak soma)) (s-flt (soma-capacitance soma)) (s-flt (soma-diameter soma))
		(s-flt (element-area soma)) (s-flt (element-volume soma)))
	(when (element-parameter soma 'adjust-area-for-trunks)
	  (format t "    Soma area adjusted for abutting segment trunks.~%"))
	(when (element-parameter soma 'nucleus-diameter)
	  (format t "    Soma nucleus diameter ~,2e um, Volume ~,2e um3~%"
		  (s-flt (element-parameter soma 'nucleus-diameter)) (s-flt (sphere-volume-from-diameter (element-parameter soma 'nucleus-diameter)))))
	(when (element-parameter soma 'membrane-area-coefficient)
	  (format t "    Membrane area coefficient: ~,2e~%" (s-flt (element-parameter soma 'membrane-area-coefficient))))
	(when (element-parameter soma 'SOMA-CYLINDER)
	  (format t "    Soma has cylinder flag set~A~A~A~%"
		  (if (soma-cylinder-diameter soma) (format nil " - Cylinder diameter ~,2e um" (s-flt (soma-cylinder-diameter soma))) "")
		  (if (soma-cylinder-length soma) (format nil ", Length ~,2e um" (s-flt (soma-cylinder-length soma))) "")
		  (if (element-parameter soma 'SOMA-CYLINDRICAL-G-AXIAL) (format nil ", G-a ~,2euS" (s-flt (element-parameter soma 'SOMA-CYLINDRICAL-G-AXIAL))) "")))))
  (when (soma-segments soma)
    (format t
	    "    ~A Soma segments: G-leak ~,2e uS, Cap ~,2e nF, Area ~,2e um^2~%"
	    (length (soma-segments soma))
	    (s-flt (SOMA-SEGMENTS-g-leak soma))
	    (s-flt (SOMA-SEGMENTS-CAP soma))
	    (s-flt (element-area soma t))))
  (when (element-parameter (element-physical-node soma) 'constant-current)
    (format t "~%   ~,2e nA constant current injected into this soma." (s-flt (element-parameter (element-physical-node soma) 'constant-current))))
  (let ((elt-string (PRINT-CELL-ELEMENT-ELEMENTS soma)))
    (when (> (length elt-string) 0) (format t "    This soma has ~A.~%" elt-string))))
	      
(defun soma-cylinder-length (soma)
  (let* ((soma (element soma 'soma))
	 (params (element-parameters soma)))
    (and (get-a-value 'soma-cylinder params) (get-a-value 'length params))))

(defun soma-cylinder-diameter (soma)
  (let* ((soma (element soma 'soma))
	 (params (element-parameters soma)))
    (and (get-a-value 'soma-cylinder params) (get-a-value 'soma-cylindrical-diameter params))))

(defun soma-center-correction (soma)
  (or (element-parameter soma 'soma-center-correction)
      ;; If a series of circles parallel to the x-axis (as generated by ntscable) define the soma
      ;; outline, then calculate the center of mass for the enclosed volume to estimate the soma center.
      (cond ((element-parameter soma 'soma-points)
	     (let ((soma-points		; make copy so sort doesn't screw up original
		    (loop for soma-circle in (element-parameter soma 'soma-points) collect (copy-list soma-circle))))
	       (setq soma-points (sort soma-points '< :key 'car))
	       (let* ((last-circle (car soma-points))
		      (temp (loop for soma-circle in (cdr soma-points)
				  collect (* (square (* 0.5 (+ (car (last soma-circle)) (car (last last-circle)))))
					     (- (car soma-circle) (car last-circle)))
				  into volumes
				  collect (loop for i from 0 to 2
						for this-comp in soma-circle
						for last-comp in last-circle
						collect (* 0.5 (+ this-comp last-comp)))
				  into cone-coords
				  do (setq last-circle soma-circle)
				  finally (return (list volumes cone-coords)))))
		 (loop for volume in (first temp)
		       for cone-coords in (second temp)
		       summing (* volume (first cone-coords)) into x-total
		       summing (* volume (second cone-coords)) into y-total
		       summing (* volume (third cone-coords)) into z-total
		       summing volume into denominator
		       finally (return (list (/ x-total denominator) (/ y-total denominator) (/ z-total denominator)))))))
	    (t '(0.0 0.0 0.0)))))

(defun create-cylinder-soma (cell diameter &key length phi theta soma-name soma-parameters location
				  shunt	; in ohms
				  cell-type
				  distal-absolute-location cylinder-soma-segment-name)
  (let* ((cell (find-or-create-cell cell cell-type))
	 (soma-name (or soma-name (format nil "~a-soma" (cell-name cell))))
	 (location (or location (cell-origin cell)))
	 (length (or length (compute-segment-length location distal-absolute-location)))
	 (total-area (* pi-single diameter length))
	 (soma (create-soma :cell cell :diameter (sqrt (/ (* 0.5 total-area) pi-single))
			    :name soma-name
			    :parameters soma-parameters
			    :location location))
	 (soma-segment
	  (create-segment (or cylinder-soma-segment-name (format nil "~A-extension" soma-name))
			  soma
			  cell
			  :diameter diameter :length length
			  :theta theta :phi (or phi (* -0.5 pi-single))
			  :absolute-location distal-absolute-location
			  :parameter-a-list (list (cons 'membrane-area-coefficient 0.5)))))
    (when shunt (element-parameter soma 'soma-shunt shunt))
    (setq *circuit-processed* nil)
    (initialize-soma-voltage soma)
    (add-soma-segment soma soma-segment t)
    (element-parameter soma 'cylinder-soma-segment soma-segment)
    soma))

(defun create-symmetric-cylinder-soma (cell diameter 
					    &key location-1 location-2 soma-location length name parameters
					    shunt ; ohms
					    cell-type
					    (membrane-area-coefficient 1.0) segment-1-name segment-2-name)
  (let* ((cell (find-or-create-cell cell cell-type))
	 (name (or name (format nil "~a-soma" (cell-name cell))))
	 (length (or length (compute-segment-length location-1 location-2)))
	 (soma-location (or soma-location
			    (when (and location-1 location-2)
			      (mapcar #'(lambda (x) (/ x 1)) (mapcar '+ location-1 location-2)))
			    (cell-origin cell)))
	 ;; (total-area (* membrane-area-coefficient pi-single diameter length))
	 (segment-area-factor (* membrane-area-coefficient (/ 2 3.0)))
	 (soma-area-factor (* membrane-area-coefficient (/ length (* diameter 3))))
	 (soma (create-soma :cell cell
			    :diameter diameter
			    :name name
			    :parameters parameters
			    :location soma-location))
	 (soma-segment-1 (create-segment (or segment-1-name (format nil "~A-segment-1" name)) soma cell
					 :diameter diameter :length (/ length 2)
					 ; :theta (* -1 pi-single 0.5)
					 :relative-location location-1
					 :parameter-a-list (list (cons 'membrane-area-coefficient segment-area-factor))))
	 (soma-segment-2 (create-segment (or segment-2-name (format nil "~A-segment-2" name)) soma cell
					 :diameter diameter :length (/ length 2)
					 :relative-location location-2
					 ; :theta (* 1 pi-single 0.5)
					 :parameter-a-list (list (cons 'membrane-area-coefficient segment-area-factor)))))
    (setq *circuit-processed* nil)
    (when shunt (element-parameter soma 'soma-shunt shunt))
    (initialize-soma-voltage soma)
    (element-parameter soma 'membrane-area-coefficient soma-area-factor)
    (add-soma-segment soma soma-segment-1)
    (add-soma-segment soma soma-segment-2)
    (element-parameter soma 'cylinder-soma-segments (list soma-segment-1 soma-segment-2))
    (element-parameter soma 'cylinder-soma-segment-1 soma-segment-1)
    (element-parameter soma 'cylinder-soma-segment-2 soma-segment-2)
    soma))

(defun find-soma-name (soma-name cell enable-automatic-cell-names automatic-name-fixing)
  (let ((candidate-soma-name
	 (if (and (not *suppress-cell-element-simple-names*) *use-simple-names*)
	     (get-cell-element-simple-name)
	     (if soma-name
		 (if (numberp soma-name)
		     (if (segment-hash-table soma-name) (format nil "~A-soma" soma-name) soma-name)
		     (let ((candidate (format nil "~A" soma-name)))
		       (if (segment-hash-table candidate) (format nil "~A-soma" soma-name) candidate)))
		 (format nil "~a-soma" (or (element-name cell 'cell) "cell"))))))
    (if enable-automatic-cell-names
	(check-element-name candidate-soma-name 'soma :automatic-name-fixing automatic-name-fixing)
	candidate-soma-name)))

(defun create-soma (&key cell cell-type name 
			 (location '(0.0 0.0 0.0)) length soma-cylinder-diameter (diameter *default-soma-diameter*) ; microns
			 parameters adjust-area-for-trunks
			 shunt		; ohms
			 (enable-automatic-cell-names *enable-automatic-cell-names*) (automatic-name-fixing *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*))
  "Returns a soma element. N.B somas are typically created by CREATE-CELL.
DIAMETER is in microns. CELL refers to either a cell structure or the name of one - if not supplied, a new cell is created of
CELL-TYPE. SHUNT [ohms, default NIL], when non-NIL, is a non-specific somatic shunt. LOCATION gives the xyz coordinates of the
SOMA in microns. When ADJUST-AREA-FOR-TRUNKS is T [default nil], then the soma area [as returned by the ELEMENT-AREA and
ELEMENT-AREA-CM2 functions] is adjusted for the areas of the faces of any abutting segments."
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((actual-soma-name (find-soma-name name cell enable-automatic-cell-names automatic-name-fixing))
	 (cell (find-or-create-cell (or cell (if name (format nil "~a-cell" name) "soma-cell")) cell-type))
	 soma)
    (if (setq soma (SOMA-HASH-TABLE actual-soma-name))
      (sim-warning (format nil "CREATE-SOMA: soma ~a already defined, ignoring" actual-soma-name))
      (let ((node (create-node actual-soma-name :cell cell :is-physical-cell-node t)))
	(setq soma (make-soma :name actual-soma-name :node node :diameter (s-flt diameter) :parameters parameters))
	(setq *circuit-processed* nil)
	(setf (cell-soma cell) soma
	      (node-elements node) (cons soma (node-elements node))
	      (SOMA-HASH-TABLE actual-soma-name) soma)
	(when shunt (element-parameter soma 'soma-shunt shunt))
	(initialize-soma-voltage soma)
	(element-parameter soma 'center-correction (soma-center-correction soma))
	(setf (node-absolute-location node) (or location (cell-origin cell)))
	(setf (node-relative-location node) (or location (cell-origin cell)))
	(element-parameter soma 'adjust-area-for-trunks adjust-area-for-trunks)
	(when length
	  (element-parameter soma 'soma-cylinder t)
	  (element-parameter soma 'length (s-flt length))
	  (element-parameter soma 'soma-cylinder-diameter (s-flt (or soma-cylinder-diameter diameter)))
	  (element-parameter soma 'soma-cylindrical-g-axial (g-axial (element-parameter soma 'length)
								     (element-parameter soma 'soma-cylinder-diameter)
								     (cell-type-ri (cell-type cell)))))))
    (setq *soma* soma)))

(defun edit-soma (soma) (menu-for-somas soma))

(defun menu-for-somas (&optional soma-arg)
  (let ((somas (coerce-to-list (element (or soma-arg (select-hash-values-menu (SOMA-HASH-TABLE) "Select Somas" :punt-if-only-one-entry t)) 'soma)))
	check-conc-ints)
    (loop for soma in somas do
	  (let ((cell-type-name (cell-type-name (cell-type (soma-cell soma))))
		(dummy1 (soma-diameter soma))
		(dummy2 (soma-include-shunt soma))
		(dummy3 (cell-type-soma-shunt (cell-type (soma-cell soma))))
		(dummy4 (or (element-parameter soma 'nucleus-diameter) 0.0))
		(dummy5 (element-parameter soma 'adjust-area-for-trunks)))
	    (choose-variable-values
	     `((dummy1 "Soma diameter [uM]" :float)
	       (dummy4 "Soma nucleus diameter [uM] (used for concentrations)" :float)
	       (dummy5 "Adjust surface area for proximal trunks" :boolean)
	       (dummy2 "Include soma shunt" :boolean)
	       ,(when dummy2 `(dummy3 ,(format nil "~A Cell type soma membrane shunt [ohms]" cell-type-name) :float)))
	     :label (format nil "Setting up parameters of soma ~A" (soma-name soma)))
	    (when (and (not (soma-include-shunt soma)) dummy2)
	      (choose-variable-values
	       `((dummy3 ,(format nil "~A Cell type soma membrane shunt [ohms]" cell-type-name) :float))))
	    (unless (= (or (element-parameter soma 'nucleus-diameter) 0) dummy4)
	      (setq check-conc-ints t))
	    (element-parameter soma 'adjust-area-for-trunks dummy5) 
	    (element-parameter soma 'nucleus-diameter (when (> dummy4 0) dummy4)) 
	    (setf (cell-type-soma-shunt (cell-type (soma-cell soma))) dummy3)
	    (setf (soma-diameter soma) dummy1)
	    (setf (soma-include-shunt soma) dummy2)))
    (set-somas-membrane-parameters)
    (when check-conc-ints (set-conc-integrators-parameters))
    (UPDATE-LINEAR-Z-IN-CELLS)))

(defun set-soma-voltage (soma voltage) (set-node-voltage (soma-node soma) voltage))

(defun set-soma-voltage-df (soma voltage) (set-node-voltage-double (soma-node soma) voltage))

(defun initialize-soma-voltage (soma &optional v-leak)
  (let ((v-leak (or v-leak (cell-type-v-leak-soma (cell-type (soma-cell soma))))))
    (set-soma-voltage soma (or (element-holding-potential soma) v-leak))))

(defun set-somas-membrane-parameters (&optional ignore-membrane-elements cell-type)
  (loop for soma being the hash-value of (soma-hash-table)
	when (or (not cell-type) (eq cell-type (cell-type (soma-cell soma))))
	do (set-soma-membrane-parameters soma ignore-membrane-elements)))

(defun set-soma-absolute-parameters (soma capacitance g-leak)
  "Set linear membrane properties of SOMA to the absolute values of CAPACITANCE [nF] and G-LEAK [uS]. Sets
:INHERIT-PARAMETERS-FROM-TYPE of SOMA to NIL. If any of the soma parameter arguments are NIL, then the original value is
retained."
  (loop for soma in (coerce-to-list (element soma 'soma)) do
	(cond-every (capacitance (setf (soma-capacitance soma) (s-flt capacitance)))
		    (g-leak (setf (soma-g-leak soma) (s-flt g-leak))))
	(setf (soma-inherit-parameters-from-type soma) nil)
	;; Adjust local membrane parameters to reflect absolute value assignment.
	(element-parameter soma 'rm (s-flt (* 1e6 (/ (element-area-cm2 soma) (soma-g-leak soma)))))
	;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
	(element-parameter soma 'cm (s-flt (* 1e-3 (/ (soma-capacitance soma) (element-area-cm2 soma)))))))

(defun set-soma-parameter (soma parameter value)
  "Set a PARAMETER distinct from the associated cell type for somas associated with SOMA, for example 'RM, 'CM, or 'V-LEAK. Sets
:INHERIT-PARAMETERS-FROM-TYPE for SOMA to NIL."
  (let ((value (s-flt value)))
    (loop for soma in (coerce-to-list (element soma 'soma)) do
	  (setf (soma-inherit-parameters-from-type soma) nil)
	  (element-parameter soma parameter value)
	  (set-soma-membrane-parameters soma))))

(defun set-soma-membrane-parameters (soma &optional ignore-membrane-elements)
  ;; Set soma membrane properties, according to the membrane parameters for the appropriate cell type.
  (let* ((cell-type (cell-type (soma-cell soma)))
	 ;; (diameter (soma-diameter soma))
	 (length (soma-cylinder-length soma))
	 (cylinder-diameter (soma-cylinder-diameter soma))
	 (soma-cylinder (element-parameter soma 'soma-cylinder))
	 (area (element-area-cm2 soma))) ;cm squared
    (when (soma-inherit-parameters-from-type soma)
      (setf ; (soma-v-leak soma) (cell-type-soma-v-leak cell-type)
	    (soma-g-leak soma) (* 1d6 (/ area (cell-type-rm-soma cell-type)))
	    (soma-g-shunt soma) (/ 1e6 (or (element-parameter soma 'soma-shunt)
					   (cell-type-soma-shunt cell-type)))
	    ;;specific capacitance is in microF/cm-sq, capacitance is in nF
	    (soma-capacitance soma) (* 1d3 area (cell-type-cm-soma cell-type))))
    (when (and soma-cylinder length cylinder-diameter)
      (element-parameter soma 'soma-cylindrical-g-axial (g-axial length cylinder-diameter (cell-type-ri (cell-type (soma-cell soma))))))
    (set-g-leak*v-leaks soma)
    (unless ignore-membrane-elements
      (loop for elt in (node-elements (soma-node soma)) unless (eq elt soma) do (set-element-membrane-parameters elt t)))))

(defun get-soma-voltage-sf (soma) (get-soma-voltage soma))

(proclaim '(inline get-soma-voltage-df))
(defun get-soma-voltage-df (soma) (node-voltage-n (soma-node soma)))

(defun get-soma-voltage (soma) (coerce (get-soma-voltage-df soma) 'single-float))

(defun soma-voltage-sf (soma)
  (typecase soma
    (soma (get-soma-voltage-sf soma))
    (t (let ((soma (element soma 'soma)))
	 (when soma (get-soma-voltage-sf soma))))))

(defun soma-voltage (&optional (soma *soma*))
  "Retrun the voltage of optional SOMA [default *soma*]."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (the sf (soma-voltage-sf soma)))

(defun soma-voltage-df (soma)
  (typecase soma
    (soma (get-soma-voltage-df soma))
    (t (let ((soma (element soma 'soma)))
	 (when soma (get-soma-voltage-df soma))))))

(proclaim '(inline get-soma-voltage-double))
(defun get-soma-voltage-double (soma) (the df (node-voltage-n (soma-node soma))))

(proclaim '(inline eval-soma))
(defun eval-soma (soma)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type soma soma))
  (let* ((alpha-cap-double (* (the df (*2/delta-t[n]*)) (soma-capacitance soma)))
	 (node (soma-node soma))
	 (node-double-floats (node-double-floats node)))
    (declare (double-float alpha-cap-double))
    (when *debug-segments*
      (format t " Soma: *2/delta-t[n]* = ~f, cap= ~f, alpha-cap=~f,V-n=~f~%" (*2/delta-t[n]*) (soma-capacitance soma) alpha-cap-double (node-voltage-n node))
      (format t " Soma ~A: alpha-charge = [alpha-cap * V-n] + alpha-charge ~%" (soma-name soma))
      (format t "  (~f +  ~f) => " (* (node-voltage-n node) alpha-cap-double) (node-alpha-charge node)))
    (setf
     (node-aref-alpha-charge node-double-floats) (+ (node-aref-alpha-charge node-double-floats) (* alpha-cap-double (node-aref-voltage-n node-double-floats)))
     (node-aref-jacobian node-double-floats) (+ (node-aref-jacobian node-double-floats) alpha-cap-double))
    (when *debug-segments* (format t "      Result: ~f~%" (node-alpha-charge node)))
    nil))

(defun eval-all-somas ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (do ((i 0 (1+ (the fn i))))
      ((= (the fn i) (the fn *soma-array-length*)))
    (eval-soma (aref (the (simple-array soma (*)) *soma-array*) i))))

(defun make-soma-array ()
  (loop for soma being the hash-value of (soma-hash-table)
	unless (node-has-ideal-voltage-source (soma-node soma))
	collect soma into somas
	finally (setf *soma-array* (list-to-array somas)
		      *soma-array-length* (length somas))))

(defun init-somas () (loop for soma being the hash-value of (soma-hash-table) do (set-g-leak*v-leaks soma)))

(defun set-g-leak*v-leaks (soma)
  (let ((g-linear-membrane (+ (if (soma-include-shunt soma) (soma-g-shunt soma) 0.0) (soma-g-leak soma))))
    (setf (soma-g-leak*v-leak soma) (coerce (* g-linear-membrane (soma-v-leak soma)) 'double-float))))
    
(defun initialize-soma-node-jacobians-and-currents ()
  ;; Calculate the time invariant component of the node jacobians, which consist of segment g-axial terms, and soma and segment g-leak terms.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for soma being the hash-value of (soma-hash-table) do
	(unless (node-has-ideal-voltage-source (soma-node soma))
	  (deccumulate-setf (node-const-current (soma-node soma)) (soma-g-leak*v-leak soma))
	  (accumulate-setf (node-const-jacobian (soma-node soma)) (+ (if (soma-include-shunt soma) (soma-g-shunt soma) 0.0) (soma-g-leak soma))))))
			 
(defun soma-dendrite-current (&optional (soma *soma*))
  (let* ((node (soma-node soma))
	 (voltage (node-voltage-n+1 node))
	 (proximal-soma-segment (proximal-soma-segment soma)))
    (if proximal-soma-segment
	(s-flt (loop for seg in (distal-segments proximal-soma-segment)
		     summing (* (segment-g-axial seg) (- voltage (node-voltage-n+1 (segment-node-2 seg))))))
	(s-flt (loop for elt in (node-elements node) when (and (segment-p elt) (eq node (segment-node-1 elt)))
		     summing (* (segment-g-axial elt) (- voltage (node-voltage-n+1 (segment-node-2 elt)))))))))

(defun add-channel-to-soma (type &optional (soma *soma*)) (add-channel-to-locations (or (soma-segments) soma) type))

(defun proximal-soma-segment (&optional (soma *soma*))
  (loop for seg in (soma-segments soma)
	when (eq (soma-node soma) (segment-node-1 seg))
	do (return seg)))
 

;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10; -*- Lyle Borg-Graham, Equipe
;; Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SYS Source file: cable_functions.lisp

(in-package "SURF-HIPPO")

;; Functions for evaluating electrical properties of somas, cables and cable compartments.

(proclaim '(inline g-element))
(defun g-element (element g-density)
  "G-DENSITY is in pS per square micron. Conductance returned is in uS."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float g-density))
  (the sf (* (the sf (element-area element)) ; um2
	     (the sf (* 1e-6		; Convert pS to uS
			g-density)))))

(proclaim '(inline cap-mem))
(defun cap-mem (length diameter cm)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float length diameter cm))
  ;; (* 1.0e-8 1.0e3 pi-single) = 3.1415927e-5
  (* 3.1415927e-5 diameter length cm))

(defun capacitance-mem (length diameter cm)
  "Returns membrane capacitance in nF of cable with dimensions LENGTH and DIAMETER [both in microns]. CM is in units of uF/cm2."
  (cap-mem length diameter cm))

(proclaim '(inline g-leak-mem))
(defun g-leak-mem (length diameter rm)
  "Returns membrane leak conductance in uS of cable with dimensions LENGTH and DIAMETER [both in microns]. RM is in units of
ohms-cm2."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float length diameter rm))
  (/ (* 1.0e-8				; cm2/um2
	diameter			; um
	length				; um
	1.0e6				; uS/ohm
	pi-single)
     rm))

(proclaim '(inline g-axial))
(defun g-axial (length diameter ri)
  "Returns axial conductance in uS of cable with LENGTH and DIAMETER in microns, and RI in ohms-cm."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float length diameter ri))
  ;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
  (/ (* 78.53982 diameter diameter)
     (* length ri)))

(defun g-soma (radius resistivity)
  "Return conductance in uS of a spherical membrane with RADIUS in microns, and specific RESISTIVITY in ohms-cm2."
  (* 1.0e6 (/ (sphere-area-cm2 radius) resistivity)))

(defun cap-soma (radius capacitance)
  "Return capacitance in nF of a spherical membrane with RADIUS in microns, and specific CAPACITANCE in uF/cm2."
  (* 1.0e3 (* (sphere-area-cm2 radius) capacitance )))

(defun r-in-soma-short-cable (ri rm a-um l-um a-soma-um rm-soma)
  "Returns somatic input resistance (ohms) to soma-short-cable structure with soma radius A-SOMA-UM in microns, soma membrane
resistivity RM-SOMA and cable membrane resistivity RM in ohm-cm2. Intracellular resistivity RI is in ohm-cm, and cable radius
A-UM is in microns."
  (/ 1.0 (+ (* 1.0e-6 (g-soma a-soma-um rm-soma))
	    (/ (* 1.0e6 (z-cable-in ri rm a-um l-um))))))

(defun max-g-in (&optional (cell *cell*) (exclude-electrodes t))
  "Linear somatic input conductance (uS) of CELL, with the cytoplasmic resistivity set to zero."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((electrodes (electrodes))
	(cell-internal (element cell 'cell)))
    (if (not cell-internal)
      (sim-error (format nil "~A is not a cell" cell))
      (let ((g-in (+ (soma-g-leak (cell-soma cell-internal))
		     (if (soma-include-shunt (cell-soma cell-internal))
		       (soma-g-shunt (cell-soma cell-internal))
		       0.0))))
	(loop for segment in (cell-segments cell-internal)
	      when (or (not exclude-electrodes) (not (member segment electrodes :test #'eq)))
	      sum (segment-g-leak segment) into result double-float
	      finally (return (s-flt (+ result g-in))))))))

(defun cell-cap (&optional (cell *cell*) (exclude-electrodes t))
  "Total capacitance of the CELL in nF."
  (let ((cell-internal (element cell 'cell)))
    (if (not cell-internal)
      (sim-error (format nil "~A is not a cell" cell))
      (let ((segments (if exclude-electrodes (segments-not-electrodes cell-internal) (cell-segments cell-internal))))
	(s-flt (+ (soma-capacitance (cell-soma cell-internal))
		  (loop for seg in segments sum (segment-capacitance seg))))))))

(defun cable-lambda (rm ri diameter)
  ;; For backward compatibility. Return electrotonic length constant in microns for cable with  RM in ohms-cm2, RI in ohms-cm, DIAMETER in microns.
  (* 1e4 (sqrt (/ (* rm diameter 0.25 1e-4) ri))))

(proclaim '(inline lambda-cable))
(defun lambda-cable-jnt (ri rm a-um)
  ;; Cable electrotonic space constant in cm. Intracellular resistivity RI in ohm-cm, membrane resistivity RM in ohm-cm2, cable radius A-UM in microns.
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((Rm (* 1e-3 rm))		; kohm-cm2
	(Ri (* 1e-3 ri))		; kohm-cm
	(a (* 1e-4 a-um)))		; cm
    (s-flt (sqrt (/ (* Rm a)
		    (* 2 Ri))))))
  
(defun lambda-cable (ri rm a-um)
  "Cable electrotonic space constant in cm. Intracellular resistivity RI in ohm-cm, membrane resistivity RM in ohm-cm2, cable radius A-UM in microns."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (s-flt (sqrt (/ (* 1.0e-4 a-um rm)
		  (* 2.0 ri)))))

(defun lambda-cable-um (ri rm a-um)
  "Cable electrotonic space constant in microns. Intracellular resistivity RI is in ohm-cm, membrane resistivity RM is in ohm-cm2,
and cable radius A-UM is in microns."
  (* 1e4 (lambda-cable ri rm a-um)))

(defun length-from-lambda (ri rm a-um L)
  "Returns cable length in um given intracellular resistivity RI [ohm-cm], membrane resistivity RM [ohm-cm2], cable radius A-UM [microns], and electrotonic
length L [dimensionless]." 
  (* 10000				; lambda-cable returns cm
     (lambda-cable ri rm a-um))
  L)
 
(proclaim '(inline electrotonic-length))
(defun electrotonic-length (length diameter ri rm &optional (ri-coefficient 1.0))
  "Returns electrotonic length of segment given explicit parameters LENGTH (uM), DIAMETER (uM), RI (ohms-cm),  RM (ohms-cm2), and RI-COEFFICIENT (dimensionless)."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (s-flt (/ length
	    (lambda-cable-um
	     (* ri ri-coefficient)
	     rm
	     (* 0.5 diameter)))))
		
(defun segment-electrotonic-length (segment)
  "Returns electrotonic length of SEGMENT."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((seg (element segment 'segment)))
    (if (not seg)
	(sim-error (format nil "~A is not a segment" segment))
	(let ((membrane-factor (get-a-value 'membrane-area-coefficient (segment-parameters seg))))
	  (electrotonic-length
	   (segment-length seg)
	   (segment-diameter seg)
	   (segment-ri seg)
	   (if membrane-factor
	       (/ (segment-membrane-resistivity seg) membrane-factor)
	       (segment-membrane-resistivity seg))
	   (segment-ri-coefficient seg))))))

(proclaim '(inline g-inf-in))
(defun g-inf-in (ri rm a-um &optional lambda-cable)
  "Input conductance of semi-infinite cable, in uS. Intracellular resistivity RI is in ohm-cm, membrane resistivity RM is in
ohm-cm2, and cable radius A-UM is in microns."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((a (* 1.0e-4 a-um))		; Convert to cm
	(lambda-cable (or lambda-cable (lambda-cable ri rm a-um))))
    (s-flt (/ (* pi-single a a 1e6)
	      (* ri lambda-cable)))))

(proclaim '(inline z-cable-in))
(defun z-cable-in (ri rm a-um l-um &optional (g-end 0.0))
  "Returns input resistance (Mohms) to sealed-end (open circuit) cable of length L-UM in microns.  Intracellular resistivity RI
is in ohm-cm, membrane resistivity RM is in ohm-cm2, and cable radius A-UM is in microns. Optional G-END is in uS."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((l (s-flt (* 1.0e-4 l-um)))
	 (lambda-cable (lambda-cable ri rm a-um))
	 (g-inf (the sf (g-inf-in ri rm a-um lambda-cable)))
	 (b-1 (s-flt (/ g-end g-inf)))
	 (tanh-l/lamb (the sf (tanh (the sf (/ l (the sf lambda-cable)))))))
    (declare (type single-float l g-inf b-1 tanh-l/lamb lambda-cable))
    (/ 1.0
       (* g-inf (/ (+ b-1 tanh-l/lamb)
		   (+ 1.0 (* b-1 tanh-l/lamb)))))))

(defun z-cable-in-seg (segment &key store-segment-z-cable-in)
  "Returns input resistance (Mohms) of SEGMENT, taking into account the tree distal to the segment, using the cable parameters."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((seg (element segment 'segment)))
    (if (not seg)
      (sim-error (format nil "~A is not a segment" segment))
      (let* ((membrane-factor (get-a-value 'membrane-area-coefficient (segment-parameters seg)))
	     (g-end (loop for distal-segment in (distal-segments seg)
			  summing (/ 1.0 (the sf (z-cable-in-seg distal-segment)))
			  into result single-float finally (return result)))
	     (z-cable-in-seg (z-cable-in
			      (the sf (* (segment-ri seg) (segment-ri-coefficient seg)))
			      (if membrane-factor
				(the sf (/ (segment-membrane-resistivity seg) (s-flt membrane-factor)))
				(segment-membrane-resistivity seg))
			      (* 0.5 (segment-diameter seg))
			      (segment-length seg)
			      g-end)))
	(when (and (or store-segment-z-cable-in *store-segment-z-cable-in*)
		   (not (get-element-parameter-fast 'soma-segment (segment-parameters seg))))
	  (set-element-parameter-fast seg 'g-cable-end g-end (segment-parameters seg))
	  (set-element-parameter-fast seg 'z-cable-in z-cable-in-seg (segment-parameters seg)))
	(values z-cable-in-seg g-end)))))

(defun segment-g-end (segment)
  (or (get-element-parameter-fast 'g-end (segment-parameters segment))
      (nth-value 1 (z-cable-in-seg segment :store-segment-z-cable-in t))))

(defun segment-g-cable-end (segment)
  (or (get-element-parameter-fast 'g-cable-end (segment-parameters segment))
      (nth-value 1 (z-cable-in-seg segment :store-segment-z-cable-in t))))

(defun segment-g-end-discrete (segment)
  (or (get-element-parameter-fast 'g-discrete-end (segment-parameters segment))
      (nth-value 1 (z-discrete-in-seg segment :store-segment-z-discrete-in t))))

(defun clear-all-z-cable-in (&optional cell-or-type)
  (let ((cell-or-type (element cell-or-type)))
    (if (cell-type-p cell-or-type)
	(loop for cell in (cell-type-cells cell-or-type) do (clear-all-z-cable-in cell))
	(loop for segment in (if (cell-p cell-or-type)
				 (cell-segments cell-or-type)
				 (segments))
	      do
	      (set-element-parameter-fast segment 'g-cable-end nil (segment-parameters segment))
	      (set-element-parameter-fast segment 'z-cable-in nil (segment-parameters segment))
	      (set-element-parameter-fast segment 'g-discrete-end nil (segment-parameters segment))
	      (set-element-parameter-fast segment 'z-discrete-in nil (segment-parameters segment))))))
			    
(defun clear-z-cable-in-to-soma (seg)
  (loop for segment in (segments-in seg) do
	(set-element-parameter-fast segment 'g-cable-end nil (segment-parameters segment))
	(set-element-parameter-fast segment 'z-cable-in nil (segment-parameters segment))
	(set-element-parameter-fast segment 'g-discrete-end nil (segment-parameters segment))
	(set-element-parameter-fast segment 'z-discrete-in nil (segment-parameters segment))))

(defun z-tree-cable-in-cell-from-stored-values (&optional (cell *cell*) include-virtual-soma)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (when (cell-tree-p cell)
    (the df (/ 1.0 
	       ;;Start at the cell soma, and work on each of the branches that originate there in turn.
	       (loop for element in (if include-virtual-soma
					(loop for elt in (node-elements (soma-node (cell-soma cell)))
					      when (typecase elt (segment t)) collect elt)
					(trunk-segments cell))
		     sum (/ 1.0 (the sf (or (get-cell-element-param-fast element 'z-cable-in)
					    (z-cable-in-seg element :store-segment-z-cable-in t))))
		     into result single-float finally (return result))))))

(defun r-in (&optional (cell *cell*))
  "Returns input resistance (Mohms) of CELL, using the cable parameters for the dendritic tree if there is one."
  (z-cable-in-cell cell))

(defun g-in (&optional (cell *cell*))
  "Returns input conductance (uS) of CELL, using the cable parameters for the dendritic tree if there is one."
  (/ (r-in cell)))
    
(defun z-cable-in-cell (&optional (cell *cell*) z-tree)
  "Returns input resistance (Mohms) of CELL, using the cable parameters for the dendritic tree if Z-TREE is not
supplied. Otherwise, the input resistance is calculated from the soma resistance and the Z-TREE argument (Mohms)."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((cell-internal (element cell 'cell)))
    (if (not cell-internal)
	(sim-error (format nil "~A is not a cell" cell))
	(/ 1.0 (+ (soma-g-leak (cell-soma cell-internal))
		  (if (soma-include-shunt (cell-soma cell-internal)) (soma-g-shunt (cell-soma cell-internal)) 0.0)
		  (s-flt (cond (z-tree (/ 1.0 z-tree))
			       ((cell-tree-p cell-internal) (/ 1.0 (z-tree-cable-in-cell cell-internal t)))
			       (t 0.0))))))))

(defun cell-tree-p (cell) (loop for seg in (node-segments (cell-soma cell)) thereis (not (electrode-p seg))))

(defun z-tree-cable-in-cell (&optional (cell *cell*) include-virtual-soma)
  "Returns input resistance (Mohms) of dendritic tree of CELL, using the cable parameters. If no tree, returns NIL. If
INCLUDE-SOMA-SEGMENTS is T, include any segments assigned to the soma."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((cell-internal (element cell 'cell)))
    (if (not cell-internal)
      (sim-error (format nil "~A is not a cell" cell))
      (when (cell-tree-p cell-internal)
	(/ 1.0
	   ;; Start at the cell soma, and work on each of the branches that originate there in turn.
	   (loop for element in (if include-virtual-soma (element-segments (cell-soma cell-internal)) (trunk-segments cell-internal))
		 sum (/ 1.0 (the sf (z-cable-in-seg element :store-segment-z-cable-in t)))
		 into result single-float finally (return result)))))))

(defun z-discrete-in-seg (segment &key (look-distally t) explicit-g-end store-segment-z-discrete-in)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((g-end (cond
		  (look-distally (loop for distal-seg in (distal-segments segment)
				       summing (the df (/ 1 (the df (z-discrete-in-seg distal-seg :store-segment-z-discrete-in store-segment-z-discrete-in))))
				       into result double-float finally (return result)))
		  (explicit-g-end (the df explicit-g-end))
		  (t 0.0d0)))
	 (g-temp (+ (the df g-end) (segment-g-leak segment)))
	 (z-discrete-in-seg (/ (+ (segment-g-axial segment) g-temp)
			       (* (segment-g-axial segment) g-temp))))			
    (when (and (or store-segment-z-discrete-in *store-segment-z-discrete-in*)
	       (not (get-element-parameter-fast 'soma-segment (segment-parameters segment))))
      (set-element-parameter-fast segment 'g-discrete-end g-end (segment-parameters segment))
      (set-element-parameter-fast segment 'z-discrete-in z-discrete-in-seg (segment-parameters segment)))
    (values z-discrete-in-seg g-end)))

(defun z-discrete-in-cell (&optional (cell *cell*) z-tree)
  "Returns somatic input resistance (Mohms) of CELL, using the compartmental network parameters."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((cell-internal (element cell 'cell)))
    (if (not cell-internal)
      (sim-error (format nil "~A is not a cell" cell))
      (/ 1.0 (+ (soma-g-leak (cell-soma cell-internal))
		(the sf (if (soma-include-shunt (cell-soma cell-internal))
                          (soma-g-shunt (cell-soma cell-internal))
                          0.0))
		(cond (z-tree (/ 1.0 z-tree))
		      ((cell-tree-p cell-internal) (/ 1.0 (z-tree-discrete-in-cell cell-internal t)))
		      (t 0.0)))))))

(defun z-tree-discrete-in-cell-from-stored-values (&optional (cell *cell*) include-virtual-soma)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((cell-internal (element cell 'cell)))
    (if (not cell-internal)
      (sim-error (format nil "~A is not a cell" cell))
      (when (cell-tree-p cell-internal)
	(s-flt (/ 1.0 
		  ;;Start at the cell soma, and work on each of the branches that originate there in turn.
		  (loop for element in (if include-virtual-soma
					 (loop for elt in (node-elements (soma-node (cell-soma cell-internal)))
					       when (typecase elt (segment t)) collect elt)
					 (trunk-segments cell-internal))
			sum (the df (/ 1.0 (the df (or (get-cell-element-param-fast element 'z-discrete-in)
						       (z-discrete-in-seg element :store-segment-z-discrete-in t)))))
			into result double-float finally (return result))))))))

(defun z-tree-discrete-in-cell (&optional (cell *cell*) include-virtual-soma)
  "Returns input resistance (Mohms) of dendritic tree of CELL, using the compartmental network parameters. If no tree, returns
NIL. If INCLUDE-SOMA-SEGMENTS is T, include any segments assigned to the soma."
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (unless *circuit-processed* (process-circuit-structure))
  (let ((cell-internal (element cell 'cell)))
    (if (not cell-internal)
      (sim-error (format nil "~A is not a cell" cell))
      (when (cell-tree-p cell-internal)
	(s-flt
	 (/ 1.0 
	    ;;Start at the cell soma, and work on each of the branches that originate there in turn.
	    (loop for element in (if include-virtual-soma
				   (loop for elt in (node-elements (soma-node (cell-soma cell-internal))) when (typecase elt (segment t)) collect elt)
				   (trunk-segments cell-internal))
		  sum (the df (/ 1.0 (the df (z-discrete-in-seg element :store-segment-z-discrete-in t))))
		  into result double-float finally (return result))))))))

(defun rho (&optional (cell *cell*))
  "Tree/soma conductance ratio of cell associated with CELL."
  (let ((cell (element-cell cell)))
    (if (zero-soma-p cell)
	(sim-error (format nil "The soma associated with ~A has zero diameter!" cell))
	(when cell
	  (/ 1.0 (* (z-tree-discrete-in-cell cell t)
		    (+ (soma-g-leak (cell-soma cell))
		       (if (soma-include-shunt (cell-soma cell)) (soma-g-shunt (cell-soma cell)) 0.0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ;;;
;;; Some protocols    ;;;
;;;                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun steady-state-linear-segment-voltage-clamp-voltage (segment distal-voltage &optional setit go-on)
  (let ((steady-state-segment-voltage-rel
	 (/ (+ (* distal-voltage (segment-g-axial segment))
	       ; (* (segment-v-leak segment) (segment-g-leak segment))
	       )
	    (+ (segment-g-end-discrete segment) (segment-g-axial segment) (segment-g-leak segment)))))
    (when (or go-on setit)
      (set-segment-voltage segment (+ steady-state-segment-voltage-rel (segment-v-leak segment))))
    (when go-on
      (loop for distal-segment in (distal-segments segment) do
	    ;; (format t "clamping distally ~A at ~A, distal v ~A ..~%" distal-segment (segment-voltage segment) distal-voltage)
	    (steady-state-linear-segment-voltage-clamp-voltage distal-segment steady-state-segment-voltage-rel t t)))
    steady-state-segment-voltage-rel))

(defun steady-state-linear-voltage-clamp (vsource &optional holding-potential)
  (unless *circuit-processed* (process-circuit-structure))
  (let* ((vsource-element (element-cell-element vsource))
	 (cell (element-cell vsource))
	 (soma (cell-soma cell))
	 (holding-potential (or holding-potential (element-current-value vsource 'voltage)))
	 (soma-holding-voltage holding-potential)
	 (soma-electrode-p (and (attached-to-soma-p vsource-element) (electrode-p vsource-element))))
    (if (not (or (soma-p vsource-element) soma-electrode-p))
	(format t "STEADY-STATE-LINEAR-VOLTAGE-CLAMP only works from soma or soma electrodes for now...~%")
	(progn
	  ;; First, setup all the g-end values.
	  (loop for seg in (trunk-segments vsource-element) do (segment-g-end-discrete seg))

	  (when soma-electrode-p
	    (setq soma-holding-voltage
		  (+ (soma-v-leak soma)
		     (/ (* (- holding-potential (soma-v-leak soma)) (cell-z-discrete-in-cell cell))
			(+ (ELECTRODE-RESISTANCE vsource-element) (cell-z-discrete-in-cell cell))))))
	  (set-element-voltage soma soma-holding-voltage)
			       
	  ;; Now apply the clamp.
	  (loop for seg in (trunk-segments vsource-element)
		;; do (format t "clamping ~A at ~AmV..~%" seg (- soma-holding-voltage (segment-v-leak seg)))
		unless (eq seg vsource-element)
		do
		(steady-state-linear-segment-voltage-clamp-voltage
		 seg (- soma-holding-voltage (segment-v-leak seg))
		 t t))))))
		
      
(defun estimate-r-input-limits (cell-name)
  (when (Y-OR-N-P-DEFAULT-NO "Do you want to change the cell membrane?") (set-circuit-elements-parameters))
  (let ((g-in (max-g-in (CELL-HASH-TABLE cell-name))))
    (format t "~%Cell ~a has a maxmimum G-in = ~,2e uS, or minimum R-in = ~,2e Mohms ~%"
	    cell-name g-in (/ 1.0 g-in))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometric stuff - see also trees.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cell-area (&optional (cell *cell*))
  "Returns the total membrane area of cell associated with CELL in square microns."
  (element-area (element-cell cell)))

(defun print-area-info (&optional (stream t))
  (loop for cell in (cells)
	do (format stream "Cell ~A: soma area ~,2f um2, tree area ~,2f um2~%"
		   (cell-name cell) (element-area (cell-soma cell)) (tree-area cell))))

(defun print-rall-info (&optional (stream t))
  (loop for cell in (cells) do
	(format stream
		"Cell ~A: 2/3 Power of sum of ~d trunk diameter~:p, each raised to 3/2 [um] - ~,2f~%"
		(cell-name cell) (length (TRUNK-SEGMENTS cell))
		(expt (loop for seg in (TRUNK-SEGMENTS cell)
			    sum (expt (segment-diameter seg) 1.5)) (/ 2.0 3.0)))))


#| ANTIQUE

;;; G-EX-SYN Returns excitatory synaptic conductance in uS of cable with dimensions length and diameter (both in microns). Uses
;;; global variable *g-ex-mem, which is in units of (ohms-cm-cm)^-1.
(defun g-ex-syn (length diameter)
  (* 1.0e6 pi-single diameter length 1.0e-8  *g-ex-mem))

;;; G-IN-SYN Returns inhibitory synaptic conductance in uS of cable with dimensions length and diameter (both in microns). Uses
;;; global variable *g-in-mem, which is in units of (ohms-cm-cm)^-1.
(defun g-in-syn (length diameter)
  (* 1.0e6 pi-single diameter length 1.0e-8  *g-in-mem))	

;;; Z-DISCRETE-IN-SEG Returns input resistance (Mohms) of segment, taking into account the tree distal to the segment, using the
;;; compartmental network parameters.
(defun z-discrete-in-seg (segment)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (the sf
       (let ((g-end 0.0)(g-temp 0.0) result)
	 (dolist (distal-segment (distal-segments segment))
	   (setq g-end (the sf
			    (+ g-end
			       (the sf
				    (/ 1.0
				       (the sf (z-discrete-in-seg distal-segment))))))))
	 (setq g-temp (the sf (+ (segment-g-leak segment)
					   g-end)))
	 (setq result
	       (/ 1.0 (the sf (/ (the sf (* (segment-g-axial segment) g-temp))
					   (the sf (+ (segment-g-axial segment) g-temp))))))
	 (element-parameter segment 'z-discrete-in result))))

(defun z-cable-in-old (ri rm a-um l-um &optional (g-end 0))
  (let ((a (* 1.0e-4 a-um))
	(l (* 1.0e-4 l-um))
	(g-inf)(b-1))
    (setq g-inf (/ (* pi-single a a 1e6) 
		   (* ri (lambda-cable ri rm a-um))))
    (setq b-1 (/ g-end g-inf))
    (/ 1.0
       (* g-inf
	  (/ (+ b-1 (tanh (/ l  (lambda-cable ri rm a-um))))
	     (+ 1 (* b-1 (tanh (/ l (lambda-cable ri rm a-um))))))))))

(defun lambda-cable-old (ri rm a-um)
  (let ((a (* 1.0e-4 a-um)))
    (sqrt (/ (* a rm)
	     (* 2.0 ri)))))

|#

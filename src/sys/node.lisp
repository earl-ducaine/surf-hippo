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


;;; SYS Source file: node.lisp

;;; Things relating to a circuit node

(in-package "SURF-HIPPO")

(defun node-cell-element (node)
  (loop for elt in (node-elements node)
	when (typecase elt
	       (segment (eq node (segment-node-2 elt)))
	       (soma t))
	do (return elt)))

(defun node-is-soma (node) (loop for elt in (node-elements node)
				 when (and (soma-p elt) (eq (soma-node elt) node))
				 do (return t)))

(defun node-is-segment (node) (not (node-is-soma node)))

(defun node-info ()
  (maphash 'print-node-position (NODE-HASH-TABLE))
  (maphash 'print-node-voltage (NODE-HASH-TABLE))
  (maphash 'print-node-elements (NODE-HASH-TABLE)))

(defun print-node-position (name nd)
  (format t "Node ~a position = ~d ~%" name (node-relative-location nd))
  (format t "Node ~a absolute position = ~d ~%" name (node-absolute-location nd)))

(defun print-node-states ()
  "Prints the vector of node voltages and delta-v's. Mainly for debugging."
  (format t "Node Voltages at time ~,4fms:~%" *real-time*)
  (format t "Node             V-n+1        V-n         jacobian   node-current~%")
  (maphash 'print-a-node-state-every (NODE-HASH-TABLE)))

(defun print-a-node-state-every (name nd)
  (unless (eq nd *ground-node*)		; don't print for ground
    (when (or (= 0 (length *debug-node-name*)) (string= name *debug-node-name*))
      (format t "~a  ~17t~a~27t~a~40t~a~53t~a~66t~%"
	      (node-name nd)
	      (node-voltage-n+1 nd)
	      (node-voltage-n nd)
	      (node-jacobian nd)
	      (node-current nd)))))

(defun print-node-dv-states ()
  ;; "Prints the vector of node voltages and delta-v's. Mainly for debugging."
  (format t "Node dv Voltages at time ~ams:~%" *real-time*)
  (format t "Node             dvdt-n        dvdt-n-1~%")
  (maphash 'print-a-node-dvdt-state  (NODE-HASH-TABLE)))

(defun print-a-node-dvdt-state (name nd)
  (unless (eq nd *ground-node*)		; don't print for ground
    (when (or (= 0 (length *debug-node-name*)) (string= name *debug-node-name*))
      (format t "~a  ~17t~a~27t~a~%"
	      (node-name nd)
	      (node-dvdt-n nd)
	      (node-dvdt-n-1 nd)))))

(defun print-a-node-state (name nd)
  (unless (eq nd *ground-node*)		; don't print for ground
    (when (or (= 0 (length *debug-node-name*)) (string= name *debug-node-name*))
      ;; "Prints the voltage and delta-v for one node."
      (format t "~a  ~17t~a~27t~a~40t~a~53t~a~66t~%"
	      (node-name nd)
	      (node-voltage-n+1 nd)
	      (node-voltage-n nd)
	      (node-jacobian nd)
	      (node-current nd)))))

(defun print-node-voltage (name nd)
  (and nd (format t "Node ~a voltage = ~a ~%" name (node-voltage-n  nd))))

(defun print-node-elements (name nd)
  (format t "Node ~a elements are: ~A ~%" name (node-elements nd))
  (dolist (element (node-elements nd))
    (format t " ~a ~%"  (named-structure-symbol element))))

(defun print-node (nd)
  (let ((nd (element nd 'node)))
    (when nd
      (let ((cell-name (element-name (node-cell nd)))
	    (name (node-name nd))
	    (rel-loc (node-relative-location nd))
	    (ab-loc (node-absolute-location nd)))
	(format t "Node ~A: cell ~a~%" name cell-name)
	(format t "   Relative/absolute position = ~d/~d ~%" rel-loc ab-loc)
	(format t "   Its elements are: ~A ~%" (node-elements nd))))))
    
(defstruct core-off-diag
  (point-diag nil)
  (lower nil))

(defun declare-ground (gnd-name)
  "Creates the ground node."
  (unless *ground-node*
    (let ((nd (NODE-HASH-TABLE gnd-name)))
      (if nd				; this node has already been created
	  (setf				;  just change things to make it a ground node
	   *ground-node* nd
	   *num-nodes* (1- *num-nodes*)) ; don't count ground in the number of nodes
	  (setf				; make a new node to be the ground
	   nd (make-node)
	   *ground-node* nd
	   (node-name nd) gnd-name
	   (node-cell nd) nil))))	;Ground is outside of the cell.
  (setf (NODE-HASH-TABLE gnd-name) *ground-node*))

(defun create-node (node-name &key cell dummy-node (is-physical-cell-node nil) (relative-location (list 0.0 0.0 0.0)))
  ;; 'Node-name' is assumed to be a concantenation of a node label and the cell-name.
  ;; Creates a new node, if not already defined. Returns the pointer to the node.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((node-name (if dummy-node (format nil "~A" (gensym)) node-name)))
    (or (NODE-HASH-TABLE node-name)
	(let ((nd (make-node :name node-name :is-physical-cell-node is-physical-cell-node
			     :relative-location relative-location))
	      (cell (unless dummy-node (or (element cell 'cell)
					   (progn
					     (format t "create-node: node ~a - creating cell Unknown" node-name)
					     (create-cell "Unknown"))))))
	  (setf (NODE-HASH-TABLE node-name) nd
		(node-cell nd) cell)
	  (when is-physical-cell-node (setf *num-nodes* (the fn (1+ (the fn *num-nodes*)))))
	  (setq *node* nd)
	  nd))))

(defun initialize-node-jacobians ()
  ;; Calculate the time invariant component of the node jacobians (node-const-jacobian), which consist of segment g-axial terms,
  ;; and soma and segment g-leak terms.
  (node-iterator
   (setf (node-const-jacobian nd) 0.0d0
	 (node-const-current nd) (the df (- (the df (or (element-parameter nd 'constant-current)  0.0d0))))
	 ;; (node-element-const-jacobian-double nd) 0.0d0
	 ;; (node-element-const-current-double nd) 0.0d0
	 ))
  (initialize-seg-node-jacobians-and-currents)
  (initialize-soma-node-jacobians-and-currents))

(defun reorder-circuit ()
  "Collects nodes to be evaluated in to *CORE-NODE-ARRAY*, ordered by the segment *BRANCH-LIST* according to the Hines
ordering. Contructs the 3 (tridiagonal) matrix arrays (upper, lower, diagonal) and the right hand side and output array."
  (loop for node being the hash-value of (NODE-HASH-TABLE) 
	unless (eq *ground-node* node) collect node into all-nodes
	unless (or (eq *ground-node* node)
		   (not (node-is-physical-cell-node node))
		   (node-has-ideal-voltage-source node))		   
	collect node into core-nodes
	finally (setq *all-node-array* (list-to-array all-nodes)
		      *ALL-NODE-ARRAY-LENGTH-1* (1- (length all-nodes))
		      *ALL-NODE-ARRAY-LENGTH* (length all-nodes)
		      *core-node-array* (make-array (length core-nodes))
		      *CORE-NODE-ARRAY-LENGTH-1* (1- (length core-nodes))
		      *CORE-NODE-ARRAY-LENGTH* (length core-nodes)))
  (order-nodes-from-hines-branch-list)
  (make-branch-arrays)
  (make-double-float-matrix-arrays)
  (adjust-off-diagonal-pointers))

(defun make-double-float-matrix-arrays ()
  (unless (and *diag-double* (= (length *diag-double*) *core-node-array-length*))
    (let ((array-param (list *core-node-array-length*)))
      (setf *diag-double* (make-array array-param :element-type 'double-float)
	    *lower-diag-double* (make-array array-param :element-type 'double-float)
	    *upper-diag-double* (make-array array-param :element-type 'double-float)
	    *v-at-half-step-double* (make-array array-param :element-type 'double-float)
	    *rhs-double* (make-array array-param :element-type 'double-float)))))
  
(defun make-branch-arrays ()
  (setf *branch-array* (make-array (list (length *branch-list*)))
	*last-seg-branch-array* (make-array (list (length *branch-list*)))
	*reverse-branch-array* (make-array (list (length *branch-list*))))
  (setq *BRANCH-ARRAY-LIMIT* (1- (length *branch-list*)))
  (loop for branch in *branch-list*
	for i upfrom 0 do
	(setf (aref *branch-array* i) branch)
	(setf (aref *last-seg-branch-array* i) (car (last branch)))
	(setf (aref *reverse-branch-array* i) (reverse branch))))

(defvar *off-diags* '())

(defun adjust-off-diagonal-pointers ()
  (clear-matrix-pointers)
  (do ((index 0 (+ 1 (the fn index))))
      ((= index (the fn *CORE-NODE-ARRAY-LENGTH*)))
    (let ((nd (aref (the (vector node) *core-node-array*) index)))
;;      (format t "index ~A~%" index) 
      (unless (or (eq nd *ground-node*) (<= *CORE-NODE-ARRAY-LENGTH* 1))
	(let ((index (node-index nd)))
	  (declare (fixnum index))
	  (if (/= index 0)		; not first node, create lower off-diag
	      (let ((off-diag (make-core-off-diag)))
		; (push off-diag *off-diags*)
		(setf (core-off-diag-lower off-diag) t)
		(setf (core-off-diag-point-diag off-diag) nd)
		(dolist (elt (node-elements nd))
		  (when (segment-p elt) (add-off-diag-segment elt nd (aref *core-node-array* (1- index)) off-diag)))))
	  (if (/= index (1- *CORE-NODE-ARRAY-LENGTH*)) ; not first node, create upper off-diag
	      (let ((off-diag (make-core-off-diag)))
		; (push off-diag *off-diags*)
		(setf (core-off-diag-lower off-diag) nil)
		(setf (core-off-diag-point-diag off-diag) nd)
		(dolist (elt (node-elements nd))
		  (when (segment-p elt) (add-off-diag-segment elt nd (aref *core-node-array* (1+ index)) off-diag))))))))))

(defun zero-v-n+1 (nd)
  ;; Sets the voltage at n+1 to zero.
  (setf (node-voltage-n+1 nd) 0.0d0))

(defun set-node-voltage (nd value)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when nd
    (setq value (coerce value 'double-float))
    (setf (node-voltage-n+1 nd) value
	  (node-voltage-n nd) value
	  (node-voltage-n-1-double nd) value))
  value)

(defun set-*node-voltage-initializations* ()
  "Set *NODE-VOLTAGE-INITIALIZATIONS* to a list with all the circuit nodes and their voltages \(node-voltage-n+1\)."
  (setq *node-voltage-initializations*
	(loop for node being the hash-value of (NODE-HASH-TABLE)
	      collect (list node (node-voltage-n+1 node))))
  nil)

(defun set-node-voltage-double (nd value)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (double-float value))
  (when nd
    (setf (node-voltage-n+1 nd) value
	  (node-voltage-n nd) value
	  (node-voltage-n-1-double nd) value))
  nil)

(defun voltage-to-voltage-index (voltage)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((voltage (typecase voltage
		    (double-float voltage)
		    ((or fixnum single-float) (coerce voltage 'double-float))
		    (t 0.0d0)))
	 (index-voltage (the df (* (the (signed-byte 32) (1- *particle-look-up-table-length*))
				   (the df (/ (the df (- voltage *particle-look-up-table-min-voltage-double*))
					      (the sf *particle-look-up-table-voltage-range*)))))))
    (if *interpolate-particle-arrays*
	(the (signed-byte 32) (min (the fn (1- *particle-look-up-table-length*))
				   (max 0 (the fn (truncate index-voltage)))))
	(the (signed-byte 32) (min (the fn (1- *particle-look-up-table-length*))
				   (max 0 (the fn (round index-voltage))))))))

(proclaim '(inline voltage-double-to-voltage-index))
(defun voltage-double-to-voltage-index (voltage)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float voltage))
  (let* ((index-voltage (the df (* (the (signed-byte 32) (1- *particle-look-up-table-length*))
				   (the df (/ (the df (- voltage *particle-look-up-table-min-voltage-double*))
					      (the sf *particle-look-up-table-voltage-range*))))))
	 (fixed-v (if *interpolate-particle-arrays*
		      (kernel:%unary-truncate index-voltage)
		      (kernel:%unary-round index-voltage)))
	 (p-max (the fn (1- *particle-look-up-table-length*))))
    (declare (fixnum fixed-v))
    (the (signed-byte 32) 
      (if (> 0 fixed-v)			; FIXED-V index must be greater than or equal to 0.
	  0
	  (if (< fixed-v p-max) fixed-v p-max) ; Limit FIXED-V index by P-MAX.
	  ))))

(proclaim '(inline voltage-double-to-voltage-index-relative))
(defun voltage-double-to-voltage-index-relative (voltage)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float voltage))
  (let ((index-voltage (* (the (signed-byte 32) (1- *particle-look-up-table-length*))
			  (/ voltage (the sf *particle-look-up-table-voltage-range*)))))
    (min (the fn (1- *particle-look-up-table-length*))
	 (the fn (round index-voltage)))))

(defun elements-with-holding-potentials () (loop for elt in (cell-elements) when (element-holding-potential elt) sum 1))
  
(defun element-holding-potential (element &optional (value nil value-supplied-p))
  "If VALUE (in mV) is supplied, then sets the 'HOLDING-POTENTIAL parameter of the circuit node(s) associated with ELEMENT and returns VALUE
(converted to double-float). If VALUE is supplied, and NIL, then the 'HOLDING-POTENTIAL is cleared for the node. Otherwise, returns the current value
of the 'HOLDING-POTENTIAL parameter for the node, if that value has been set previously."
  (let ((cell-element-node (element-node element)))
    (when cell-element-node
      (if value-supplied-p
	(element-parameter cell-element-node 'holding-potential (when value (d-flt value)))
	(element-parameter cell-element-node 'holding-potential)))))

(defun set-holding-potentials-to-current-values ()
  "Set the holding potential for all somas and segments to their current voltage."
  (mapcar #'(lambda (elt) (element-holding-potential elt (element-value elt))) (cell-elements)))
  
(defun clear-holding-potentials ()
  "Clear holding potentials for all somas and segments."
  (element-holding-potential (cell-elements) nil))

(proclaim '(inline node-aref-particle-voltage))
(defun node-aref-particle-voltage (node-double-floats &optional initial-state node)			 
  (declare (optimize (safety 0) (speed 3) (space 1) (compilation-speed 0)))
  (if initial-state
      ;; Use V(t_n) for initial state.
      (the df
	   (or 
	    (get-a-value 'holding-potential (node-parameters node))
	    (node-aref-voltage-n node-double-floats)))
      ;; Estimate voltage at time (t-prime_n+1/2) halfway between midpoints of last step and current step, or equivalently,
      ;; halfway between the current particle grid step and the next one.  Depending on size of these two steps, V(t-prime_n+1/2)
      ;; is either an interpolation between or an extrapolation beyond V(t_n-1) and V(t_n).
      (+  (* 0.5d0 (+ (node-aref-voltage-n-1 node-double-floats) (node-aref-voltage-n node-double-floats)))
	  (* (*half-delta-t-prime[n]*) (node-aref-dvdt-n-1 node-double-floats)))))

(proclaim '(inline node-particle-voltage))
(defun node-particle-voltage (node &optional initial-state)
  ;; Node voltage at time (t-prime_n+1/2).
  (node-aref-particle-voltage (node-double-floats node) initial-state node))

(proclaim '(inline update-node-v-index))
(defun update-node-v-index (node &optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 1) (compilation-speed 0)))
  ;; First evaluate the voltage node (the actual circuit node that the particle is driven by), and derive an index from the
  ;; voltage for the particle type's alpha and beta arrays.
  (flet ((FAST-FRACTIONAL-PART-FLET (number)
	   ;; Returns second result of TRUNCATE, where NUMBER is double float.
	   (declare (double-float number))
	   (let ((df-integer-part (ext:truly-the fn (kernel:%unary-truncate number))))
	     (- number df-integer-part))))
    (when (node-has-v-dep-element node)
      (let ((voltage-n-prime (node-particle-voltage node initial-state)))
	(declare (double-float voltage-n-prime))
	(setf (node-prt-v-index node) (VOLTAGE-DOUBLE-TO-VOLTAGE-INDEX voltage-n-prime))	    
	(when *interpolate-particle-arrays*
	  (setf (node-prt-v-index-rem node) (coerce (fast-fractional-part-flet voltage-n-prime) 'single-float))))))
  nil)

;(defun node-check-steady-state (nd)     ;
;  "To see if the voltages have reached dc steady state at this node yet."
;  (if (> (/ (abs (- (node-voltage-n+1 nd) (node-voltage-n nd))) (* *time-step* *mrt*))
;         (+ vabs (* vrel (abs (node-voltage-n+1 nd)))))
;      nil
;      t))

(defun constant-current-elements () (loop for cell-element in (cell-elements) when (element-constant-current cell-element) collect cell-element))

(defun element-constant-current (element)
  "Returns constant current term [nA] if it exists to the node associated with ELEMENT, otherwise nil."
  (element-parameter (element-physical-node element) 'constant-current))

(defun add-constant-current-to-element (element current) (add-constant-current element current))

(defun add-constant-current (element &optional current)
  "Adds a constant CURRENT [nA] to the cell elements associated with ELEMENT. This is equivalent to including a current source at
the element with a fixed DC value, and will replace the previously assigned value for a constant current to ELEMENT, if any. If current is not supplied, or
NIL, will remove any assigned constant current from ELEMENT."
  (let ((current (unless (or (not current) (zerop current)) (coerce current 'double-float))))
    (loop for node in (coerce-to-list (element-node element)) do
	  (element-parameter node 'constant-current current))))

(defun clear-element-constant-currents () (clear-constant-currents))

(defun clear-element-constant-current (element) (remove-element-parameter (element-physical-node element) 'constant-current))

(defun clear-constant-currents ()
  "Removes any constant current terms from all the circuit nodes"
  (node-iterator (remove-element-parameter nd 'constant-current)))

(defun init-all-nodes (&optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 1) (compilation-speed 0)))
  ;; Initializes all nodes, updates v-indexes.
  (do ((index 0 (+ 1 (the fn index))))
      ((= index (the fn *all-NODE-ARRAY-LENGTH*)))
    (let* ((nd (aref (the (vector node) *all-node-array*) index))
	   (node-dfs (node-double-floats nd)))
      (when initial-state (set-element-parameter-fast nd 'fixed-voltage nil (node-parameters nd)))
      (setf (node-aref-jacobian node-dfs) (node-aref-const-jacobian node-dfs)
	    (node-aref-alpha-charge node-dfs) 0.0d0
	    (node-aref-current node-dfs) (node-aref-const-current node-dfs))
      (update-node-v-index nd initial-state))
   nil))

(defun start-node (nd)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let ((node-double-floats (node-double-floats nd)))
    (setf (node-double-floats-aref-dvdt-n-1 node-double-floats) 0.0d0
	  (node-double-floats-aref-dvdt-n node-double-floats) 0.0d0

	  (node-double-floats-aref-voltage-n node-double-floats)
	  (node-double-floats-aref-voltage-n+1 node-double-floats)

	  (node-double-floats-aref-voltage-n-1 node-double-floats)
	  (node-double-floats-aref-voltage-n+1 node-double-floats)))
  nil)

(defun element-resting-potential (element)
  "Return the membrane resistance leak potential of the cell element associated with ELEMENT."
  (let* ((element (element-cell-element element))
	 (cell-type (cell-type (element-cell element))))
    (typecase element
      (soma (cell-type-v-leak-soma cell-type))
      (segment (if (member element (soma-segments (cell-soma (element-cell element))))
		   (cell-type-v-leak-soma cell-type)
		   (d-flt (cell-type-v-leak-dendrite cell-type)))))))
	  
(defun init-node-voltages-slots-and-matrix ()
  ;; Starts the circuit off by voltage clamping all the nodes to the defined resting potential, for example, the leak battery, and
  ;; setup voltage array indexes for the appropriate nodes. Calls (init-all-nodes t) to clear tri-diag matrix, and initialize all
  ;; the accumulator fields, including (node-jacobian nd), (node-alpha-charge nd) and (node-current nd).  example, the leak
  ;; battery.
  (loop for cell-type in (cell-types) do
	(let ((v-leak-dendrite (d-flt (cell-type-v-leak-dendrite cell-type)))
	      (v-leak-soma (d-flt (cell-type-v-leak-soma cell-type))))
	  (loop for cell in (cell-type-cells cell-type) do
		(loop for seg in (cell-segments cell) do
		      (set-node-voltage-double (segment-node-2 seg) ; v-leak-dendrite
					       (or (element-holding-potential seg)
						   (if (= (segment-v-leak seg) v-leak-dendrite)
						     v-leak-dendrite
						     (d-flt (segment-v-leak seg))))))
		(loop for seg in (soma-segments (cell-soma cell)) do
		      (set-node-voltage-double (segment-node-2 seg) (or (element-holding-potential seg) v-leak-soma)))
		(set-node-voltage-double (soma-node (cell-soma cell))
					 (or (element-holding-potential (cell-soma cell)) v-leak-soma)))))
  
  (loop for electrode in (electrodes) do
	(set-node-voltage-double (segment-node-2 electrode) ; v-leak-dendrite
				 (or (element-holding-potential electrode)
				     (d-flt (segment-v-leak electrode)))))
				  
  ;; If *USE-NODE-VOLTAGE-INITIALIZATIONS* is T, set node voltages specified in *NODE-VOLTAGE-INITIALIZATIONS*.
  (when *use-node-voltage-initializations*
    (dolist (pair *node-voltage-initializations*)
      (set-node-voltage (element-physical-node (car pair)) (if (numberp (cadr pair)) (cadr pair) (cdr pair)))))

  (eval-fixed-voltage-nodes)
  (do ((index 0 (+ 1 (the fn index))))
      ((= index (the fn *all-NODE-ARRAY-LENGTH*)))
    (let ((nd (aref *all-node-array* index)))
      (start-node nd)))			; Set appropriatly the "past" voltages and derivatives.
					; [node-voltage-n, node-voltage-n-1 nd <- node-voltage-n+1 nd].
      
  (init-all-nodes t))

#|
(defun set-diag-rhs-floats ()
  (declare (optimize (safety 0) (speed 3) (space 1) (debug 0) (compilation-speed 0)))
  (do ((index 0 (+ 1 (the fn index))))
      ((= index (the fn *CORE-NODE-ARRAY-LENGTH*)))
    (let ((nd (aref (the (simple-array node) *core-node-array*) index)))
      ;; Fills both *rhs* and *diag* from the node slots. For Hines, rhs is only the charge and current term.
      (setf (aref (the matrix-float *diag-double*) (the fn (node-index nd))) (node-jacobian nd)
	    (aref (the matrix-float *rhs-double*) (the fn (node-index nd)))  (- (node-alpha-charge nd) (node-current nd)))))
  nil)
|#

(defun set-diag-rhs-floats ()
  (declare (optimize (safety 0) (speed 3) (space 1) (debug 0) (compilation-speed 0)))
  (do ((index 0 (+ 1 (the fn index))))
      ((= index (the fn *CORE-NODE-ARRAY-LENGTH*)))
    (let ((nd (aref (the (simple-array node (*)) *core-node-array*) index)))
      ;; Fills both *rhs* and *diag* from the node slots. For Hines, rhs is only the charge and current term.
      (setf (aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*))
		    ; matrix-float
		    *diag-double*) (the fn (node-index nd)))
	    (node-jacobian nd)
  
	    (aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*))
		    ; matrix-float
		    *rhs-double*) (the fn (node-index nd)))
	    (the df (- (node-alpha-charge nd) (node-current nd))))))
  nil)


(defun eval-node-floats ()
  ;; Transfers *v-at-half-step*->core-node-v-at-half-step, and does explicit half step.
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (do ((index 0 (the fn (+ 1 index))))
      ((= index (the fn *CORE-NODE-ARRAY-LENGTH*)))
    (let ((node (aref (the (SIMPLE-ARRAY node (*))
			; (vector node)
			*core-node-array*) index))
	  (v-at-half-step-double (aref (the (SIMPLE-ARRAY DOUBLE-FLOAT (*))
					 ; matrix-float
					 *v-at-half-step-double*) index)))
      (setf (node-voltage-n+1 node)
	    (the df (- (+ v-at-half-step-double v-at-half-step-double)
		       (node-voltage-n node))))))
  nil)
  

(defun eval-all-nodes ()
  ;; Sets the RHS for the core-nodes, solves the matrix, updates the voltage estimate with the new delta-V's.
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (set-diag-rhs-floats)

  (when *print-matrix* (print-matrix))
  (hines-solve)				; Solve the tri-diagonal matrix.

  (when *print-matrix* (print-matrix-solution))
  (eval-node-floats))

(defun print-matrix-solution ()
  (format t "~%The solution is:~%")
  (format t "index       V-at-half-step~%")
  (dotimes (i *num-unknowns*) (format t "~5d ~,18f~%" i (aref *v-at-half-step-double* i)))
  (format t "~%"))

(defun print-matrix ()
  (format t "~%The matrix is:~%")
  (format t "index :    lower            diag                  upper                  rhs~%")
  (dotimes (i *num-unknowns*)
    (format t "~d (~S): ~,18f~22t~,18f~42t~,18f~54t~,18f~%"
	    i (node-name (aref *core-node-array* i)) (aref *lower-diag-double* i)
	    (aref *diag-double* i) (aref *upper-diag-double* i) (aref *rhs-double* i)))
  (format t "~%"))

(defun advance-nodes ()
  ;; Advances the system one step in time.
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (do ((index 0 (+ 1 (the fn index))))
      ((= index (the fn *all-NODE-ARRAY-LENGTH*)))
    (let ((nd (aref (the (vector node) *all-node-array*) index)))
      (psetf
       (node-voltage-n-1-double nd)  (node-voltage-n nd)
       (node-voltage-n nd)    (node-voltage-n+1 nd)
       (node-dvdt-n-1 nd) (node-dvdt-n nd)))))

(proclaim '(inline get-node-dvdt))
(defun get-node-dvdt (nd)
  (declare (optimize (safety 0) (debug 0) (speed 3) (space 3)))
  (if (= (*delta-t[n]*) 0)
      0.0d0
      (/ (- (node-voltage-n+1 nd) (node-voltage-n nd))
	 (*delta-t[n]*))))


;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

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


;;; SYS Source file: hines.lisp


;; Code for ordering the circuit and solving the almost tri-diagonal matrix, taken from Hines 1984.


(in-package "SURF-HIPPO")



;;; ** APPLYING THE METHOD OF HINES TO THE BRANCHED TREE STRUCTURE OF EACH CELL **

;;; (Note that for the Hines method, "delta-v" is actually V(t + dt/2).)

;;; This description of branches is derived from Hines, "Efficient computation of branched nerve
;;; equations "Int. J. Bio-Medical Computing (15) (1984) p69-76. A branch is composed of connected
;;; segments. The ends of the branch are determined by those segments which (at either end of the
;;; segment) connect to more than one segment. The branches of a cell, and in turn the branch
;;; segments, are numbered as follows: Choose any branch of the tree which is connected at one end
;;; to the soma. Number the segments of that branch so that the segment connected to the soma is the
;;; last segment. The first segment of this branch ('trunk') will be called the 'branch node'. Note
;;; that this branch node is not the same thing as a true circuit node (as referenced by Surf-Hippo).

;;; The soma is also considered a 'branch node'.

;;; All branches connecting to a branch node have their segments numbered so that their last segment
;;; connects to this node. Their first segments are also called branch nodes (as long as other
;;; segments are connected to them), and this segment-numbering process continues until all the
;;; segments are numbered. The last numbered branches, or 'twigs', all have one end (their first
;;; segments) unconnected. Each branch node becomes the center of a Wye network.

;;; Continue this procedure for all other segments connected to the soma.

;;; Branches are numbered as follows: Assume that there are N branches. Starting at soma, number all
;;; the branches connecting to the soma starting with N and decrementing. Continue numbering all the
;;; branches that are connected to the previous set of branches, decrementing the branch number.
;;; Continue working out on the tree until all the branches are labeled.

;;; The two circuit nodes (proximal and distal) associated with each segment are ordered according
;;; to the branch number and the number of the attached segment in the proximal direction. Note that
;;; since the circuit is a tree, there will only be one such segment for each node, whereas there
;;; can be multiple segments attached to a node in the distal direction. For example, if branch 33
;;; has 2 segments and branch 34 has 3 segments, then the nodes would be ordered: ...33-1, 33-2,
;;; 34-1, 34-2, 34-3... The soma node has the highest node index.




;; BUILD-BRANCH-LIST This is called from LOCATE-CELL-NODES, where cell nodes are located seqentially
;; from the soma outward, and from GET-AND-LOCATE-DISTAL-SEGMENTS, which is called recursively from
;; LOCATE-CELL-NODES (for each branch originating at the soma).
(defun build-branch-list (seg)
  (unless (node-has-ideal-voltage-source (segment-node-2 seg))
    (let ((last-seg (caar *branch-list*)) ; Get the last segment added to the *branch-list*
	  (last-seg-d-node-segs 0)	; Last seg's prox node segs (which includes last seg).
	  (last-seg-d-to-seg nil))	; Is last segment distal to current seg?
      (when last-seg
	;; Count the segments attached to the last segment's proximal node. If this is greater
	;; than 2, then this segment was a node of the last branch.
	(dolist (elt (node-elements (segment-node-2 last-seg)))
	  (when (eq (named-structure-symbol elt) 'segment)
	    (incf last-seg-d-node-segs)))
	;; See if current segment is distal to last segment.
	(dolist (elt (node-elements (segment-node-2 last-seg)))
	  (when (and (eq (named-structure-symbol elt) 'segment) (eq elt seg))
	    (setq last-seg-d-to-seg t))))    
      ;; Now update *branch-list*.
      (setq *branch-list*
	    (if (or (not last-seg-d-to-seg) ;If this seg not connected to last seg,
		    (> last-seg-d-node-segs 2) ;or the last seg was node of the latest branch,
		    (not last-seg))	;or we are at the beginning of *branch-list*,
		(cons (list seg) *branch-list*) ;then START a new branch with the new seg.
	      
		;; Else ADD seg to latest branch.    
		(cons (cons seg (car *branch-list*)) (cdr *branch-list*)))) 
					
      (when *debug-hines*
	(format t "BBL: seg ~a; last-seg ~a - last-seg-dis-segs ~a add-branch-now ~a ~%"
		(segment-name seg) 
		(if last-seg (segment-name last-seg))  last-seg-d-node-segs 
		(or (> last-seg-d-node-segs 2) ;If the *last* seg was node of the latest branch,
		    (not last-seg-d-to-seg) (not last-seg)))))))





;;; **** Solving the System **** Upper triangularization of the matrix is accomplished by repeated
;;; applications of the procedure TRIANG. Define a 'twig' as any branch whose distal end is *not*
;;; connected to an un-TRIANGed branch (including no branch at all). Now upper triangularize the
;;; matrix by doing the following until there are only twigs left: Collect the set of all current
;;; twigs, except any twigs which have already been TRIANGed. Apply TRIANG to the remaining set of
;;; twigs, in the order that they appear in the matrix(?). Repeat.

(proclaim '(inline triang))
#|
(defun TRIANG (branch last-seg)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (cons branch)
	   (type matrix-float *diag-double* *lower-diag-double* *upper-diag-double* *rhs-double*))
  ;; 'branch' is a list of segments in the branch, in order of their node indexes. Note that for the soma branch,
  ;; the soma will be skipped. We want to go from second segment of branch to last segment. If branch has only one
  ;; segment, then process that one at least for its branch-node-index.
  (let* (
	 ;; (last-seg (car (last branch)))
	 ;; (branch-length (the fn (length branch)))
	 (branch-length>1 (cadr branch)	; (> branch-length 1)
	   ))
    (declare (type segment last-seg))
    (do* ((temp-branch (if branch-length>1 (cdr branch) branch) (cdr temp-branch))
	  (segment (car temp-branch) (car temp-branch)))
	 ((null temp-branch))
      (declare (type segment segment))
      (unless (node-has-ideal-voltage-source (segment-node-2 segment))
	(let ((node-index (node-index (segment-node-2 segment)))
	      (node-index-1 (- (node-index (segment-node-2 segment)) 1)))
	  (when branch-length>1
	    (let ((coefficent (the df (/ (aref *lower-diag-double* node-index) (aref *diag-double* node-index-1)))))
	      (setf (aref *diag-double* node-index)
		    (- (aref *diag-double* node-index) (* (aref *upper-diag-double* node-index-1) coefficent))
		    (aref *RHS-DOUBLE* node-index)
		    (- (aref *RHS-DOUBLE* node-index) (* (aref *RHS-DOUBLE* node-index-1) coefficent)))))
	  (when (and (segment-branch-node-index segment)
		     (eq segment last-seg))		     
	    (let ((coefficent (the df (/ (- (segment-g-axial segment)) (aref *diag-double* node-index)))))
	      (setf (aref *diag-double* (segment-branch-node-index segment))
		    (- (aref *diag-double* (segment-branch-node-index segment))
		       (* (- (segment-g-axial segment)) coefficent))
		    (aref *RHS-DOUBLE* (segment-branch-node-index segment))
		    (- (aref *RHS-DOUBLE* (segment-branch-node-index segment))
		       (* (aref *RHS-DOUBLE* node-index) coefficent))))))))))
|#
;; NG's version
(defun TRIANG (branch last-seg)
  ;; NG, j'ai rajoute l'option de compilation-speed a 0, 31/10/97
  ;; ca fait gagner 10%.
  (declare (optimize (safety 0) (speed 3) (space 1) (debug 0) (compilation-speed 0))
	   (cons branch) 
	   (type (simple-array double-float) *diag-double* *lower-diag-double* *upper-diag-double* *rhs-double*))
  ;; 'branch' is a list of segments in the branch, in order of their node indexes. Note that for the soma branch,
  ;; the soma will be skipped. We want to go from second segment of branch to last segment. If branch has only one
  ;; segment, then process that one at least for its branch-node-index.
  (let ((branch-length>1 (cadr branch)))
    (declare (type segment last-seg))
    (do* ((temp-branch (if branch-length>1 (cdr branch) branch) (cdr temp-branch))
	  (segment (car temp-branch) (car temp-branch)))
	 ((null temp-branch))
      (declare (type segment segment) (type (or null cons) temp-branch))
      (unless (node-has-ideal-voltage-source (segment-node-2 segment))
	(let* ((node-index (node-index (segment-node-2 segment)))
	       (node-index-1 (- node-index 1)))
	  (declare (fixnum node-index node-index-1))
	  (when branch-length>1
	    (let ((coefficient (ext:truly-the df (/ (aref *lower-diag-double* node-index)
						    (aref *diag-double* node-index-1)))))
	      (declare (double-float coefficient))
	      (setf (aref *diag-double* node-index)
		    (ext:truly-the df (- (aref *diag-double* node-index)
					 (ext:truly-the df (* (aref *upper-diag-double* node-index-1) coefficient))))
		    (aref *RHS-DOUBLE* node-index)
		    (ext:truly-the df (- (aref *RHS-DOUBLE* node-index)
					 (ext:truly-the df (* (aref *RHS-DOUBLE* node-index-1) coefficient)))))))
	  (when (and (segment-branch-node-index segment)
		     (eq segment last-seg))		     
	    (let ((coefficient (the df (/ (- (segment-g-axial segment)) (aref *diag-double* node-index)))))
	      (declare (double-float coefficient))
	      (setf (aref *diag-double* (segment-branch-node-index segment))
		    (ext:truly-the df (- (aref *diag-double* (segment-branch-node-index segment))
					 (ext:truly-the df (* (- (segment-g-axial segment)) coefficient))))
		    (aref *RHS-DOUBLE* (segment-branch-node-index segment))
		    (ext:truly-the df (- (aref *RHS-DOUBLE* (segment-branch-node-index segment))
					 (ext:truly-the df (* (aref *RHS-DOUBLE* node-index) coefficient))))))))))))


;;; Now back-substitute the upper-triangular matrix.
(proclaim '(inline bksub))
#|
(defun BKSUB (reverse-branch)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (cons reverse-branch)
	   (type matrix-float *diag-double* *upper-diag-double* *rhs-double* *v-at-half-step-double*))
  ;; 'branch' is a list of segments in the branch, in order.
  (do* ((first-time t nil)
	(rev-branch reverse-branch (cdr rev-branch))
	(segment (car rev-branch) (car rev-branch)))
       ((null rev-branch))
    (declare (type segment segment))
    (unless (node-has-ideal-voltage-source (segment-node-2 segment))
      (let ((node-index (node-index (segment-node-2 segment))))
	(declare (fixnum node-index))	; Is this necessary?
	(setf (aref *v-at-half-step-double* node-index)
	      (/ (- (aref *RHS-DOUBLE* node-index)
		    (if first-time
			(if (segment-branch-node-index segment)
			    (* (aref *v-at-half-step-double* (segment-branch-node-index segment))
			       (- (segment-g-axial segment)))
			    0.0d0)
			(* (aref *v-at-half-step-double* (the fn (+ node-index 1)))
			   (aref *upper-diag-double* node-index))))
		 (aref *diag-double* node-index)))))))
|#
;; NG's version
(defun BKSUB (reverse-branch)
  (declare (optimize (safety 0) (speed 3) (space 1) (debug 0) (compilation-speed 0))
	   (cons reverse-branch)
	   (type (simple-array double-float) *diag-double* *upper-diag-double* *rhs-double* *v-at-half-step-double*))
  ;; 'branch' is a list of segments in the branch, in order.
  (do* ((first-time t nil)
	(rev-branch reverse-branch (cdr rev-branch))
	(segment (car rev-branch) (car rev-branch)))
       ((null rev-branch))
    (declare (type segment segment))
    (unless (node-has-ideal-voltage-source (segment-node-2 segment))
      (let ((node-index (node-index (segment-node-2 segment))))
	(declare (fixnum node-index))
	;; que du numerique a partir d'ici.
	(setf (aref *v-at-half-step-double* node-index)
	      (ext:truly-the
	       df
	       (/ (- (aref *RHS-DOUBLE* node-index)
		     (ext:truly-the
		      df
		      (if first-time
			  (if (segment-branch-node-index segment)
			      (* (aref *v-at-half-step-double* (segment-branch-node-index segment))
				 (- (segment-g-axial segment)))
			      0.0d0)
			  (* (aref *v-at-half-step-double* (ext:truly-the fn (+ node-index 1)))
			     (aref *upper-diag-double* node-index)))))
		  (aref *diag-double* node-index))))))))




;;(proclaim '(notinline triang bksub))
#|
(defun HINES-SOLVE ()
  (declare (optimize (safety 0) (speed 3) (debug 0) (space 0) (compilation-speed 0))
  	   (type matrix-float *diag-double* *rhs-double* *v-at-half-step-double*)
	   (type (simple-array cons 1) *branch-array* *reverse-branch-array*)
	   (type (simple-array segment 1) *last-seg-branch-array*))
  (do ((i 0 (1+ (the fn i))))
      ((> (the fn i) (the fn *branch-array-limit*)))
    (TRIANG (aref *branch-array* i) (aref *last-seg-branch-array* i)))
  ;; Before back-substitution of the branches, take care of soma nodes first.
  (do ((i 0 (1+ (the fn i))))
      ((= (the fn i) (the fn *soma-array-length*)))
    (let ((soma (aref (the (simple-array soma (*)) *soma-array*) i)))
      (setf (aref *v-at-half-step-double* (the fn (node-index (soma-node soma))))
	    (/ (aref *RHS-DOUBLE* (node-index (soma-node soma)))
	       (aref *diag-double* (node-index (soma-node soma)))))))
  (do ((i (the fn *branch-array-limit*) (1- (the fn i))))
      ((< (the fn i) 0))
    (BKSUB (aref *reverse-branch-array* i))))
|#
;;; NG's version
(defun HINES-SOLVE ()
  (declare (optimize (safety 0) (speed 3) (debug 0) (space 1) (compilation-speed 0))
  	   (type (simple-array double-float) *diag-double* *rhs-double* *v-at-half-step-double*)
	   (type (simple-array cons) *branch-array* *reverse-branch-array*)
	   (type (simple-array segment) *last-seg-branch-array*))
  (do ((i 0 (ext:truly-the fn (1+ (ext:truly-the fn i)))))
      ((> (ext:truly-the fn i) (ext:truly-the fn *branch-array-limit*)))
    (declare (fixnum i))
    (TRIANG (aref *branch-array* i) (aref *last-seg-branch-array* i)))
  ;; Before back-substitution of the branches, take care of soma nodes first.
  (do ((i 0 (ext:truly-the fn (1+ (ext:truly-the fn i)))))
      ((= (ext:truly-the fn i) (ext:truly-the fn *soma-array-length*)))
    (declare (fixnum i))
    (let ((soma-index (node-index (soma-node (aref (the (simple-array soma) *soma-array*) i)))))
      (declare (fixnum soma-index))
      (setf (aref *v-at-half-step-double* soma-index)
	    (ext:truly-the df (/ (aref *RHS-DOUBLE* soma-index)
				 (aref *diag-double* soma-index))))))
  (do ((i (the fn *branch-array-limit*) (ext:truly-the fn (1- (ext:truly-the fn i)))))
      ((< (ext:truly-the fn i) 0))
    (declare (fixnum i))
    (BKSUB (aref *reverse-branch-array* i))))
  

		


;; ********************* ********************* ********************* ********************* *********************
;; ********************* ********************* ********************* ********************* *********************
;; ********************* ********************* ********************* ********************* *********************


;; (aref *lower-diag-double* i) = (aref *A* i i-1)
;; (aref *upper-diag-double* i) = (aref *A* i i+1)



(defun order-nodes-from-hines-branch-list ()
  (setq *num-unknowns* 0)
  (loop for cell in (cells) do (order-cell-nodes-from-hines-branch-list cell))
  ;;The proximal node of the proximal segment in each branch is a "branch node". Store the index of
  ;;that node in the segment structure.
  (loop for branch in *branch-list* do
	(let ((element (car (last branch))))
	  (setf (segment-branch-node-index element)
		(and (not (node-has-ideal-voltage-source (segment-node-1 element)))
		     (node-index (segment-node-1 element)))))))



(defun order-cell-nodes-from-hines-branch-list (cell)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum *num-unknowns*))
  (loop for branch in *branch-list* 
	when (eq (segment-cell (car branch)) cell)
	do (loop for segment in branch do
		 (let ((nd (segment-node-2 segment)))
		   (unless (node-has-ideal-voltage-source nd)
		     (setf (node-index nd) *num-unknowns*)
		     (when *debug-hines* (format t "node ~a index = ~a ~%"   (node-name nd) (node-index nd)))
		     (setf (aref (the (simple-array t (*)) *core-node-array*) *num-unknowns*) nd)
		     (incf (the fn *num-unknowns*))))))
  (let ((nd (soma-node (cell-soma cell))))
    (unless (node-has-ideal-voltage-source nd)
      (setf (node-index nd) (the fn *num-unknowns*))
      (when *debug-hines* (format *debug-hines* "node ~a index = ~a ~%" (node-name nd) (node-index nd)))
      (setf (aref (the (simple-array t (*)) *core-node-array*) *num-unknowns*) nd)
      (incf (the fn *num-unknowns*)))))








;;; For debugging.
(defun look-at-branch-list ()
  (loop for branch in  *branch-list* do
    (print 'NEW-BRANCH)
    (loop for segment in (reverse branch) do
      (print (segment-name segment))
      (format t " branch-node-index = ~a, " (segment-branch-node-index segment))
      (format t " index = ~a" (node-index (segment-node-2 segment))))))

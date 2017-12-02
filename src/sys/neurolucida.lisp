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


;;; SYS Source file: neurolucida.lisp


(in-package "SURF-HIPPO")


#|

Some notes on parsing neurolucida files.

The proximal location of each segment is taken (inferred) from the neurolucida file format. There may
be cases where this location is only referred to once in the file, in the sense that a segment is
defined as starting from a location which is not shared by any other cell location. In these cases,
the proximal location (from the file) is assigned to the segment's :DUMMY-PROXIMAL-NODE-LOCATION slot,
and the proximal node is chosen as the closest node in the rest of the circuit. The location
specified by the :DUMMY-PROXIMAL-NODE-LOCATION slot is used for calculating the length and drawing
geometry of the segment, while the actual proximal node (the segment's :NODE-1 slot) is the
connection used by the electrical circuit.

The soma of the circuit is really a hack at the moment, since the neurolucida files just define
cylinderical segments. For all segments that are assigned to the "soma" by the neurolucida format (e.g. a
"minor code" of 41 or 42), a flag is set in the segment :PARAMETERS a-list in an entry whose CAR is
'SOMA-SEGMENT using the function

  (element-parameter seg 'soma-segment t)

Also, a list in the circuit's soma :PARAMETERS a-list is an entry whose CAR is 'SEGMENTS and whose CDR
is a list of segments "assigned" by the original file as corresponding to the original cell's soma.
This list of segments, when it exists, is used by SURF-HIPPO when the cell soma is selected in a
histology window, i.e. the set of segments is highlighted.

Segments assigned to the soma may be retrieved by

         soma-segments &optional CELL
  
which returns a list of segments which are conceptually assigned to the actual cell soma.

Conceptual soma segments may added by the function

         add-soma-segment SOMA SEG

and may be removed (from the soma assignment, not from the circuit) with

         remove-soma-segment SOMA SEG


Problems that show up in the graphical rendition of a cell or otherwise can be traced to the
neurolucida file by using the following form (for example referencing segment 527): 

* (element-parameter 527 'file-line-number)
6793
*

This gives the line number in the original file which defined the distal location and radius of the
segment. In this example, it was determined that segment 527 did not look right, so with the above
form it was discovered that the 6793rd line in the anatomy file defined the segment.


Now look at file -

               .
               .
               .

[1,2]  (37.21, 18.54, 16.00)  0.58
[1,2]  (37.60, 18.54, 16.00)  0.58
[1,2]  (37.60, 19.30, 16.00)  0.58
[1,2]  (37.98, 19.30, 16.00)  0.58   <- line 6793
[10,5]  (37.60, 19.30, 16.00)  0.58
[1,2]  (37.60, 19.30, 16.00)  0.58
[1,2]  (39.13, 18.92, 16.00)  0.58
[1,2]  (39.51, 18.92, 16.00)  0.58
               .
               .
               .

The problem is that there is a loop back after the [10,5] line which connects a segment to the
segment at location (37.60, 19.30, 16.00). Two solutions are possible: either block the creation of
the segment at (37.98, 19.30, 16.00), or specify a branch point at (37.60, 19.30, 16.00). The second
solution requires adding the following line: 


[1,2]  (37.60, 18.54, 16.00)  0.58
[1,2]  (37.60, 19.30, 16.00)  0.58
[1,2]  (37.98, 19.30, 16.00)  0.58
[10,5]  (37.60, 19.30, 16.00)  0.58

;; adding this line because otherwise we have a loop
[2,1]  (37.60, 19.30, 16.00)  0.58

[1,2]  (37.60, 19.30, 16.00)  0.58
[1,2]  (39.13, 18.92, 16.00)  0.58
[1,2]  (39.51, 18.92, 16.00)  0.58

The preceeding discussion assumes that the described process does not in fact bend back and pass
through a point in space that was already assigned to another point on the process. In the
translation algorithm, it is assumed that there should be a unique assignment of points in space to
anatomical points.


The parsing of these files by SURF-HIPPO allows lines to be commented out by a leading ";".

|#

;; FIND-NODE Finds node whose :RELATIVE-LOCATION corresponds to LOCATION. If none found, returns a
;; new node named NAME with cell name CELL-NAME.
(defun find-node (location &key temp-name cell-name real-name first-nodes)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for node in first-nodes
	when (and (node-RELATIVE-LOCATION node)
		  (= (the sf (car (node-RELATIVE-LOCATION node))) (the sf (car location)))
		  (= (the sf (cadr (node-RELATIVE-LOCATION node))) (the sf (cadr location)))
		  (= (the sf (caddr (node-RELATIVE-LOCATION node))) (the sf (caddr location))))
	do (when real-name
	     (format t "changing node from ~A to ~A" (node-name node) real-name)
	     (remhash (node-name node) (NODE-HASH-TABLE))
	     (setf (NODE-HASH-TABLE real-name) node
		   (node-name node) real-name))
	;; (format t "Found node ~A from ~A.~%" (node-name node) location)
	(return node)
	finally
	;; (format t "Found node find-node-from-hash tn ~A, rn ~A.~%" temp-name real-name)
	(return (find-node-from-hash location :temp-name temp-name :cell-name cell-name :real-name real-name))))


(defun find-node-from-hash (location &key temp-name cell-name real-name)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for node being the hash-value of (NODE-HASH-TABLE)
	when (and (node-RELATIVE-LOCATION node)
		  (= (the sf (car (node-RELATIVE-LOCATION node))) (the sf (car location)))
		  (= (the sf (cadr (node-RELATIVE-LOCATION node))) (the sf (cadr location)))
		  (= (the sf (caddr (node-RELATIVE-LOCATION node))) (the sf (caddr location))))
	do (when real-name
	     (remhash (node-name node) (NODE-HASH-TABLE))
	     (setf (NODE-HASH-TABLE real-name) node
		   (node-name node) real-name))
	(return node)
	finally (when (or temp-name real-name)
		  (let ((name (or (and temp-name
				       (typecase temp-name
						 (number (* -1 (the fn temp-name)))
						 (t (format nil "temp-~A" temp-name))))
				  real-name)))
		    ;; (format t "  Find-node-from-hash creating new node ~A~%" name)
		    (return (create-node name
					 :is-physical-cell-node t
					 :cell cell-name :relative-location location))))))


(defvar *neurolucida-soma-diameter* 0.0 "In microns. For creating a soma node with negligible dimensions.")

(defvar *neurolucida-parsing-soma-segments* nil) ; When T, we are parsing lines in a neurolucida
						 ; file that follow a [2,41] code (signifying a
						 ; soma), until the next [10,*] code.

(defvar *neurolucida-parsing-axon-segments* nil) ; When T, we are parsing lines in a neurolucida
						 ; file that follow a [2,21] code (signifying a
						 ; soma), until the next [10,*] code or any code
						 ; that is not [*,21] or [*,22].


(defvar *neurolucida-one-location-per-node* nil) ; If T, then 
  
(defun read-neurolucida-file (filename &optional cell-name)
  ;; For processing Neurolucida format anatomy files. 
  (let* ((*add-cell-name-to-segs* (or *add-cell-name-to-segs* (> (length *loaded-circuit-parts*) 1)))
	 (cell (create-cell (or cell-name (pathname-name filename))))
	 (cell-name (cell-name cell))
	 soma
	 (soma-name (format nil "~a-soma" cell-name))
	 (*print-pretty* nil)
	 soma-location last-seg
	 *use-simple-names* *add-cell-name-to-segs*
	 line start (segment-id (1+ (hash-table-count (SEGMENT-HASH-TABLE)))))
    (with-open-stream (stream (open filename :direction :input))
      (loop while (setq line (read-line stream nil nil nil))
	    for line-number from 0
	    do (multiple-value-bind (code xyz radius neurolucida-codes)
		   (parse-neurolucida-line line)
		 (case code
		   (soma
		    (setq soma-location xyz
			  start xyz)
		    (find-node xyz :real-name soma-name :cell-name cell-name)
		    (setq soma (create-soma :cell cell-name
					    :diameter *neurolucida-soma-diameter*
					    :name soma-name))
		    (setf (node-relative-location (soma-node soma)) xyz)) ; A "[2,42]" code was read
		   (start (setq start xyz)) ; A "[2,xx]" code was read, where xx is not 42
		   (segment		; A "[1,xx]" code was read
					; (format t "Segment to be created ~A~%" (+ 1 segment-id))
		    (unless (equal start xyz) ; bogus points not allowed
		      (incf segment-id)
		      (when *neurolucida-one-location-per-node*
			(find-node xyz :real-name segment-id ; (format nil "~A" segment-id)
				   :cell-name cell-name))
		      (let ((seg (create-segment segment-id
						 (find-node start :temp-name segment-id
					; (format nil "temp-~A" (incf temp-segment-id))
							    :cell-name cell-name
							    :first-nodes
							    (when last-seg
							      (list ; (segment-node-1 last-seg)
								    (segment-node-2 last-seg))))
						 cell
						 :diameter (* 2.0 radius)
						 :relative-location xyz)))
			(setq start xyz
			      last-seg seg)
			(element-parameter seg 'neurolucida-codes neurolucida-codes)			
			(element-parameter seg 'file-line-number line-number)
			(cond
			  (*neurolucida-parsing-soma-segments*
			   (add-soma-segment soma seg)
			   (element-parameter seg 'soma-segment t))
			  (*neurolucida-parsing-axon-segments*
			   (element-parameter seg 'axon-segment t)))))))))
      ; (zero-soma-position cell)
      ))
  (destroy-zero-length-segments (segments))
  t)

(defun check-neurolucida-file (filename)
  (let ((*print-pretty* nil)
	branch-points match
	segment-points
	line start)
    (format t "Reading file...~%") 
    (with-open-stream (stream (open filename :direction :input))
      (loop while (setq line (read-line stream nil nil nil))
	    for line-number from 0
	    do (multiple-value-bind (code xyz radius neurolucida-codes)
		   (parse-neurolucida-line line)
		 (case code
		   ((start soma) (push (list xyz line-number) branch-points)
		    (setq start xyz))
		   (segment		; A "[1,xx]" code was read
					; (format t "Segment to be created ~A~%" (+ 1 segment-id))
		    (unless (equal start xyz) ; bogus points not allowed
		      (push (list xyz line-number) segment-points))
		    (setq start xyz)
		    )))))
    (format t "Checking points...~%") 
    (loop for segment-point in segment-points
					;	  do (format t "Checking ~A...~%" segment-point)
	  when (setq match (loop for candidate in segment-points
				 when (and
				       (not (eq (cadr segment-point) (cadr candidate)))
				       (loop for num1 in (car segment-point)
					     for num2 in (car candidate)
					     unless (= num1 num2) do (return nil)
					     finally (return t)))
				 do (return candidate)))
	  do (format t "Redundant segment points: Location ~A, Lines ~A and ~A~%"
		     (car segment-point)
		     (cadr segment-point)
		     (cadr match))
	  and collect match into matches)))

(defun parse-neurolucida-line (line)
  ;; Ignores lines unless they start with "["
  (when (and (> (length line) 0) (equal (schar line 0) #\[))
    (multiple-value-bind (major-code index)
	(read-from-string line nil nil :start 1)
      (let ((minor-code (parse-integer line :junk-allowed t :start (1+ index))))
	(when (< minor-code 111)
	  (case major-code
	    (10 (setq *neurolucida-parsing-soma-segments* nil)
		(setq *neurolucida-parsing-axon-segments* nil))	      
	    ((1 2)
	     (multiple-value-bind (x index)
		 (read-from-string line nil nil :start (1+ (search "(" line)))
	       (multiple-value-bind (y index)
		   (read-from-string line nil nil :start (1+ index))
		 (let ((Z (read-from-string line nil nil :start (1+ index)))
		       (radius (read-from-string line nil nil :start (1+ (search ")" line)))))
		   (case major-code
		     (2 (values (case minor-code
				  (41 (setq *neurolucida-parsing-soma-segments* t) 'soma)
				  ((22 21) (setq *neurolucida-parsing-axon-segments* t) 'start)
				  (t 'start))
				(list x y z) nil
				(list major-code minor-code)))
		     (1 (case minor-code
			  (42 (setq *neurolucida-parsing-soma-segments* t))
			  ((22 21) (setq *neurolucida-parsing-axon-segments* t))
			  (t (setq *neurolucida-parsing-axon-segments* nil)
			     (setq *neurolucida-parsing-soma-segments* nil)))
			(values 'segment (list x y z) radius (list major-code minor-code))))))))))))))
		   
;;;[10,101]  (-11.09, -184.92, -14.70)  0.24
;;;[1,2]  (-11.09, -184.92, -14.70)  0.24
;;;[2,2]  (-13.75, -103.75, -10.50)  0.24
;;;[1,2]  (-14.69, -108.55, -11.30)  0.24
;;;[1,2]  (-14.84, -108.55, -11.30)  0.24
;;;[1,2]  (-15.16, -114.64, -11.50)  0.24
;;;[1,2]  (-14.37, -117.84, -12.60)  0.24
;;;[1,2]  (-13.59, -121.20, -13.20)  0.24
;;;[1,2]  (-14.22, -126.16, -13.00)  0.24
#|
(progn 	
  (initialize-globals-for-circuit)
  (READ-NEUROLUCIDA-FILE  "/home/lyle/surf-hippo/anatomy/turner/ca1/n120.fix")
  (setf (soma-diameter *soma*) 1.0)
;  (loop for seg in (segments)
;	unless (node-name (segment-node-1 seg))
;	do (setf (segment-node-1 seg) (soma-node (car (somas))))
;	(push seg (node-elements (soma-node (car (somas))))))


  (loop for seg in (segments)
	unless (or (eq (segment-node-1 seg) (soma-node *soma*)) (proximal-segment seg))
	do (print-element seg) (transfer-node-elements (segment-node-1
							seg) (soma-node *soma*)))


  (PROCESS-CIRCUIT-STRUCTURE T)
  (setq *circuit* "n120")
)
|#

#|
	(setq temp (loop for seg in (segments)
	      unless (or (eq (segment-node-1 seg) (soma-node *soma*))
			 (proximal-segment seg))
	      collect (length (segments-out seg))) g t)


(loop for seg in (segments)
      when (search "temp" (node-name (segment-node-1 seg)))
      collect seg)
|#

(defun print-prox-neighbors (segments &optional draw-them)
  (loop for elt in (if (consp segments) segments (list segments ))
	do (setq elt (element elt))
	when (segment-p elt)
	do (multiple-value-bind (neighbor distance)
	       (closest-element elt (segments-out elt) t)
	     (format t "Distance btwn ~A and soma: ~A,~%   Closest elt to proximal node: ~A at ~A~%" 
		     elt
		     (as-the-crow-flies *soma* (node-absolute-location (segment-node-1 elt)))
		     neighbor distance)
	     (when draw-them
	       (let ((win (car (win-menu))))
		 (when win
		   (draw-chosen-one (list neighbor elt) win)
		   (opal:update win)
		   (go-ahead-menu)

		   ))))))



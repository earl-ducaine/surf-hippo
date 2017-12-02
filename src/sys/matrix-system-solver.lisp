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


;;; SYS Source file: matrix-system-solver.lisp
(in-package "SURF-HIPPO")

;; SOLVE-MATRIX-SYSTEM
;;
;; Implicit integration for X(i+1), for the set of N differential equations
;;
;;        dX/dt = AX
;;
;; The key arg ELEMENT is just used to find already created arrays.
;;
(defun solve-matrix-system (A Xi delta-t &key element A-LEFT RHS P)
  (declare (optimize (speed 3) (space 0) (compilation-speed 0))
	   (double-float delta-t)
	   (type (simple-array double-float (* *)) A)
	   (type (simple-array double-float (*)) Xi))
  (let* ((N (length Xi))
	 (N-1 (1- N))
	 (delta-t/2 (/ delta-t 2))
	 (rhs (or RHS (find-element-array element 'rhs N)))
	 (A-left (or A-LEFT (find-element-array element 'left-hand-matrix (list N N))))
	 (P (or P (find-element-array element 'permutation-array N 'fixnum))))
    (declare (type (simple-array fixnum (*)) P)
	     (type (simple-array double-float (* *)) A-left)
	     (type (simple-array double-float (*)) RHS)
	     (fixnum N N-1))
    (loop for i fixnum from 0 to N-1 do
	  (loop for j fixnum from 0 to N-1 do
		(setf (aref A-left i j) (- (if (= i j) 1.0 0.0) (* delta-t/2 (aref A i j))))))
    (matrix-multiply-mat-col-double A Xi rhs) ; A*Xi -> RHS, which still needs adjusting below
    (loop for i fixnum from 0 to N-1 do (setf (aref rhs i) (+ (* delta-t/2 (aref rhs i)) (aref Xi i))))
    (matrix-solve-double a-left P rhs)	; result into RHS
    ))

(defun make-differential-equation-a-array (nested-list-of-coefficients)
  (double-float-2darray (2dseq-to-array nested-list-of-coefficients)))


#|

(defvar a (make-array '(4 4) :element-type 'double-float))
(defvar a-left (make-array '(4 4) :element-type 'double-float))
(defvar rhs (make-array '(4) :element-type 'double-float))
(defvar X (make-array '(4) :element-type 'double-float))
(defvar p (make-array '(4) :element-type 'fixnum))

(let* ((delta-t 1.0d0)
       (time-list '())
       (n 5)
       (rhs (make-array n :element-type 'double-float))
       (p (make-array n :element-type 'fixnum))
       (a-left (make-array (list n n) :element-type 'double-float))
       (a (make-array (list n n) :element-type 'double-float))
       (X (make-array n :element-type 'double-float))
       (results (make-array n)))
  (loop for i from 0 to (1- n) do (setf (aref results i) '()))
  (loop for i from 0 to (1- n) do (setf (aref x i) 0.0d0))
  (setf (aref x 0) 1.0d0)
  (loop for i from 0 to (1- n) do
	(loop for j from 0 to (1- n) do
	      (setf (aref A i j)
		    (cond ((= i j) (* -.01d0 (1- n)))
			  ; ((= (abs (- i j)) 1) .01d0)
			  (t (/ 0.0136d0 (abs (- i j)))
			     ; 0.0d0
			     )))))

  (format t "A: ~A  ~%A-left:~A ~% rhs: ~A ~% X: ~A ~%P: ~A~%"
	  a a-left rhs x p)
  
  (loop for time from 0.0 to 1300 by delta-t do
	(loop for i from 0 to (1- n) do
	      (push (aref x i) (aref results i))) 
	(push time time-list)
	(solve-matrix-system A X delta-t :A-LEFT a-left :rhs RHS :p P)
	(loop for i from 0 to (1- n) do
	      (setf (aref x i) (aref rhs i))))
  
  (plot-timed-data
   (loop for i from 0 to (1- n) 
	 collect (aref results i))
   nil time-list))

|#

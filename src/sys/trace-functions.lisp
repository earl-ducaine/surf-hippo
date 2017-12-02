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


;;; SYS Source file: trace-functions.lisp
(in-package "SURF")

; The following version of transformed-node-output transforms the plot-list into the synaptic innactivation
; of the input.  The parameters are: the time constant of the innactivation
; and the time constant of the reactivation.

(defun SYNAPTIC-INACTIVATION (plot-list tau1 tau2)
  (without-floating-underflow-traps		
    (let ((transformed-plot-list nil)
	  (times (cadar plot-list))       
	  dt)                             
      (dolist (nodes plot-list)           
	(let* ((concentrations  (car nodes))
	       (input          (car concentrations))      
	       (close          (/ tau1 (+ tau1 (* tau2 input))))      
	       (output         (list (* input close)))
	       (t1             (car times))) 
	      (do ((tt (cdr times) (cdr tt)) (cc (cdr concentrations) (cdr cc))) 
		  ((not tt))
		(setq dt (- (car tt) t1))
		(setq t1 (car tt))
		(setq close (+ close (- (* (- 1 close) (/ dt tau2)) (* close input (/ dt tau1)))))
		(setq input (car cc))
		(setq output (nconc output (list (* input close)))))
	      (setq transformed-plot-list (nconc transformed-plot-list (list (list output times))))))
      transformed-plot-list)))





; The following version of transformed-node-output transforms the plot-list into the synaptic innactivation
;  of the input via second messanger feedforward.  The parameters are: the time constants of activation and innactivation,
; the messenger's degree of cooperativity, and the threshold of innactivation.

(defun SECOND-MESSENGER (plot-list tau1 tau2 power threshold)
  (without-floating-underflow-traps		
    (let ((transformed-plot-list nil)
	  (times (cadar plot-list))       
	  dt)                             
      (dolist (nodes plot-list)           
	(let* ((concentrations  (car nodes))
	       (input          (car concentrations))
	       (inhibitor      (/ (* input tau2) tau1)) 
	       (output         (list (/ input (+ threshold (expt inhibitor power)))))
	       (t1             (car times))) 
	      (do ((tt (cdr times) (cdr tt)) (cc (cdr concentrations) (cdr cc))) 
		  ((not tt))
		(setq dt (- (car tt) t1))
		(setq t1 (car tt))
		(setq inhibitor (+ inhibitor (* dt (- (/ input tau1) (/ inhibitor tau2)))))
		(setq input (car cc))
		(setq output (nconc output (list (/ input (+ threshold (expt inhibitor power)))))))
	      (setq transformed-plot-list (nconc transformed-plot-list (list (list output times))))))
      transformed-plot-list)))





; The following output function transforms the output to an exponential release function of the
; difference from resting potential. The parameters are the voltage-constant and the constant that
; multiplies the output.

(defun EXPONENTIAL-RELEASE (plot-list voltage-constant output-constant)
  (without-floating-underflow-traps                       
    (static-release
      `(function (lambda (x) (* ,output-constant (exp (/ (- x *e-l*) (float ,voltage-constant))))))
      plot-list)))




; The following output function transforms the output to a linear release function of the
; difference from resting potential.  

(defun LINEAR-RELEASE (plot-list)
  (without-floating-underflow-traps                       
    (static-release
      `(function (lambda (x) (- x *e-l*)))
      plot-list)))



; If the input is above resting potential, then the following function transforms the output to a
; linear release function of the difference from resting potential.  Otherwise, the output is zero.

(defun LINEAR-THRESHOLD-RELEASE (plot-list)
  (without-floating-underflow-traps                       
    (static-release
      `(function (lambda (x) (if (> x *e-l*) (- x *e-l*) 0))) 
      plot-list)))


;;;This function is a subroutine to be used with various synaptic release transforms.  The parameter transform
;;;is the transform to be performed on the plot-list. 

(defun STATIC-RELEASE (transform plot-list)
  (let ((transformed-plot-list '()))
    (dolist (values plot-list)
      (setq transformed-plot-list
	    (nconc transformed-plot-list (list (list (mapcar (eval transform) (car values)) (cadr values))))))
    transformed-plot-list))



;;; This function chooses the sequences of operations to be applied to the plot-list and set its parameters. 

(defun TRANSFORM-NODE-OUTPUT (plot-list)
  (synaptic-inactivation (exponential-release plot-list 5.0 .1) 1000.0 1000.0))  ;;; voltage-constant, release-constant
                                                                       ;;; innactivation, and reactivation.


;;; The following function extracts the portion of a data-list that lies between time beginning and end.

(defun PARTIAL-DATA  (plot-list beginning end) 
  (let ((transformed-plot-list nil)
	(times (cadar plot-list))
	head-cuts
	tail-cuts)
    (do ((i 0 (+ i 1)) (list times (cdr list)))
	((>= (car list) beginning) (setq head-cuts i) (setq times list))) 
    (do ((i 0 (+ i 1)) (list times (cdr list)))
	((or (not list) (>  (car list) end)) (setq tail-cuts (- (length times) i)) (setq times (butlast times tail-cuts))))
    (dolist (nodes plot-list)
      (let ((voltages  (butlast (nthcdr head-cuts (car nodes)) tail-cuts)))
	(setq transformed-plot-list (nconc transformed-plot-list (list (list voltages times))))))
    transformed-plot-list))








;;; not important functions


;;; FIND-TIME-INDEX Returns the array index for the output arrays that corresponds to "target-time".
(defun find-time-index (target-time)
  (let ((time (reverse *sim-reverse-plot-time-list*))
	t-1 t+1) 
    (do ((index 0 (1+ index)))
	(())
      (setq t-1 (nth index time)
	    t+1 (nth (+ 1 index) time))
      (if (and (< target-time t+1)(>= target-time t-1))
	  (return index)))))

(defun convert-data-time-base (data-list delta-t &optional duration)
  (let* ((output-data '())
	 (time-list *sim-plot-time-list*)
	 (t-n (car time-list))
	 (t-n+1 (cadr time-list))
	 (y-n (car data-list))
	 (y-n+1 (cadr data-list)))
    (do ((output-time 0.0 (+ output-time delta-t)))
	((or (if duration (= output-time duration))
	     (null y-n+1)))
      (loop until (if (or (if duration (= output-time duration))
			  (null y-n+1))
		      t
		      (<= t-n output-time t-n+1))
	    do (setq time-list (cdr time-list)
		     data-list (cdr data-list)
		     t-n (car time-list)
		     t-n+1 (cadr time-list)
		     y-n (car data-list)
		     y-n+1 (cadr data-list)))
      (if  (not (or (if duration (= output-time duration))
		    (null y-n+1)))
	   (push (/ (+ (* y-n (- t-n+1 output-time))
		       (* y-n+1 (- output-time t-n)))
		    (- t-n+1 t-n))
		 output-data)))
    output-data))
	
	
;; make-even-time-list
(defun make-even-time-list (list delta-t &optional duration)
  (let ((even-list '())(time-list '())
	(list-array (list-to-array list)))
    (dotimes (i (round (/ duration delta-t)))
      (push (aref list-array (find-time-index (* i delta-t))) even-list)
      (push (* i delta-t) time-list))
    (list  even-list time-list)
	  ))

;;; MAKE-SNAPSHOT Returns a list of two lists, the first corresponding to the elements from each array in the
;;; array-list which correspond to "time", and the second given by the argument "x-list". 
(defun make-snapshot (time array-list x-list)
 (let ((time-index (find-time-index time))(y-list '()))
 (dolist (array array-list)
 (setq y-list (nconc y-list (list (aref array time-index 1)))))
 (list (list y-list x-list))))


;;; MAKE-ALBUM
(defun make-album (time-list array-list x-list)
 (let ((output-list '()))
 (dolist (time time-list)
 (let ((plot-list (make-snapshot time array-list x-list)))
	(setq output-list (nconc output-list plot-list))))
 output-list))






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

;;; SYS Source file: working.lisp

(in-package "SURF-HIPPO")


(defvar fixed-data '())


;; check this
(defun save-fixed (label)
  (push (list (convert-data-time-lists (reverse (car (retrieve-plot-data (list (list *plot-soma-voltages* `node-voltage)))))
				 (reverse *sim-reverse-plot-time-list*) 1.0)
	      label)
        fixed-data)
  t)	



(defun plot-g (index-1 index-2 current y-label)
  (let ((label (format nil "~A-~A"
		       (cdr (nth index-1 fixed-data))
		       (cdr (nth index-2 fixed-data))))
	(g-list (loop for y1 in (caar (nth index-1 fixed-data))
		      for y2 in (caar (nth index-2 fixed-data))
		      for time1 in (cadar (nth index-1 fixed-data))
		      for time2 in (cadar (nth index-2 fixed-data))
		      when (> (- y2 y1) 0) collecting (/ current (- y2 y1)) into g-list
		      when (> (- y2 y1) 0) collecting time1 into time1-list
		      when (> (- y2 y1) 0) collecting time2 into time2-list
		      finally (return (list g-list time1-list time2-list)))))
    (break)
    (plot-timed-data (list (car g-list))
		     (list label)
		     (nth 1 g-list)
		     :y-label y-label)))







(defun plot-t-dep (tstart tref deltag qten)
  (let ((xy (loop for temp from (float tstart) to 310 by 1.0 collecting (- temp 273) into temperature
		  collecting (exp (/ (/ (* 1000 deltag) -1.987) temp)) into rate
		  finally (return (list temperature rate))))
	(qxy (loop for temp from (float tstart) to 310 by 1.0 collecting (- temp 273) into temperature
		   collecting (* (exp (/ (/ (* 1000 deltag) -1.987) tref))
				 (expt qten (/ (- temp tref) 10.0)))  into rate
		   finally (return (list temperature rate)))))
    (plot-xy-data (list xy qxy) 
		  (list (format nil "Boltzman Equation - Delta G = ~AKcal/mole" deltag)
			(format nil "Qten = ~A, T_ref = ~AC" qten (- tref 273)))
		  :x-min (- tstart 273) :x-inc 4	:x-label "Temperature (C)" :y-label "Rate coefficient"))) 


(defun chaos (r a x_0 &optional (finish 500) (start 0.0))
  (let (x-list time-list)
    (do ((time start (1+ time))
	 (x (* r x_0 (- a x_0))
	    (* r x (- a x))))
	((= time finish))
      (push x x-list)
      (push time time-list))
;    (break)
    (plot-timed-data
     (list (reverse x-list))
     '("chaos")
     (reverse time-list))))

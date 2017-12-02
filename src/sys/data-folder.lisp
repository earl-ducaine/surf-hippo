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


;;; SYS Source file: data-folder.lisp

(in-package "SURF-HIPPO")

;;; The format of *DATA-FOLDER* is:

;;; ((simulation-1-name . (('time . time-step-or-time-list)
;;;                        ('data . ((trace-name units data-list) (trace-name units data-list) ... ))))
;;;  (simulation-2-name . (('time . time-step-or-time-list)
;;;                        ('data . ((trace-name units data-list) (trace-name units data-list) ... ))))
;;;             .
;;;             .
;;;             .
;;;             .
;;;
;;;  (simulation-n-name . (('time . time-step-or-time-list)
;;;                        ('data . ((trace-name units data-list) (trace-name units data-list) ... ))))
;;; )

(defun plot-folder-traces ()
  (let ((dummy1 :simulation))
    (choose-variable-values
     '((dummy1 "Choose folder traces by" :choose (:simulation :trace-units)))
     :label "Select Folder Traces According to Simulations or Data Type")
    (let ((selected-simulations
	   (choose-list-values-from-keys
	    (loop for thing in *data-folder* collect (list (car thing) thing)) nil
	    :label "Choose Simulations in Data Folder")))
      (case dummy1
	(:simulation
	 (loop for simulation in selected-simulations
	       do
	       (let ((leftover-data-lists (cadr (assoc 'data (cadr simulation)))))
		 (loop while leftover-data-lists do
		       (let ((current-units (nth 1 (car leftover-data-lists))))
			 (setq leftover-data-lists 
			       (loop for data-list in leftover-data-lists
				     when (equal current-units (nth 1 data-list))
				     collect data-list into current-data-lists
				     else  collect data-list into leftover-data-lists
				     finally
				     (let ((*sim-reverse-plot-time-list*
					    (if (listp (cadr (assoc 'time (cadr simulation))))
						(cadr (assoc 'time (cadr simulation)))
						(reverse
						 (loop for time from 0.0 to (length (caar current-data-lists))
						       collect time))))
					   (plotted-data-lists
					    (choose-list-values-from-keys
					     (loop for current-data-list in current-data-lists
						   collect (list (car current-data-list) current-data-list))
					     nil
					     :label (format nil "Plot Simulation ~a traces with ~a units"
							    (car simulation) current-units))))
				       (when plotted-data-lists
					 (let ((*simulation-name* (car simulation))
					       (*create-new-plot-windows* t)
					       (*overlay-simulations nil))
					   (surf-plot
					    (loop for plotted-data-list in plotted-data-lists
						  collect (car (last plotted-data-list)))
					    (loop for plotted-data-list in plotted-data-lists
						  collect (first plotted-data-list))
					    (format nil "~a Traces" (nth 1 (car plotted-data-lists)))
					    :y-label (nth 1 (car plotted-data-lists))))))
				     (return leftover-data-lists))))))))
	(:trace-units
	 (let ((leftover-data-lists (loop for simulation in selected-simulations
					  nconcing
					  (loop for data-list in (cadr (assoc 'data (cadr simulation)))
						collect (list data-list (car simulation)))))
	       plot-lists
	       available-units)
	   (loop while leftover-data-lists do
		 (setq leftover-data-lists
		       (let ((current-units (nth 1 (caar leftover-data-lists))))
			 (push current-units available-units)
			 (loop for data-list in leftover-data-lists
			       when (equal current-units (nth 1 (car data-list)))
			       collect data-list into current-data-lists
			       else collect data-list into remainder
			       finally
			       (push current-data-lists plot-lists)
			       (return remainder)))))
	   (setq available-units (choose-list-values available-units nil :label "Choose trace units to plot"))
	   (loop for plot-list in plot-lists
		 when (member (nth 1 (caar plot-list)) available-units :test 'equal)
		 do
		 (let ((units (nth 1 (caar plot-list)))
		       time-list)
		   (loop for data-list in
			 (choose-list-values-from-keys
			  (loop for unit-trace in plot-list collect (list (caar unit-trace) unit-trace)) nil
			  :label (format nil "Choose traces with ~a units" units))
			 collect (nth 0 (car data-list)) into labels
			 when (equal units "Event")
			 collect (nth 2 (car data-list)) into event-lists
			 else
			 collect
			 (list (let ((time-info
				      (cadr (assoc 'time
						   (cadr
						    (find (cadr data-list) *data-folder* :key 'car :test 'equal))))))
				 (if (listp time-info)
				     time-info
				     (if time-list time-list
					 (setq time-list (reverse
							  (loop for time from 0.0 to (length  (nth 2 (car data-list))) by
								time-info collect time))))))
			       (nth 2 (car data-list)))
			 into xy-data-lists
			 finally
			 (when event-lists
			   (raster-plots :event-element-labels labels :only-event-generators nil
					 :event-data-lists event-lists))
			 (when xy-data-lists
			   (let ((title (let ((dummy1 (format nil "Data Folder ~a Units Plot" units)))
					  (choose-variable-values
					   '((dummy1 "Plot title" :string))
					   :label
					   (format nil "Choose Title for Data Folder Plot with ~a Units" units))
					  dummy1)))
			     (plot-xy-data
			      xy-data-lists
			      labels
			      :win (get-plot-window units nil title)
			      :y-label units
			      ))))))))))))
	 
	     
  

(defun traces-to-folder (&optional automatic)
  (let* ((folder-list
	  (assoc *simulation-name* *data-folder*))
	 (folder-traces
	  (cadr (assoc 'data (cadr folder-list))))
	 (folder-trace-names
	  (loop for trace-list in folder-traces collect (car trace-list)))
	 (data-lists
	  (loop for plot-list-info in *plot-lists-info*
		when
		(and (symbol-value (nth 3 plot-list-info))
		     (symbol-value (nth 0 plot-list-info)))
		nconcing
		(loop for pointer in
		      (if automatic
			  (symbol-value (nth 0 plot-list-info))
			  (choose-list-values
			   (symbol-value (nth 0 plot-list-info))
			   (loop for orig-trace-name in (symbol-value (nth 0 plot-list-info))
				 when (member (format nil "~a-~a" orig-trace-name *simulation-name*)
					      folder-trace-names :test 'equal)
				 collect orig-trace-name)
			   :label (format nil "Data Folder: Store/Remove ~a ~a Traces "
					  *simulation-name* (nth 5 plot-list-info))))
		      collect
		      (list (format nil "~a-~a" pointer *simulation-name*)
			    (nth 5 plot-list-info)
			    (if
			     (equal 'Event (nth 2 plot-list-info))
			     (get-events pointer)
			     (retrieve-single-data pointer (nth 2 plot-list-info))))))))
    (setq *data-folder* (remove folder-list *data-folder*))
    (when data-lists	 
      (push
       (list *simulation-name*
	     (list
	      (list 'time (if (eq *INTEGRATION-TIME-REFERENCE* :fixed) ; *use-fixed-step*
			      *user-step* *sim-plot-time-list*))
	      (list 'data data-lists)))			  
       *data-folder*))
    nil))
	  

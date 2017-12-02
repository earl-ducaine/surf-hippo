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


;;; SYS Source file: cell-graphics-virtual-schema.lisp


(IN-PACKAGE "SURF-HIPPO")

(s-value virtual-segment :line-style (o-formula (if (and (gvl :parent :window) (gvl :parent :window :colorize))
						  (access-*line-styles-array*-for-segment-voltage
						   (aref (gvl :item-values) 5) ; thickness
						   (aref (gvl :item-values) 6)) ; segment
						  (or (aref (gvl :item-values) 4) ; linestyle
						      (and (gvl :parent :window)
							   (create-instance nil (gvl :parent :window :default-line-style)
									    (:thickness (aref (gvl :item-values) 5))))))))

(s-value virtual-soma :line-style (o-formula (access-*line-styles-array*-for-segments-fast 0 (gvl :filling-style :FOREGROUND-COLOR) 100.0 nil)))

(s-value virtual-soma :filling-style (o-formula (if (and (gvl :parent :window)
							 (gvl :parent :window :colorize)
							 (fourth (gvl :item-values)))
						  (access-*fill-styles*-for-soma-voltage (fifth (gvl :item-values)))
						  (or (fourth (gvl :item-values))
						      (and (gvl :parent :window)
							   (gvl :parent :window :default-graphics-filling-style))))))

#|
(defmacro generate-update-slots-function (virtual-thing)
  `(let ((slots (g-value ,virtual-thing :update-slots)))
    `#'(lambda (dummy dummy-update-slots-values)
	 ,@(loop for slot in slots
		 for dummy-vals-indx fixnum from 0
		 collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx)
			   (g-value dummy ,slot))))))

(defmacro generate-update-slots-function (virtual-thing)
  `(let ((slots (g-value ,virtual-thing :update-slots)))
    `#'(lambda (dummy dummy-update-slots-values)
	 ,@(loop for slot in slots
		 for dummy-vals-indx fixnum from 0
		 collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx)
			   (g-value dummy ,slot))))))

(defmacro generate-update-slots-function (virtual-thing)
  #'(lambda (dummy dummy-update-slots-values)
      `(let ((slots (g-value ,virtual-thing :update-slots)))
	,@(loop for slot in slots
	   for dummy-vals-indx fixnum from 0
	   collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx)
		     (g-value dummy ,slot))))))

(s-value virtual-segment :generate-update-slots-function
	 (GENERATE-UPDATE-SLOTS-FUNCTION virtual-segment))

(s-value virtual-segment :generate-update-slots-function
	 #'(LAMBDA (DUMMY DUMMY-UPDATE-SLOTS-VALUES)
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 0) (G-VALUE DUMMY :VISIBLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 1) (G-VALUE DUMMY :FAST-REDRAW-P))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 2) (G-VALUE DUMMY :X1))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 3) (G-VALUE DUMMY :X2))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 4) (G-VALUE DUMMY :Y1))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 5) (G-VALUE DUMMY :Y2))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 6) (G-VALUE DUMMY :LINE-STYLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 7) (G-VALUE DUMMY :FILLING-STYLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 8) (G-VALUE DUMMY :DRAW-FUNCTION))
	     ))
|#



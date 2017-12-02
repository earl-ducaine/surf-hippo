;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

#|

====================================================================================================
			       The Surf-Hippo Neuron Simulator System
====================================================================================================

This code was written as part of the Surf-Hippo Project, originally at the Center for Biological
Information Processing, Department of Brain and Cognitive Sciences, Massachusetts Institute of
Technology, and currently at the Neurophysiology of Visual Computation Laboratory, CNRS.
                                                                                 
Permission to use, copy, modify, and distribute this software and its documentation for any purpose
and without fee is hereby granted, provided that this software is cited in derived published work,
and the copyright notice appears in all copies and in supporting documentation. The Surf-Hippo
Project makes no representations about the suitability of this software for any purpose. It is
provided "as is" without express or implied warranty.
                                                                                 
If you are using this code or any part of Surf-Hippo, please contact surf-hippo@ai.mit.edu to be put
on the mailing list.
                                                                                 
Copyright (c) 1989 - 2003, Lyle J. Graham                                                                                              

|#

;; This file is modified from Garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; LBG 3/30/94 Taken from the original ps.lisp file, since these methods for virtual aggregates
;;; were commented out for some reason (?).

;;;
;;; Make-PS-File
;;;
;;; The function Make-PS-File generates postscript files from Garnet windows.
;;; The resulting files may be sent directly to a postscript printer or
;;; included in larger Scribe and LaTex documents.
;;;
;;; Designed and implemented by Andrew Mickish
;;;

(in-package "OPAL")


(define-method :ps-register-fn OPAL:VIRTUAL-AGGREGATE (obj)
  (let ((dummy-item (g-value obj :dummy-item)))
    (when dummy-item
      (kr-send dummy-item :ps-register-fn dummy-item)
      (check-ls-color dummy-item))))

;;
;; Virtual Aggregate
;;

(define-method :ps-object OPAL:VIRTUAL-AGGREGATE (gob)
	          (declare ; (optimize (safety 0) (speed 3) (space 1))
		   )
	       (let* ((dummy (g-value gob :dummy-item))
		      (update-info (g-value dummy :update-info))
		      (item-array (g-value gob :item-array))
		      (array-size (g-value gob :array-length)))
		 (if (numberp array-size)
		     ;; 1-dimensional
		     (dotimes (n (g-value gob :next-available-rank))
		       (s-value dummy :rank n)
		       (s-value dummy :item-values  (aref  item-array n))
		       (opal::update-slots-values-changed dummy 0 update-info)
		       (when (and (aref item-array n) (g-value dummy :visible))
			 (kr-send dummy :ps-object dummy)))
		     ;; 2-dimensional
		     (dotimes (n (first array-size))
		       (dotimes (m (second array-size))
			 (s-value dummy :rank1 m)
			 (s-value dummy :rank2 n)
			 (s-value dummy :item-values (aref item-array m n))
			 (opal::update-slots-values-changed dummy 0 update-info)
			 (when (and (aref item-array m n) (g-value dummy :visible))
			   (kr-send dummy :ps-object dummy)))))))

(define-method :ps-object OPAL:LINE (obj)
	         (declare ; (optimize (safety 0) (speed 3) (space 1))
		  )
  ;; Parameters: line-halftone, line-cap, line-join, dash-pattern,
  ;;             line-thickness, x2, y2, x1, y1
  (when (g-value obj :line-style)
    (print-line-qualities obj)
    (format T "~S ~S ~S ~S " (g-value obj :x2)
	    (convert-y (g-value obj :y2))
	    (g-value obj :x1)
	    (convert-y (g-value obj :y1)))
    (format T "DrawLine~%")))


;;; lbg 4/9/94 some changes which speed up make-ps-file by factor of ~600.

#|
(defun print-graphic-qualities (obj)
  (print-line-qualities obj)
  (let ((filling-style (g-value obj :filling-style)))
    ;; Print fill-halftone
    (print-color-info filling-style :foreground-color)))
|#

;; Change to make thin polylines
(defvar *use-sublinear-polyline-ps-thickness* t "When T, the thickness of thin polylines in PS files is reduced.")
(export '(*USE-SUBLINEAR-POLYLINE-PS-THICKNESS*))

(defun print-line-qualities (obj)
;;     (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((line-style (g-value obj :line-style))
	(polyline-p (and *use-sublinear-polyline-ps-thickness* (member opal::polyline (g-value obj :is-a)))))
    (if line-style
	(let* ((line-thickness (let ((lt (g-value line-style :line-thickness)))
				 (if polyline-p
				   (cond ((= lt 0) 0)
					 ((= lt 1) 0.5)
					 ((= lt 2) 1)
					 ((= lt 3) 2)
					 (t lt))
				   (if (eq lt 0) 1 lt))))
	       (line-cap (case (g-value line-style :cap-style)
			   (:butt 0) (:round 1) (:projecting 2)
			   (otherwise 0)))
	       (line-join (case (g-value line-style :join-style)
			    (:miter 0) (:round 1) (:bevel 2)
			    (otherwise 0)))
	       (dash-pattern (let ((dp (g-value line-style :dash-pattern)))
			       (if dp
				   (substitute #\[ #\(
				     (substitute #\] #\)
				       (concatenate 'string
					 (prin1-to-string dp) " ")))
				   "[] "))))
	  (print-color-info line-style :foreground-color)
	  (format T "~S ~S " line-cap line-join)
;;; here is the change lbg
;;	  (format T   dash-pattern)
	  (format T "~a"  dash-pattern)
	  (format T "~S " line-thickness))
	;; Don't draw a line
	(format T "[0 0 0] 0 0 [] -1 "))))

(defun print-color-info (style ground)
  (declare ; (optimize (safety 0) (speed 3) (space 1))
   )
  (if style
      (let* ((color (g-value style ground))
	     (stipple (g-value style :stipple))
	     (gray (if stipple (g-value stipple :percent))))
	(if (arbitrary-pattern-p style)
	    (handle-arbitrary-pattern style)
	    (if gray
	        (format t "[~A dup dup] "
			(ps-number
			 (float (/ (- 100 gray) 100))))
		(let ((red (g-value color :red))
		      (green (g-value color :green))
		      (blue (g-value color :blue)))
		  (if *color-p*
                      (format t "[~A ~A ~A] "
			      (ps-number-between-zero-and-one red)
                              (ps-number-between-zero-and-one green)
                              (ps-number-between-zero-and-one blue))
					;(format t "[0 0 0] ")
		      (format t "[~A dup dup] "
			      (/
			       (the single-float (+ (the single-float red)
						    (the single-float
							 (+
							  (the single-float  green)
							  (the single-float blue)))))
			       3.0))
		      )))))
					; Should we just print a gray scale? "[~A dup dup] "
      (format t "null ")))


(defun convert-angle (x)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (round (the single-float (* 180.0 (the single-float (/ x 3.1415927))))))

(defun ps-number (x)
  (declare ;(optimize (safety 0) (speed 3) (space 1))
   )
  ;; If x is float, format with two decimal places.  Else, format as integer.
  (if (or (integerp x) (= x (round x)))
      (prin1-to-string (round x))
      (format NIL "~,2F" x)))

(defun ps-number-between-zero-and-one (x)
  (declare ; (optimize (safety 0) (speed 3) (space 1))
   )
  ;; If x is float, format with two decimal places.  Else, format as integer.
  (if (integerp x)
      (cond ((= 0 x) "0")
	    (t "1"))
      (cond ((= 0.0 (the single-float x)) "0")
	    ((= 1.0 (the single-float x)) "1")
	    (t (format NIL "~,2F" x)))))

(defun print-window-background (win)
  (let ((background-color (g-value win :background-color)))
    (if background-color
	(let ((red (g-value background-color :red))
	      (green (g-value background-color :green))
	      (blue (g-value background-color :blue)))
	  (if *color-p*
	      (format t "0 0 ~S ~S [0 0 0] 0 0 [] -1 [~A ~A ~A] DrawRectangle~%"
		      ;; XXX This size will be wrong if clip-p is NIL
		      (g-value win :width) (g-value win :height)
		      ;;; XXX: Optimize with BLACK and WHITE color tokens
		      (ps-number-between-zero-and-one red)
		      (ps-number-between-zero-and-one green)
		      (ps-number-between-zero-and-one blue))
	      (format t "0 0 ~S ~S [0 0 0] 0 0 [] -1 [~A dup dup] DrawRectangle~%"
		      (g-value win :width) (g-value win :height)
		      (ps-number (float (/ (+ red green blue) 3)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Multipoints
;;
;;  This code right now just reproduces the DrawPolyline ps code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *multipoint-fn*
  "
/MultipointDict 20 dict def
/DrawMultipoint { % x1 y1 {{x y ...} ...} line-color
		% join cap dash thickness fill-color => -
    gsave MultipointDict begin
	/fill-color exch def  /thickness exch def  /dash-pattern exch def
	/line-join exch def  /line-cap exch def  /line-color exch def
	% Don't draw the path of the fill if the filling-style is null.
	fill-color null eq not {
		3 copy
		newpath
		3 1 roll moveto
		{ aload length 2 idiv { lineto } repeat } forall
		fill-color FillShape
		} if

	newpath
	.5 -.5 translate

	% Stroke after every sub-array to avoid a limitcheck error
	2 index 2 index moveto
	{ aload length 2 sub 2 idiv { lineto } repeat
	2 copy lineto
	line-color line-cap line-join dash-pattern thickness StrokeShape
	moveto
	} forall

	currentpoint
	3 -1 roll sub abs .01 lt
	3 1 roll sub abs .01 lt and {
	  0 0 rlineto closepath
	} if

	line-color line-cap line-join dash-pattern
	thickness
%	dup -1 ne { .5 add } if % fudge outline width thicker
	StrokeShape

    end grestore
} def")

(define-method :ps-register-fn OPAL:MULTIPOINT (obj)
  (pushnew *multipoint-fn* *required-ps-fns*)
  (check-fs-and-ls-color obj))


(define-method :ps-object OPAL:MULTIPOINT (obj)
  ;; Parameters: x1, y1, ..., xn, yn, n
  (when (and (or (g-value obj :line-style) (g-value obj :filling-style))
	     (g-value obj :point-list))
    (let ((point-list (g-value obj :point-list))
	  (counter 0)
	  (points '()))
      ;; Convert all the y-coordinates while printing
      (format T "~S ~S {~%" (car point-list) (convert-y (cadr point-list)))
      (setf point-list (cddr point-list))
      (dotimes (i (truncate (length point-list) 2))
	(setf points (cons (car point-list)
			   (cons (convert-y (cadr point-list)) points)))
	(setf point-list (cddr point-list))
	(incf counter)
	(when (>= counter 64)
	  (write-points points)
	  (setf counter 0)
	  (setf points '())))
      (if (> counter 0)
	  (write-points points))
      (write-string "} "))
    (print-graphic-qualities obj)
    (format T "DrawMultipoint~%")))


;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

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

;; GUI Source file: linestyles.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Contains stuff for linestyles for PLOT-HACK system (depends on the Garnet GUI toolset from CMU).

;; Linestyles are stored in the 4 dimensional *LINE-STYLES-ARRAY* and in the list *ALL-LINE-STYLES*. The styles in *LINE-STYLES-ARRAY*
;; are referenced according to thickness (in pixels), dash pattern, color and shading, while the members of *ALL-LINE-STYLES* are
;; named descriptively, e.g. THIN-YELLOW-LINE, DASHED-LINE, DOTTED-LINE.

(defvar *default-line-style-family* :thin-color)
(defvar *all-line-style-sets* nil)
(defvar	*all-line-styles* nil)

(defvar *maximum-line-styles-thickness* 20)

(defvar *dash-patterns* (list nil '(10 7) '(2 3) '(5 7)  '(15 15) '(7 3) '(9 4 3 4) '(2 8)))
(defvar *line-styles-dash-patterns* '(0 (10 10) (5 5) (2 2)))

(defvar *line-styles-colors* '(black red green blue orange cyan purple yellow white))

;; there must be a better way.....
(export ; *line-styles-colors*
 '(black red green blue orange cyan purple yellow white))

(defvar *line-styles-opal-colors* (loop for color in *line-styles-colors* collect (get-opal-color color)))
(defvar *line-styles-opal-colors-no-white*
  (loop for color in *line-styles-opal-colors* unless (equal color opal::white) collect color))

(defvar *line-styles-shadings* '(100.0 50.0 25.0))

(defvar *line-styles-array* (make-array (list (1+ *maximum-line-styles-thickness*)
					(length *line-styles-dash-patterns*)
					(length *line-styles-colors*)
					(length *line-styles-shadings*))))

(proclaim '(type (simple-array * (* * * *)) *line-styles-array*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun debug-*line-styles-array*2 (&optional print-all)
  (multiple-value-bind (thickness-dimension dash-dimension color-dimension shading-dimension)
      (values-list (array-dimensions *line-styles-array*))
    (dotimes (thickness-index thickness-dimension)
      (dotimes (dash-index dash-dimension)
	(dotimes (color-index color-dimension)
	  (dotimes (shading-index shading-dimension)
	    (let ((line-style (aref *line-styles-array* thickness-index dash-index color-index shading-index)))
	      (when (or print-all (and line-style (not (numberp line-style))))
		(format t "~A at thick ~d, dash ~d, color INDEX ~D, ~d, shade ~d~%"
			line-style
			thickness-index
			(nth dash-index *line-styles-dash-patterns*)
			color-index (nth color-index *line-styles-colors*)
			(nth shading-index *line-styles-shadings*))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; thickness nil, 0 and 1 are identical (?)

;; Not very careful terminology below - A "line", e.g. 'thin-line, is actually a line-style.

;; in opal/create-instances.lisp
;; (create-instance 'opal::LINE-0 opal:line-style (:constant T))
;; (defvar opal::THIN-LINE opal::LINE-0)
;; (create-instance 'opal::LINE-1 opal:line-style (:constant T) (:line-thickness 1))

(create-instance 'thin-line opal:line-style (:line-thickness 1) (:join-style :round))
(create-instance 'thin-white-line thin-line (:foreground-color opal::white))
(create-instance 'faint-line thin-line (:foreground-color (get-opal-color 'black 50)))

(create-instance 'dashed-line thin-line (:line-style :dash) (:dash-pattern (list 4 4)))
(create-instance 'dashed-line-1 thin-line (:line-style :dash) (:dash-pattern (list 10 10)))
(create-instance 'dashed-line-2 thin-line (:line-style :dash) (:dash-pattern (list 5 5)))

(create-instance 'dotted-line thin-line (:line-style :dash) (:dash-pattern (list 1 1)))
(create-instance 'dotted-line-1 dotted-line (:dash-pattern (list 1 8)))
(create-instance 'dotted-line-2 dotted-line (:dash-pattern (list 2 6)))

(create-instance 'thick-line thin-line (:line-thickness 2))
(create-instance 'thick-dashed-line thick-line (:line-style :dash) (:dash-pattern (list 4 4)))
(create-instance 'thick-dotted-line thick-line (:line-style :dash) (:dash-pattern (list 1 1)))

(create-instance 'very-thick-line thin-line (:line-thickness 5))
(create-instance 'very-thick-dashed-line very-thick-line (:line-style :dash) (:dash-pattern (list 4 4)))
(create-instance 'very-thick-dotted-line very-thick-line (:line-style :dash) (:dash-pattern (list 1 1)))

;; the plot keys are 40 pixels long, so make each dash pattern at most 20 pixels so that we can see at least two cycles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Line style family sets
;;
;; These are used in data plots.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-dash-string (dash-pattern)
  (if dash-pattern (concatenate-string-list (loop for dash in dash-pattern collect (format nil "~D" dash)) :string-spacer "-") "no-dash"))

(defun make-line-styles (prototype quality-list quality name-root)
  (loop for attribute in quality-list
	collect (let ((sym (read-from-string (format nil "~A-~A-line" name-root (funcall (case quality
											   ((:grey-dash :dash) 'get-dash-string)
											   (:color 'opal::name-for-schema)
											   (:width 'identity))
											 attribute)))))
		  (if (and (boundp sym) (schema-p (eval sym)))
		    (eval sym)
		    (case quality
		      (:color (create-instance sym prototype (:constant nil) (:foreground-color attribute)))
		      (:width (create-instance sym prototype (:line-style :solid) (:constant nil) (:line-thickness attribute)))
		      (:dash (create-instance sym prototype (:line-style (if attribute :dash :solid)) (:constant nil) (:dash-pattern attribute)))
		      (:grey-dash (create-instance sym prototype
						   (:line-style (if attribute :dash :solid)) (:constant nil) (:dash-pattern attribute)
						   (:foreground-color (get-opal-color 'black 50)))))))))

(defparameter DASHED (make-line-styles thin-line *dash-patterns* :dash "thin"))
(push 'dashed *all-line-style-sets*)
(defparameter THICK-DASHED (make-line-styles thick-line *dash-patterns* :dash "thick"))
(push 'thick-dashed *all-line-style-sets*)
(defparameter GREY-DASHED (make-line-styles thin-line *dash-patterns* :grey-dash "grey-thin"))
(push 'grey-dashed *all-line-style-sets*)

(defparameter VARYING-WIDTH (make-line-styles thin-line (list-of-nums 10 1 1) :width "width"))
(push 'varying-width *all-line-style-sets*)
(defparameter DOUBLE-VARYING-WIDTH (make-line-styles thin-line(list-of-nums 10 1 2) :width "width"))
(push 'double-varying-width *all-line-style-sets*)

(defparameter THIN-COLOR (nconc (make-line-styles thin-line *line-styles-opal-colors-no-white* :color "thin")
				 (list opal:dashed-line opal:dotted-line)))
(push 'thin-color *all-line-style-sets*)
(defparameter THICK-COLOR (nconc (make-line-styles thick-line *line-styles-opal-colors-no-white* :color "thick")
				  (list thick-dashed-line thick-dotted-line)))
(push 'thick-color *all-line-style-sets*)
(defparameter VERY-THICK-COLOR (nconc (make-line-styles very-thick-line *line-styles-opal-colors-no-white* :color "very-thick")
				       (list very-thick-dashed-line very-thick-dotted-line)))
(push 'very-thick-color *all-line-style-sets*)

(defparameter THIN-DOTTED-COLOR (make-line-styles dotted-line *line-styles-opal-colors-no-white* :color "thin-dotted"))
(push 'thin-dotted-color *all-line-style-sets*)
(defparameter THIN-DOTTED-1-COLOR (make-line-styles dotted-line-1 *line-styles-opal-colors-no-white* :color "thin-dotted-1"))
(push 'thin-dotted-1-color *all-line-style-sets*)
(defparameter THIN-DOTTED-2-COLOR (make-line-styles dotted-line-2 *line-styles-opal-colors-no-white* :color "thin-dotted-2"))
(push 'thin-dotted-2-color *all-line-style-sets*)

(defparameter THIN-DASHED-COLOR (make-line-styles dashed-line *line-styles-opal-colors-no-white* :color "thin-dashed"))
(push 'thin-dashed-color *all-line-style-sets*)
(defparameter THIN-DASHED-1-COLOR (make-line-styles dashed-line-1 *line-styles-opal-colors-no-white* :color "thin-dashed-1"))
(push 'thin-dashed-1-color *all-line-style-sets*)
(defparameter THIN-DASHED-2-COLOR (make-line-styles dashed-line-2 *line-styles-opal-colors-no-white* :color "thin-dashed-2"))
(push 'thin-dashed-2-color *all-line-style-sets*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-access-line-style (color shading thickness dash)
  (create-instance nil opal:line-style
		   (:join-style :round)
		   (:line-thickness thickness)
		   (:shading shading)
		   (:line-style (if (or (not dash) (eq 0 dash)) :SOLID :dash))
		   (:dash-pattern (unless (or (not dash) (eq 0 dash)) dash))
		   (:color color)
		   (:foreground-color (get-opal-color color shading))))

(proclaim '(inline dash-index))
(defun dash-index (dash)
  (if dash
    (or (position dash (the list *line-styles-dash-patterns*) :test 'equal)
	(if (numberp dash) (min (max (the fn dash) 0) (1- (length (the cons *line-styles-dash-patterns*))))
	    (position 0 (the list *line-styles-dash-patterns*) :test 'equal)))
    0))

#|
(proclaim '(inline log-quantize-shading))
(defun log-quantize-shading (shading)
  (unless (or (not shading) (= 100.0 shading))
    (the sf (* 100.0 (the sf (expt 2.0 (round (the sf (log (the sf (/ shading 100.0)) 2)))))))))
|#

(proclaim '(inline shading-index))
(defun shading-index (shading)
  (or (if (or (not shading) (= 100.0 shading))
	0	  
	(position shading (the list *line-styles-shadings*) :test '=))
      (multiple-value-bind (shading index)
	  (FIND-CLOSEST-LIST-VALUE *line-styles-shadings* shading)
	(declare (ignore shading))
	index)))

(proclaim '(notinline access-*line-styles-array*-fast))
(defun access-*line-styles-array*-fast (thickness color &optional (shading 100.0) dash)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum thickness)
	   (single-float shading))
  (let* ((thickness-index (fixnum-bound 0 thickness *maximum-line-styles-thickness*))
	 (dash-index (dash-index dash))
	 (color-index (if (integerp color) color (color-index color)))
	 (shading-index (shading-index shading)))
    (declare (fixnum thickness-index dash-index shading-index thickness))
    (if (and color-index (= thickness thickness-index))
      (let ((style (aref *line-styles-array* thickness-index dash-index color-index shading-index)))
	(when (numberp style)
	  (setq style (create-access-line-style color shading thickness dash))
	  (setf (aref *line-styles-array* thickness-index dash-index color-index shading-index) style))
	style)
      (create-access-line-style color shading thickness dash))))

(defun access-*line-styles-array* (thickness &optional color (shading 100.0) dash thickness-is-a-fix)
  (access-*line-styles-array*-fast (if thickness-is-a-fix thickness (round thickness)) color (s-flt (or shading 100.0)) dash))

(defun pick-thickness (thickness &optional color (shading 100.0) dash)
  (access-*line-styles-array*-fast (round thickness) color (s-flt (or shading 100.0)) dash))
  
(defun basic-line-style-access (thickness-index color-index shading-index)
  (aref *line-styles-array* thickness-index 0 color-index shading-index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line-style-family-menu (&optional line-style-family-symbol length)
  (let ((dummy1 line-style-family-symbol))
    (choose-variable-values
     '((dummy1 "Choose family of line styles:"
	       :choose (
			:dashed :grey-dashed :thick-dashed
			:varying-width :double-varying-width
			:thin-color :thick-color :very-thick-color
			:thin-dotted-color :thin-dotted-1-color :thin-dotted-2-color
			:thin-dashed-color :thin-dashed-1-color :thin-dashed-2-color
			)))
     :title "Line Style Family")
    (get-line-styles dummy1 length)))


(defun get-line-styles (&optional (line-style-family-symbol *default-line-style-family*) length)
  ;; Calling with nothing will always get the set pointed to by *DEFAULT-LINE-STYLE-FAMILY*.
  (case line-style-family-symbol
    ((:thin-color :thin) thin-color)
    ((:thick-color :thick :thick-lines) thick-color)
    ((:very-thick-color :very-thick :very-thick-lines) very-thick-color)

    (:varying-width (if (numberp length) (loop for count from 1 to length collect (nth (1- count) varying-width)) varying-width))
    (:double-varying-width (if (numberp length) (loop for count from 1 to length collect (nth (1- count) double-varying-width))double-varying-width))
    
    ((:dashed :thin-dashed :thin-dashed :dashed) dashed)
    ((:thick-dashed :thick-dashed) thick-dashed)
    ((:grey-dashed) grey-dashed)

    ((:thin-dotted :thin-dotted-color) thin-dotted-color)
    ((:thin-dotted-1 :thin-dotted-1-color) thin-dotted-1-color)
    ((:thin-dotted-2 :thin-dotted-2-color) thin-dotted-2-color)

    ((:thin-dashed-color :thin-dashed) thin-dashed-color)
    ((:thin-dashed-1 :thin-dashed-1-color) thin-dashed-1-color)
    ((:thin-dashed-2 :thin-dashed-2-color) thin-dashed-2-color)))

(defun line-style-menu (&key (default-style thin-line) (label "Choose a line style") text (choose-from-components t) (choose-from-classes t)
			(do-all-at-once t) (only-one-choice t) (style-options *all-line-style-sets*))
  (let ((dummy1 (cond ((and choose-from-components choose-from-classes) nil)
		      (choose-from-components :components)
		      (choose-from-classes :classes)))
	(default-style (or default-style opal:thin-line)))
    (unless dummy1
      (choose-variable-values
       '((dummy1 "Choose line style from:" :choose (:classes :components)))
       :label label :text text))
    (let ((result
	   (case dummy1
	     (:classes (choose-list-values-from-keys
			(delete-duplicates
			 (loop for style-family in (coerce-to-list style-options)
			       nconcing (loop for style in (symbol-value style-family) collect (list (opal::name-for-schema style) style)))
			 :test 'equal)
			(list default-style)
			:do-all-at-once do-all-at-once :only-one-choice only-one-choice :label label :text text))
	     (:components (line-styles-from-components-menu :default-style default-style :label label :text text)))))
      (if (and only-one-choice (consp result)) (car result) result))))

(defun line-style-nice-name (line-style)
  (loop for style-set in *all-line-style-sets*
	when (member line-style (symbol-value style-set))
	do (return (opal::name-for-schema line-style))
	finally (return (format nil  "~A, ~A~A, Thickness ~A"
				(if (gv line-style :dash-pattern)
				  (format nil "Dash ~A" (gv line-style :dash-pattern))
				  "Solid")
				(if (= (gv line-style :shading) 100)
				  ""
				  (format nil "~D% Shading, " (round (gv line-style :shading))))
				(string-capitalize (string (gv line-style :color)))
				(gv line-style :line-thickness)))))
		      
(defun line-styles-from-components-menu (&key default-style (label "Menu For Line Style Components") text)
  (let* ((default-style (or default-style (if (= 0 (aref *line-styles-array* 0 0 0 0)) dotted-line-1 (aref *line-styles-array* 0 0 0 0))))
	 (dummy1 (princ-to-string (or (gv default-style :dash-pattern) 0)))
	 (dummy2 (gv default-style :line-thickness))
	 (dummy3 (princ-to-string (or (gv default-style :shading) 100.0)))
	 (dummy4 (princ-to-string (gv default-style :color))))
    (choose-variable-values
     `((dummy1 "Choose a dash pattern (0 is solid):" :choose ,(mapcar 'princ-to-string  *line-styles-dash-patterns*) :rank-margin 4)
       (dummy2 "Thickness (0-20)" :integer)
       (dummy3 "Shading (percent)" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
       (dummy4 "Color:" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 5))
     :text text :label label)
    (access-*line-styles-array* dummy2 (read-from-string dummy4) (read-from-string dummy3) (read-from-string dummy1))))

(defun thick-line-menu (&key (default-style thick-line) (label "Choose a thick line style") (do-all-at-once t) (only-one-choice t))
  (line-style-menu :default-style default-style :label label
		   :do-all-at-once do-all-at-once :only-one-choice only-one-choice
		   :style-options (list thick-color thick-dashed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop for list in (list dashed thick-dashed grey-dashed
			varying-width double-varying-width
			thin-color thick-color very-thick-color
			thin-dotted-color thin-dotted-1-color thin-dotted-2-color
			thin-dashed-color thin-dashed-1-color thin-dashed-2-color)
      do (loop for style in list do (export (read-from-string (opal::name-for-schema style)))))

(setq *all-line-styles* (loop for set in *all-line-style-sets* nconc (copy-list (symbol-value set))))

(export '(line-style-nice-name
	  *default-line-style-family*
	  basic-line-style-access
	  get-line-styles
	  pick-thickness
	  LINE-STYLE-FAMILY-MENU
	  debug-*line-styles-array*
	  thin-line thin-white-line faint-line
	  dashed-line dashed-line-1 dashed-line-2
	  dotted-line dotted-line-1 dotted-line-2
	  thick-line thick-dashed-line thick-dotted-line
	  very-thick-line very-thick-dashed-line very-thick-dotted-line

	  dashed thick-dashed grey-dashed
	  varying-width double-varying-width
	  thin-color thick-color very-thick-color
	  thin-dotted-color thin-dotted-1-color thin-dotted-2-color
	  thin-dashed-color thin-dashed-1-color thin-dashed-2-color

	  line-style-menu line-styles-from-components-menu thick-line-menu

	  *maximum-line-styles-thickness* *line-styles-dash-patterns*
	  *line-styles-colors* *line-styles-shadings*
	  *ALL-LINE-STYLE-SETS*  *all-line-styles*
	  *line-styles-opal-colors*	  *LINE-STYLES-ARRAY*
	  ACCESS-*LINE-STYLES-ARRAY* ACCESS-*LINE-STYLES-ARRAY*-FAST create-access-line-style
	  ))



;;;;;;;;;;;;;; OLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|	    
(defun colored-line-style (color &optional thickness dash)
  (cond
   ((or (eql color opal:red)(eql color 'red))
    (case dash
      (1 thin-dashed-1-RED-line)
      (2 thin-dashed-2-RED-line)
      (3 thin-dotted-RED-line)
      (4 thin-dotted-2-RED-line)
      (t (case thickness
	   (5 very-thick-RED-line)
	   (2 thick-RED-line)
	   (t opal:RED-line)))))
   ((or (eql color opal:green)(eql color 'green))
    (case dash
      (1 thin-dashed-1-GREEN-line)
      (2 thin-dashed-2-GREEN-line)
      (3 thin-dotted-GREEN-line)
      (4 thin-dotted-2-GREEN-line)
      (t (case thickness
	   (5 very-thick-GREEN-line)
	   (2 thick-GREEN-line)
	   (t opal:GREEN-line)))))
   ((or (eql color opal:blue)(eql color 'blue))
    (case dash
      (1 thin-dashed-1-BLUE-line)
      (2 thin-dashed-2-BLUE-line)
      (3 thin-dotted-BLUE-line)
      (4 thin-dotted-2-BLUE-line)
      (t (case thickness
	   (5 very-thick-BLUE-line)
	   (2 thick-BLUE-line)
	   (t opal:BLUE-line)))))
   ((or (eql color opal:yellow)(eql color 'yellow))
    (case dash
      (1 thin-dashed-1-YELLOW-line)
      (2 thin-dashed-2-YELLOW-line)
      (3 thin-dotted-YELLOW-line)
      (4 thin-dotted-2-YELLOW-line)
      (t (case thickness
	   (5 very-thick-YELLOW-line)
	   (2 thick-YELLOW-line)
	   (t opal:YELLOW-line)))))
   ((or (eql color opal:orange)(eql color 'orange))
    (case dash
      (1 thin-dashed-1-ORANGE-line)
      (2 thin-dashed-2-ORANGE-line)
      (3 thin-dotted-ORANGE-line)
      (4 thin-dotted-2-ORANGE-line)
      (t (case thickness
	   (5 very-thick-ORANGE-line)
	   (2 thick-ORANGE-line)
	   (t opal:ORANGE-line)))))
   ((or (eql color opal:cyan)(eql color 'cyan))
    (case dash
      (1 thin-dashed-1-CYAN-line)
      (2 thin-dashed-2-CYAN-line)
      (3 thin-dotted-CYAN-line)
      (4 thin-dotted-2-CYAN-line)
      (t (case thickness
	   (5 very-thick-CYAN-line)
	   (2 thick-CYAN-line)
	   (t opal:CYAN-line)))))
   ((or (eql color opal:purple)(eql color 'purple))
    (case dash
      (1 thin-dashed-1-PURPLE-line)
      (2 thin-dashed-2-PURPLE-line)
      (3 thin-dotted-PURPLE-line)
      (4 thin-dotted-2-PURPLE-line)
      (t (case thickness
	   (5 very-thick-PURPLE-line)
	   (2 thick-PURPLE-line)
	   (t opal:PURPLE-line)))))
   (t (case dash
	(1 dashed-1)
	(2 dashed-2)
	(3 dotted-line-1)
	(4 dotted-line-2)
	(t (case thickness
	     (5  very-thick-line)
	     (2 thick-black-line)
	     (t thin)))))))
|#



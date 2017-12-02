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

;; GUI Source file: colors.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Color and color fill stuff, also including color maps.
;;;
;;; The idea of the *COLOR-LIBRARY* and *COLOR-FILL-LIBRARY* libraries is twofold: one to save memory and time by having new colors or filling styles made
;;; only when necessary, and two to avoid the color allocation error when there are too many colors in the X display. The maximum number of colors that can
;;; eventually be allocated is determined by the variable *COLOR-LIBRARY-RESOLUTION*, which sets the resolution for each of the red blue and green
;;; components (0 to 1) of the created colors. Thus, if this variable is 0.02, there can be a total of 50 * 50 * 50 (= 125000) colors in the library. It
;;; remains to be seen what is the maximum.

;;; Another idea is the use of *COLOR-LIBRARY-MAXIMUM*, which punts the creation of new color if the number of entries in the color library reaches this
;;; level.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *color-library* nil)
(defvar *color-fill-library* nil)
(defvar *color-library-resolution* 0.02)
(defvar *color-library-maximum* 500)

(defparameter *basic-opal-colors*	; Leave out yellow since it is hard to see 
  (list opal:black
	opal:red
	opal:green
	opal:blue
	;; opal:yellow
	opal:purple
	opal:cyan
	opal:orange))

(defparameter *basic-opal-colors-w-white* (append *basic-opal-colors* (list opal:white)))

(defun opal-thing-is-black (thing)
  (black-p (cond ((or (opal::is-a-p thing opal::line-style) (opal::is-a-p thing opal::filling-style))
		  (g-value thing :foreground-color)))))
		 
(defun black-p (thing) (when (opal::is-a-p thing opal::color)
			 (= (g-value thing :red) (g-value thing :blue) (g-value thing :green) 0)))
			
(defun clear-color-libraries ()
  (loop for color in *color-library* do (opal:destroy color))
  (loop for color in *color-fill-library* do (opal:destroy color))
  (setq *color-fill-library* nil *color-library* nil))

(defun map-color-to-library-index (color library-resolution)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type single-float library-resolution))
  (* library-resolution (round (bound-val color 1.0 0.0) library-resolution)))

(proclaim '(notinline get-color-from-library))
(defun get-color-from-library (red green blue &key (library-resolution *color-library-resolution*) colors-are-indexes)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((red-index (if colors-are-indexes red (map-color-to-library-index red library-resolution)))
	(blue-index (if colors-are-indexes blue (map-color-to-library-index blue library-resolution)))
	(green-index (if colors-are-indexes green (map-color-to-library-index green library-resolution)))
	(temp-error-sq 0.0)
	(min-error-sq nil)
	(closest-color))
    (declare (type single-float red-index blue-index green-index temp-error-sq))
    (loop for color in *color-library*
	  when (and (= (the sf (g-value color :red)) red-index)
		    (= (the sf (g-value color :green)) green-index)
		    (= (the sf (g-value color :blue)) blue-index))
	  do (return color)
	  else
	  do (setq temp-error-sq
		   (+ (square (- (the sf (g-value color :red)) red-index))
		      (square (- (the sf (g-value color :green)) green-index))
		      (square (- (the sf (g-value color :blue)) blue-index))))
	  (when (or (not min-error-sq) (< temp-error-sq (the sf min-error-sq)))
	    (setq min-error-sq temp-error-sq closest-color color))
	  finally
	  (return
	   (if (> (length (the cons *color-library*)) (the fn *color-library-maximum*))
	     closest-color
	     (let ((color (create-instance nil opal:color (:red red-index) (:green green-index) (:blue blue-index))))
	       ;; (format t "Pushing ~A on *color-library* (current length ~A)~%" color (length *color-library*))
	       (push color *color-library*)
	       color))))))

(proclaim '(inline get-color-fill-from-library))
(defun get-color-fill-from-library (red green blue &key (library-resolution *color-library-resolution*) colors-are-indexes)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((red-index (if colors-are-indexes red (map-color-to-library-index red library-resolution)))
	(blue-index (if colors-are-indexes blue (map-color-to-library-index blue library-resolution)))
	(green-index (if colors-are-indexes green (map-color-to-library-index green library-resolution))))
    (declare (type single-float red-index blue-index green-index))
    (loop for color-fill in *color-fill-library*
	  when (and (= (the sf (g-value color-fill :foreground-color :red)) red-index)
		    (= (the sf (g-value color-fill :foreground-color :green)) green-index)
		    (= (the sf (g-value color-fill :foreground-color :blue)) blue-index))
	  do (return color-fill)
	  finally
	  (return
	   (let ((color-fill (create-instance nil opal:filling-style
					      (:foreground-color (get-color-from-library red-index green-index blue-index
											 :library-resolution library-resolution
											 :colors-are-indexes t)))))
	     (push color-fill *color-fill-library*)
	     color-fill)))))

(defun get-shaded-color-from-library (reference-color shading-percent)
  (let ((coeff (the sf (* 0.01 shading-percent))))
    (get-color-from-library
     (+ (- 1 coeff) (* coeff (the sf (g-value reference-color :red))))
     (+ (- 1 coeff) (* coeff (the sf (g-value reference-color :green))))
     (+ (- 1 coeff) (* coeff (the sf (g-value reference-color :blue)))))))

(defun get-opal-color (&optional color shading-percent)
  ;; When COLOR is NIL, assume black.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((color (or color opal:black)))
    (cond ((and (not shading-percent)
		(eq 'schema (type-of color))
		(eq opal::color (car (g-value color :is-a))))
	   color)
	  ((or (and (eq 'schema (type-of color))
		    (eq opal::color (car (g-value color :is-a))))
	       (find-symbol (STRING-upcase color) 'opal))
	   (let ((opal-color (if (and (eq 'schema (type-of color))
				      (eq opal::color (car (g-value color :is-a))))
			       color
			       (symbol-value (find-symbol (STRING-upcase color) 'opal)))))
	     (unless (eq opal::color (car (g-value opal-color :is-a)))
	       (setq opal-color opal:black))
	     (if shading-percent (get-shaded-color-from-library opal-color shading-percent) opal-color)))
	  (t opal:black))))

(proclaim '(inline get-opal-color-to-fill))
(defun get-opal-color-to-fill (color &optional shading-percent)
  (let ((found-color (get-opal-color color shading-percent)))
    (get-color-fill-from-library (g-value found-color :red) (g-value found-color :green) (g-value found-color :blue))))

(proclaim '(inline get-gray-fill))
(defun get-gray-fill (value) (get-color-fill-from-library value value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color maps
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The color-map names refer to RGB functions, defined below.
(defvar *default-color-map* 'hot-jet-rgb-map)
;; '(hot-jet-rgb-map SH-ORIGINAL-RGB-map HOT-RGB-MAP jet-rgb-map GRAY-RGB-MAP PINK-RGB-MAP hot-cold-rgb-map))
(defvar *color-map-functions* '(hot-jet-rgb-map GRAY-RGB-MAP PINK-RGB-MAP hot-cold-rgb-map HOT-RGB-MAP)) 

(defun rgb-map-values (rgb-map length)
  (let ((length (s-flt-num length))
	rs gs bs)
    (loop for step from 1.0 to length do
	  (multiple-value-bind (r g b)
	      (funcall rgb-map (/ step length))
	    (push r rs)
	    (push g gs)
	    (push b bs))
	  finally (return (values (reverse rs) (reverse gs) (reverse bs))))))

#|
;; RGB map functions should take a single float INDEX argument from 0.0 to 1.0, and return as single-float values (red green blue), each between 0.0 and 1.0
;;
;; For example,
(defun foo-rgb-map (index)
  (values
   (cond ((< index 0.5) (/ index 2)			; red
	  t index))
   0				; green
   index))			; blue
|#

(defun hot-cold-rgb-map (index)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((index (- (* (the sf index) 2) 1)))
    (values
     (cond				; Red
      ((< index (/ -2.0 3)) (/ (+ (- index) (/ -2.0 3)) (/ 1 3.0)))
      ((< index 0.0) 0.0)
      ((< index (/ 1.0 3))
       (/ index (/ 1.0 3)))
      (t 1.0))
     (cond				; Green
      ((< index (/ -2.0 3)) 1.0)
      ((< index (/ -1.0 3)) (/ (+ (- index) (/ -1.0 3)) (/ 1 3.0)))
      ((< index 0.0) 0.0)
      ((< index (/ 0.0 3)) (/ (+ (- index) (/ 0.0 3)) (/ 1 3.0)))
      ((< index (/ 1.0 3)) 0.0)
      ((< index (* 2 (/ 1.0 3)))
       (/ (- index (/ 1.0 3)) (/ 1.0 3)))
      (t 1.0))
     (cond				; Blue
      ((< index (/ -1.0 3)) 1.0)
      ((< index 0) (* 3 (- index)))
      ((< index (* 2 (/ 1.0 3))) 0.0)
      (t (/ (- index (* 2 (/ 1.0 3))) (/ 1.0 3)))))))

(defun SH-ORIGINAL-RGB-map (index)
  ;; An ad-hoc mapping.
  (declare (optimize (safety 0) (speed 3) (space 1))
	     (single-float index))
  (values index (- 1.0 (* 2 (abs (- 0.5 index)))) (abs (- 1.0 index))))

(defun hot-rgb-map (index)
  ;; Inspired by MATLAB(tm).
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float index))
  (values (cond
	   ((< index (/ 1.0 3))
	    (/ index (/ 1.0 3)))
	   (t 1.0))
	  (cond
	   ((< index (/ 1.0 3)) 0.0)
	   ((< index (* 2 (/ 1.0 3)))
	    (/ (- index (/ 1.0 3)) (/ 1.0 3)))
	   (t 1.0))
	  (cond
	   ((< index (* 2 (/ 1.0 3))) 0.0)
	   (t (/ (- index (* 2 (/ 1.0 3))) (/ 1.0 3))))
	  ))

(defun pink-rgb-map (index)
  ;; Inspired by MATLAB(tm).
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float index))
  (values-list
   (mapcar #'(lambda (h g)
	       (declare (optimize (safety 0) (speed 3) (space 1))
			(single-float h g))
	       (sqrt (+ (* 2/3 g) (* 1/3 h))))
	   (multiple-value-list (hot-rgb-map index))
	   (multiple-value-list (gray-rgb-map index)))))

(defun GRAY-RGB-MAP (index)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float index))
  (values index index index))

(defun jet-rgb-map (index)
  ;; Inspired by MATLAB(tm).
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float index))
  (values
   (cond
    ((< index (* 3 0.125)) 0.0)
    ((< index (* 5 0.125)) (/ (- index (* 3 0.125)) 0.25))
    ((< index (* 7 0.125)) 1.0)
    (t (- 1 (/ (- index (* 7 0.125)) 0.25))))
   (cond
    ((< index (* 1 0.125)) 0.0)
    ((< index (* 3 0.125)) (/ (- index (* 1 0.125)) 0.25))
    ((< index (* 5 0.125)) 1.0)
    ((< index (* 7 0.125)) (- 1 (/ (- index (* 5 0.125)) 0.25)))
    (t 0.0))
   (cond
    ((< index 0.125) (/ (+ index 0.125) 0.25))
    ((< index (* 3 0.125)) 1.0)
    ((< index (* 5 0.125)) (- 1 (/ (- index (* 3 0.125)) 0.25)))
    (t 0.0))))

(defun hot-jet-rgb-map (index)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float index))
  (values
   (cond
    ((<= index (/ 3.0)) 0.0)
    ((< (/ 3.0) index (/ 2 3.0)) (* 3 (- index (/ 3.0))))
    (t 1.0))
   (cond
    ((<= index (/ 3.0)) (* 3 index))
    ((< (/ 3.0) index (/ 2 3.0)) 1.0)
    (t (- 1 (* -3 (- (/ 2 3.0) index)))))
   (cond
    ((<= index (/ 3.0)) 1.0)
    ((< (/ 3.0) index (/ 2 3.0)) (- 1 (* -3 (- (/ 3.0) index))))
    (t 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (proclaim '(notinline get-opal-variable-color))
(proclaim '(notinline get-opal-variable-color))
(defun get-opal-variable-color (variable &key (min-variable -100.0) (max-variable 50.0) (map *default-color-map*))
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float min-variable max-variable variable))
;;  (format t "min-variable ~A, max-variable ~A, variable ~A~%" min-variable max-variable variable)
  (let ((variable-index (/ (- (the sf (bound-val variable max-variable min-variable)) min-variable)
			   (- max-variable min-variable))))
    (apply 'get-color-from-library (multiple-value-list (funcall map variable-index)))))

(proclaim '(notinline get-number-fill))
(defun get-number-fill (value colorp)
  ;; VALUE is between 0 and 1
  (if colorp
    (get-opal-color-to-fill (get-opal-variable-color (coerce value 'single-float) :min-variable 0.0 :max-variable 1.0))
    (get-gray-fill value)))

(defun color-to-fill (color &optional shading-percent) (when color (get-opal-color-to-fill color shading-percent)))

(defun colors-to-fill (colors)
  (if (not (listp colors))
    (setq colors (list colors)))
  (unless (null-list-p colors)
    (if (= (length colors) 1)
      (color-to-fill (car colors))
      (let ((name ""))
	(loop for color in colors
	      for i from 0
	      when color
	      summing (g-value color :red) into red
	      and summing (g-value color :blue) into blue
	      and summing (g-value color :green) into green
	      and do (setq name (concatenate 'string name (kr::name-for-schema color)))
	      finally (return (get-color-fill-from-library (/ red i) (/ green i) (/ blue i))))))))

(defun extract-color (color-ref &optional default-color-ref)
  (when (or color-ref default-color-ref)
    (if (is-a-p color-ref opal:color)
      color-ref
      (or (symbol-to-opal-color color-ref)
	  (extract-color default-color-ref)))))

(defun symbol-to-opal-color (value)
  (case value
    ((:red red) opal:RED)
    ((:green green) opal:GREEN)
    ((:blue blue) opal:BLUE)
    ((:yellow yellow) opal:YELLOW)
    ((:orange orange) opal:ORANGE)
    ((:cyan cyan) opal:CYAN)
    ((:purple purple) opal:PURPLE)
    ((:black black) opal:black)
    ((:white white) opal:white)))

(defun string-to-opal-color (string)
  (let ((string (string-upcase string)))
    (cond ((string= string "RED") opal:RED)
	  ((string= string "GREEN") opal:GREEN)
	  ((string= string "BLUE")  opal:BLUE)
	  ((string= string "YELLOW") opal:YELLOW)
	  ((string= string "ORANGE") opal:ORANGE)
	  ((string= string "CYAN") opal:CYAN)
	  ((string= string "PURPLE") opal:PURPLE)
	  ((string= string "BLACK") opal:black)
	  ((string= string "WHITE") opal:white))))

(defun opal-color-to-string (color)
  (cond ((equal color opal:RED) "RED")
	((equal color opal:GREEN) "GREEN")
	((equal color opal:BLUE) "BLUE")
	((equal color opal:YELLOW) "YELLOW")
	((equal color opal:ORANGE) "ORANGE")
	((equal color opal:CYAN) "CYAN")
	((equal color opal:PURPLE) "PURPLE")
	((or (black-p color) (equal color opal:black)) "BLACK")
	((equal color opal:white) "WHITE")))

(proclaim '(inline color-index))
(defun color-index (color)
  (the fn (typecase color
	    (schema (or (position (the schema color) (the list *line-styles-opal-colors*)) 0))
	    (t (or (position (the symbol color) (the list *line-styles-colors*))
		   (position 'black (the list *line-styles-colors*))
		   0)))))

(defun transfer-opal-color-values (from-color-schema to-color-schema)
  (s-value to-color-schema :red (g-value from-color-schema :red))
  (s-value to-color-schema :blue (g-value from-color-schema :blue))
  (s-value to-color-schema :green (g-value from-color-schema :green)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some particular colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Names and values taken from Gnome Color Browser

(defun make-special-colors ()
  (export (defparameter CHARTREUSE1 (get-color-from-library 0.4961 0.9961 0.0)))
  (export (defparameter CHARTREUSE3 (get-color-from-library 0.3984 0.8008 0.0))))

(eval-when (load) (make-special-colors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*color-library* *color-fill-library* *color-library-resolution* *color-library-maximum*
	  OPAL-THING-IS-BLACK black-p symbol-to-opal-color string-to-opal-color *basic-opal-colors* *basic-opal-colors-w-white*
	  extract-color
	  color-index
	  TRANSFER-OPAL-COLOR-VALUES
	  OPAL-COLOR-TO-STRING
	  *DEFAULT-COLOR-MAP* *color-map-functions*
	  rgb-map-values hot-rgb-map hot-cold-rgb-map GRAY-RGB-MAP PINK-RGB-MAP SH-ORIGINAL-RGB-map JET-RGB-MAP HOT-JET-RGB-MAP
	  get-opal-color GET-OPAL-variable-COLOR
	  get-opal-color-to-fill GET-COLOR-FROM-LIBRARY
	  GET-COLOR-FILL-FROM-LIBRARY
	  *COLOR-LIBRARY-RESOLUTION* *color-library* *color-fill-library*
	  GET-GRAY-FILL get-number-fill
	  clear-color-libraries
	  COLOR-TO-FILL
	  COLORs-TO-FILL))


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

;; GUI Source file: declare.lisp


(IN-PACKAGE "WINDOWS-HACK")


;;; This file defines some types, constants and global variables used by the GUI files. Note that global variables are defined in many other files.

(deftype fn () 'fixnum)
(deftype df () 'double-float)
(deftype sf () 'single-float)
;; Maybe this wil be useful.
#+:cmu (deftype index () `(unsigned-byte 32))
#-:cmu (deftype index () 'fixnum)

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant e 2.71828182845904523536028747135266249775724709369996L0)
(defconstant e-single (coerce e 'single-float))
(defconstant pi-single (COERCE user::PI 'SINGLE-FLOAT))
(defconstant pi-over-2 (/ pi-single 2.0))
(defconstant pi-over-4 (/ pi-single 4.0))
(defconstant pi/2 (/ pi-single 2.0))
(defconstant pi/4 (/ pi-single 4.0))
(defconstant ZERO 0e0)

(defconstant radians/degree (/ (* 2 pi) 360)) ; 0.017453292519943295d0
(defconstant degrees/radian (/ 360 (* 2 pi))) ; 57.29577951308232d0


(defvar *window-default-font* (opal:get-standard-font :sans-serif :roman :medium))

(export '(sf df fn index
	  RADIANS/degree degrees/RADIAN e
	  e-single pi-over-2 pi-over-4 pi-single pi/2 pi/4 zero *window-default-font*))

(proclaim '(single-float e-single pi-over-2 pi-over-4 pi-single pi/2 pi/4 zero))

;; *****************************************
;;
;; Dummys for menus.
;;
;; *****************************************

;; Creates dummy1 - dummy50 - I am sure this is not the "correct" way....
;(eval (cons 'defvars-w-value
;            (loop for i from 1 to 50
;                  collect (list (read-from-string (format nil "dummy~d" i))
;                                nil))))
;;
;(defvar menu-dummys
;  (loop for i from 1 to 100
;        collect (list (read-from-string (format nil "dummy~d" i))
;                      nil)))
;  
;(defvars-w-value (eval menu-dummys))
;
;(loop for dummy-list in menu-dummys do
;      (export (car dummy-list)))
#|
(defun get-a-dummy (index &optional value)
  (let ((sym (read-from-string (format nil "dummy~d" index))))
    `(defvar ,sym) (export sym)
    (setf (symbol-value sym) value)
    sym))
|#

(defun get-a-dummy (index &optional value)
  (let ((sym (read-from-string (format nil "dummy~d" index))))
    (eval `(defvar ,sym nil)) (export sym)
    (setf (symbol-value sym) value)
    sym))
  
(defun list-of-dummys (number-or-values)
  (let ((total-dummies (if (numberp number-or-values) number-or-values (length number-or-values))))
    (loop for count from 1 to total-dummies
	  collect (get-a-dummy count (when (consp number-or-values) (nth (1- count) number-or-values))))))

(defvar *dummy-variables* (list-of-dummys 100))

#|
(defun list-of-dummy-strings (number-or-values)
  (let ((total-dummies (if (numberp number-or-values) number-or-values (length number-or-values)))
	sym)
    (loop for count from 1 to total-dummies
	  do (setq sym (read-from-string (format nil "dummy-string-~d" count)))
	  unless (boundp sym) do `(defvar ,sym ,nil) (export sym)
	  do (setf (symbol-value sym) (when (consp number-or-values) (nth (1- count) number-or-values)))
	  collect sym)))

|#
(defun list-of-dummy-strings (number-or-values)
  (let ((total-dummies (if (numberp number-or-values) number-or-values (length number-or-values)))
	sym)
    (loop for count from 1 to total-dummies
	  do (setq sym (read-from-string (format nil "dummy-string-~d" count)))
	  unless (boundp sym)
	  do (eval `(defvar ,sym nil)) (export sym)
	  when (consp number-or-values)
	  do (setf (symbol-value sym) (nth (1- count) number-or-values))
	  collect sym)))

(export '(list-of-dummys list-of-dummy-strings))
			 
	  

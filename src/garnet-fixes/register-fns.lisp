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

(in-package "OPAL")

;; 11/4/98
;;
;; Hack the ps-register-fn methods for virtual-aggregates and aggregadgets so that all the relevant
;; component ps-register-fns and other ps bookeeping are accounted for. Note that these methods
;; essentially do what opal::do-all-components *should* do in Register-Fns-In-Win.

;; These two functions encapsulate the innards of Register-Fns-In-Win.
(defun parse-arbitrary-fill-pattern-required-ps-fns (comp)
  (when (arbitrary-pattern-p (g-value comp :filling-style))
    (pushnew *arbitrary-pattern-fn* *required-ps-fns*)
    (pushnew *def-image-fn* *required-ps-fns*)
    (let ((image (g-value comp :filling-style :stipple :image)))
      (unless (assoc image *image-list*)
	(push (cons image (make-image-name)) *image-list*))))
  (when (arbitrary-pattern-p (g-value comp :line-style))
    (pushnew *arbitrary-pattern-fn* *required-ps-fns*)
    (pushnew *def-image-fn* *required-ps-fns*)
    (let ((image (g-value comp :line-style :stipple :image)))
      (unless (assoc image *image-list*)
	(push (cons image (make-image-name)) *image-list*)))))

(defun full-parse-ps-register-fn (thing)
  (kr-send thing :ps-register-fn thing)
  (parse-arbitrary-fill-pattern-required-ps-fns thing))

;; Redefine Register-Fns-In-Win to use the innards written in exposed form above.
(defun Register-Fns-In-Win (win subwindows-p)
  (check-color (g-value win :background-color))
  (when (and (g-value win :aggregate)
	     (g-value win :aggregate :visible))
    (opal:do-all-components (g-value win :aggregate)
      #'(lambda (comp)
	  (when (g-value comp :visible)
	    (full-parse-ps-register-fn comp)))))
  (when subwindows-p
    (let ((wins (g-value win :child)))
      (dolist (sub-win wins)
	(Register-Fns-In-Win sub-win subwindows-p)))))

;; Redefine this method to take care of the :IS-A elements of a virtual-aggregate.
(define-method :ps-register-fn OPAL:VIRTUAL-AGGREGATE (obj)
  (let ((dummy-item (g-value obj :dummy-item)))
    (when dummy-item
      (full-parse-ps-register-fn dummy-item)
      (check-ls-color dummy-item)
;      (dolist (what-it-is (g-value dummy-item :is-a))
;	(full-parse-ps-register-fn what-it-is))
 )))     

;; This method is missing in the GARNET code.
(define-method :ps-register-fn OPAL:AGGREGADGET (obj)
  (dolist (part (g-value obj :parts))
    (let ((part-obj (nth 1 part)))
      (full-parse-ps-register-fn part-obj))))



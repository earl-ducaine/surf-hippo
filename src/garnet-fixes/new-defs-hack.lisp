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


;; improvements to some macros and functions

;; GARNET-FIXES Source file: new-defs-hack.lisp

(in-package "OPAL")

(define-method :destroy-me opal:view-object (object &optional (top-level-p T))
 (if object
  (let* ((the-window (g-value object :window))
	 (parent  (g-local-value object :parent))
	 (erase-p (and top-level-p the-window parent
		    (not (g-local-value object :already-tried-to-destroy)))))
    (if (and top-level-p parent)
	(let ((known-as (g-local-value object :known-as)))
	  (s-value parent :components
		   (delete object (g-local-value parent :components)))
	  (mark-as-changed parent :components)
	  (if known-as (destroy-slot parent known-as))))
    (s-value object :already-tried-to-destroy t)
    (if erase-p
	(update the-window (not (carefully-erase object the-window))))
    (destroy-schema object)
    )))


  
(defmacro merge-bbox (dest-bbox source-bbox)
  `(when (bbox-valid-p ,source-bbox)
    (if (bbox-valid-p ,dest-bbox)
	(progn
	  (setf (bbox-x1 ,dest-bbox)
		(FIXNUM-MIN (the fixnum (bbox-x1 ,dest-bbox)) (the fixnum (bbox-x1 ,source-bbox))))
	  (setf (bbox-y1 ,dest-bbox)
		(FIXNUM-MIN (the fixnum (bbox-y1 ,dest-bbox)) (the fixnum (bbox-y1 ,source-bbox))))
	  (setf (bbox-x2 ,dest-bbox)
	        (fixnum-MAX (the fixnum (bbox-x2 ,dest-bbox)) (the fixnum (bbox-x2 ,source-bbox))))
	  (setf (bbox-y2 ,dest-bbox)
		(fixnum-MAX (the fixnum (bbox-y2 ,dest-bbox)) (the fixnum (bbox-y2 ,source-bbox)))))
	(progn
	  (setf (bbox-x1 ,dest-bbox) (bbox-x1 ,source-bbox))
	  (setf (bbox-y1 ,dest-bbox) (bbox-y1 ,source-bbox))
	  (setf (bbox-x2 ,dest-bbox) (bbox-x2 ,source-bbox))
	  (setf (bbox-y2 ,dest-bbox) (bbox-y2 ,source-bbox))
	  (setf (bbox-valid-p ,dest-bbox) T)))))

;; Returns T iff the dimensions of two bboxes are different. Ignores valid-p.
(defmacro bbox-dims-differ (bb1 bb2)
  `(not (and
	  (= (the fixnum (bbox-x1 ,bb1)) (the fixnum (bbox-x1 ,bb2)))
	  (= (the fixnum (bbox-y1 ,bb1)) (the fixnum (bbox-y1 ,bb2)))
	  (= (the fixnum (bbox-x2 ,bb1)) (the fixnum (bbox-x2 ,bb2)))
	  (= (the fixnum (bbox-y2 ,bb1)) (the fixnum (bbox-y2 ,bb2))))))

;;; Updates the bbox given (probably the object's :old-bbox slot value) with
;;; the values from the object.  This *presumes* that the object is visible!
(defmacro update-bbox (object bbox)
    `(let ((left (g-value ,object :left))
	   (top  (g-value ,object :top )))
	(setf (bbox-x1 ,bbox) left)
	(setf (bbox-y1 ,bbox) top)
	(setf (bbox-x2 ,bbox) (the fixnum (+ (the fixnum left) (the fixnum (g-value ,object :width )))))
	(setf (bbox-y2 ,bbox) (the fixnum (+ (the fixnum top) (the fixnum (g-value ,object :height)))))
	(setf (bbox-valid-p ,bbox) T)))
 
;;; Returns true if they intersect (ignores the valid bit!)
(defmacro bbox-intersect-p (bb1 bb2)
 `(and (<= (the fixnum (bbox-x1 ,bb1)) (the fixnum (bbox-x2 ,bb2)))   ;; 1 not right of 2
       (<= (the fixnum (bbox-x1 ,bb2)) (the fixnum (bbox-x2 ,bb1)))   ;; 2 not right of 1
       (<= (the fixnum (bbox-y1 ,bb1)) (the fixnum (bbox-y2 ,bb2)))   ;; 1 not below 2
       (<= (the fixnum (bbox-y1 ,bb2)) (the fixnum (bbox-y2 ,bb1))))) ;; 2 not below 1


;; Erases this bbox from this window (or its buffer). Ignores valid bit.
#-GARNET-V3.0
(defun erase-bbox (bb drawable buffer buffer-gc)
  (declare ; (optimize (safety 0) (speed 3) (space 1))
   )
  (if buffer
      (let ((background (xlib:gcontext-background buffer-gc)))
        (xlib:with-gcontext (buffer-gc :function opal::*copy*
				       :foreground background)
	  (xlib:draw-rectangle buffer buffer-gc
			       (bbox-x1 bb)
			       (bbox-y1 bb)
			       (the fixnum (- (the fixnum (bbox-x2 bb)) (the fixnum (bbox-x1 bb))))
			       (the fixnum (- (the fixnum (bbox-y2 bb)) (the fixnum (bbox-y1 bb))))
			       t)))
      (xlib:clear-area drawable
		       :x (bbox-x1 bb)
		       :y (bbox-y1 bb)
		       :width (the fixnum (- (the fixnum (bbox-x2 bb)) (the fixnum (bbox-x1 bb))))
		       :height (the fixnum (- (the fixnum (bbox-y2 bb)) (the fixnum (bbox-y1 bb)))))))

;; Takes a bbox and a clip mask, and goes through and sets the fields properly
;; within the clip mask.  Ignores valid bit.
(defmacro bbox-to-clip-mask (bb clip-mask)
  `(let ((cm ,clip-mask))
     (setf (car cm) (bbox-x1 ,bb))
     (setf (car (setq cm (cdr cm))) (bbox-y1 ,bb))
     (setf (car (setq cm (cdr cm))) (the fixnum (- (the fixnum (bbox-x2 ,bb)) (the fixnum (bbox-x1 ,bb)))))
     (setf (cadr cm) (the fixnum (- (the fixnum (bbox-y2 ,bb)) (the fixnum (bbox-y1 ,bb)))))))



#|
(define-method :draw opal:polyline (gob line-style-gc filling-style-gc drawable root-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (xlib-gc-line (opal-gc-gcontext line-style-gc))
	 (xlib-gc-filling (opal-gc-gcontext filling-style-gc))
	 (point-list (aref update-vals *polyline-point-list*))
	 (lstyle     (aref update-vals *polyline-lstyle*))
	 (fstyle     (aref update-vals *polyline-fstyle*))
	 (x-draw-fn (get (aref update-vals *polyline-draw-function*) :x-draw-function)))
    (when point-list
      (when fstyle
        (set-filling-style fstyle filling-style-gc xlib-gc-filling root-window x-draw-fn)
        (xlib:draw-lines drawable xlib-gc-filling point-list :fill-p t))

      (when lstyle
        (set-line-style lstyle line-style-gc xlib-gc-line root-window x-draw-fn)
        (xlib:draw-lines drawable xlib-gc-line point-list)))))

|#


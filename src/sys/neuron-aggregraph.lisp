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

;; SYS Source file: neuron-aggregraph.lisp

;; Code for plotting a tree graph of a loaded cell. Eventually make this into Scholl plot.

(in-package "SURF-HIPPO")

(create-instance 'neuron-graph opal:aggregraph
		 (:node-prototype (create-instance nil opal::aggregraph-node-prototype
						   (:parts
						    `((:box ,opal:roundtangle
							    (:filling-style ,opal:white-fill)
							    (:top ,(kr:o-formula (kr:gvl :parent :top)))
							    (:left ,(kr:o-formula (kr:gvl :parent :left)))
							    (:width ,(kr:o-formula (+ (kr:gvl :parent :text-al :width) 8)))
							    (:height ,(kr:o-formula (+ (kr:gvl :parent :text-al :height) 8)))
							    (:radius 5))
						      (:text-al ,opal:multi-text 
								(:left ,(kr:o-formula (+ (kr:gvl :parent :left) 4)))
								(:top ,(kr:o-formula (+ (kr:gvl :parent :top) 4)))
								(:string ,(kr:o-formula (kr:gvl :parent :info))))))))
						   
		 (:children-function #'(lambda (proximal-element depth)
					 (declare (ignore depth))
					 (distal-segments proximal-element)))
		 (:info-function #'(lambda (proximal-element) (format nil "~A" (element-name proximal-element)))))

(defun neuron-graph-choose-root-menu (&key (old-root *soma*) (title "Choose root element for Neuron Graph"))
  (choose-specific-elements (cell-elements) (element old-root) :only-one-choice t :label title))

(defun neuron-graph (&key (width 400) (height 400) title)
  (let* ((root (neuron-graph-choose-root-menu))
	 (source-root (list (cond ((cell-element-p root) root)
				  (t (element-soma root))))))
    (when source-root
      (let* ((neuron-graph (create-instance nil neuron-graph (:source-roots source-root)))
	     (win (create-scrolling-display-window :width width :height height
						   :mode :aggregraph
						   :display-object neuron-graph
						   :title (GET-win-TITLE-STRING
							   (or title
							       (if (> (length *circuit*) 0)
								 (concatenate-strings *circuit* ": Neuron Graph")
								 "Surf-Hippo Neuron Graph"))))))
	(add-neuron-graph-interactors win)
	;; (s-value neuron-graph :node-prototype :text-al :font (font-menu nil "Font for Neuro Graph"))
	(resurrect-opal-win win)))))

(defun neuron-graph-window-menu-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (let ((neuron-graph (g-value window :display-object)))
     ;; Use :remove-root and :make-root methods?
     ;; (s-value neuron-graph :source-roots (list (neuron-graph-choose-root-menu :old-root (g-value neuron-graph :source-roots))))
     (s-value neuron-graph :node-prototype :text-al :font (font-menu nil "Font for Neuro Graph")))))

(defun add-neuron-graph-interactors (win)
  (let ((scroll-win (g-value win :scrolling-window)))
    (create-instance nil ph::mark-plotline-interactor (:Window `(,win ,(g-value scroll-win :clip-window) ,(g-value scroll-win :inner-window))))
    (create-instance nil ph::edit-plotlines-interactor (:Window `(,win ,(g-value scroll-win :clip-window) ,(g-value scroll-win :inner-window))))
    (create-instance nil window-menu-Interactor
		     (:Window `(,win ,(g-value scroll-win :clip-window) ,(g-value scroll-win :inner-window)))
		     (:final-function #'neuron-graph-window-menu-inter-function))))


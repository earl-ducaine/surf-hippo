;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SON-OF-PLOT-HACK; Base: 10 -*-

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

;; GUI Source file: virtual-things.lisp

(in-package "SON-OF-PLOT-HACK")

;; For virtual things, there are assumed parameter locations in the :item-values array, including those for virtual-line,
;; virtual-circle, virtual-rectangle and all others (virtual-gob).

(defun if-black-then-xor-draw-function ()
  (if (black-p (cond ((gvl :filling-style)
		      (gvl :filling-style :foreground-color))
		     ((gvl :line-style)
		      (gvl :line-style :foreground-color))))
    :xor :copy))

(export '(IF-BLACK-THEN-XOR-DRAW-FUNCTION  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIRTUAL-LINE item-values array-> #(x1 y1 x2 y2 line-style)
;;
(defmacro virtual-line-x1 (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 0)))
(defmacro virtual-line-y1 (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 1)))
(defmacro virtual-line-x2 (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 2)))
(defmacro virtual-line-y2 (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 3)))
(defmacro virtual-line-line-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 4))

(defun make-virtual-line-item-values-array (x1 y1 x2 y2 line-style) (collect-to-array x1 y1 x2 y2 line-style))

(create-instance 'virtual-basic-line opal:line
		 (:constant :visible :fast-redraw-p :filling-style) ; (:fast-redraw-p t)
		 (:line-style (o-formula (when (and (gvl :parent) (gvl :parent :window))
					   (create-plot-line-style (or (virtual-line-line-style) (gvl :parent :window :default-line-style))
								   (gvl :parent :window)))))
		 (:visible t) (:filling-style nil)
		 (:lower-bounded-line-thickness (o-formula (fixnum-max 1 (gvl :line-style :line-thickness))))
		 (:left (o-formula (fixnum-min (gvl :x1) (gvl :x2))))
		 (:top (o-formula (fixnum-min (gvl :y1) (gvl :y2))))
		 (:width (o-formula (abs (- (the fn (gvl :x1)) (the fn (gvl :x2))))))
		 (:height (o-formula (abs (- (the fn (gvl :y1)) (the fn (gvl :y2)))))))

(create-instance 'virtual-line virtual-basic-line
		 (:x1 (o-formula (virtual-line-x1))) (:y1 (o-formula (virtual-line-y1))) (:x2 (o-formula (virtual-line-x2))) (:y2 (o-formula (virtual-line-y2)))
		 (:update-function 'update-virtual-line))

(export '(virtual-basic-line
	  virtual-line-x1
	  virtual-line-y1
	  virtual-line-x2
	  virtual-line-y2
	  virtual-line-line-style
	  make-virtual-line-item-values-array
	  virtual-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIRTUAL-CIRCLE item-values array -> #(left top width height filling-style)
;;
(defmacro virtual-circle-left (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 0))) 
(defmacro virtual-circle-top (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 1))) 
(defmacro virtual-circle-width (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 2))) 
(defmacro virtual-circle-height (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 3))) 
(defmacro virtual-circle-filling-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 4))
(defmacro virtual-circle-line-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 5))
(defmacro virtual-circle-center-x (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 6))) 
(defmacro virtual-circle-center-y (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 7))) 

(defun make-virtual-circle-item-values-array (left top width height filling-style line-style center-x center-y)
  (collect-to-array left top width height filling-style line-style center-x center-y))

(create-instance 'virtual-circle opal:circle
		 (:center-x (o-formula (virtual-circle-center-x)))
		 (:center-y (o-formula (virtual-circle-center-y)))
		 (:left (o-formula (virtual-circle-left)))
		 (:top (o-formula (virtual-circle-top)))
		 (:width (o-formula (virtual-circle-width)))
		 (:height (o-formula (the fn (gvl :width))))
		 (:line-style
		  (o-formula (when (and (gvl :parent) (gvl :parent :window))
			       (create-plot-line-style (or (virtual-circle-line-style) (gvl :parent :window :default-line-style))
						       (gvl :parent :window))))
					; (o-formula (or (virtual-circle-line-style) (gvl :parent :window :default-line-style)))
		  )
		 (:filling-style (o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
					       (create-plot-filling-style (or (virtual-circle-filling-style)
									      (gvl :parent :window :default-graphics-filling-style))
									  (gvl :parent :window)))))
		 ;; (:filling-style (o-formula (or (virtual-circle-filling-style) (gvl :parent :window :default-graphics-filling-style))))
		 ;; (:draw-function (IF-BLACK-THEN-XOR-DRAW-FUNCTION))
		 (:update-function 'update-virtual-circle))

(create-instance 'virtual-circle-w/o-border virtual-circle (:line-style nil))

(export '(virtual-circle-left
	  virtual-circle-top
	  virtual-circle-width
	  virtual-circle-height
	  virtual-circle-filling-style
	  virtual-circle-line-style
	  make-virtual-circle-item-values-array
	  virtual-circle
	  virtual-circle-w/o-border
	  virtual-circle-center-x
	  virtual-circle-center-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIRTUAL-RECTANGLE item-values array -> #(left top width height filling-style)
;;
(defmacro virtual-rectangle-left (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 0)))
(defmacro virtual-rectangle-top (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 1)))
(defmacro virtual-rectangle-width (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 2)))
(defmacro virtual-rectangle-height (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 3)))
(defmacro virtual-rectangle-filling-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 4))

(defun make-virtual-rectangle-item-values-array (left top width height filling-style) (collect-to-array left top width height filling-style))

(create-instance 'virtual-rectangle opal:rectangle
		 (:line-style nil)
		 ;; (:draw-function (IF-BLACK-THEN-XOR-DRAW-FUNCTION))
		 (:filling-style (o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
					      (create-plot-filling-style (or (virtual-rectangle-filling-style)
									     (gvl :parent :window :default-graphics-filling-style))
									 (gvl :parent :window)))))
		 ;; (:filling-style (o-formula (virtual-rectangle-filling-style)))
		 (:left (o-formula (virtual-rectangle-left)))
		 (:top (o-formula (virtual-rectangle-top)))
		 (:width (o-formula (virtual-rectangle-width)))
		 (:height (o-formula (virtual-rectangle-height))))

(export '(virtual-rectangle-left
	  virtual-rectangle-top
	  virtual-rectangle-width
	  virtual-rectangle-height
	  virtual-rectangle-filling-style
	  make-virtual-rectangle-item-values-array
	  virtual-rectangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIRTUAL-POLYLINE item-values array -> #(point-list line-style filling-style)
;;
(defmacro virtual-polyline-point-list (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 0)))
(defmacro virtual-polyline-line-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 1))
(defmacro virtual-polyline-filling-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 2))

(defun make-virtual-polyline-item-values-array (point-list line-style filling-style) (collect-to-array point-list line-style filling-style))

(create-instance 'virtual-polyline opal:polyline
		 ;; (:draw-function (o-formula (IF-BLACK-THEN-XOR-DRAW-FUNCTION)))
		 (:point-list (o-formula (virtual-polyline-point-list)))
		 ;; (:line-style (o-formula (virtual-polyline-line-style)))
		 (:line-style (o-formula (when (and (gvl :parent) (gvl :parent :window))
					   (create-plot-line-style (or (virtual-polyline-line-style) (gvl :parent :window :default-line-style))
								   (gvl :parent :window)))))
		 (:filling-style (o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
					      (create-plot-filling-style (or (virtual-polyline-filling-style)
									     (gvl :parent :window :default-graphics-filling-style))
									 (gvl :parent :window)))))
		 ;; (:filling-style (o-formula (virtual-polyline-filling-style)))
		 (:update-function 'update-virtual-polyline))
		 

(export '(virtual-polyline-point-list virtual-polyline-line-style virtual-polyline-filling-style make-virtual-polyline-item-values-array virtual-polyline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Virtual GOBs
;; both the aggregadget and straight circle version work may 4 2001,  but for now only the :update-function method works for
;; non-aggregadget dummy items of virtual aggregates.
;;
;; Virtual GOB item-values array -> #(x-center y-center x-left x-right y-top y-bottom line-style filling-style)
;;
(defmacro virtual-gob-x-center (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 0)))
(defmacro virtual-gob-y-center (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 1)))
(defmacro virtual-gob-x-left (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 2)))
(defmacro virtual-gob-x-right (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 3)))
(defmacro virtual-gob-y-top (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 4)))
(defmacro virtual-gob-y-bottom (&optional (item-values-ref `(gvl :item-values))) `(the fn (aref (the simple-array ,item-values-ref) 5)))
(defmacro virtual-gob-line-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 6))
(defmacro virtual-gob-filling-style (&optional (item-values-ref `(gvl :item-values))) `(aref (the simple-array ,item-values-ref) 7))

(defun make-virtual-GOB-item-values-array (x-center y-center x-left x-right y-top y-bottom line-style filling-style)
  (collect-to-array x-center y-center x-left x-right y-top y-bottom line-style filling-style))

;; The parent versions are used for parts of aggregadgets
(defmacro virtual-gob-parent-x-center () `(virtual-gob-x-center (gvl :parent :item-values)))
(defmacro virtual-gob-parent-y-center () `(virtual-gob-y-center (gvl :parent :item-values)))
(defmacro virtual-gob-parent-x-left () `(virtual-gob-x-left (gvl :parent :item-values)))
(defmacro virtual-gob-parent-x-right () `(virtual-gob-x-right (gvl :parent :item-values)))
(defmacro virtual-gob-parent-y-top () `(virtual-gob-y-top (gvl :parent :item-values)))
(defmacro virtual-gob-parent-y-bottom () `(virtual-gob-y-bottom (gvl :parent :item-values)))
(defmacro virtual-gob-parent-line-style () `(virtual-gob-line-style (gvl :parent :item-values)))
(defmacro virtual-gob-parent-filling-style () `(virtual-gob-filling-style (gvl :parent :item-values)))

(defun gvl-schema-window (&optional schema)
  (or (gvl :window)
      (when (gvl :parent)
	(or (gvl :parent :window)
	    (gvl-schema-window (gvl :parent))))))

(defun virtual-fillable-line-style-o-formula (line-style-reference)
  ;; So that a filled object will have the appropriate black or white border, whereas if it is unfilled, the border color will
  ;; match the line color.
  (when (and (gvl :parent) (gvl :parent :window))
    (create-plot-line-style
     (when line-style-reference
       (if (gvl :parent :window :fill-scatter)
	 (if (BLACK-p (gvl :parent :window :background-color))
	   thin-white-line thin-black-line)
	 line-style-reference))
     (gvl :parent :window))))

(defun virtual-fillable-parts-gob-line-style-o-formula () (virtual-fillable-line-style-o-formula (virtual-gob-parent-line-style)))
(defun virtual-fillable-gob-line-style-o-formula () (virtual-fillable-line-style-o-formula (virtual-gob-line-style)))


(create-instance 'virtual-dot opal:circle
		 (:line-style (o-formula (virtual-fillable-gob-line-style-o-formula)))
		 (:filling-style (o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
					      (create-plot-filling-style (or (virtual-gob-filling-style)
									     (gvl :parent :window :default-graphics-filling-style))
									 (gvl :parent :window)))))
		 ;; (:line-style (o-formula (virtual-gob-line-style)))
		 ;; (:filling-style (o-formula (virtual-gob-filling-style)))
		 ;; (:draw-function (IF-BLACK-THEN-XOR-DRAW-FUNCTION))
		 (:left (o-formula (virtual-gob-x-left)))
		 (:top (o-formula (virtual-gob-y-top)))
		 (:width (o-formula (the fn (gvl :height))))
		 (:height (o-formula (- (virtual-gob-y-bottom) (virtual-gob-y-top))))
		 (:update-function 'UPDATE-VIRTUAL-DOT))



(create-instance 'virtual-cross opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
		 (:parts `((:h-line ,virtual-basic-line 
				    ;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
				    (:line-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window))
							       (create-plot-line-style
								(or (virtual-gob-parent-line-style)
								    (gvl :parent :window :default-line-style))
								(gvl :parent :window)))))
				    (:x1 ,(o-formula (virtual-gob-parent-x-left)))
				    (:x2 ,(o-formula (virtual-gob-parent-x-right)))
				    (:y1 ,(o-formula (virtual-gob-parent-y-center)))
				    (:y2 ,(o-formula (virtual-gob-parent-y-center))))
			   (:v-line ,virtual-basic-line
				    ;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
				    (:line-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window))
							       (create-plot-line-style (or (virtual-gob-parent-line-style) (gvl :parent :window :default-line-style))
										       (gvl :parent :window)))))
				    (:x1 ,(o-formula (virtual-gob-parent-x-center)))
				    (:x2 ,(o-formula (virtual-gob-parent-x-center)))
				    (:y1 ,(o-formula (virtual-gob-parent-y-top)))
				    (:y2 ,(o-formula (virtual-gob-parent-y-bottom)))))))

(create-instance 'virtual-tilted-cross opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
		 (:parts `((:first-line ,virtual-basic-line
					;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
					(:line-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window))
								   (create-plot-line-style (or (virtual-gob-parent-line-style)
											       (gvl :parent :window :default-line-style))
											   (gvl :parent :window)))))
					(:x1 ,(o-formula (virtual-gob-parent-x-left)))
					(:x2 ,(o-formula (virtual-gob-parent-x-right)))
					(:y1 ,(o-formula (virtual-gob-parent-y-top)))
					(:y2 ,(o-formula (virtual-gob-parent-y-bottom))))
			   (:second-line ,virtual-basic-line
					 ;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
					 (:line-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window))
								    (create-plot-line-style (or (virtual-gob-parent-line-style)
												(gvl :parent :window :default-line-style))
											    (gvl :parent :window)))))
					 (:x1 ,(o-formula (virtual-gob-parent-x-left)))
					 (:x2 ,(o-formula (virtual-gob-parent-x-right)))
					 (:y1 ,(o-formula (virtual-gob-parent-y-bottom)))
					 (:y2 ,(o-formula (virtual-gob-parent-y-top)))))))

(create-instance 'virtual-up-triangle opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,virtual-polyline
				  ;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
				  (:line-style ,(o-formula (virtual-fillable-parts-gob-line-style-o-formula)))
				  ;; (:filling-style ,(o-formula (virtual-gob-parent-filling-style)))
				  (:filling-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
								(create-plot-filling-style (or (virtual-gob-parent-filling-style)
											       (gvl :parent :window :default-graphics-filling-style))
											   (gvl :parent :window)))))
				  (:point-list ,(o-formula (list (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-bottom)
								 (virtual-gob-parent-x-center)
								 (virtual-gob-parent-y-top)
								 (virtual-gob-parent-x-right)
								 (virtual-gob-parent-y-bottom)
								 (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-bottom))))))))

(create-instance 'virtual-down-triangle opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,virtual-polyline
				  ;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
				  (:line-style ,(o-formula (virtual-fillable-parts-gob-line-style-o-formula)))
				  ;; (:filling-style ,(o-formula (virtual-gob-parent-filling-style)))
				  (:filling-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
								(create-plot-filling-style (or (virtual-gob-parent-filling-style)
											       (gvl :parent :window :default-graphics-filling-style))
											   (gvl :parent :window)))))
				  (:point-list ,(o-formula (list (virtual-gob-parent-x-center)
								 (virtual-gob-parent-y-bottom)
								 (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-top)
								 (virtual-gob-parent-x-right)
								 (virtual-gob-parent-y-top)
								 (virtual-gob-parent-x-center)
								 (virtual-gob-parent-y-bottom))))))))

(create-instance 'virtual-box opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,virtual-polyline
				  ;; (:line-style ,(o-formula (virtual-gob-parent-line-style)))
				  (:line-style ,(o-formula (virtual-fillable-parts-gob-line-style-o-formula)))
				  ;; (:filling-style ,(o-formula (virtual-gob-parent-filling-style)))
				  (:filling-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
								(create-plot-filling-style (or (virtual-gob-parent-filling-style)
											       (gvl :parent :window :default-graphics-filling-style))
											   (gvl :parent :window)))))
				  (:point-list ,(o-formula (list (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-bottom)
								 (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-top)
								 (virtual-gob-parent-x-right)
								 (virtual-gob-parent-y-top)
								 (virtual-gob-parent-x-right)
								 (virtual-gob-parent-y-bottom)
								 (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-bottom))))))))

(create-instance 'virtual-diamond opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,virtual-polyline
				  (:line-style ,(o-formula (virtual-fillable-parts-gob-line-style-o-formula)))
				  ;; (:filling-style ,(o-formula (virtual-gob-parent-filling-style)))
				  (:filling-style ,(o-formula (when (and (gvl :parent) (gvl :parent :window) (gvl :parent :window :fill-scatter))
								(create-plot-filling-style (or (virtual-gob-parent-filling-style)
											       (gvl :parent :window :default-graphics-filling-style))
											   (gvl :parent :window)))))
				  (:point-list ,(o-formula (list (virtual-gob-parent-x-center)
								 (virtual-gob-parent-y-bottom)
								 (virtual-gob-parent-x-left)
								 (virtual-gob-parent-y-center)
								 (virtual-gob-parent-x-center)
								 (virtual-gob-parent-y-top)
								 (virtual-gob-parent-x-right)
								 (virtual-gob-parent-y-center)
								 (virtual-gob-parent-x-center)
								 (virtual-gob-parent-y-bottom))))))))

(defvar *scatter-symbols* '(:dot :cross :box :down-triangle :up-triangle :diamond :tilted-cross))
(defvar *scatter-symbols-closed-curves* '(:dot :triangle :down-triangle :up-triangle :diamond :box))
(defvar *scatter-symbols-open-curves* '(:cross :tilted-cross))

(defun scatter-symbol-to-prototype (scatter-symbol)
  (case scatter-symbol
    (:dot virtual-dot)
    (:cross virtual-cross)
    ((:triangle :down-triangle) virtual-down-triangle)
    (:up-triangle virtual-up-triangle)
    (:diamond virtual-diamond)
    (:box virtual-box)
    (:tilted-cross virtual-tilted-cross)
    (t virtual-dot)))

(defun scatter-prototype-to-symbol (scatter-prototype)
  (cond 
    ((eq scatter-prototype virtual-dot) :dot)
    ((eq scatter-prototype virtual-cross) :cross)
    ((eq scatter-prototype virtual-down-triangle) :down-triangle)
    ((eq scatter-prototype virtual-up-triangle) :up-triangle)
    ((eq scatter-prototype virtual-diamond) :diamond)
    ((eq scatter-prototype virtual-box) :box)
    ((eq scatter-prototype virtual-tilted-cross) :tilted-cross)
    (t :dot)))

(defun scatter-prototype-to-namestring (scatter-prototype)
  (cond 
    ((eq scatter-prototype virtual-dot) "dot")
    ((eq scatter-prototype virtual-cross) "cross")
    ((eq scatter-prototype virtual-down-triangle) "triangle")
    ((eq scatter-prototype virtual-up-triangle) "up-triangle")
    ((eq scatter-prototype virtual-diamond) "diamond")
    ((eq scatter-prototype virtual-box) "box")
    ((eq scatter-prototype virtual-tilted-cross) "tilted-cross")
    (t "dot")))

(export '(virtual-gob-x-center virtual-gob-y-center virtual-gob-x-left virtual-gob-x-right
	  virtual-gob-y-top virtual-gob-y-bottom virtual-gob-line-style virtual-gob-filling-style

	  make-virtual-GOB-item-values-array virtual-gob-parent-x-center virtual-gob-parent-y-center virtual-gob-parent-x-left virtual-gob-parent-x-right
	  virtual-gob-parent-y-top virtual-gob-parent-y-bottom virtual-gob-parent-line-style virtual-gob-parent-filling-style

	  virtual-dot virtual-cross virtual-tilted-cross virtual-up-triangle virtual-down-triangle virtual-box virtual-diamond

	  *scatter-symbols* *scatter-symbols-closed-curves* *scatter-symbols-open-curves*

	  scatter-symbol-to-prototype scatter-prototype-to-symbol scatter-prototype-to-namestring))




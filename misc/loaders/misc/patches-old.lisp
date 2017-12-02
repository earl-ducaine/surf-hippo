;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF-HIPPO ; Base: 10; -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; taken from cell-graphics.lisp

;; the calls to sin and cos when the motion arrow is created (at end
;; of draw-stimulus-time) don't work properly (not called?) - by using
;; the indirection via sintest and costest, the code works. demo by
;; drawing star-amacrine-ds with stimulus drawn on initial image load,
;; then load this file and redraw histo.


;; apparently there are assumptions made at compile time of the declared values of globals in sin or
;; cos args.  
(IN-PACKAGE "SURF-HIPPO")




(defun draw-stimulus-times (agg)
  (when (or (eq *light-stimulus :apparent-motion)
	    (> *light-stimulus-start-time 0) (< *light-stimulus-stop-time *user-stop-time*))

    (opal:add-component
     agg
     (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
		      (:top *histology-window-stimulus-time-distance-from-top*)
		      (:string (format nil "~Dms" (round *user-start-time*)))
		      (:left (o-formula
			      (round (- *histology-window-stimulus-time-distance-from-left* (* 0.5 (gvl :width))))))
		      (:font (opal:get-standard-font :serif :bold-italic :medium)))
     :where :front)
    (opal:add-component
     agg
     (create-instance nil opal:text
		      (:visible t) (:type 'stimulus-label)
		      (:top *histology-window-stimulus-time-distance-from-top*)
		      (:string (format nil "~Dms" (round *user-stop-time*)))
		      (:left (o-formula (round (- (+ *histology-window-stimulus-time-distance-from-left*
						     *histology-window-stimulus-time-length*)
						  (* 0.5 (gvl :width))))))
		      (:font (opal:get-standard-font :serif :bold-italic :medium)))
     :where :front)
    (loop for start in (if (eq *light-stimulus :apparent-motion)
			   (list *bar-a-start-time *bar-b-start-time)
			 (list *light-stimulus-start-time))
	  for stop in (if (eq *light-stimulus :apparent-motion)
			  (list *bar-a-stop-time *bar-b-stop-time)
			(list *light-stimulus-stop-time))
	  for label in '("A" "B")
	  do
	  (let ((x1 (round (+ *histology-window-stimulus-time-distance-from-left*
			      (* (max 0.0 (/ start *user-stop-time*)) *histology-window-stimulus-time-length*))))
		(x2 (+  *histology-window-stimulus-time-distance-from-left*
			(round (* (min 1.0 (/ stop *user-stop-time*)) *histology-window-stimulus-time-length*)))))
	    (when (= x1 x2) (setq x2 (1+ x1)))
	    (when (eq *light-stimulus :apparent-motion)
	      (opal:add-component
	       agg
	       (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
				(:top *histology-window-stimulus-time-distance-from-top*)
				(:string label)
				(:left (o-formula (round (- (+ x1 (* 0.5 (- x2 x1))) (* 0.5 (gvl :width))))))
				(:font (opal:get-standard-font :serif :bold-italic :large)))
	       :where :front))	    
	    (opal:add-component
	     agg
	     (create-instance nil opal:line
			      (:visible t) (:type 'stimulus-label)
			      (:x1 x1) (:x2 x2)
			      (:y1 (- *histology-window-stimulus-time-distance-from-top* 3))
			      (:y2 (- *histology-window-stimulus-time-distance-from-top* 3))
			      (:draw-function :copy)
			      (:line-style
			       (create-instance
				nil opal:line-style
				(:line-thickness 6)
				(:filling-style (create-instance nil opal:filling-style (:foreground-color opal:black)))))
			      (:line-thickness 6)) :where :front)))))

(defun draw-stimulus-time-line (agg)
  ;; Time line
  (unless (and (>= *light-stimulus-stop-time *user-stop-time*) (= *light-stimulus-start-time 0)
	       (not (eq *light-stimulus :apparent-motion)))
    (opal:add-component
     agg
     (create-instance nil opal:line
		      (:visible t) (:type 'stimulus-label)
		      (:x1 *histology-window-stimulus-time-distance-from-left*)
		      (:x2 (o-formula (+ (gvl :x1) *histology-window-stimulus-time-length*)))
		      (:y1 *histology-window-stimulus-time-distance-from-top*)
		      (:y2 *histology-window-stimulus-time-distance-from-top*)
		      (:draw-function :copy)) :where :front)))

(defun sintest (arg) 
  (sin arg))
(defun costest (arg) 
  (cos arg))

(defun create-motion-arrow (arrow-center-x arrow-center-y)
  (create-instance
   nil gg::arrow-line
   (:type 'stimulus-label) (:open-p t) (:line-style opal:line-2)
   (:x1 (round (+ arrow-center-x (* -15 (sintest (- (- *light-theta) (if *light-direction 0 pi)))))))
   (:x2 (round (+ arrow-center-x (* 15 (sintest (- (- *light-theta) (if *light-direction 0 pi)))))))
   (:y1 (round (+ arrow-center-y (* -1 -15 (costest (- *light-theta (if *light-direction 0 pi)))))))
   (:y2 (round (+ arrow-center-y (* -1 15 (costest (- *light-theta (if *light-direction 0 pi)))))))))

(defun create-motion-arrow (arrow-center-x arrow-center-y)
  (create-instance
   nil gg::arrow-line
   (:type 'stimulus-label) (:open-p t) (:line-style opal:line-2)
   (:x1 (round (+ arrow-center-x (* -15 (sin (- (- *light-theta) (if *light-direction 0 pi)))))))
   (:x2 (round (+ arrow-center-x (* 15 (sin (- (- *light-theta) (if *light-direction 0 pi)))))))
   (:y1 (round (+ arrow-center-y (* -1 -15 (cos (- *light-theta (if *light-direction 0 pi)))))))
   (:y2 (round (+ arrow-center-y (* -1 15 (cos (- *light-theta (if *light-direction 0 pi)))))))))

(defun create-motion-arrow (arrow-center-x arrow-center-y
					   &key (light-theta *light-theta))
  (create-instance
   nil gg::arrow-line
   (:type 'stimulus-label) (:open-p t) (:line-style opal:line-2)
   (:x1 (round (+ arrow-center-x (* -15 (sin (- (- light-theta) (if *light-direction 0 pi)))))))
   (:x2 (round (+ arrow-center-x (* 15 (sin (- (- light-theta) (if *light-direction 0 pi)))))))
   (:y1 (round (+ arrow-center-y (* -1 -15 (cos (- light-theta (if *light-direction 0 pi)))))))
   (:y2 (round (+ arrow-center-y (* -1 15 (cos (- light-theta (if *light-direction 0 pi)))))))))

(defvar *bar* 1.0)
(defun foobar ()
  (format t "angle:~A cos:~A~%" *bar* (cos *bar*)))

(defvar *baz* pi-single)
(proclaim '(single-float *baz*))
(defun foobaz ()
  (format t "angle:~A cos:~A~%" *baz* (cos *baz*)))
	  


(defun foo ()
  (format t "angle:~A cos~A~%" *baz* (cos *baz*)))

(defun foo ()
  (let ((angle (* 1.5 (- (- *light-theta) (if *light-direction 0.0d0 pi)))))
    (format t "angle:~A cos~A~%"
	    angle
	    (cos angle))))




(defun create-motion-arrow (arrow-center-x arrow-center-y)
  (let ((sin-x1-arg (sin (- (- *light-theta) (if *light-direction 0.0d0 pi))))
	(sin-x2-arg (sin (the df (- (- *light-theta) (if *light-direction 0.0d0 pi)))))
	(cos-y1-arg (costest (- *light-theta (if *light-direction 0 pi)))))
;;    (format t "sin-x1-arg: ~A, sin-x2-arg: ~A, cos-y1-arg: ~A~%" sin-x1-arg sin-x2-arg cos-y1-arg)
    (create-instance
     nil gg::arrow-line
     (:type 'stimulus-label) (:open-p t) (:line-style opal:line-2)
     (:x1 (round (+ arrow-center-x (* -15 sin-x1-arg))))
     (:x2 (round (+ arrow-center-x (* 15 sin-x2-arg))))
     (:y1 (round (+ arrow-center-y (* -1 -15 cos-y1-arg))))
     (:y2 (round (+ arrow-center-y (* -1 15 (cos (- *light-theta (if *light-direction 0 pi))))))))))

(defun add-motion-arrow (agg stimulus-label)
  (let ((arrow-center-x (truncate (+ 
				   (g-value stimulus-label :left)
				   (* 0.5 (g-value stimulus-label :width)))))
	(arrow-center-y (truncate (+ *histology-window-stimulus-time-distance-from-top*
				     17 (g-value stimulus-label :height)))))
    (opal:add-component agg (create-motion-arrow arrow-center-x arrow-center-y):where :front)))

(defun create-stimulus-label (agg label)
  (opal:add-component
   agg
   (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
		    (:top *histology-window-stimulus-time-distance-from-top*)
		    (:string (if (and (>= *light-stimulus-stop-time *user-stop-time*)
				      (= *light-stimulus-start-time 0))
				 (concatenate 'string label " - On for entire run")
			       label))
		    (:left (o-formula (round (- (+ *histology-window-stimulus-time-distance-from-left*
						   (* 0.5 *histology-window-stimulus-time-length*))
						(* 0.5 (gvl :width))))))
		    (:font (opal:get-standard-font :serif :bold-italic :large)))
   :where :front))

(defun draw-stimulus-time (label agg &optional motion-arrow)
  (destroy-stimulus-line agg)
  (draw-stimulus-times agg)
  (draw-stimulus-time-line agg)
  (unless (eq *light-stimulus :apparent-motion)
    (let ((stimulus-label (create-stimulus-label agg label)))
      (when motion-arrow (add-motion-arrow agg stimulus-label)))))







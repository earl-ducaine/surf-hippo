(in-package "SON-OF-PLOT-HACK")

(defun random-dots (count &optional (x-range 1.0) (y-range 1.0))
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float x-range y-range))  
  (loop for count fixnum from 1 to count
	collect (random x-range) into x
	collect (random x-range) into y
	finally (return (list x y))))

(defvar *polyline-p* t)
#|
(defun new-add-polyline-to-plot (plot-agg points line-style connect-ends)
  (loop for points in (massage-points points) do
	(opal:add-component           
	 plot-agg (create-instance nil (if *POLYLINE-P* opal:polyline opal:multipoint)
				   ;; (:constant t)
				   (:line-style line-style)
				   (:point-list (if (not connect-ends)
						  points
						  (let ((first-x (first points))
							(first-y (second points)))
						    (nconc (nconc points (list first-x)) (list first-y))))))
	 :where :back)))
|#
(defun get-data-plot-agg (win)
  (loop for comp in (g-value win :aggregate :components)
	when (eq 'PH::DATA-PLOT (g-value comp :type))
	do (return comp)))

#|
(defun add-polyline-to-plot (plot-agg plot-win x-list y-list line-style &key connect-ends)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type KR::SCHEMA plot-win plot-agg))
  (opal:add-component			;Data Points
   plot-agg		 
   (create-instance nil (if *POLYLINE-P* opal:polyline opal:multipoint)
		    ;; (:constant t)
		    (:point-list (loop for x in (if connect-ends
						  (concatenate 'list x-list (list (first x-list)))
						  x-list)
						
				       for y in (if connect-ends
						  (concatenate 'list y-list (list (first y-list)))
						  y-list)
				       when (and (numberp x) (numberp y))
				       collect (x-plot-win-float (s-flt x) plot-win)
				       and
				       collect (y-plot-win-float (s-flt y) plot-win)))
		    (:line-style line-style))))
|#

(defun get-data-plot-agg (win)
  (loop for comp in (g-value win :aggregate :components)
	when (eq 'PH::DATA-PLOT (g-value comp :type))
	do (return comp)))

(defun test-foo ()
  (profile::unprofile)
  (profile::profile			;xlib:display-invoke-after-function
					;xlib::get-put-items
					;xlib::drawable-display))
					;xlib::force-gcontext-changes-internal


   xlib::RADIANS->INT16
 ; XLIB::DRAW-ARC-INNER-REQUEST-1-FOO ;
 ; XLIB::DRAW-ARC-INNER-REQUEST-1-BAR ; XLIB::DRAW-ARC-INNER-REQUEST-1-FOO
 ; XLIB::DRAW-ARC-INNER-REQUEST ; XLIB::DRAW-ARC-INNER-FOO ;XLIB::DRAW-ARC-INNER-FOO;  xlib:draw-arc
;  XLIB::NEW-FULL-CIRCLE-P-FOO
;   XLIB::barFOO-BUFFER-NEW-REQUEST-NUMBER;xlib::BUFFER-NEW-REQUEST-NUMBER
;    XLIB::ASET-INT16 XLIB::ASET-CARD16 XLIB::ASET-CARD8 XLIB::aref-card8 XLIB::aref-int8 XLIB::aset-int8
;    XLIB::ASET-CARD29 xlib::aref-int32 xlib::aset-int32 xlib::aref-card32 xlib::aset-card32

;  plot-xy-data random-dots



   ; draw-virtual-circle
   )
  (time (ph::foo))
  (profile::report-time)
  (profile::unprofile))
  

(defun foo ()
  (let* ((count 1000)
	 (num-randoms 20))
    (loop for random-set from 1 to num-randoms
	  collect
	  (let* ((random-dots (random-dots count))
		 (win (plot-xy-data random-dots
				    nil  :width 400 :height 400
				    :connect-data-points nil :scatter t
				    :x-min 0 :y-min 0 :x-max 1 :y-max 1
				    :y-symbol-width 2
					; :preserve-plot-layout nil
					; :preserve-plot-layout t
				    ))
		 (multipoint (car (g-value (PH::GET-DATA-PLOT-AGG win) :components))))
	    (g-value multipoint :item-array)) into item-arrays
	  finally

	  (let ((multipoint (car (g-value (PH::GET-DATA-PLOT-AGG *twin*) :components))))
	    (time
	     (dotimes (i 100)
	       (loop for item-array in item-arrays do
		     (s-value multipoint :item-array item-array)
		     (opal::update *twin* t))
					; (format t "done ~A~%" i)
	       ))))))



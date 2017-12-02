#|
(defun plot-histogram (xy-data-list
		       &key win (bin-width 1.0) y-inc (y-min 0.0) y-max x-min x-max x-origin y-origin (x-origin-tick t) (y-origin-tick t)
		       (title "Histogram Data") title-position data-type
		       (x-label "") (y-label "")
		       x-axis-tick-skip x-axis-tick-mark-skip (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
		       x-are-fns (y-are-fns t)
		       x-label-v-position x-label-h-position y-label-v-position y-label-h-position
		       stipple-percent (bar-border-p t)
		       overlay accomodate-all-overlays prompt-for-overlay
		       axes-type	; (:STANDARD :SIMPLE :NONE)
		       x-scale-l% x-scale-t% y-scale-t%
		       (simple-axis-x-value-p t) (simple-axis-y-value-p t)
		       UPDATE-FIXED-GAP
		       use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0)
		       use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
		       font comment (comment-position *default-comment-position*) (upper-right-comment "") 
		       left top (width 400) (height 400))
  ;;
  ;; XY-DATA-LIST = '(X Y), where X is a list containing the histogram bin indices and Y is a list of the actual histogram values.
  ;;		     Y  , where Y is a list of the actual histogram values.
  ;; Histogram are constructed with vertical bars whose left edges are defined by the X entries in XY-DATA-LIST, and whose widths
  ;; are defined by BIN-WIDTH. If either X-AXIS-TICK-SKIP or X-AXIS-TICK-MARK-SKIP is NIL [default], then reasonable values will be
  ;; chosen automatically.
  ;;
  (unless win (setq win (get-plot-window :xy data-type overlay :title title :mode :histogram
					 :accomodate-all-overlays (unless prompt-for-overlay accomodate-all-overlays)
					 :prompt-for-overlay prompt-for-overlay :left left :top top)))
  (when win
    (multiple-value-bind (x-list y-list) 
	(if (consp (cadr xy-data-list))
	    (values (cdr xy-data-list) (cadr xy-data-list))
	    (values (list-of-nums (length xy-data-list) (or x-min 0.0) bin-width) xy-data-list))
	    
	    
      (when axes-type (s-value win :axes-type axes-type))
      (when x-scale-l% (s-value win :x-scale-l% x-scale-l%))
      (when x-scale-t% (s-value win :x-scale-t% x-scale-t%))
      (when y-scale-t% (s-value win :y-scale-t% y-scale-t%))
      (s-values win simple-axis-x-value-p simple-axis-y-value-p)
      (when UPDATE-FIXED-GAP
	(UPDATE-FIXED-GAP win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap))
      (when font (s-values win font (comment-font font) (plot-axis-font font)))
      (add-temp-comment win upper-right-comment)
      (add-local-and-global-comment comment comment-position win)
      (s-value win :bin-width (float bin-width))
      (s-values win bar-border-p)
      (when y-label-h-position (s-values win y-label-h-position))
      (s-values win stipple-percent) (s-value win :x-label-h-position :center) 
      (unless y-max (setq y-max (float (max-of-list y-list))))  (unless y-inc (setq y-inc (/ y-max 5)))

      (load-x-lists win (list x-list))
      (load-y-lists win (list y-list))

      (let ((x-inc (if (> (length x-list) 1)
		       (- (nth 1 x-list) (nth 0 x-list))
		       bin-width)))

      	(loop for x-list in (g-value win :x-lists)
	      for y-list in (g-value win :y-lists)
	      do (let ((number-of-bins (round (/ (- (max-of-list x-list) (min-of-list x-list)) x-inc))))
		   (s-value win :x-axis-tick-skip (or x-axis-tick-skip
						      (if (< number-of-bins 10)
							  0
							  (round (/ number-of-bins 5)))))
		   (s-value win :x-axis-tick-mark-skip (or x-axis-tick-mark-skip
							   (if (< number-of-bins 10)
							       0
							       (round (/ number-of-bins 10)))))))
							   
	(s-values win y-axis-tick-skip y-axis-tick-mark-skip)

	(unless (and (g-value win :overlay) (not (g-value win :accomodate-all-overlays)))
	  (setup-plot win		; :scale scale
		      :x-origin x-origin :y-origin y-origin
		      :x-label-v-position x-label-v-position
		      :x-label-h-position x-label-h-position
		      :y-label-v-position y-label-v-position
		      :y-label-h-position y-label-h-position
		      :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
		      :x-inc x-inc
		      :x-min-spec x-min :x-max-spec (if x-max x-max (+ (max-of-list x-list) bin-width))
		      :x-label x-label :y-label y-label
		      :x-are-fns x-are-fns :y-are-fns y-are-fns 
		      :y-inc y-inc :y-min-spec y-min :y-max-spec y-max :width width :height height :consider-labels t))
	(let* ((plot-agg (get-plot-agg win 'data-plot t))
	       (stipple-percent (or (g-value win :stipple-percent) (unless (g-value win :bar-border-p) 50)))
	       (line-style (when (g-value win :bar-border-p) thick-line))
	       (filling-style  (when stipple-percent (get-opal-color-to-fill 'black stipple-percent))))
	  (loop for x-list in (g-value win :x-lists)
		for y-list in (g-value win :y-lists)
		do (loop for x in (car x-list) ; (car xy-data-list)
			 for y in (car y-list) ; (cadr xy-data-list)
			 when (>= (- (g-value win :x-max) bin-width) x) 
			 do (add-histo-bin plot-agg x y nil nil line-style filling-style))))
	(when title-position (add-title win :position title-position))
	(plot-windows-finishing win)))))

(defun plot-histogram (xy-data-list
		       &key ; (bin-width 1.0)
		       y-inc (y-min 0.0) y-max x-min x-max x-origin y-origin (x-origin-tick t) (y-origin-tick t) x-are-fns (y-are-fns t)
		       (title "Histogram Data") title-position data-type (x-label "") (y-label "")
		       x-axis-tick-skip x-axis-tick-mark-skip (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
		       x-label-v-position x-label-h-position y-label-v-position y-label-h-position
		       stipple-percent (bar-border-p t)
		       overlay accomodate-all-overlays prompt-for-overlay
		       axes-type	; (:STANDARD :SIMPLE :NONE)
		       x-scale-l% x-scale-t% y-scale-t%
		       (simple-axis-x-value-p t) (simple-axis-y-value-p t)
		       update-fixed-gap use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0)
		       use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
		       font comment (comment-position *default-comment-position*) (upper-right-comment "") 
		       left top (width 400) (height 400)
		       (win (get-plot-window :xy data-type overlay :title title :mode :histogram :left left :top top
					     :accomodate-all-overlays (unless prompt-for-overlay accomodate-all-overlays) :prompt-for-overlay
					     prompt-for-overlay )))
  ;; XY-DATA-LIST = (X Y), where X is a list containing the histogram bin indices and Y is a list of the actual histogram values,
  ;;                or
  ;;		     Y   , where Y is a list of the actual histogram values.
  ;; Histogram are constructed with vertical bars whose left edges are defined by the X entries in XY-DATA-LIST, and whose widths
  ;; are defined by BIN-WIDTH. If either X-AXIS-TICK-SKIP or X-AXIS-TICK-MARK-SKIP is NIL [default], then reasonable values will be
  ;; chosen automatically.
;  (printvars xy-data-list bin-width y-inc y-min y-max x-min x-max x-origin y-origin x-origin-tick y-origin-tick x-are-fns y-are-fns)
  (when win
    (multiple-value-bind (x-list y-list) 
	(if (consp (cadr xy-data-list))
	    (values (first xy-data-list) (second xy-data-list))
	    (values (list-of-nums (length xy-data-list) (or x-min 0.0) bin-width) xy-data-list))
      (s-values win simple-axis-x-value-p simple-axis-y-value-p (y-axis-tick-mark-length 5))
      (cond-every
       (axes-type (s-value win :axes-type axes-type))
       (x-scale-l% (s-value win :x-scale-l% x-scale-l%))
       (x-scale-t% (s-value win :x-scale-t% x-scale-t%))
       (y-scale-t% (s-value win :y-scale-t% y-scale-t%))
       (font (s-values win font (comment-font font) (plot-axis-font font))))
      (when UPDATE-FIXED-GAP (UPDATE-FIXED-GAP win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap
					       use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap))
      (add-temp-comment win upper-right-comment)
      (add-local-and-global-comment comment comment-position win)
      (s-values win bar-border-p (bin-width (float bin-width)) stipple-percent (x-label-h-position :center))
      (when y-label-h-position (s-values win y-label-h-position))
      (unless y-max (setq y-max (float (max-of-list y-list))))
      (unless y-inc (setq y-inc (/ y-max 5)))
      (load-x-lists win (list x-list))
      (load-y-lists win (list y-list))
      (let ((x-inc (if (> (length x-list) 1)
		       (- (nth 1 x-list) (nth 0 x-list))
		       bin-width)))
	(loop for x-list in (g-value win :x-lists)
	      for y-list in (g-value win :y-lists)
	      do (let ((bins (round (/ (- (max-of-list x-list) (min-of-list x-list)) x-inc))))
		   (s-value win :x-axis-tick-skip (or x-axis-tick-skip
						      (if (< bins 10)
							  0
							  (round (/ bins 5)))))
		   (s-value win :x-axis-tick-mark-skip (or x-axis-tick-mark-skip
							   (if (< bins 10)
							       0
							       (round (/ bins 10)))))))
							   
	(s-values win y-axis-tick-skip y-axis-tick-mark-skip)

	(unless (and (g-value win :overlay) (not (g-value win :accomodate-all-overlays)))
	  (setup-plot win :width width :height height :consider-labels t
		      :x-origin x-origin :x-origin-tick x-origin-tick :y-origin y-origin :y-origin-tick y-origin-tick
		      :x-label x-label :x-label-v-position x-label-v-position :x-label-h-position x-label-h-position
		      :y-label y-label :y-label-v-position y-label-v-position :y-label-h-position y-label-h-position
		      :x-inc x-inc :x-min-spec x-min :x-max-spec (if x-max x-max (+ (max-of-list x-list) bin-width))
		      :x-are-fns x-are-fns :y-are-fns y-are-fns 
		      :y-inc y-inc :y-min-spec y-min :y-max-spec y-max))
	(let ((plot-agg (get-plot-agg win 'data-plot t))
	      (stipple-percent (or (g-value win :stipple-percent) (unless (g-value win :bar-border-p) 50)))
	      (line-style (when (g-value win :bar-border-p) thick-line))
	      (filling-style  (when stipple-percent (get-opal-color-to-fill 'black stipple-percent))))
	  (loop for x-list in (g-value win :x-lists)
		for y-list in (g-value win :y-lists)
		do (loop for x in (car x-list) ; (car xy-data-list)
			 for y in (car y-list) ; (cadr xy-data-list)
			 when (>= (- (g-value win :x-max) bin-width) x) 
			 do (add-histo-bin plot-agg x y nil nil line-style filling-style))))
	(when title-position (add-title win :position title-position))
	(plot-windows-finishing win)))))

|#

#|
;; old version
;; GET-PLOT-POINT-LIST Note that X-SEQ and Y-SEQ can be either lists and/or simple-arrays of single-floats. :X-TRACE-OFFSET and
;; :Y-TRACE-OFFSET are applied to the data *after*, and :X-DATA-OFFSET and :Y-DATA-OFFSET are applied to the data *before* log, if
;; enabled.
(defun get-plot-point-list (x-seq y-seq win &key (x-trace-offset 0.0) (y-trace-offset 0.0) (use-timed-data-x-constraints t) only-visible)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let* ((x-start (s-flt (or (gv win :delta-t-start) 0)))
	 (x-trace-offset (s-flt (or x-trace-offset 0.0)))
	 (y-trace-offset (s-flt (or y-trace-offset 0.0)))
	 (margin (the fn (round *GET-PLOT-POINT-LIST-margin*)))
	 (base (the sf (or (gv win :log-base) e-single)))
	 (base-p (gv win :log-base))

	 ;; These are in pixels
	 (y-max-pixels (min (1- (fn-gv win :y-bound-max)) (the fn (+ margin (fn-gv win :height)))))
	 (x-max-pixels (min (1- (fn-gv win :x-bound-max)) (the fn (+ margin (fn-gv win :width)))))
	 (y-min-pixels (max (1+ (fn-gv win :x-bound-min)) (- margin)))
	 (x-min-pixels (max (1+ (fn-gv win :x-bound-min)) (- margin)))

	 ;; These are in data units
	 (x-min (gv win :x-min))
	 (x-max (gv win :x-max))
	 (y-min (gv win :y-min))
	 (y-max (gv win :y-max))
	 (x-log (gv win :x-log)) (y-log (gv win :y-log))
	 (x-data-scale (s-flt (or (gv win :x-data-scale) 1.0)))
	 (y-data-scale (s-flt (or (gv win :y-data-scale) 1.0)))
	 (x-data-offset (s-flt (or (gv win :x-data-offset) 0)))
	 (y-data-offset (s-flt (or (gv win :y-data-offset) 0)))
	 (last-x 0) (last-y 0) (this-x 0) (this-y 0))
    (declare (single-float x-data-offset y-data-offset x-min x-max y-trace-offset x-trace-offset x-data-scale y-data-scale)
	     (fixnum last-x last-y this-x this-y y-max-pixels x-max-pixels y-min-pixels x-min-pixels))
    (cond ((and (listp x-seq) (listp y-seq)) ; For now almost all cases are with both x and y seq as lists
	   (get-plot-point-list-from-lists
	    x-seq x-log x-min x-max x-min-pixels x-max-pixels x-data-offset x-trace-offset x-data-scale
	    use-timed-data-x-constraints
	    y-seq y-log y-min y-max y-min-pixels y-max-pixels y-data-offset y-trace-offset y-data-scale 
	    base base-p win only-visible))
	  ((and (numberp x-seq) (listp y-seq))
	   (let (point-list (delta-x-float (float x-seq)))
	     (do ((x x-start (the sf (+ x delta-x-float))) ; LAST-DELTA-T copies this algorithm.
		  (y-seq y-seq (cdr y-seq)))
		 ((null y-seq) point-list)
	       (let ((offsetted-x (* x-data-scale (+ x-data-offset (s-flt x))))
		     (offsetted-y (* y-data-scale (+ y-data-offset (s-flt (car y-seq))))))
		 (when (or x-log (not use-timed-data-x-constraints)
			   (<= x-min offsetted-x x-max))
		   (setq this-x (x-plot-win-float (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base))) win t)
			 this-y (y-plot-win-float (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base))) win t))))
	       (when (and (not (and (= this-x last-x) (= this-y last-y)))
			  (<= x-min-pixels this-x x-max-pixels)
			  (<= y-min-pixels this-y y-max-pixels))
		 (push this-y point-list)
		 (push this-x point-list)
		 (setq last-x this-x last-y this-y)))
	     point-list))
	  ((and (listp x-seq) (arrayp y-seq))
	   (let (point-list)
	     (do* ((x-seq x-seq (cdr x-seq))
		   (index 0 (1+ index)))
		 ((or (= index (1- (length (the (simple-array sf (*)) y-seq)))) (null x-seq)) point-list)
	       (push (y-plot-win-float (the sf (+ y-trace-offset (the sf (log (aref (the (simple-array sf (*)) y-seq) index))))) win t) point-list)
	       (push (x-plot-win-float (+ x-trace-offset (s-flt (car x-seq))) win t) point-list))
	     point-list))
	  ((and (arrayp x-seq) (listp y-seq))
	   (let (point-list)
	     (do* ((y-seq y-seq (cdr y-seq))
		   (index 0 (1+ index)))
		 ((or (= index (1- (length (the (simple-array sf (*)) x-seq)))) (null y-seq)) point-list)
	       (push (y-plot-win-float (+ y-trace-offset (the sf (log (s-flt (car y-seq))))) win t) point-list)
	       (push (x-plot-win-float (+ x-trace-offset (aref (the (simple-array sf (*)) x-seq) index)) win t) point-list))
	     point-list))
	  (t				; Both sequences are arrays.
	   (let (point-list)
	     (dotimes (index (min (length (the (simple-array sf (*)) y-seq)) (length (the (simple-array sf (*)) x-seq))))
	       (push (y-plot-win-float (+ y-trace-offset (the sf (log (aref (the (simple-array sf (*)) y-seq) index)))) win t) point-list)
	       (push (x-plot-win-float (+ x-trace-offset (aref (the (simple-array sf (*)) x-seq) index)) win t) point-list))
	     point-list)))))
|#

#|
(defun x-plot-win-float-unbounded (x-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float x-dat)
	   (values fixnum))
  (the fn (+ (the fn (g-value win :x-plot-left-gap))
	     (the fn (round (* (/ (- x-dat (the sf (g-value win :xfrmd-x-data-min)))
				  (the sf (g-value win :xfrmd-x-data-mag)))
			       (the fn (g-value win :plot-area-width))))))))
|#


#|
(defun y-plot-win-float-unbounded (y-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float y-dat)
	   (values fixnum))
  (+ (the fn (round (* (the fn (g-value win :plot-area-height))
		       (/ (- (the sf (g-value win :xfrmd-y-data-min)) y-dat)
			  (the sf (g-value win :xfrmd-y-data-mag))))))
     (- (the fn (g-value win :height))
	(the fn (g-value win :y-plot-bottom-gap)))))
|#
#|
(defun get-plotting-point-list (x-seq y-seq win &key (x-trace-offset 0.0) (y-trace-offset 0.0) (x-start 0.0) use-timed-data-x-constraints only-visible)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* (point-list
	 (x-trace-offset (or x-trace-offset 0.0))
	 (y-trace-offset (or y-trace-offset 0.0))
	 (margin (the fn (round *GET-PLOTTING-POINT-LIST-margin*)))
	 (base (the sf (or (g-value win :log-base) e-single)))
	 (base-p (g-value win :log-base))

	 ;; These are in pixels
	 (max-y (min (1- (the fn (g-value win :y-bound-max))) (the fn (+ margin (the fn (g-value win :height))))))
	 (max-x (min (1- (the fn (g-value win :x-bound-max))) (the fn (+ margin (the fn (g-value win :width))))))
	 (min-y (max (1+ (the fn (g-value win :x-bound-min))) (- margin)))
	 (min-x (max (1+ (the fn (g-value win :x-bound-min))) (- margin)))

	 ;; These are in data units
	 (x-min (g-value win :x-min))
	 (x-max (g-value win :x-max))
	 (x-log (g-value win :x-log)) (y-log (g-value win :y-log))
	 (x-data-offset (s-flt (or (g-value win :x-data-offset) 0)))
	 (y-data-offset (s-flt (or (g-value win :y-data-offset) 0)))
	 (last-x 0) (last-y 0) (this-x 0) (this-y 0))
    (declare (single-float x-data-offset y-data-offset x-min x-max
			   y-trace-offset x-trace-offset)
	     (fixnum last-x last-y this-x this-y max-y max-x min-y min-x))
    ;; For now almost all cases are with both x and y seq as lists
    (cond ((and (listp x-seq) (listp y-seq))
	   (do ((x-seq x-seq (cdr x-seq))
		(y-seq y-seq (cdr y-seq)))
	       ((or (null x-seq) (null y-seq)) point-list)
	     (let ((offsetted-x (the sf (+ x-data-offset (the sf (car x-seq)))))
		   (offsetted-y (the sf (+ y-data-offset (the sf (car y-seq))))))
	       (when (or x-log (not use-timed-data-x-constraints) (<= x-min (the sf (car x-seq)) x-max))
		 (setq this-x (x-plot-win-float (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base)))
						win t)
		       this-y (y-plot-win-float (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base)))
						win t)))
	       (when (and (not (and (= this-x last-x) (= this-y last-y)))
			  (<= min-x this-x max-x)
			  (<= min-y this-y max-y))
		 (push this-y point-list)
		 (push this-x point-list)
		 (setq last-x this-x last-y this-y)))))
	  
          ((and (numberp x-seq) (listp y-seq))
           (let ((x-seq-float (float x-seq)))
             (do ((x x-start (the sf (+ x x-seq-float)))
                  (y-seq y-seq (cdr y-seq)))
                 ((null y-seq) point-list)
               (let ((offsetted-x (+ x-data-offset (s-flt x)))
		     (offsetted-y (+ y-data-offset (s-flt (car y-seq)))))
		 (when (or x-log (not use-timed-data-x-constraints)
			   (<= x-min offsetted-x x-max))
		   (setq this-x (x-plot-win-float (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base)))
						  win t)
			 this-y (y-plot-win-float (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base)))
						  win t))))
	       (when (and (not (and (= this-x last-x) (= this-y last-y)))
			  (<= min-x this-x max-x)
			  (<= min-y this-y max-y))
		 (push this-y point-list)
		 (push this-x point-list)
		 (setq last-x this-x last-y this-y)))))
	  ((and (listp x-seq) (arrayp y-seq))
	   (do* ((x-seq x-seq (cdr x-seq))
		 (index 0 (1+ index)))
		((or (= index (1- (length (the (simple-array sf (*)) y-seq)))) (null x-seq)) point-list)
	     (push (y-plot-win-float
		    (the sf (+ y-trace-offset (the sf (log (aref (the (simple-array sf (*)) y-seq) index)))))
		    win t)
		   point-list)
	     (push (x-plot-win-float (+ x-trace-offset (s-flt (car x-seq))) win t) point-list)))
	  ((and (arrayp x-seq) (listp y-seq))
	   (do* ((y-seq y-seq (cdr y-seq))
		 (index 0 (1+ index)))
		((or (= index (1- (length (the (simple-array sf (*)) x-seq)))) (null y-seq)) point-list)
	     (push (y-plot-win-float (+ y-trace-offset (the sf (log (s-flt (car y-seq))))) win t) point-list)
	     (push (x-plot-win-float (+ x-trace-offset (aref (the (simple-array sf (*)) x-seq)
							     index)) win t) point-list)))
	  (t				; Both sequences are arrays.
	   (dotimes (index (min (length (the (simple-array sf (*)) y-seq)) (length (the (simple-array sf (*)) x-seq))))
	     (push (y-plot-win-float (+ y-trace-offset (the sf (log (aref (the (simple-array sf (*))
									       y-seq) index)))) win t)
		   point-list)
	     (push (x-plot-win-float (+ x-trace-offset (aref (the (simple-array sf (*)) x-seq)
							     index)) win t) point-list))))
	  
    point-list))
|#
#|
(defun get-plotting-point-list-from-lists
    (x-seq x-log x-min x-max x-off-win-min-pixels x-off-win-max-pixels
	   x-data-offset x-trace-offset use-timed-data-x-constraints 
	   y-seq y-log y-min y-max y-off-win-min-pixels y-off-win-max-pixels
	   y-data-offset y-trace-offset 
	   base base-p win only-visible)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum x-off-win-min-pixels x-off-win-max-pixels y-off-win-min-pixels y-off-win-max-pixels)
	   (single-float x-trace-offset x-data-offset y-trace-offset y-data-offset base x-min x-max y-min y-max))
  (let* ((last-x-pixels 0) (this-x-pixels 0) (last-y-pixels 0) (this-y-pixels 0)
	 (x-off-plot-min-pixels (x-plot-win-float x-min win))
	 (x-off-plot-max-pixels (x-plot-win-float x-max win))
	 (y-off-plot-min-pixels (y-plot-win-float y-max win))
	 (y-off-plot-max-pixels (y-plot-win-float y-min win))
	 (x-left-border-pixels
	  (if (g-value win :apply-horizontal-borders) x-off-plot-min-pixels x-off-win-min-pixels))
	 (x-right-border-pixels
	  (if (g-value win :apply-horizontal-borders) x-off-plot-max-pixels x-off-win-max-pixels))
	 (y-left-border-pixels
	  (if (g-value win :apply-vertical-borders) y-off-plot-min-pixels y-off-win-min-pixels))
	 (y-right-border-pixels
	  (if (g-value win :apply-vertical-borders) y-off-plot-max-pixels y-off-win-max-pixels))
	 (plot-point-skip-mod (the fn (or (when (numberp (g-value win :plot-point-skip))
					    (1+ (the fn (g-value win :plot-point-skip))))
					  1)))
	 (plot-point-skip-mod-one-p (= plot-point-skip-mod 1))
	 point-list this-point-visible last-point-valid)
    (declare (fixnum last-x-pixels this-x-pixels last-y-pixels this-y-pixels
		     x-off-plot-min-pixels x-off-plot-max-pixels y-off-plot-min-pixels y-off-plot-max-pixels))
    (do ((x-seq x-seq (cdr x-seq))
	 (y-seq y-seq (cdr y-seq))
	 (count 0 (1+ count)))
	((or (null x-seq) (null y-seq)) point-list)
      (declare (fixnum count))
      (when (or plot-point-skip-mod-one-p
		(zerop (mod (the (unsigned-byte 29) count) (the (unsigned-byte 29) plot-point-skip-mod))))
	(let ((offst-x-data-sf (+ x-data-offset (the sf (car x-seq))))
	      (offst-y-data-sf (+ y-data-offset (the sf (car y-seq))))
	      (last-point-visible this-point-visible))
	  (when (or t
		    x-log (not use-timed-data-x-constraints) ; (<= x-min (the sf (car x-seq)) x-max)
		    )
	    (setq this-x-pixels
		  
		  (the fn (x-plot-win-float (+ x-trace-offset (log-or-not offst-x-data-sf x-log base-p base))
					    win t ; (not (g-value win :include-border-points))
					    ))
		  this-y-pixels
		 (the fn (y-plot-win-float (+ y-trace-offset (log-or-not offst-y-data-sf y-log base-p base))
					    win t ; (not (g-value win :include-border-points))
					    ))
	  ))
	  (unless nil ; (and (= this-x-pixels last-x-pixels) (= this-y-pixels last-y-pixels))
	    ;; (when *debug-plot-border-point* (format t "x-data ~A, y-data ~A~%" (car x-seq) (car y-seq)))
	    (if (and (<= x-left-border-pixels this-x-pixels x-right-border-pixels)
		     (<= y-left-border-pixels this-y-pixels y-right-border-pixels))
		(progn
		  (setq this-point-visible t)
		  (unless last-point-visible
		    (when (and (not only-visible) (g-value win :include-border-points) last-point-valid)
		      (multiple-value-bind (border-x-pixels border-y-pixels)
			  (border-point last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
					x-left-border-pixels x-right-border-pixels
					y-left-border-pixels y-right-border-pixels
					t)
			(push nil point-list)
			(push border-y-pixels point-list)
			(push border-x-pixels point-list))))
		  (push this-y-pixels point-list)
		  (push this-x-pixels point-list))
		(progn
		  (setq this-point-visible nil)
		  (when (and last-point-visible (not only-visible) (g-value win :include-border-points))
		    (when *debug-plot-border-point* (format t "x-data ~A, y-data ~A~%" (car x-seq) (car y-seq)))
		    (multiple-value-bind (border-x-pixels border-y-pixels)
			(border-point last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
				      x-left-border-pixels x-right-border-pixels
				      y-left-border-pixels y-right-border-pixels
				      nil)
		      (push border-y-pixels point-list)
		      (push border-x-pixels point-list)
		      (push nil point-list)))))
	    (setq last-x-pixels this-x-pixels last-y-pixels this-y-pixels last-point-valid t)))))
    point-list))
|#
#|
(defmacro generic-plot-function-wrapper (function-name required-args optional-args key-args body)
  `(defun ,function-name ))

(defmacro generic-plot-function-key-args ()
  '(progn
    win title prompt-for-overlay (overlay *overlay-all-plots*)
    (ACCOMODATE-all-overlays *accomodate-all-overlays*)
    (preserve-plot-layout *preserve-plot-layout*)
    invert-y-axis-label y-label-vertical-position
    (x-label-vertical-position :below) (x-label *default-x-label*) (y-label *default-y-label*)
    x-min x-max x-inc x-origin y-min y-max y-inc y-origin x-log y-log log-base
    x-are-fns y-are-fns
    (include-x-tick-at-0 :follow-window)
    (include-y-tick-at-0 :follow-window)
    axes-type (x-origin-tick nil) (y-origin-tick t)
    (x-axis-p t) (y-axis-p t)
    line-styles use-same-line-style
    upper-right-hand-comment comment (comment-position *default-comment-position*)

    use-fixed-top-gap (fixed-top-gap 0)
    use-fixed-bottom-gap (fixed-bottom-gap 0)
    use-fixed-right-gap (fixed-right-gap 0)
    use-fixed-left-gap (fixed-left-gap 0)
    update-fixed-gap-parameters

    left top width height
    (update t) (resurrect t) (visible t)))
				   
|#
#|
(defun label-traces (plot-agg number-of-curves line-styles)
  (let* ((win (g-value plot-agg :window))
	 (max-key-width (max-key-width win number-of-curves)))
    (loop for curve-num from 0 to (1- number-of-curves)
	  when (member curve-num (g-value win :trace-order))
	  collect (nth curve-num (g-value win :label-list)) into labels
	  and collect (get-scatter-symbol win curve-num) into scatter-symbols
	  and collect (if (g-value win :use-same-line-style)
			  (car line-styles)
			  (nth (mod curve-num (length line-styles)) line-styles))
	  into plot-line-styles
	  finally
	  (loop for curve-num from 0
		for label in labels
		for scatter-symbol in scatter-symbols
		for line-style in plot-line-styles
		do
		(add-key-and-label plot-agg curve-num line-style label scatter-symbol max-key-width)))))
|#
#|
(defmacro plot-interactor-wrapper (interactor body)
  `(let ((window (first-interactor-window ,interactor))
	 *automatic-run*)
     (unless (INTERACTORS-RUNNING window)
       (s-value window :window-menu-interactor-running t)
       (when (consp window) (loop for win in window when (g-value win :mode) do (setq window win)))
       ,body
       (when (opal-obj-exists window) (s-value window :window-menu-interactor-running nil)))))
|#

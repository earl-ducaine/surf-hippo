
(unless (find-package 'PLOT-HACK-NEW)
  (make-package 'PLOT-HACK-NEW :use '(ph)))

(in-package 'PLOT-HACK-new

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                         ************* PLOT-TIMED-DATA *************
;;
;; DATA-LISTS are one or more sets of Y data point sequences; a single set of lists is for adding new data to a window
;; (which may have data already), and multiple sets are for creating overlaid plots from scratch. In the former case,
;; the format is a list of sequences: '((ydata1-seq) (ydata2-seq)...), where each sequence may be a list or an array of
;; Y points. In the latter case, the format is a list of lists of lists: '(((ydata1A-list) (ydata2A-list))
;; ((ydata1B-list) (ydata2B-list))...). In this case the data points must all be in lists. Note that this second format
;; is the way that the y data is stored in the :y-lists slot of the plotting window. The format for the optional
;; TIME-BASE argument is the same, except that it is possible for there to be only one X data point list corresponding
;; to a set of Y data point lists; this may occur, for example, when there is a common time base for a set of Y
;; data. LABEL-LIST is an optional list of labels, the number of labels typically corresponding to the number of Y data
;; point sequences is the first set in the DATA-LISTS list of sequences of Y data point sequences.  If TIME-BASE is NIL,
;; then the time base is inferred from the DELTA-T argument (default 1.0). If LABEL-LIST is NIL, AND the target window
;; has no label-list, AND :LABEL-TRACES is t (default) then labels of the form "CANONIC-LABEL-i" are used, where i
;; ranges from 1 to the number of data lists, and CANONIC-LABEL is taken from the argument of the same name (default
;; "DATA"). The ACCOMODATE-OVERLAYS argument causes the plot dimensions to change in order to accomodate subsequent
;; overlaid data.  The data and time points *must* be single floats!!

;; :X-TRACE-OFFSET and :Y-TRACE-OFFSET are applied to the data *after*, and :X-DATA-OFFSET and :Y-DATA-OFFSET are
;; applied to the data *before* log (when :X-LOG and/or :Y-LOG is set).

;; :AXES-TYPE can be either NIL (axes type unchanged for plotting to an old window, with the default :STANDARD) :SIMPLE,
;; :STANDARD or :NONE. You can also enable X or Y axes with the :X-AXIS-P and :Y-AXIS-P arguments (both default to T).
 
;; When the :AUTO-WF-SETUP argument is non-NIL, then a waterfall plot will be made with the traces stacked vertically
;; (spaced by the largest amplitude trace), the trace labels centered vertically to the right of each trace, and the
;; simple axes set in the lower left corner.

;; If the :ERASE-DATA-AFTER-PLOT key argument is T (default NIL), then data that is normally stored in a (standard) plot
;; window is erased after the plot is done, but not the displayed image. This may be useful if there are a lot of plots
;; and memory is getting tight. While a window whose data has been erased may be printed and examined with the cross
;; hairs, zooming, unzooming, restoring, or other rescaling will not be possible.

(defvar *plot-timed-data-default-key-values*
  `((:delta-t-start 0.0) (:timed-data t)
    (:overlay *overlay-plots*) (:ACCOMODATE-all-overlays *accomodate-all-overlays*) (:preserve-plot-layout *preserve-plot-layout*)
    (:update-window t) (:resurrect-window t) (:visible t)
    (:CANONIC-LABEL "DATA")
    (:x-label *default-x-label*) (:y-label *default-y-label*) (:y-label-horizontal-position :left)
    (:default-number-of-x-incs 5) (:default-number-of-y-incs 5) ; If no explicit value for axis increments
    (:x-axis-p t) (:y-axis-p t) (:simple-axis-x-value-p t) (:simple-axis-y-value-p t)
    (:x-origin-tick nil) (:y-origin-tick t)
    (:include-x-tick-at-0 :follow-window) (:include-y-tick-at-0 :follow-window)
    (:stipple-percent 100)
    (:comment-position *default-comment-position*)
;    (:fixed-top-gap 0) (:fixed-bottom-gap 0) (:fixed-right-gap 0) (:fixed-left-gap 0)
    (:draw-grid *default-plot-grid-p*)
    (:label-traces *label-plot-traces*) 
;    (:x-data-offset 0.0) (:y-data-offset 0.0) (:x-trace-offset 0.0) (:y-trace-offset 0.0)
    (:connect-data-points t) 
    (:scatter-symbol-borderp t) (:fill-scatter t)
    (:x-symbol-width *default-scatter-size*) (:y-symbol-width *default-scatter-size*)
    (:save-markers t)
    (:waterfall-trace-label-skip 0) (:waterfall-label-offset 0.0)))

(defun search-for-key-value (key key-value-pairs)
  (let ((key-value (find key key-value-pairs :test 'eq :key 'car)))
    (values key-value (true-p key-value))))

(defun find-value-after-key (key key-value-list)
  (let ((key-found (position key key-value-list)))
    (values (when key-found (nth (1+ key-found) key-value-list))
	    (true-p key-found))))
  
(export '(find-value-after-key))

(defun get-plot-timed-data-window (keywords-and-values)
  (let* ((supplied-win (or (find-value-after-key :window keywords-and-values) (find-value-after-key :win keywords-and-values)))
	 (plot-type (cond ((find-value-after-key :waterfall keywords-and-values) :waterfall)
			  ((find-value-after-key :polar keywords-and-values) :polar)
			  (supplied-win (gv supplied-win :plot-type))
			  (t :xy)))
	 (preserve-plot-layout (find-value-after-key :preserve-plot-layout keywords-and-values))
	 (accomodate-all-overlays (find-value-after-key :accomodate-all-overlays keywords-and-values)))
    (get-plot-window plot-type
		     (find-value-after-key :data-type keywords-and-values)
		     (find-value-after-key :overlay keywords-and-values)
		     :default-win supplied-win
		     :preserve-window-dims (find-value-after-key :preserve-window-dimensions keywords-and-values)
		     :title (format nil "~A" (or (find-value-after-key :title keywords-and-values) (and supplied-win (g-value supplied-win :title)) "Data Plot"))
		     :prompt-for-overlay (find-value-after-key :prompt-for-overlay keywords-and-values)
		     :session-name (find-value-after-key :session-name keywords-and-values)
		     :save-markers (find-value-after-key :save-markers keywords-and-values)
		     :preserve-plot-layout preserve-plot-layout
		     :accomodate-all-overlays accomodate-all-overlays)))

(defun add-plot-timed-data-keyword-args-and-default-values (win keywords-and-values)
  ;; First add the default values
  (mapcar #'(lambda (key-default) (s-value win (car key-default) (typecase (cadr key-default) (symbol (symbol-value (cadr key-default))) (t (cadr key-default)))))
	  *plot-timed-data-default-key-values*)
  ;; Now transfer all specified keyword value pairs into slots of the WIN
  (do ((keywords-and-values keywords-and-values (cddr keywords-and-values)))
      ((null keywords-and-values))
    (case (car keywords-and-values)
      (:title nil)			; Pick out any keyword args that we don't want to use directly as slots
      (t (s-value win (car keywords-and-values) (cadr keywords-and-values))))))

(defun go-ahead-with-plot-timed-data (win)
  (and (not (gv win :data-erased)) (or (gv win :data-lists) (gv win :y-lists) (gv win :replot-win-point-list) (gv win :restore-plot) (gv win :unzoom) (gv win :revise-plot))))

(defun store-prior-slot-values-for-plot-timed-data (win)
  (s-values win				; Store prior values of various slots
	    (x-min-prior (gv win :x-min))
	    (x-max-prior (gv win :x-max))
	    (y-min-prior (gv win :y-min))
	    (y-max-prior (gv win :y-max))
	    (prior-flat-y-data (gv win :flat-y-data))
	    (prior-flat-x-data (gv win :flat-x-data))))

(defun plot-timed-data-new (data-lists &optional label-list (time-base 1.0) &rest keywords-and-values)
  (let* ((supplied-win (or (find-value-after-key :window keywords-and-values) (find-value-after-key :win keywords-and-values)))
	 (preserve-plot-layout (find-value-after-key :preserve-plot-layout keywords-and-values))
	 (win (get-plot-timed-data-window keywords-and-values)))
    (s-value win :new-win-p (not (eq win supplied-win)))
    (store-prior-slot-values-for-plot-timed-data win)
    (unless preserve-plot-layout (s-values-nil win x-axis-min x-axis-max y-axis-min y-axis-max)) ; Clear some slot values from an existing window.
    (s-values win data-lists label-list time-base) ; Load in required and optional arguments
    (ADD-PLOT-TIMED-DATA-KEYWORD-ARGS-AND-DEFAULT-VALUES win keywords-and-values) ; Load in remaining "keyword" rest arguments.
    
    (when (go-ahead-with-plot-timed-data win)
      (when (gv win :line-styles) (s-value win :plot-line-style-family (gv win :line-styles)))
      (when (or (gv win :new-win-p) (not (gv win :preserve-window-attributes)))
	(s-value win :bin-width (s-flt (or (gv win :bin-width) 0)))
	(when label-list (s-value win :label-list (coerce-to-list label-list))))
      (add-plot-timed-data-comments win)
      (set-waterfall-and-polar-slots win)
      (if (gv win :replot-win-point-list)
	  (s-values-nil win restore-plot unzoom) ; just to be safe.
	  (when (gv win :restore-plot) (s-value win :unzoom nil)))
      (if (or (gv win :replot-win-point-list) (gv win :restore-plot) (gv win :unzoom) (gv win :revise-plot))
	  (s-value win :overlay nil)
	  (progn			; Otherwise new plot, maybe overlay.
	    ;; For the Y values, the precedence is :y-lists argument > data-lists
	    (when (gv win :data-lists) (load-y-lists win (gv win :data-lists)))
	    ;; For the X values, the precedence is :x-lists argument > time-base > delta-t
	    (unless (or (and (not (gv win :data-lists)) (gv win :x-lists)) (sequencep (gv win :time-base)))
	      (s-value win :delta-t (s-flt (s-value win :time-base (or (gv win :time-base) 1.0)))))
	    (when (and (not (gv win :x-lists)) (gv win :time-base)) (load-x-lists win (gv win :time-base)))))
      (add-traces-and-finish-up-plot-timed-data-window win))))

(defun add-plot-timed-data-comments (win)
  (when (gv win :upper-right-hand-comment) (add-temp-comment win (gv win :upper-right-hand-comment)))
  (let ((comment (gv win :comment))
	(comment-position (gv win :comment-position) win))
    (if (and comment (eq (or *global-plot-comment-position* *default-comment-position*) comment-position))
	(let ((*global-plot-comment* (if (zerop (length *global-plot-comment*)) comment (concatenate 'string *global-plot-comment* (format nil "~%") comment))))
	  (add-global-plot-comment win))
	(progn (when comment (add-comment win comment :position comment-position))
	       (add-global-plot-comment win)))))

(defun set-waterfall-and-polar-slots (win)
  (case (gv win :plot-type)
    ((:xy :waterfall) (s-value win :polar nil))
    (:polar (s-value win :polar t)))
  (when (and (eq (gv win :plot-type) :waterfall)
	     (or (eq (gv win :waterfall) :auto) (gv win :auto-wf-setup) (gv win :auto-setup)))
    (s-values win
	      (auto-wf-setup (not (or (gv win :replot-win-point-list) (gv win  :restore-plot) (gv win :unzoom))))
	      (label-waterfall t)
	      (waterfall t)
	      (wf-skirt t)))
  (when (eq :waterfall (gv win :plot-type))
    (s-value win :waterfall (cond ((gv win :auto-wf-setup) :auto)
				  ((gv win :waterfall) t))))
  (s-value win :use-same-line-style (or (gv win :waterfall) (gv win :use-same-line-style))))

(defun add-traces-and-finish-up-plot-timed-data-window (win)
  (let ((num-curves-per-group (length (car (gv win :y-lists)))))
    (unless (= (length (gv win :label-list)) num-curves-per-group) (s-value win :label-list (get-canonic-labels num-curves-per-group (gv win :canonic-label))))
    (clear-up-label-list win (gv win :label-list))
    (let* ((old-plot-agg (car (find-plot-agg win 'data-plot)))
	   (renew-plot-agg-p (renew-plot-agg-p win old-plot-agg))
	   (plot-agg (if renew-plot-agg-p (add-plot-agg win 'data-plot) old-plot-agg))
	   (line-styles (parse-line-styles-for-plot (gv win :line-styles) win (gv win :waterfall) (gv win :use-same-line-style) num-curves-per-group))
	   (trace-references (fix-list (or (no-nils (gv win :trace-order)) (list-of-nums num-curves-per-group)))))
      (when (run-setup-plot-p win) (setup-plot-timed-data win))
      (loop for data-group from 0 
	    for data-group-y-lists in (gv win :y-lists)
	    for data-group-x-lists in (gv win :x-lists) do
	    (loop for trace-reference in trace-references
		  for curve-num from 0 do
		  (let* ((curve-num-*-x-offset (* curve-num (gv win :x-trace-offset)))
			 (curve-num-*-y-offset (* curve-num (gv win :y-trace-offset)))
			 (y-list (nth trace-reference data-group-y-lists))
			 (x-ref (time-base-from-data-group-x-lists data-group-x-lists trace-reference))
			 (label (nth trace-reference (gv win :label-list)))
			 (line-style (if (consp line-styles) (nth (mod curve-num (length line-styles)) line-styles) line-styles))
			 (wf-x-offset (when (gv win :waterfall) (+ (gv win :waterfall-base-x-offset) curve-num-*-x-offset)))
			 (wf-y-offset (when (gv win :waterfall) (+ (gv win :waterfall-base-y-offset) curve-num-*-y-offset)))
			 (points (get-plot-point-list x-ref y-list win :x-trace-offset wf-x-offset :y-trace-offset wf-y-offset :only-visible t)))
		    (cond-every
		     ((and points (or (gv win :waterfall) (gv win :connect-data-points)))
		      (if (gv win :use-bins)
			  (add-histo-bins plot-agg points (when (gv win :waterfall) (+ (gv win :waterfall-base-y-offset) curve-num-*-y-offset)))
			  (new-add-polyline-to-plot plot-agg points line-style (gv win :polar))))
		     ((and points (gv win :waterfall) (gv win :wf-skirt)) (add-wf-skirt plot-agg win points)) ; Blanked area under curve for waterfall plots.
		     ((and points (gv win :scatter)) (add-scatter-points-to-plot plot-agg win points line-style (get-scatter-symbol win curve-num) :data-points curve-num))
		     ((gv win :linear-regression) (plot-linear-regression x-ref y-list win label line-style))
		     ((and (= data-group 0) label (gv win :waterfall) (gv win :label-waterfall)
			   (= 0 (mod curve-num (1+ (fn-gv win :waterfall-trace-label-skip)))))
		      (add-waterfall-label plot-agg label curve-num-*-x-offset curve-num-*-y-offset))))))
      (cond-every
       ((gv win :label-traces) (label-traces plot-agg num-curves-per-group line-styles (relevant-labels win num-curves-per-group)))
       ((gv win :erase-data-after-plot) (erase-plot-data win))
       ((gv win :odd-quadrant-diagonal) (mark-plot-odd-quadrant-diagonal win))
       ((gv win :even-quadrant-diagonal) (mark-plot-even-quadrant-diagonal win)))
      (plot-windows-finishing win
			      (cond ((gv win :resurrect-window) :resurrect)
				    ((gv win :update-window) :update))
			      (when renew-plot-agg-p old-plot-agg)))))

#|

Slot definitions:

:x-max      The maximum displayed actual x value of the data, considering log if necessary
:x-data-max The maximum x value over all the data, prior to log
:x-axis-max  The maximum displayed x axis value
:x-origin   The Y intercept on the X axis
:x-inc      The increment between tick marks on the X axis
:x-max-spec  The maximum displayed x value of the data specified by the function arguments
:x-max-prior The original maximum displayed x value of the data, when replotting to a window


|#

(defun setup-plot-timed-data (win)
  (let* ((y-seqs (parse-plot-y-seqs win))
	 (x-seqs (car (gv win :x-lists)))
	 (accomodate-all (and (> (number-of-overlays win) 1) (gv win :overlay) (gv win :accomodate-all-overlays)))) ; Only OK for windows that have data.
    (s-values win
	      (number-of-traces (length (or (gv win :trace-order) y-seqs)))
	      (replotting (or (gv win :replot-win-point-list) (gv win :restore-plot) (gv win :unzoom)))
	      (consider-labels (and (gv win :label-traces) (not (gv win :waterfall))))
	      (x-are-fns (or *force-plot-x-fixnums* (gv win :x-are-fns)))
	      (y-are-fns (or *force-plot-y-fixnums* (gv win :y-are-fns)))
	      (x-label (when (gv win :x-label) (if (stringp (gv win :x-label)) (gv win :x-label) (format nil "~A" (gv win :x-label)))))
	      (y-label (when (gv win :y-label) (if (stringp (gv win :y-label)) (gv win :y-label) (format nil "~A" (gv win :y-label))))))
    
    (s-values-s-flt win			; Establish or clean up some numerical slots.
		    (delta-t-start (gv win :delta-t-start))
		    (data-length (apply 'max (mapcar 'length y-seqs)))
		    (log-base (or (gv win :log-base) e))
		    (waterfall-base-x-offset (or (gv win :waterfall-base-x-offset) 0.0))
		    (waterfall-base-y-offset (or (gv win :waterfall-base-y-offset) 0.0))
		    (x-trace-offset (or (gv win :x-trace-offset) 0))
		    (y-trace-offset (or (gv win :y-trace-offset) 0))
		    (x-data-offset (if (gv win :waterfall) (gv win :waterfall-base-x-offset) (or (gv win :x-data-offset) 0)))
		    (y-data-offset (if (gv win :waterfall) (gv win :waterfall-base-y-offset) (or (gv win :y-data-offset) 0)))
		    (x-axis-number-coefficient (or (gv win :x-axis-number-coefficient) 1))
		    (y-axis-number-coefficient (or (gv win :y-axis-number-coefficient) 1)))

    (cond-every
     ((or (gv win :auto-wf-setup) (gv win :auto-x-scaling)) (s-values-nil win x-inc x-max x-min x-origin))
     ((or (gv win :auto-wf-setup) (gv win :auto-y-scaling)) (s-values-nil win y-inc y-max y-min y-origin))
     ((gv win :auto-wf-setup) (s-values win
					(y-plot-top-gap-extra *default-y-plot-top-gap-extra-waterfall*)
					(waterfall-base-x-offset 0.0) (waterfall-base-y-offset 0.0)
					(x-data-scale 1.0) (y-data-scale 1.0)
					(x-data-offset 0.0) (y-data-offset 0.0))))
    (s-values-if-non-nil win
			 (x-min-spec (gv win :x-min))
			 (x-max-spec (gv win :x-max))
			 (y-min-spec (gv win :y-min))
			 (y-max-spec (gv win :y-max)))
    (when (< (x-data-min win) 0) (s-value win :timed-data nil))
    (when (and (gv win :timed-data) (not (gv win :x-log))) ; May change :x-max-spec and :display-sub-domain. This is to make the end of the time a little neater.
      (let ((plot-window-top-x (plot-window-top-x win))) ; References :delta-t-start
	(s-values win
		  (x-max-spec (or (gv win :x-max-spec) plot-window-top-x))
		  (display-sub-domain (< (gv win :x-max-spec) plot-window-top-x)))))
					
    (s-values win			;  In case of a REPLOT-WIN-POINT-LIST, RESTORE-PLOT, or UNZOOM, the X/Y specified max/mins must be set.
	      (X-MAX-MIN-SPECIFIED (true-p (or (gv win :x-max-spec) (gv win :x-min-spec))))
	      (Y-MAX-MIN-SPECIFIED (true-p (or (gv win :y-max-spec) (gv win :y-min-spec)))))
    (cond ((gv win :replot-win-point-list) (set-plot-timed-data-replot-mins-maxs win)) ; Converted mouse coordinates (left top width height) to data coordinates.
	  ((or (gv win :restore-plot) (gv win :unzoom)) (set-plot-timed-data-restore-mins-maxs-origins win)))
    ;; If X-MIN-SPEC is not specified and the minimum value over the x-sequences is 0, then fix X-MIN-SPEC = 0.
    (unless (gv win :x-min-spec) (if (numberp x-seqs)
				     (s-value win :x-min-spec (or (gv win :delta-t-start) 0))
				     (when (= (a-bit-less x-seqs) 0) (s-value win :x-min-spec 0.0))))
    (loop do
	  (s-values win
		    (x-data-max (x-data-max win))
		    (x-data-min (x-data-min win))
		    (x-overall-data-min (x-data-min win t))			
		    (y-data-max (y-data-max win (gv win :display-sub-domain) (gv win :x-min-spec) (gv win :x-max-spec)))
		    (y-data-min (y-data-min win (gv win :display-sub-domain) (gv win :x-min-spec) (gv win :x-max-spec)))
		    (y-overall-data-min (y-data-min win (gv win :display-sub-domain) (gv win :x-min-spec) (gv win :x-max-spec) t)))
	  (s-value win :display-sub-domain (or (gv win :display-sub-domain) (and (gv win :x-min-spec) (> (gv win :x-min-spec) (gv win :x-data-min)))))
	  (when (gv win :auto-wf-setup)
	    (when *waterfall-fixed-y-max* (s-value win :y-data-max *waterfall-fixed-y-max*))
	    (when *waterfall-fixed-y-min* (s-value win :y-data-min *waterfall-fixed-y-min*))
	    (s-values win
		      (x-trace-offset (* (- 1.0 (bound-val (or (gv win :auto-waterfall-x-trace-overlap) *auto-waterfall-x-trace-overlap*) 1.0 0.0))
					 (- (gv win :x-data-max) (gv win :x-data-min))))
		      (y-trace-offset (* (- 1.0 (bound-val (or (gv win :auto-waterfall-y-trace-overlap) *auto-waterfall-y-trace-overlap*) 1.0 0.0))
					 (- (gv win :y-data-max) (gv win :y-data-min))))))
	  until (check-plot-log-parameters win))
  
    (s-values win
	      (xfrmd-x-data-max (if (gv win :x-log) (log (gv win :x-data-max) (gv win :log-base)) (gv win :x-data-max)))
	      (xfrmd-x-data-min (if (gv win :x-log) (log (gv win :x-data-min) (gv win :log-base)) (gv win :x-data-min)))
	      (xfrmd-x-data-mag (abs (- (gv win :xfrmd-x-data-max) (gv win :xfrmd-x-data-min))))
	      (a-bit-more-than-the-x (+ (if accomodate-all (max (gv win :x-axis-max) (gv win :xfrmd-x-data-max)) (gv win :xfrmd-x-data-max))
					(if (gv win :timed-data) 0.0 (* *plot-data-a-bit-more-than-the-x-or-y-coeff* (gv win :xfrmd-X-data-mag)))))
	      (a-bit-less-than-the-x (- (if accomodate-all (min (gv win :x-axis-min) (gv win :xfrmd-x-data-min)) (gv win :xfrmd-x-data-min))
					(if (gv win :timed-data) 0.0 (* *plot-data-a-bit-more-than-the-x-or-y-coeff* (gv win :xfrmd-X-data-mag)))))
	      (x-axis-max (if (and (not (gv win :waterfall)) (gv win :x-max-spec))
			      (gv win :x-max-spec)
			      (if (gv win :x-are-fns) (ceiling (gv win :a-bit-more-than-the-x)) (gv win :a-bit-more-than-the-x))))
	      (x-axis-min (if (and (not (gv win :waterfall)) (gv win :x-min-spec))
			      (gv win :x-min-spec)
			      (if (and (not (gv win :x-log)) (or (numberp x-seqs) (gv win :timed-data)))
				  (or (gv win :delta-t-start) 0)
				  (if (gv win :x-are-fns) (floor (gv win :a-bit-less-than-the-x)) (gv win :a-bit-less-than-the-x)))))
	      (xfrmd-y-data-max (if (gv win :y-log) (log (gv win :y-data-max) (gv win :log-base)) (gv win :y-data-max)))
	      (xfrmd-y-data-min (if (gv win :y-log) (log (gv win :y-data-min) (gv win :log-base)) (gv win :y-data-min)))
	      (xfrmd-y-data-mag (abs (- (gv win :xfrmd-y-data-max) (gv win :xfrmd-y-data-min))))
	      (a-bit-more-than-the-y (+ (if accomodate-all
					    (max (gv win :xfrmd-y-data-max) (if (gv win :prior-flat-y-data) (gv win :prior-y-data-max) (gv win :y-axis-max)))
					    (gv win :xfrmd-y-data-max))
					(* *plot-data-a-bit-more-than-the-x-or-y-coeff* (gv win :xfrmd-y-data-mag))))
	      (a-bit-less-than-the-y (- (if accomodate-all
					    (min (gv win :xfrmd-y-data-min) (if (gv win :prior-flat-y-data) (gv win :prior-y-data-min) (gv win :y-axis-min)))
					    (gv win :xfrmd-y-data-min))
					(* *plot-data-a-bit-more-than-the-x-or-y-coeff* (gv win :xfrmd-y-data-mag))))
	      (y-axis-max (cond ((and (gv win :fix-to-unity-mag-if-so) (and (= 1.0 (gv win :xfrmd-y-data-mag)) (= 1.0 (gv win :xfrmd-y-data-max)))) 1.0)
				((and (not (gv win :waterfall)) (gv win :y-max-spec)) (gv win :y-max-spec))
				(t (if (gv win :y-are-fns) (ceiling (gv win :a-bit-more-than-the-y)) (gv win :a-bit-more-than-the-y)))))
	      (y-axis-min (cond ((and (gv win :fix-to-unity-mag-if-so) (and (= 1.0 (gv win :xfrmd-y-data-mag)) (= 0.0 (gv win :xfrmd-y-data-min)))) 0.0)
				((and (not (gv win :waterfall)) (gv win :y-min-spec)) (gv win :y-min-spec))
				(t (if (gv win :y-are-fns) (floor (gv win :a-bit-less-than-the-y)) (gv win :a-bit-less-than-the-y))))))
    (s-values win
	      (x-max (setup-plot-timed-data-xy-extrema win :x t))
	      (x-min (setup-plot-timed-data-xy-extrema win :x nil))
	      (y-max (setup-plot-timed-data-xy-extrema win :y t))
	      (y-min (setup-plot-timed-data-xy-extrema win :y nil)))
    (when (gv win :waterfall)
      (unless (eq (gv win :axes-type) :none) (s-value win :axes-type :simple))
      (if (> (gv win :y-trace-offset) 0)
	  (s-value win :y-axis-max (gv win :y-max))
	  (when (< (gv win :y-trace-offset) 0) (s-value win :y-axis-min (gv win :y-min))))
      (if (> (gv win :x-trace-offset) 0)
	  (s-value win :x-axis-max (gv win :x-max))
	  (when (< (gv win :x-trace-offset) 0) (s-value win :x-axis-min (gv win :x-min)))))

    ;; Set :X-MIN, :X-MAX, :X-MAG, :X-MIN-LIMIT, :X-MAX-LIMIT, :X-AXIS-MIN, :X-AXIS-MAX, and as well for Y
    ;; Note that :X-MAX, :X-MIN, :Y-MAX and :Y-MIN are eventually set by the corresponding :*-AXIS-* values. The appropriate values as supplied
    ;; by the function args are only changed when there is flat data. In addition, unless :*-MAX-MIN-SPECIFIED, all of these values are cleaned up with
    ;; GET-NICE-MAG.:X-MIN, :X-MAX, :Y-MIN, :Y-MAX now track :X-AXIS-MIN, :X-AXIS-MAX, :Y-AXIS-MIN, :Y-AXIS-MAX, respectively.
    (when (s-value win :flat-y-data (= (gv win :y-max) (gv win :y-min)))
      (let ((y-actual-max (gv win :y-max))
	    (zero-y (= (gv win :y-max) 0))
	    (positive-y (> (gv win :y-max) 0)))
	(s-values win
		  (y-max (if zero-y 1.0 (* y-actual-max (+ 1 (if positive-y 0.5 -0.5)))))
		  (y-min (if zero-y -1.0 (* y-actual-max (+ 1 (if positive-y -0.5 0.5)))))
		  (y-axis-max (gv win :y-max))
		  (y-axis-min (gv win :y-min)))
	(if (not zero-y)
	    (s-value win :y-origin (if positive-y (gv win :y-min) (gv win :y-max)))
	    (cond ((> *plotted-constant-value-threshold-for-shifting-origin* y-actual-max 0)
		   (s-value win :y-origin (or (gv win :y-origin) (- *plotted-constant-value-threshold-for-shifting-origin*))))
		  ((< (- *plotted-constant-value-threshold-for-shifting-origin*) y-actual-max 0)
		   (s-value win :y-origin (or (gv win :y-origin) *plotted-constant-value-threshold-for-shifting-origin*)))))))
    (when (s-value win :flat-x-data (= (gv win :x-max) (gv win :x-min)))
      (s-values win
		(x-max (+ 1.0 (gv win :x-max)))
		(x-min (- (gv win :x-min) 1.0))
		(x-axis-max (gv win :x-max))
		(x-axis-min (gv win :x-min))))
    (s-values win
	      (y-axis-max (max-of-only-nums (gv win :y-origin) (get-nice-mag (gv win :y-axis-max) (if (gv win :Y-MAX-MIN-SPECIFIED) 0 (- (gv win :y-max) (gv win :y-min))))))
	      (y-axis-min (min-of-only-nums (gv win :y-origin) (get-nice-mag (gv win :y-axis-min) (if (gv win :Y-MAX-MIN-SPECIFIED) 0 (- (gv win :y-max) (gv win :y-min))))))
	      (x-axis-max (max-of-only-nums (gv win :x-origin) (get-nice-mag (gv win :x-axis-max) (if (gv win :X-MAX-MIN-SPECIFIED) 0 (- (gv win :x-max) (gv win :x-min))))))
	      (x-axis-min (min-of-only-nums (gv win :x-origin) (get-nice-mag (gv win :x-axis-min) (if (gv win :X-MAX-MIN-SPECIFIED) 0 (- (gv win :x-max) (gv win :x-min))))))
	      (y-max (gv win :y-axis-max)) (y-min (gv win :y-axis-min))
	      (x-max (gv win :x-axis-max)) (x-min (gv win :x-axis-min))
	      (y-mag (- (gv win :y-max) (gv win :y-min)))
	      (y-max-limit (+ (gv win :y-axis-max) (gv win :y-mag)))
	      (y-min-limit (- (gv win :y-axis-min) (gv win :y-mag)))
	      (x-mag (- (gv win :x-max) (gv win :x-min)))
	      (x-max-limit (+ (gv win :x-axis-max) (gv win :x-mag)))
	      (x-min-limit (- (gv win :x-axis-min) (gv win :x-mag))))

    (set-plot-timed-data-incs-origin-label-position win)
    (unless (or (gv win :replotting) (gv win :revise-plot)) (set-plot-win-orig-parameters win)) ; This is a first time plot.
    (set-plot-gaps win)
    (draw-all-axes win)
    (s-values-nil win auto-x-scaling auto-y-scaling)
    (when (or (<= (gv win :x-max) (gv win :x-min))
	      (<= (gv win :y-max) (gv win :y-min)))
      (sim-error (format nil "Plot window ~A was specified with bogus ~A" (gv win :title)
			 (cond ((and (<= (gv win :x-max) (gv win :x-min)) (<= (gv win :y-max) (gv win :y-min)))
				(format nil "X/Y max (~a/~a) min (~A/~a) values.~%" (gv win :x-max) (gv win :y-max) (gv win :x-min) (gv win :y-min)))
			       ((<= (gv win :x-max) (gv win :x-min)) (format nil "X max (~a) min (~a) values.~%" (gv win :x-max) (gv win :x-min)))
			       (T (format nil "Y max (~a) min (~a) values.~%" (gv win :Y-max) (gv win :Y-min)))))))
    (when (gv win :polar) (setup-polar-plot win))
    (s-values win (has-been-setup t))))

(defun setup-polar-plot (win)	
  (let ((axis-inc (max (or (gv win :x-inc) 0) (or (gv win :y-inc) 0))))
    (s-values win
	      (y-inc axis-inc) (x-inc axis-inc)
	      (width (max (gv win :width) (gv win :height)))
	      (height (gv win :width))
	      (x-origin-tick nil) (y-origin-tick nil)
	      (x-label "")
	      (x-max (if (and (gv win :x-max) (gv win :y-max))
			 (max (gv win :x-max) (gv win :y-max))
			 (or (gv win :y-max) (gv win :x-max))))
	      (y-max (gv win :x-max))
	      (y-min (- (gv win :x-max)))
	      (x-min (- (gv win :x-max))))))

(defun setup-plot-timed-data-xy-extrema (win axis maxp)
  (+ (if maxp
	 (case axis
	   (:y (if (and (gv win :waterfall) (gv win :waterfall-y-data-max) (gv win :use-waterfall-y-data-max))
		   (gv win :waterfall-y-data-max)
		   (gv win :y-axis-max)))
	   (:x (gv win :x-axis-max)))
	 (gv-generic-to-axis win axis 'axis-min))
     (if (gv win :waterfall) (* (gv-generic-to-axis win axis 'trace-offset) (1- (gv win :number-of-traces))) 0.0)))

(defun set-plot-timed-data-incs-origin-label-position (win)
  (cond-every ((and (not (gv win :x-inc)) (gv win :x-are-fns)) (s-value win :x-inc (adjust-axis-min-max-for-ints win :x)))
	      ((and (not (gv win :y-inc)) (gv win :y-are-fns)) (s-value win :y-inc (adjust-axis-min-max-for-ints win :y))))
  (s-values win
	    (x-origin (calculate-x-origin win))
	    (y-origin (calculate-y-origin win))
	    (y-inc (calculate-y-inc win))
	    (x-inc (calculate-x-inc win))
	    (x-label-vertical-position (or (gv win :x-label-vertical-position)
					   (if (> (- (gv win :y-axis-max) (gv win :y-origin))
						  (- (gv win :y-origin) (gv win :y-axis-min)))
					       :below :above)))))

(defun calculate-x-origin (win)
  (s-flt (or (gv win :x-origin)
	     (get-nice-mag (bound-val 0.0 (gv win :x-axis-max) (gv win :x-axis-min)) (- (gv win :x-axis-max) (gv win :x-axis-min))))))

(defun calculate-y-origin (win)
  (s-flt (or (gv win :y-origin)
	     (if (gv win :TIMED-DATA)
		 (progn
		   (when (and (not (gv win :y-axis-root)) (> (gv win :y-axis-max) 0) (< (gv win :y-axis-min) 0))
		     (s-value win :y-axis-root 0.0))
		   (if (> (abs (gv win :y-axis-max)) (abs (gv win :y-axis-min)))
		       (gv win :y-axis-min) (gv win :y-axis-max)))
		 (get-nice-mag (bound-val 0.0 (gv win :y-axis-max) (gv win :y-axis-min)) (- (gv win :y-axis-max) (gv win :y-axis-min)))))))

(defun calculate-y-inc (win)
  ;; Avoid small y-incs which do not advance axis tick positions in draw-full-cartesian-axes
  (let ((float-inc (max (case (gv win :mode)
			  (:histogram 1)
			  (t (abs (* .01 (gv win :y-mag)))))
			(if (and (gv win :y-inc) (not (= 0 (gv win :y-inc))))
			    (float (gv win :y-inc))
			    (/ (get-nice-mag
				(if (gv win :waterfall)
				    (gv win :xfrmd-y-data-mag)
				    (- (gv win :y-axis-max) (gv win :y-axis-min))))
			       (case (gv win :plot-type)
				 (polar 4.0)
				 (:waterfall 3.0)
				 (t (or (gv win :default-number-of-y-incs) 5.0))))))))
    (s-flt (if (gv win :y-are-fns) (max 1 (round float-inc)) float-inc))))

(defun calculate-x-inc (win)
  (let ((float-inc (case (gv win :plot-type)
		     (:polar (gv win :y-inc))
		     (t (if (and (gv win :x-inc) (not (= 0 (gv win :x-inc))))
			    (float (gv win :x-inc))
			    (/ (get-nice-mag
				(if (gv win :waterfall)
				    (gv win :xfrmd-x-data-mag)
				    (- (gv win :x-axis-max) (gv win :x-axis-min))))
			       (or (gv win :default-number-of-x-incs) 5.0)))))))
    (s-flt (if (gv win :x-are-fns) (max 1 (round float-inc)) float-inc))))

(defun set-plot-timed-data-replot-mins-maxs (win)
  (let* ((replot-win-point-list (gv win :replot-win-point-list))
	 (replot-x-min (x-plot-win-inv (first replot-win-point-list) win))
	 (replot-x-max (x-plot-win-inv (+ (first replot-win-point-list) (third replot-win-point-list)) win))
	 (replot-y-min (y-plot-win-inv (+ (second replot-win-point-list) (fourth replot-win-point-list)) win))
	 (replot-y-max (y-plot-win-inv (second replot-win-point-list) win)))		  
    (if (or (= replot-x-min replot-x-max) (= replot-y-min replot-y-max)) ; Avoid over zooming - see below
	(s-values win (x-min-spec (gv win :x-min-prior)) (x-max-spec (gv win :x-max-prior)) (y-min-spec (gv win :y-min-prior)) (y-max-spec (gv win :y-max-prior)))
	(let* ((x-seqs (car (gv win :x-lists)))
	       (min-x-mag (if (numberp x-seqs) ; This means a :DELTA-T spec
			      x-seqs
			      (/ (- (car (last (car x-seqs))) (first (car x-seqs)))
				 (gv win :data-length))))
	       (min-maxs (list (gv win :x-min-prior) (gv win :y-min-prior) (gv win :x-max-prior) (gv win :y-max-prior))))
	  (unless (eq min-maxs (car (gv win :min-max-lists))) ; Save if new values are really new.
	    (push min-maxs (gv win :min-max-lists)))
	  (s-values win (x-min-spec replot-x-min) (x-max-spec replot-x-max) (y-min-spec replot-y-min) (y-max-spec replot-y-max))))))

(defun set-plot-timed-data-restore-mins-maxs-origins (win)
  (let ((min-maxs (pop (gv win :min-max-lists))))
    (if (and (gv win :unzoom) min-maxs)
	(s-values win (x-min-spec (first min-maxs)) (y-min-spec (second min-maxs)) (x-max-spec (third min-maxs)) (y-max-spec (fourth min-maxs)))
	(s-values win
		  (x-min-spec (gv win :orig-x-min)) (x-max-spec (gv win :orig-x-max)) (y-min-spec (gv win :orig-y-min)) (y-max-spec (gv win :orig-y-max))
		  (x-origin (gv win :orig-x-origin)) (x-inc (gv win :orig-x-axis-inc)) (y-origin (gv win :orig-y-origin)) (y-inc (gv win :orig-y-axis-inc))))))


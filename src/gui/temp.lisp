
#|
(defmacro virtual-x-center () `(first (gvl :parent :item-values)))
(defmacro virtual-y-center () `(second (gvl :parent :item-values)))
(defmacro virtual-x-left () `(third (gvl :parent :item-values)))
(defmacro virtual-x-right () `(fourth (gvl :parent :item-values)))
(defmacro virtual-y-top () `(fifth (gvl :parent :item-values)))
(defmacro virtual-y-bottom () `(sixth (gvl :parent :item-values)))
(defmacro virtual-linestyle () `(seventh (gvl :parent :item-values)))
;(defmacro virtual-linestyle () wh::thin-black-line)
(defmacro virtual-fillstyle () `(eighth (gvl :parent :item-values)))
|#

(defun foo ()
  (let* ((win (get-plot-window  :xy 'foo nil :width 200 :height 200)))
    (opal::update win)
    (let* (
	   ;; (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
	   (item-array (sequence-to-gen-array `((20 20 0 40 0 40 ,thick-black-line nil)
						(120 120 100 140 100 140 ,thin-black-line nil))))
	   (plot-agg (get-plot-agg win 'data-plot t))
	   (v-agg
	    (create-instance nil opal:virtual-aggregate
					;			   (:window win) ; Essential for initializing o-formulas and slots?
			     (:item-array item-array) (:point-in-item nil) (:item-prototype ph::virtual-dot))))
			   
      ;; can't do this!
      ;; (s-value v-agg :item-array item-array)
;            (resurrect-opal-win win :raise t :visible t)
;;                  (resurrect-opal-win win  :visible t)
     (essential-resurrect-opal-win-for-scatter win)
      (opal:add-component plot-agg v-agg :where :front)
      (s-value win :visible t)          (opal::update win)
      (format t "~A~%" plot-agg)

;      (resurrect-opal-win win :raise t :visible t)
      )))

(defun essential-resurrect-opal-win-for-scatter (win)
  ;; Prior to adding v-agg
  (s-value win :visible t)
;  (opal:update win t)
    (opal:update win)
  )


(defun foo-v-agg ()
  (let* ((v-agg (create-instance nil opal:virtual-aggregate
		;		 (:item-array item-array) (:point-in-item nil)
				 (:item-prototype (create-instance nil opal:circle
								   (:line-style (o-formula (ph::virtual-linestyle)))
								   (:height (o-formula
									     (- (ph::virtual-y-bottom) (ph::virtual-y-top))
									     0)))))))
    v-agg))



(defun foo ()
  (let ((data (get-uniform-xy-variants 1000)))
    (plot-scatter data
		  nil
		  :preserve-plot-layout t
		  :preserve-window-attributes t
		  :preserve-window-dimensions t
		  
		  :x-symbol-width 2)))

(defun noisy ()
  (let ((noise (random-phase-sequence 400 :return-list t :high-cutoff 100)))
    (plot-timed-data noise nil nil
		     :width 500
		     :y-origin 3
		     :x-inc 200
		     :x-are-fns t
		     :overlay t
		   ;  :scatter t
		  ;   :x-symbol-width 4
		     :y-inc 1
		     :y-min -3 :y-max 3
		     :preserve-plot-layout t
		     
		  :preserve-window-attributes t
		  :preserve-window-dimensions t)))

(defun test-noisy ()
  (profile::unprofile)
  (profile::profile
   random-phase-sequence
   plot-timed-data
   )
  (dotimes (x 200) (noisy))
  (profile::report-time)
  (profile::unprofile))

(defun test-foo ()
  (profile::unprofile)
  (profile::profile
					;    DESTROY-SCHEMA ;; kr::destroy-schema
;   OPAL::DESTROY-ME-METHOD-AGGREGATE
;   OPAL::DESTROY-ME-METHOD-VIEW-OBJECT

   ;;OPAL::DESTROY-METHOD-VIEW-OBJECT
;   OPAL::DESTROY-ME-METHOD-VIRTUAL-AGGREGATE ;; wh::CLEAR-WINDOW-AGGREGATE-COMPONENTs
;   ph::get-plot-window
;   ph::transfer-plotting-vars
;   wh::add-temp-comment
;   wh::add-local-and-global-comment
;   
;   get-uniform-xy-variants
;   ph::update-plot-xy-win-xy-data-lists
;   ph::setup-plot
;   ph::add-xy-data-to-plot
;   ph::add-scatter-points-to-plot-from-xy-data-list
;   ph::FROB-PLOT-FOR-VIRTUAL-SCATTER
;  PH::DRAW-VIRTUAL-CIRCLE
;  OPAL::INITIALIZE-METHOD-VIRTUAL-AGGREGATE ; ph::make-virtual-scatter-aggregate
;  PH::UPDATE-VIRTUAL-DOT  ; ph::destroy-wrapper
;   ph::plot-windows-finishing
   )
  (dotimes (x 20) (foo))
  (profile::report-time)
  (profile::unprofile))

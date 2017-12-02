#|


(select-hash-values-menu (get-type-hash-table 'particle) "foo" :direction :horizontal :rank-margin 2 :max-per-menu 30
   
   
)

(let* ((dt 0.001)
       (dvdt (differentiate-wave (element-data *soma*) (current-sim-plot-time-list))))
    (plot-timed-data
		    dvdt
		    nil (current-sim-plot-time-list)
		    :delta-t-start (* 0.5 (cadr (current-sim-plot-time-list))) :x-min 0 :title 'dvdt)
  (plot-timed-data (differentiate-wave
		    (convert-data-time-lists dvdt (current-sim-plot-time-list) dt)
		    dt)
		   nil dt :delta-t-start dt :x-min 0 :title 'd2vdt2))
(element-data-d2dt2 *soma* dt)

(defun element-data-d2dt2 (element dt &key (time-ref (current-sim-plot-time-list))
				   data-type type state-index)
  (let ((dvdt (differentiate-wave (element-data element data-type type state-index) time-ref)))
    (differentiate-wave	(convert-data-time-lists dvdt time-ref dt) dt)))

(defun foo ()
  (loop for dt in '(0.1 0.01 0.001)
	collect
	(let ((d2vdt2 (ELEMENT-DATA-D2DT2 *soma* dt))) 
	  (list (list-of-nums   (length d2vdt2) 0.0 dt)
		d2vdt2)) into output
	collect dt into labels
	finally (plot-xy-data output labels)))

(defun element-data-d2dt2 (element dt &key (time-ref (current-sim-plot-time-list)) data-type type state-index)
  (let* ((data (element-data element data-type type state-index))
	 (dvdt (differentiate-wave data time-ref))
	 (dvdt-time-ref (midpoints time-ref))
	 (dvdt-dted (convert-data-time-lists dvdt dvdt-time-ref dt))
	 (data-d2dt2 (differentiate-wave dvdt-dted)))
    (format t "length data ~a, dvdt ~a, dvdt-dted ~a, data-d2dt2 ~a~%"
	    (length data)
	    (length dvdt) (length dvdt-dted) (length data-d2dt2))
    (format t "dvdt-dted dt ~A, ~A ~A~%" dt (first dvdt-dted) (second dvdt-dted))
    (values data-d2dt2 (midpoints dvdt-time-ref))))

|#
#|
(defun foo ()
  (let (data-time)
    (loop for dt in '(0.1 0.01  0.001;  0.0005
			  )
	  do
	  (setq data-time (convert-data-time-lists (element-data *soma*) (current-sim-plot-time-list) dt t t))
	  collect (reverse data-time)
	  into xys
	  collect (list (midpoints (nth 1 data-time))
			(differentiate-wave (nth 0 data-time) dt))
	  into dtdxys

					;	  collect (list (midpoints  (current-sim-plot-time-list))
					;			(differentiate-wave (element-data *soma*) (current-sim-plot-time-list)))
					;	  into dxys

	  collect (list (midpoints (midpoints (nth 1 data-time)))
			(differentiate-wave (differentiate-wave (nth 0 data-time) dt) dt))
	  into d2xys
	  collect dt into labels
	  finally
	  (let* ((orig-dxdt (differentiate-wave (element-data *soma*) (current-sim-plot-time-list)))
		 (orig-dxdt-time-ref (midpoints (current-sim-plot-time-list)))
		 (orig-d2xdt2 (differentiate-wave orig-dxdt orig-dxdt-time-ref))
		 (orig-d2xdt2-time-ref (midpoints orig-dxdt-time-ref)))
	    (plot-xy-data xys labels :title 'data)
	    (plot-xy-data (cons (list orig-dxdt-time-ref orig-dxdt) dtdxys) (cons 'orig labels) :title 'dxdata)
	    (plot-xy-data (cons (list orig-d2xdt2-time-ref orig-d2xdt2) d2xys) (cons 'orig labels) :title 'd2xdata)))))

	  
|#

(plot-xy-data
 '(((1.5648844 10.699529 21.95849 39.666794 75.725845 122.119576 173.50945
	       228.54443 286.04602 344.99402 404.81415 464.55478)
    (-45.74879 -46.059753 -46.32085 -46.659492 -47.66058 -45.830254 -45.990845
	       -46.816235 -47.08468 -47.06537 -45.19097 -46.03826))
   (
    (1.5251083 10.675758 21.933466 39.527397 75.23701 121.28123 172.31216 226.96523
	       283.96616 342.3216 401.42456 460.95395)
    (-46.27819 -46.115456 -45.624817 -46.724293 -45.852055 -46.60294 -45.94589
	       -47.50606 -46.420143 -46.319054 -46.435024 -46.63366)))
 nil :x-min 0 :x-max 500)

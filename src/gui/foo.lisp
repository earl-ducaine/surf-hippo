(in-package "SON-OF-PLOT-HACK")

(defun random-dots (count &optional (x-range 1.0) (y-range 1.0))
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float x-range y-range))  
  (loop for count fixnum from 1 to count
	collect (random x-range) into x
	collect (random x-range) into y
	finally (return (list x y))))

(defvar *polyline-p* t)

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

(defun get-data-plot-agg (win)
  (loop for comp in (g-value win :aggregate :components)
	when (eq 'PH::DATA-PLOT (g-value comp :type))
	do (return comp)))

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


(defun foo ()
  (let* ((count 10000)
	 (random-dots-1 (random-dots count))
	 (random-dots-2 (random-dots count)))
    (plot-xy-data random-dots-1
		  nil  :width 500 :height 500
		  :connect-data-points nil :scatter t
		  :x-symbol-width 3
		  :preserve-plot-layout t)
    (let ((random-1 (loop for x in (nth 0 random-dots-1)
			  for y in (nth 1 random-dots-1)
			  collect (ph::x-plot-win-float x *twin*) into out
			  collect (ph::x-plot-win-float y *twin*) into out
			  finally (return out)))
	  (random-2 (loop for x in (nth 0 random-dots-2)
			  for y in (nth 1 random-dots-2)
			  collect (ph::x-plot-win-float x *twin*) into out
			  collect (ph::x-plot-win-float y *twin*) into out
			  finally (return out)))

	  (multipoint (car (g-value (PH::GET-DATA-PLOT-AGG *twin*) :components))))
      (dotimes (i 100)
;	(format t "i ~D~%" i)
	(s-value multipoint :point-list (if (oddp i) random-1 random-2))
	(opal::update *twin* t)))))


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


;;; SYS Source file: renewal-process.lisp
(in-package "SURF-HIPPO")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code written by Nicolas Gazeres.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Related code found in randoms.lisp


;;; SF-FIND-POISSON-WAITING-TIME
;;; --------
;;; Function optimized for single-float

(proclaim '(inline SF-FIND-POISSON-WAITING-TIME))
(defun SF-FIND-POISSON-WAITING-TIME (rate)
  "Returns a realization of an interval, in a Poisson process of the given 'rate'. if rate is in spikes/sec, the interval will be in seconds."
  (declare (optimize (safety 1) (speed 3) (space 1)) (single-float rate))
  (let ((df-rate (coerce rate 'double-float)))
    (declare (double-float df-rate))
    (coerce (/ (log (df-random-not-zero 1.0d0)) (- df-rate)) 'single-float)))

(proclaim '(inline SF-FIND-GAMMA-WAITING-TIME))
(defun SF-FIND-GAMMA-WAITING-TIME (lambda order)
  "Returns a realization of a random variable distributed following a gamma distribution of parameter 'lambda'
and of given 'order'. order must be an integer. For t>=0,

                                     (lambda*t)^(order-1)
       g(t) = P( t<T<t+dt ) = lambda -------------------- exp(-lambda*t)
                                          (r-1)!

IMPORTANT: If lambda is given in spikes/sec, the interval generated is in milliseconds."
  (declare (optimize (safety 1) (speed 3) (space 0))  (single-float lambda) (fixnum order))
  (let ((val 0.0d0)
	(df-lambda (coerce lambda 'double-float)))
    (declare (double-float val))
    (dotimes (k order)
      (declare (fixnum k))
      (incf val (- (log (df-random-not-zero 1.0d0)))))
    (* 1000.0 (coerce (/ val df-lambda) 'single-float))))


;;; MAKE-EVENTS-RENEWAL
;;; ------------

(defun MAKE-EVENTS-RENEWAL (array syn-type)
  (GAMMA-ARRIVAL-TIMES array *user-start-time* *user-stop-time* 1.0 (element-parameter syn-type 'thinning-factor)))

;;; SWITCHING-PAIRS
;;; ---------------
;;; cette fonction prend en argument un array et une fonction en escalier, et retourne la liste des
;;; temps ou la fonction dans l'array traverse les valeurs de la fonction en escalier.
;;; '((11.0 . 1) (100.0 . 5) (1000.0 . 50))
;;; a pair like 11.0 . 1 means that, for x less than 11.0, thinning equals 1.
;;; returns an acons like : '((100.0 . 1) (200.0 . 10) (1000.0 . 1))
;;; the returned pair always contains at least the acons (*user-stop-time* . something)

(defun SWITCHING-PAIRS (array thinnings &optional (step 1.0))
  (when (> (length array) 0)
    (loop with old-switch = (cdr (find (aref array 0) thinnings :key #'car :test #'<))
	  with switch = 0
	  for val across array
	  for time from 0.0 by step
	  do (setq switch (cdr (find val thinnings :key #'car :test #'<)))
	  unless (= switch old-switch)
	  collect (cons time old-switch) into out
	  and do (setq old-switch switch)
	  finally
	  (return (append out (list (cons time (cdr (find val thinnings :key #'car :test #'<)))))))))


;;; *THINNING-STEP-ACONS*
;;; -------------

(defvar *DEFAULT-THINNING-STEP-ACONS* '((11.0 . 1) (75.0 . 5) (1000.0 . 50))
  "this acons holds the stepwise function relating the thinning-factor to the current frequency.")







;;; MAKE-EVENTS-SWITCHING
;;; --------------

;;; La fonction ne considere que la liste THINNING-STEP-ACONS du synapse-type.
;;; - Cette version permet de commencer la sequence a n'importe quel temps, en particulier *user-start-time*.
;;;   On considere implicitement que ARRAY contient une fonction qui commence a l'instant START.
;;; - Cette version concatene les realizations de GRPs individuels par un NCONC, pas par un APPEND !

(defun MAKE-EVENTS-SWITCHING (array syn-type &optional (start *user-start-time*))
  (declare (optimize (speed 3)))
  (let ((thinnings (element-parameter syn-type 'thinning-step-acons)))
    (loop with old-time single-float = start
	  for pair in (switching-pairs array thinnings)   ;; car pair is time, cdr pair is thinning
	  nconc (GAMMA-ARRIVAL-TIMES array old-time (car pair) 1.0 (cdr pair)) into out
	  do (setq old-time (car pair))
	  finally (return out))))

;;(setq a0 (make-array '(500) :element-type 'single-float :initial-element 10.0))
;;(MAKE-EVENTS-SWITCHING a0   (element "LIGHT-AUTO-OFF"))
;; -> BUG : le dernier element est la deux fois...
;;
;; (gamma-arrival-times a0 0.0 500.0 1.0 1) bugge aussi
;; GAMMA-ARRIVAL-TIMES BUGGE QUAND t-stop est grand, ici 500.
;; MAIS NE BUGGE PAS QUAND t-stop est petit, ie. 100.0 !!!
;; NE BUGGE PAS QD a0 est un float
;; --> ca concerne la partie array !







;;; EXAMPLE OF SYNAPSE-TYPE SPECIFICATIONS
;;; ----------------------------------------
;;; Add this to the synapse-type-def form :
;;;
;;;    (make-events-function . MAKE-EVENTS-SWITCHING)
;;;    (thinning-step-acons . ((11.0 . 1) (75.0 . 5) (1000.0 . 50)))
;;;
;;; The regularity parameter for the switching GRP will be 1 for frequencies strictly below 11 Hz, 5
;;; for frequencies strictly below 75.0 Hz and 50 for frequencies below 1000.0 Hz.
;;; Warning : Don't quote the a-list.

;;; To create a working array easily :
;;; (setq a1 (test-response))
;;; (event-lists-raster (loop for k below 20
;;;       collect (make-events-switching a1 NIL '((11.0 . 1) (75.0 . 100) (1000.0 . 1)))))

;;; To generate events using the switching GRP method :
;;;(funcall #'make-events-switching
;;;         (get-light-response-array (car (synapses)))
;;;         (car (synapse-types)))



;;; SWITCHING-GRP-ARRIVAL-TIMES
;;; --------
;;; This generates a switching GRP of constant rate, but with time-dependent regularity.
;;; The order are specified as a piecewise constant function (fonction en escalier).
;;; The process starts at t=0.
;;; rate is in spikes/sec.
;;; duration and step are in milliseconds.
;;; orders can be :
;;; '((200.0 . 1) (400.0 . 5) (600.0 . 50) (800.0 . 10) (1000.0 . 1))

(defun SWITCHING-GRP-ARRIVAL-TIMES (rate duration step orders)
  (loop with start = 0.0
	for pair in orders
	until (> start duration)
	append (GAMMA-ARRIVAL-TIMES rate start (min (car pair) duration) step (cdr pair)) into out
	do (setq start (car pair))
	finally (return out)))


(defun FALSE-SWITCHING-GRP-ARRIVAL-TIMES (rate duration orders)
  (loop with start = 0.0
	for pair in orders
	until (> start duration)
	append (SIMPLE-GRP-ARRIVAL-TIMES rate start (min (car pair) duration) (cdr pair))
	into out
	do (setq start (car pair))
	finally (return out)))




(defun CORRELATED-UNIT-RATE-POISSON-PROCESSES (correlation start stop)
  "correlation is proportion of correlation, between 0 and 1."
  (if (not (<= 0.0 correlation 1.0))
      (break "correlation should be between 0 and 1.")
      (let ((correlated-process (gamma-arrival-times correlation start stop 1.0 1))
	    (1-correl (- 1.0 correlation)))
	(values
	 (sort (append correlated-process (gamma-arrival-times 1-correl start stop 1.0 1)) #'<)
	 (sort (append correlated-process (gamma-arrival-times 1-correl start stop 1.0 1)) #'<)))))



(defun TEST-CORRELATED-PROCESSES (correlation)
  "Tests the above routine. This should return a float close to correlation."
  (/ (loop for k below 1000
	   sum (multiple-value-bind (p1 p2) (CORRELATED-UNIT-RATE-POISSON-PROCESSES correlation 0.0 1000.0)
		 (length (intersection p1 p2)))) 1000.0))




















;;; the following call were used to make the figure :
;;; (FIG-SWITCHING-GRP :bin 2.0 :nb-trials 500 :limit-raster 25  :true-switching-grp NIL)
;;; (FIG-SWITCHING-GRP :bin 2.0 :nb-trials 500 :limit-raster 25  :true-switching-grp T)

(defun FIG-SWITCHING-GRP (&key
			  (nb-trials 25)
			  (bin 5.0)
			  (width 500)
			  (with-text NIL)
			  (no-psth NIL)
			  (no-raster NIL)
			  (limit-raster NIL)
			  (true-switching-grp T)
			  (orders '((200.0 . 1) (400.0 . 5) (600.0 . 50) (800.0 . 10) (1000.0 . 1))))
  (let ((*create-new-plot-windows* T)
	(events (if true-switching-grp
		    (loop for k below nb-trials
			  collect (SWITCHING-GRP-ARRIVAL-TIMES 50.0 1000.0 1.0 orders))
		    (loop for k below nb-trials
			  collect (FALSE-SWITCHING-GRP-ARRIVAL-TIMES 50.0 1000.0 orders)))))
    (unless no-raster
      (event-lists-raster (if limit-raster
			      (subseq events 0 limit-raster)
			      events)
			  :stop 1000.0
			  :width width
			  :event-width 1    ;; petite barre
			  :no-labels T
			  :update-fixed-gap  t
			  :use-fixed-right-gap T
			  :fixed-right-gap  50
			  :use-fixed-left-gap T
			  :fixed-left-gap 50))
    (unless no-psth
      (plot-psth events
	    :x-min 0.0
	    :x-max 1000.0
	    :step bin
	    :bin-skip 24
	    :give-in-frequency T
	    :nb-trials nb-trials
	    :width width
	    :height 200
	    :with-comments with-text
	    :update-fixed-gap  t
	    :use-fixed-bottom-gap T
	    :fixed-bottom-gap 30
	    :use-fixed-top-gap T
	    :fixed-top-gap 30
	    :use-fixed-right-gap T
	    :fixed-right-gap  50
	    :use-fixed-left-gap T
	    :fixed-left-gap   50
	    ))))










;;; INVERSE-SEG
;;; ------
;;; Inverse une valeur d'ordonnee sur un segment de droite. La droite passe par les points :
;;; (x-max - delta-x, y-min) and (x-max, y-max).

(defmacro INVERSE-SEG (delta-x x-max y-min y-max y-solve)
  (let ((delta-x-loc (gensym))
	(x-max-loc (gensym))
	(y-min-loc (gensym))
	(y-max-loc (gensym))
	(y-solve-loc (gensym)))
    `(let ((,delta-x-loc ,delta-x)
	   (,x-max-loc ,x-max)
	   (,y-min-loc ,y-min)
	   (,y-max-loc ,y-max)
	   (,y-solve-loc ,y-solve))
      (- ,x-max-loc (* (/ ,delta-x-loc (- ,y-max-loc ,y-min-loc)) (- ,y-max-loc ,y-solve-loc))))))


;;; PDF-INSTANCE
;;; -------
;;; Computes a realization of a random value with a given pdf.
;;; step is the step of an simple Euler integration method.

(defun PDF-INSTANCE (pdf &optional (step 1.0))
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float step)
	   (function pdf))
  (let ((integral 0.0)
	(old-integral 0.0)
	(uniform-va-instance (random 1.0)))
    (declare (single-float integral old-integral uniform-va-instance))
    (loop for time single-float from 0.0 by step
	  until (> integral uniform-va-instance)
	  do
	  (setq old-integral integral)
	  (incf integral (* step (the sf (funcall pdf time))))
	  finally (return (inverse-seg step time old-integral integral uniform-va-instance)))))


;;; TEST-PDF, TEST-PDF-2
;;; ------
;;; ceci est simplement une distribution de probabilite test :
;;; Elle vaut 2x entre 0 et 1 et 0 ailleurs.
;;;
;;; Exemple:
;;; (psth (list (loop for k below 5000 collect (PDF-INSTANCE #'test-pdf 0.05))) :step 0.05)

(defun TEST-PDF (x)
  (declare (optimize (safety 0) (speed 3) (space 0)) (single-float x))
  (if (< x 1.0) (* 2.0 x) 0.0))

(defun TEST-PDF-2 (x)
  (declare (optimize (safety 0) (speed 3) (space 0)) (single-float x))
  (/ (exp (- (/ x 10.0))) 10.0))



;;; SERIAL-COEFF
;;; serial correlation coefficients
;;;
(defun SERIAL-COEFF (interval-seq order)
  "'interval-seq' is a list of *INTERVALS*. 'order' is the distance between intervals."
  (let ((mean (/ (reduce #'+ interval-seq) (length interval-seq))))
    (format T "moyenne=~A~%" mean)
    (/ (loop for interval-1 in interval-seq
	     for interval-2 in (last interval-seq (- (length interval-seq) order))
	     sum (* (- interval-1 mean) (- interval-2 mean)))
       (length interval-seq))))



(defun INTERVALS-FROM-TIMES (event-times-process)
  (loop for val-1 in event-times-process
	for val-2 in (cdr event-times-process)
	collect (- val-2 val-1)))





(Defun DEMO-THINNING (&key (nb-trials 25) (bin-size 5.0))
  (let ((array (make-array '(500) :element-type 'single-float :initial-element 50.0))
	(*create-new-plot-windows* T)
	(*plot-window-height* 200)
	(*plot-window-width*  560))
    (loop for k in '(1 2 4 5 6 100) do
	  (let* ((*thinning-factor* k)
		 (events (loop for k below nb-trials collect (gamma-arrival-times array 0.0 500.0 1.0))))
	    (event-lists-raster events
				;; :width width
				:title (format NIL "Erlang Process Events - Thinning=~A " *thinning-factor*)
				:stop 500.0
				:no-labels T
				:update-fixed-gap  t
				:use-fixed-right-gap T
				:fixed-right-gap  50
				:use-fixed-left-gap T
				:fixed-left-gap   50)
	    (plot-psth  events
		   :x-min 0.0 :x-max 500.0
		   :step bin-size :bin-skip 24
		   :give-in-frequency T
		   :title (format NIL  "Erlang Process PSTH - Thinning=~A " *thinning-factor*)
		   :nb-trials nb-trials
		   ;; :width width
		   :height 200
		   :update-fixed-gap  t
		   :use-fixed-right-gap T
		   :fixed-right-gap  50
		   :use-fixed-left-gap T
		   :fixed-left-gap   50)))))

(defun ERLANG+PSTH (&key (nb-trials 25) (bin-size 5.0))
  (let ((array (make-array '(500) :element-type 'single-float :initial-element 50.0))
	(*create-new-plot-windows* T)
	(*plot-window-height* 200)
	(*plot-window-width*  560))
    (loop for k in '(1 5 10 100) do
	  (let* ((*thinning-factor* k)
		 (events (loop for k below nb-trials collect (gamma-arrival-times array 0.0 500.0 1.0))))
	    (event-lists-raster events
				;; :width width
				:title (format NIL "Erlang Process Events - Thinning=~A " *thinning-factor*)
				:stop 500.0
				:no-labels T
				:update-fixed-gap  t
				:use-fixed-right-gap T
				:fixed-right-gap  50
				:use-fixed-left-gap T
				:fixed-left-gap   50)
	    (plot-psth  events
		   :x-min 0.0 :x-max 500.0
		   :step bin-size :bin-skip 24
		   :give-in-frequency T
		   :title (format NIL  "Erlang Process PSTH - Thinning=~A " *thinning-factor*)
		   :nb-trials nb-trials
		   :width *plot-window-width*
		   :height 200
		   :update-fixed-gap  t
		   :use-fixed-right-gap T
		   :fixed-right-gap  50
		   :use-fixed-left-gap T
		   :fixed-left-gap   50)))))



(defun PSTH-DISTRIB (&key (nb 100) (bin 1.0) (bin-skip 19)
			  (order 1) (rate 1.0)
			  (x-max 1.0)
			  (x-inc 10.0)
			  (x-axis-tick-skip 4)
			  (what :gamma)   ;; :gamma :modified :uniform :stationary-grp :simple-grp
			  (give-in-frequency NIL)
			  (normalize T)
			  (y-max 1.0)
			  (y-inc 10.0)
			  (with-comments T)
			  )
  (let* ((values
	  (remove NIL
		  (loop for k below nb
			collect (case what
				  (:gamma   (list (/ (SF-FIND-GAMMA-WAITING-TIME rate order) order)))
				  (:modified (list (SF-FIRST-INTERVAL-STAT-GRP order rate)))
				  (:uniform (list (* (sf-random-not-zero 1.0) x-max)))
				  (:stationary-grp  (gamma-arrival-times (* 1000.0 rate) 0.0 x-max bin order))
				  (:simple-grp  (simple-grp-arrival-times (* 10.0e-3 rate) 0.0 x-max order)))
			)))
	 (*create-new-plot-windows* T))

    (format T "Moyenne=~A~%" (/ (loop for val in values sum (car val)) nb))
    (setq *comment-font*   (opal:get-standard-font :serif :bold-italic :large))
    (setq *plot-axis-font* (opal:get-standard-font :serif :bold-italic :large))
    (let ((win-psth (PLOT-PSTH values
                          :step bin
			  :x-min 0.0
                          :x-max x-max
                          :title (format NIL "Erlang ~A" order)
                          :bin-skip bin-skip
                          :normalize normalize
                          :give-in-frequency give-in-frequency
                          :y-axis-label-location :upper-right
                          ;; :y-max y-max
                          :x-label "msec"
                          :y-label "Hz"
			  :with-comments with-comments
			  :update-fixed-gap  t
			  :use-fixed-bottom-gap T
			  :fixed-bottom-gap 30
			  :use-fixed-top-gap T
			  :fixed-top-gap 30
			  :use-fixed-right-gap T
			  :fixed-right-gap  50
			  :use-fixed-left-gap T
			  :fixed-left-gap   50
			  )))

      (s-value win-psth :axes-type :standard)
      (s-value win-psth :x-axis-p NIL)
      (s-value win-psth :x-axis-tick-skip x-axis-tick-skip)
      (s-value win-psth :x-max x-max)
      (s-value win-psth :x-axis-inc x-inc)

      (s-value win-psth :y-axis-p T)
      (s-value win-psth :y-axis-inc y-inc)

      (s-value win-psth :x-axis-label (if with-comments "msec" ""))
      (s-value win-psth :y-axis-label (if with-comments "Hz" ""))

      ;; (s-value win-psth :y-axis-label-location :upper-right)
      (s-value win-psth :x-label-h-position :center)

      (s-value win-psth :x-are-fns T)
      (s-value win-psth :y-are-fns T)

      (let ((*automatic-run* T)) (ph::histogram-menu win-psth)))))

;; pour la figure :
;;(psth-distrib :nb 10000 :bin 1.0 :bin-skip 49 :rate 10.0e-3 :order 5 :x-max 200.0 :give-in-frequency T
;;              :with-comments NIL :y-inc 5 :what :gamma)


;;; --------------------------------
;;; A GARDER :
;;; pour generer la loi gamma 5, de moyenne 50.0
;;; (psth-distrib :nb 5000 :bin 1.0 :bin-skip 49 :rate 5.0e-2 :order 5 :x-max 200.0 :give-in-frequency T)
;;;   avec (SF-FIND-GAMMA-WAITING-TIME rate order).
;;; --------------------------------
;;; pour generer la oi modifiee pour le premier renouvellement
;;; (psth-distrib :nb 100000 :bin 1.0 :bin-skip 49 :rate 10.0e-3 :order 5 :give-in-frequency T)
;;; --------------------------------





;;; TEST-POISSON
;;; -------
;;; Cette fonction evalue graphiquement l'ecart entre la distribution aleatoire fournie par
;;; FIND-POISSON-RANDOM-TIME et la distribution exponentielle attendue.
;;; test : (TEST-POISSON :limit 1.0 :bin 0.005 :sample 100000)

(defun TEST-POISSON (&key (bin 1.0) (sample 10000) (limit 1.0))
  (let* ((freq 10.0)
	 (size (floor (/ limit bin)))
	 (histo-array (make-array (list size))))
    (loop for k below sample
	  do (let* ((rand-value (sf-find-poisson-waiting-time freq))
		    (index (floor (/ rand-value bin))))
	       (incf (aref histo-array index))))
    (plot-timed-data (list histo-array
			   (loop for k below limit by bin
				 collect (* sample bin freq (exp (- (* k freq))))))
		     '("Estimated Distribution Function"
		       "Theoretical Distribution  Function")
		     bin
		     :delta-t-start 0.0)))

(defun CROSS-CORR (events-1 events-2 start stop bin-width)
  ;; events-1 and events-2 sorted in ascending order.
  (declare (optimize (speed 3) (safety 0) (compilation-speed 3) (debug 0))
	   (list events-1 events-2) (single-float start stop bin-width))
  (let ((histo (make-histo :values (make-list (truncate (the sf (- stop start)) bin-width)
					      :initial-element 0)
			   :start start :stop stop :bin-width bin-width)))
    (check-histo histo)
    (loop for val1 single-float in events-1
	  until (null events-2)
	  do
	  (loop until (or (null events-2) (>= (the sf (car events-2)) val1))
		do (setq events-2 (cdr events-2)))
	  (loop for val2 single-float in events-2
		do (let ((bin (truncate (the single-float (- val2 val1)) bin-width)))
		     (when (< bin stop)
		       (incf (the fn (nth bin (histo-values histo))))))))

    histo))

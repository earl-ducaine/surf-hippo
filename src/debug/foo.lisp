(let* ((step 0.1)
       (actual-duration 10)
       (duration (/ actual-duration step))
       (sinewave1
       (let* ((period 10) (amp 1)  (frequency (/ period)) (phase 0))
	 (sinewave amp duration frequency :phase phase :step step)))
      (sinewave2
       (let* ((period 9) (amp 1) (frequency (/ period)) (phase 0))
	 (sinewave amp duration frequency :phase phase  :step step))))
  (plot-timed-data (list sinewave1 sinewave2))) 
  
  
(defmacro genffasfdasdfsym-defvar (value)
  (let ((name (gensym)))
    `(defparameter ,name ,value))
  (asdfasdime)
  (let ((name (gensym)))
    `(defparameter ,name ,value))
  (ewewe))


(let ((*overlay-plots* t))
  (caows)
  (loop for i from 1 to 10 do
       (plot-timed-data (loop for count from 1 to 4 collect 
			     (loop for index from 1 to 10 collect (+ (* 2 count) (random 1.0))))
			nil 1
			:width 500 :height 500 :scatter t :x-are-fns t :y-are-fns t)))


(let ((*overlay-plots* t)(number-of-waves 1))
  (caows)

  (plot-timed-data
   (loop for count from 1 to number-of-waves collect (sinewave 1 10  .1  :offset count :phase (* 0  count)))
   nil
   (loop for count from 1 to number-of-waves collect (sinewave 1 10  .1  :offset count :phase (* 40  count)))
			:width 1500 :height 500 :scatter t :x-are-fns t :y-are-fns t)))



;; '(((ydata1A-list) (ydata2A-list)) ((ydata1B-list) (ydata2B-list))...).

(loop for set from 1 to 3 do
     (let ((y-data (loop for trace from 1 to 2 collect (loop for count from 0 to 3 collect (+ trace (random 0.10))))))
       (plot-timed-data y-data nil nil
			 :overlay (> set 1)
			))
   finally
     (format t "x-lists ~A, y-lists ~A~%" (g-value *twin* :x-lists) (g-value *twin* :y-lists)))



(let ((y-data (loop for set from 1 to 3 do (loop for trace from 1 to 2 collect (loop for count from 0 to 3 collect (+ trace (random 0.10)))))))
  (plot-timed-data y-data nil nil
					; :overlay (> set 1)
		   )
  (format t "x-lists ~A, y-lists ~A~%" (g-value *twin* :x-lists) (g-value *twin* :y-lists)))

(let ((y-data (loop for set from 1 to 3 collect
		   (loop for trace from 1 to 2
		      collect (loop for count from 0 to 3 collect (+ trace (random 0.10)))))))
  (printvars y-data)
  (plot-timed-data y-data nil nil
					; :overlay (> set 1)
		   )
   ;  (format t "x-lists ~A, y-lists ~A~%" (g-value *twin* :x-lists) (g-value *twin* :y-lists))
  )

(defun foo ()
  (loop for set from 1 to 3 do ; collect
       (let ((y-data 
		     (loop for trace from 1 to 2
			collect (loop for count from 0 to 3 collect (+ trace (random 0.10)))))
	     (x-data set))
    (plot-timed-data y-data nil x-data
		     :overlay (> set 1)
		     )
    ;; (format t "x-lists ~A, y-lists ~A~%" (g-value *twin* :x-lists) (g-value *twin* :y-lists))
    )))

(plot-timed-data
 '(((1.0606145 1.0154321 1.0530999 1.0583636) (2.0182538 2.0375657 2.0286484 2.0110762))
   ((1.0355432 1.0698754 1.0645564 1.0271059) (2.0791783 2.013402 2.0258787 2.0114162))
   ((1.0854944 1.0171297 1.0848991 1.0747398) (2.0680544 2.096711 2.0249066 2.0947797)))
 nil
  '(1 2 3) ;  '(((1 4 10 11)(2 5 11 12))   ((1 4 10 11)(2 5 11 12))   ((1 4 10 11)(2 5 11 12)))
 :scatter t)

(g-value *twin* :x-lists)

(caows)



(loop for group in '(((1.0606145 1.0154321 1.0530999 1.0583636) (2.0182538 2.0375657 2.0286484 2.0110762))
		     ((1.0355432 1.0698754 1.0645564 1.0271059) (2.0791783 2.013402 2.0258787 2.0114162))
		     ((1.0854944 1.0171297 1.0848991 1.0747398) (2.0680544 2.096711 2.0249066 2.0947797)))
     collect
     (loop for trace in group collect (list (list-of-nums (length trace)) (scale-float-list 0.0001 trace))))



(plot-xy-data
 '((((0.0 1.0 2.0 3.0) (1.06061445e-4 1.01543206e-4 1.05309984e-4 1.05836356e-4))
  ((0.0 1.0 2.0 3.0) (2.0182537e-4 2.0375657e-4 2.0286483e-4 2.0110761e-4)))
 (((0.0 1.0 2.0 3.0) (1.0355432e-4 1.06987536e-4 1.06455635e-4 1.0271059e-4))
  ((0.0 1.0 2.0 3.0) (2.0791782e-4 2.013402e-4 2.0258786e-4 2.0114162e-4)))
 (((0.0 1.0 2.0 3.0) (1.0854944e-4 1.0171296e-4 1.08489905e-4 1.0747398e-4))
  ((0.0 1.0 2.0 3.0) (2.0680545e-4 2.0967108e-4 2.0249066e-4 2.0947796e-4)))))

;; ok
(plot-xy-data
 '(((0.0 1.0 2.0 3.0) (1.06061445e-4 1.01543206e-4 1.05309984e-4 1.05836356e-4))
  ((0.0 1.0 2.0 3.0) (2.0182537e-4 2.0375657e-4 2.0286483e-4 2.0110761e-4)))
 nil :scatter t :transpose-data nil)

;; ok
(let* ((x-list1 `(1.06e-4 1.015e-4 1.053e-4 1.0583e-4))
       (y-list1 '(0.0 1.0 2.0 3.0))
       (x-list2 '(2.018e-4 2.037e-4 2.028e-4 2.011e-4))
       (y-list2 '(0.0 1.0 2.0 3.0))
       (x-lists `(,x-list1 ,x-list2))
       (y-lists `(,y-list1 ,y-list2))
       (xy-data-ref `((,x-list1 ,y-list1)
		      (,x-list2 ,y-list2)))
       (win
	(plot-xy-data
	 nil ;xy-data-ref
	 nil :scatter t :transpose-data nil
	 :x-lists x-lists :y-lists y-lists
	 )
	 ))
  (printvars xy-data-ref)
  (format t ":x-lists ~A~%:y-lists ~A~%" (g-value win :x-lists)(g-value win :y-lists)
	  ))


;; bugs
(plot-xy-data
 '(((0.0 1.0 2.0 3.0) (1.06061445e-4 1.01543206e-4 1.05309984e-4 1.05836356e-4))
  ((0.0 1.0 2.0 3.0) (2.0182537e-4 2.0375657e-4 2.0286483e-4 2.0110761e-4)))
 nil :scatter t :transpose-data t)

(caows)

(plot-xy-data
 '(((0.0 1.0 2.0 3.0) (1.0606145 1.0154321 1.0530999 1.0583636))
   ((0.0 1.0 2.0 3.0) (2.0182538 2.0375657 2.0286484 2.0110762)))
 nil :scatter t :transpose-data nil)

(plot-xy-data
 '(((1.0606145 1.0154321 1.0530999 1.0583636) (0.0 1.0 2.0 3.0) )
   ( (2.0182538 2.0375657 2.0286484 2.0110762) (0.0 1.0 2.0 3.0)))
 nil :scatter t :transpose-data nil)

(multiple-value-bind (spks-stims freq-stims-lists  gains)
    (F/I :STEP 0.01 :STIMULUS-start-mag 0.25 :STIMULUS-stop-mag .6 :RETURN-VALUES t :DURATION 100 :plot-gains t :kill-all-output nil :raster-plots nil :intervals 2)
  (printvars gains))

(((35.42255 57.66188 30.027613 48.905785 25.05144 37.145096 24.899559 31.526274 27.18978 31.350218 36.30044 33.68613 37.102867)
  (0.35 0.45 0.55 0.65000004 0.75000006 0.8500001 0.9500001 1.0500001 1.1500001 1.2500001 1.3500001 1.4500002 1.5500002))
 ((35.42255 57.66188 72.6984 83.942535 92.80097 100.28369 106.62497 112.10688 116.9315 121.15662 124.935616 128.53838 131.67621)
  (0.35 0.45 0.55 0.65000004 0.75000006 0.8500001 0.9500001 1.0500001 1.1500001 1.2500001 1.3500001 1.4500002 1.5500002))
 ((30.027613 48.905785 63.627396 75.39519 84.89929 92.89486 99.674034 105.845764 111.19108 115.90276 120.287735)
  (0.55 0.65000004 0.75000006 0.8500001 0.9500001 1.0500001 1.1500001 1.2500001 1.3500001 1.4500002 1.5500002)))
     

; XY-DATA-REF = '(((x1 x1 ... x1)(y1 y1 ... y1))
;;                ((x2 x2 ... x2)(y2 y2 ... y2))
;;                ....
;;                ((xn xn ... xn)(yn yn ... yn)))
;;



(max-depth (g-value *twin* :y-lists))
(let ((y-data (g-value *twin* :y-lists)))
  (let ((y-data-depth (max-depth y-data)))
  (case y-data-depth
  ;; (1 2 3)
    (1 (list (list y-data)))
    (2 (list y-data))
    (3 y-data)
    (t (sim-error "Malformed Y-DATA.")))))



(max-depth '(((1.0 1.0 1.0 1.0) (2.0 2.0 2.0 2.0) (3.0 3.0 3.0 3.0)
                         (4.0 4.0 4.0 4.0))
                        ((1.0 1.0 1.0 1.0) (2.0 2.0 2.0 2.0) (3.0 3.0 3.0 3.0)
                         (4.0 4.0 4.0 4.0))
                        ((1.0 1.0 1.0 1.0) (2.0 2.0 2.0 2.0) (3.0 3.0 3.0 3.0)
                         (4.0 4.0 4.0 4.0))
                        ((1.0 1.0 1.0 1.0) (2.0 2.0 2.0 2.0) (3.0 3.0 3.0 3.0)
                         (4.0 4.0 4.0 4.0))))

(defmacro gensym-defvar (value)
  (let ((name (gensym)))
    `(defparameter ,name ,value)))


(defmacro gensym-defvar (value)
;  (let ((name (gensym)))
  `(defparameter ,(gensym) ,value)
)
     

(let ((w-center (gensym-defvar 100.0)))
  (choose-variable-values `((,w-center "Center of window along X direction [um]:" :number))))


(let ((window-center (gensym-defvar 1.0)))
  `((,window-center "Center of window along X direction [um]:" :number)))

(defun foo (window-center)
  (let ((window-center (gensym-defvar window-center)))
    (choose-variable-values `((,window-center "Center of window along X direction [um]:" :number)))
    (printvars window-center)
    ;;    (setq return-value window-center)
    ;; `((,window-center "Center of window along X direction [um]:" :number))
    (symbol-value window-center) 
    ))

(defun foo (window-center)
  (defvar window-center window-center)
;  (choose-variable-values `((window-center "Center of window along X direction [um]:" :number)))
    ;;    (setq return-value window-center)
  ;; (symbol-value window-center) ;; `((,window-center "Center of window along X direction [um]:" :number))
  window-center
    ))


(defun foo (window-center)
  (let ((window-center (gensym-defvar window-center))
	(return-value nil))
    (choose-variable-values `((,window-center "Center of window along X direction [um]:" :number)))
     (symbol-value window-center)
     (setq return-value window-center)
    (format t "window-center = ~A~%" window-center)
    ))

(defun foo (&key (window-center 123))
  (defvars (window-center))
  (let ((window-center 3))
    (choose-variable-values
     '((window-center "Center of window along X direction [um]:" :number)))
    window-center))

(defun foo (&key (window-center 123))
  (defvars (window-center))
    (choose-variable-values
     '((window-center "Center of window along X direction [um]:" :number)))
    window-center)

(defun foo (&key (w-center 123))
    (defvars (w-center))
    (choose-variable-values
     '((w-center "Center of window along X direction [um]:" :number)))
    w-center)


(defun foo ()
  (let ((dummy1 :y)
	(dummy2 "This is the end")
	(dummy3 t)
	(dummy4 12.34))
    (choose-variable-values
     `(;(dummy1 "Print as integers:" :choose (:x_&_y :x :y :neither) :rank-margin 4 :label-left)
       ;(:comment)
       (dummy2 ,(format nil "Y axis label asdf~%asdf asdfasd fasdf ") :string)       
       ;(dummy3 "Use roots below for tick marks (default is origin)" :boolean)
					;(dummy4 "Value for numeric root for Y axis tick marks" :float)
       )
     :title "Axes etc. menu for")))


gg::Motif-Scrolling-Labeled-Box

(create-instance 'motif-labeled-box-gadget gg::Motif-Scrolling-Labeled-Box
  (:left 10) (:label-offset 10)
  (:font (o-formula *menu-font*))
  (:selection-function #'labeled-box-gadget-selection-function))

(s-value wh::motif-labeled-box-gadget :font (o-formula *menu-font*))



(create-instance 'motif-check-boolean-set-button-gadget garnet-gadgets:motif-check-button
		 (:selection-function #'x-button-selection-function) (:left 10) (:button-width 18)
		 (:font (o-formula *menu-font*)))

(defun x-boolean-set-button (global-var-menu-list &optional (top 20))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((gadget (if *motif-gui-p*
		    (create-instance nil motif-check-boolean-set-button-gadget)
		    (create-instance nil x-boolean-set-button-gadget)
		    ;; (retrieve-menu-thing *motif-check-boolean-set-buttons* motif-check-boolean-set-button-gadget)
		    ;; (retrieve-menu-thing *x-boolean-set-buttons* x-boolean-set-button-gadget)
		    )))
    (s-value gadget :variable (global-var-from-menu-list global-var-menu-list))
    (s-value gadget :top top)
    (s-value gadget :string (return-comment-string-or-var global-var-menu-list))
    ;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :selected (symbol-value (global-var-from-menu-list global-var-menu-list)))
    gadget))

(defun string-first-line (string) (string-head string (position #\Newline string)))

(choose-variable-values
     `(;(*printer* "Specify printer" :string)
       ;; (*print-simulator-time* "Print run and user time stats after simulation" :boolean)
       ;(*show-time-remaining* "Show time remaining during simulation" :boolean)
       ;(*time-window-update-time* "Time (actual) in seconds between time window updates." :integer)
       (*session-name* "Name this session (spaces will map to _)" :string)
       ;(*circuit* "Circuit name" :string)
       ;(*SIMULATION-PLOT-WINDOW-COMMENT* "Comment for plot windows" :string)
       ;(*username* "User name" :string)
       ;(*always-add-host-name-to-windows* :boolean)
       (*displayed-host-name* :string)))

       (defun foo (dummy2)
 (let ((positions '(:left :right :top))(dummy5 12312312))
 (choose-variable-values
  `((dummy1 "Position:" :choose ,positions)
    (*real-time* "This is an integer" :number)
        (*real-time*  :number)
       (dummy10 ,(format nil "Append new text to any existing comment at same position~%or not") :boolean)
       (dummy2 "String this :asdf asdf asdf asdf asdfas dfasdf" :string)
       
       ))
 dummy2))

(defun x-boolean-set-button (global-var-menu-list &optional (top 20))
  (let ((gadget  (create-instance nil motif-check-boolean-set-button-gadget)))
    (s-value gadget :variable (global-var-from-menu-list global-var-menu-list))
    (s-value gadget :top top)
    (s-value gadget :string (return-comment-string-or-var global-var-menu-list))
    (gv gadget :value) ;; Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (s-value gadget :selected (symbol-value (global-var-from-menu-list global-var-menu-list)))
    gadget))

(defun labeled-box-text-set-button (global-var-menu-list &optional (top 20))
  (let* ((variable-symbol (global-var-from-menu-list global-var-menu-list))
	 (original-value (if (symbol-value variable-symbol) (format nil "~a" (symbol-value variable-symbol)) ""))
	 (gadget (create-instance nil  garnet-gadgets:motif-scrolling-labeled-box
		   (:original-value original-value)
		   (:variable variable-symbol)
		   (:top top)
		   (:left 10)
		   (:label-offset 10)
		   (:font (o-formula *menu-font*))
		   (:label-string (return-comment-string-or-var global-var-menu-list t t))
		   (:selection-function #'(lambda (scrolling-labeled-box-object value)
					    (set (gv scrolling-labeled-box-object :variable) value)
					    (s-value scrolling-labeled-box-object :original-value (gv scrolling-labeled-box-object :value)))))))
    (set variable-symbol original-value) ; Make sure that the var is set to a string.
    ;;Access the :value slot to initialize the formula in the slot and establish dependencies (Garnet Manual p.300).
    (gv gadget :value)
    (s-value gadget :value "")		; Reused gadgets will have parameters reflecting their last use, such as cursor positions. This seems to reset things.
    (s-value gadget :value original-value)
    (s-value gadget :width (labeled-menu-box-width gadget original-value))
    gadget))


(let* ((original-value "")
      (variable-symbol 'dummy1)
      (top 0) (label-string "")
      (gadget (create-instance
		  nil  garnet-gadgets:motif-scrolling-labeled-box
		(:original-value original-value)
		(:variable variable-symbol)
		(:top top)
					;		  (:left (cond ((member :label-left global-var-menu-list) (+ 20 (gv label :width)))
					;			       (t (gv prototype :left))))
		(:left 10)
		(:label-offset 10)
		;; (:font (o-formula *menu-font*))
		(:label-string label-string)
		(:selection-function #'(lambda (scrolling-labeled-box-object value)
					 (set (gv scrolling-labeled-box-object :variable) value)
					 (s-value scrolling-labeled-box-object :original-value (gv scrolling-labeled-box-object :value)))))))
  gadget)

(defvar *gadget*)
(setq *gadget* #k<KR-DEBUG:MOTIF-SCROLLING-LABELED-BOX-17125>)
(g-value *gadget* :field-text :string :cut-strings)
(s-value *gadget* :field-text :string :cut-strings
	 (o-formula 'foo))

(s-value #k<KR-DEBUG:MOTIF-SCROLLING-LABELED-BOX-18836> :field-text :string :cut-strings
    (o-formula
     (let* ((string (gvl :string)
	      )
	    (font (gvl :xfont))
	    ;; Structs will be NIL if formula has never been evaluated
	    (structs (g-value (gv :self) :cut-string-structs)))
       (printvars string font structs)
)))

(s-value #k<KR-DEBUG:MOTIF-SCROLLING-LABELED-BOX-18836> :field-text
	 :string (o-formula (let ((value (gvl :parent :value)))
			      (if value value ""))))

	 
(s-value #k<KR-DEBUG:MOTIF-SCROLLING-LABELED-BOX-18836> :field-text :string :cut-strings
    (o-formula
     (let* ((string (gvl :string))
	    (font (gvl :xfont))
	    ;; Structs will be NIL if formula has never been evaluated
	    (structs (g-value (gv :self) :cut-string-structs)))
       (do* ((old-structs structs (cdr old-structs))
	     (struct (car old-structs) (car old-structs))
	     (i -1 j)
	     (j 0)
	     (substring nil))
	    ((null i) (progn
			;; Throw away old cut-strings that we didn't use
			(when old-structs
			  (let ((last-cdr (nthcdr (- (length structs)
						     (length old-structs)
						     1)
						  structs)))
			    (setf (cdr last-cdr) NIL)))
			structs))
	 (format t "(position #\Newline string :start (1+ i)) = ~A~%"
		 (position #\Newline string :start (1+ i)))
	 (setf j (position #\Newline string :start (1+ i))
	       substring (if (or j substring)
			     (subseq string (1+ i) j)
			     string))
	 (multiple-value-bind (width dummy2 dummy3 left-bearing)
	     (gem:text-extents (or (gvl :window)
				   (gv device-info :current-root))
			       (gvl :font) substring)
	   (declare (ignore dummy2 dummy3))
	   ;; Reuse an old struct, if possible
	   (cond
	     (struct
	      (setf (cut-string-string struct) substring)
	      (setf (cut-string-width struct) width)
	      (setf (cut-string-left-bearing struct) left-bearing))
	     (t (setf structs
		      ;; Note: only append when we're adding a new line, and
		      ;; the object has never had this many lines.
		      (append structs
			      (list (make-cut-string
				     :string substring
				     :width width
				     :left-bearing left-bearing))))))))
       )
     )
    )


(s-values #k<KR-DEBUG:PLOT-WINDOW-729781> ; ph::plot-window
		 (y-bound-max (o-formula (+ (gvl :height) ph::*plot-window-xy-bound-border* ;100
					     )))
		 (y-bound-min (o-formula (- ph::*plot-window-xy-bound-border*)) ; -100
			       )
		 (x-bound-max (o-formula (+ (gvl :width) ph::*plot-window-xy-bound-border* ;100
					     )))
		 (x-bound-min (o-formula (- ph::*plot-window-xy-bound-border*)) ; -100
			      ))

(defmacro protocol-with-figs
    (label body)
  `(let ((*simulation-annotation* ,label)
	 (existing-plot-wins (all-plot-windows)))
     ,body
     (print-window-or-something
      (get-new-windows existing-plot-wins (all-plot-windows))
      ...
	 )))

(protocol-with-figs 'fig1 (goferit))


Plot distribution of electronic lengths





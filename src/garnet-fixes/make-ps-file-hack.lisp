;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

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

;; This file is modified from Garnet source code.

;; GARNET-FIXES Source file: make-ps-file-hack.lisp

;;; LBG 6/10/94 Edited Make-PS-File to always include the PostScript definitions for *LINE-FN*, *TEXT-FN*, *POLYLINE-FN*. 

(in-package "OPAL")


;; For some reason, calling this function on a info-window (which contains
;; GG:MOTIF-SCROLLING-WINDOW-WITH-BARS and OPAL:MULTIFONT-TEXT) neglects to add the postscript
;; definitions found in opal::*line-fn*, opal::*text-fn*, and opal::*polyline-fn* to the beginning
;; of the ps file. Therefore, we will just add these to:

;;    (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*
;;				  *line-fn* *text-fn* *polyline-fn*))

#|
(defun Make-PS-File (windows file-name
		     &key left top scale-x scale-y landscape-p
		          (paper-size :letter)
		          (position-x :center) (position-y :center)
		          (left-margin *1-inch*) (right-margin *1-inch*)
			  (top-margin *1-inch*) (bottom-margin *1-inch*)
		          (borders-p :motif)
			  (subwindows-p T) (clip-p T) (color-p T)
			  (background-color opal:white)
			  (title (if (schema-p windows)
				     (g-value windows :title)
				     "Untitled"))
			  (creator (concatenate 'string
				     "Make-PS-File -- Garnet Version "
				     user::Garnet-Version-Number))
			  (for "")
			  (comment "")
			  ;; For debugging the bbox computations
			  ;(debug NIL)
			  )
"Generate an Encapsulated PostScript file from Garnet windows.

Requires a Garnet WINDOW (or list of windows), and a PostScript
output FILE-NAME.  Optional arguments that affect the position
and appearance of the picture are:

POSITION-X - :LEFT, :CENTER, or :RIGHT. Default :CENTER.  

POSITION-Y - :TOP, :CENTER, or :BOTTOM.  Default :CENTER. 

LEFT-MARGIN, RIGHT-MARGIN, TOP-MARGIN, BOTTOM-MARGIN - Distance in
points, default 72.

LEFT, TOP - Distance in points, or default NIL to use POSITION-X
and POSITION-Y. 

SCALE-X, SCALE-Y - Scale factor for image.  Default is NIL, which means
the image will be automatically scaled to fit on the page.

LANDSCAPE-P - T or NIL, to rotate 90 degrees or portrait.  Default
is NIL.

PAPER-SIZE - :LETTER, :A4, or (WIDTH HEIGHT) in points specifies
page size. Default :LETTER.

SUBWINDOWS-P - T or NIL to include subwindows or not.  Default T.

BORDERS-P - T, NIL, :GENERIC, or :MOTIF, frames to print around windows.
Default :GENERIC.

CLIP-P - T, NIL, or (LEFT TOP WIDTH HEIGHT) in screen coordinates,
controls clipping.  Default T.

COLOR-P - T or NIL controls use of color.  Default T.

BACKGROUND-COLOR - Opal color, background fill.  Default is opal:white.

TITLE, CREATOR, FOR, COMMENT - Strings for header comments. 
"
  (let (region-left region-right region-top region-bottom
	region-width region-height)

    (if (not (listp windows))
	(setf windows (list windows)))

    ;; We call *temp-win* a "window", but it is really just a KR object
    ;; that has the same slots as a window.
    (setf *temp-win* (create-schema NIL
		       (:visible t)
		       (:background-color background-color)
		       (:border-width 0)
		       (:omit-title-bar-p t)
		       (:child windows)))
    (cond
      ((and clip-p (listp clip-p))
       (setf region-left (first clip-p))
       (setf region-top (second clip-p))
       (setf region-width (third clip-p))
       (setf region-height (fourth clip-p))
       (setf clip-p T)) ; Since the temp window has the same dimensions
			; as the clip region, just clip to the window
      (t
       (setf region-left opal:*screen-width*)
       (setf region-right 0)
       (setf region-top opal:*screen-height*)
       (setf region-bottom 0)

       ;; Figure maximum window size.
       (dolist (win (g-value *temp-win* :child))

	 (let* ((agg (g-value win :aggregate))
	        (wl (+ (g-value win :left)
		       (if clip-p 0 (if agg (g-value agg :left) 0))))
		(wt (+ (g-value win :top)
		       (if clip-p 0 (if agg (g-value agg :top) 0))))
		(wr (+ wl (if clip-p
			      (g-value win :width)
			      (if agg (g-value agg :width) 0))))
		(wb (+ wt (if clip-p
			      (g-value win :height)
			      (if agg (g-value agg :height) 0)))))

	   ;; Adjust for borders.
	   (multiple-value-bind
	    (border-left border-right border-top border-bottom)
	    (window-borders win borders-p)

	    (setf region-left
		  (min region-left wl))
	    (setf region-right
		  (max region-right
		       (+ wr border-left border-right)))
	    (setf region-top
		  (min region-top
		       (- wt border-top)))
	    (setf region-bottom
		  (max region-bottom
		       (+ wb border-bottom))))))

       (setf region-width (- region-right region-left))
       (setf region-height (- region-bottom region-top))))

    (s-value *temp-win* :left region-left)
    (s-value *temp-win* :top region-top)
    (s-value *temp-win* :width region-width)
    (s-value *temp-win* :height region-height)
    (s-value *temp-win* :border-width 0)
    (s-value *temp-win* :left-border-width 0)
    (s-value *temp-win* :right-border-width 0)
    (s-value *temp-win* :top-border-width 0)
    (s-value *temp-win* :bottom-border-width 0)
    (s-value *temp-win* :aggregate
	     (create-schema NIL
			    (:left 0) (:top 0)
			    (:width region-width)
			    (:height region-height)))

    (setf *clip-left* region-left)
    (setf *clip-right* region-right)
    (setf *clip-top* region-top)
    (setf *clip-bottom* region-bottom)
    (setf *color-p* color-p)
    (setf *font-list* NIL)

    ;; ** HERE IS THE CHANGE ** LBG 6/11/94
;;  (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*))
    (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*
				  *line-fn* *text-fn* *polyline-fn*))

    (setf *image-list* NIL)
    (setf *image-cnt* 0)
    (setf *piximage-list* NIL)
    (setf *piximage-cnt* 0)

    (when borders-p
      (pushnew *generic-window-frame-fn* *required-ps-fns*)
      (if (eq borders-p :motif)
	  (pushnew *motif-window-frame-fn* *required-ps-fns*)))
    (pushnew *rectangle-fn* *required-ps-fns*)

    (register-fns-in-win *temp-win* subwindows-p)

    ; Deal with the paper size
    (cond
      ((listp paper-size)
       (setf *page-width* (car paper-size)
	     *page-height* (cadr paper-size)))
      ((eq paper-size ':letter)
       (setf *page-width* 612
	     *page-height* 792))
      ((eq paper-size ':a4)
       (setf *page-width* 594
	     *page-height* 842))
      (t
       (error "unknown paper-size -- try :paper-size (width height)")))

    ; Rotate size 90 degrees if landscape.
    (if landscape-p
	(let ((temp *page-width*))
	  (setf *page-width* *page-height*)
	  (setf *page-height* temp)))

    ;; Adjust margins
    (setf *print-area-width* (- *page-width* left-margin right-margin))
    (setf *print-area-height* (- *page-height* top-margin bottom-margin))

    ;; Center window (or clipping region) on page by default
    ;; If not clipping, then look at the top-level aggregate instead of window
    (unless (and scale-x scale-y)
      (cond (scale-x
	     (setf scale-y (if (> region-height *print-area-height*)
			       (float (/ *print-area-height* region-height))
			       1)))
	    (scale-y
	     (setf scale-x (if (> region-width *print-area-width*)
			       (float (/ *print-area-width* region-width))
			       1)))
	    (t (setf scale-x (min 1 (float (/ *print-area-width*
					      region-width))
				  (float (/ *print-area-height*
					    region-height)))
		     scale-y scale-x))))

    ;; Adjust print area dimensions since it is getting scaled, too
    (setf *print-area-width* (ceiling (/ *print-area-width* scale-x)))
    (setf *print-area-height* (ceiling (/ *print-area-height* scale-y)))
    (unless left
      (setf left (case position-x
		   (:left (setf left 0))
		   (:center (setf left (round (- *print-area-width*
						 region-width) 2)))
		   (:right (setf left (- *print-area-width*
					 region-width))))))
    (unless top
      (setf top (case position-y
		  (:top (setf top 0))
		  (:center (setf top (round (- *print-area-height*
					       region-height) 2)))
		  (:bottom (setf top (- *print-area-height*
					region-height))))))

    ;; Compute compensation for windows with no title-bars
    (do* ((wins (g-value *temp-win* :child) (cdr wins))
	  (win (if wins (first wins)) (if wins (first wins)))
	  (this-left-thickness (if win (g-value win :left-border-width) 0)
			       (if win (g-value win :left-border-width) 0))
	  (this-top-thickness (if win (g-value win :top-border-width) 0)
			      (if win (g-value win :top-border-width) 0))
	  (left-thickness this-left-thickness
			  (max left-thickness this-left-thickness))
	  (top-thickness this-top-thickness
			 (max top-thickness this-top-thickness))
	  (left-different? NIL)
	  (top-different? NIL)
	  )
	 ((null wins) (progn
			(setf *thickest-left-border*
			      (if left-different? left-thickness 0))
			(setf *thickest-top-border*
			      (if top-different? top-thickness 0))))
      (unless (eq left-thickness this-left-thickness)
	(setf left-different? T))
      (unless (eq top-thickness this-top-thickness)
	(setf top-different? T)))

    ;; Compute boundingbox  llx = lower-left-x, urx = upper-right-x
    (cond
      (landscape-p
       (setf *llx* (round (+ left-margin (* top scale-y))))
       (setf *lly* (round (+ bottom-margin (* left scale-x))))
       (setf *urx* (round (+ *llx* (* region-height scale-y))))
       (setf *ury* (round (+ *lly* (* region-width scale-x)))))
      (t
       (setf *llx* (round (+ left-margin (* left scale-x))))
       (setf *lly* (round (+ bottom-margin (* (- *print-area-height*
						 (+ top region-height))
					      scale-y))))
       (setf *urx* (round (+ *llx* (* region-width scale-x))))
       (setf *ury* (round (+ *lly* (* region-height scale-y))))))

    ;; Now write everything to the file
    (Write-PS-To-File *temp-win* file-name
		      left top
		      scale-x scale-y landscape-p
		      left-margin bottom-margin
		      borders-p subwindows-p clip-p
		      title creator for comment NIL)
    ;; Reset color variable
    (setf *file-uses-color-p* NIL)

    ;; Clean up temporary window if one was allocated
    (when *temp-win*
      (s-value *temp-win* :child NIL)
      (opal:destroy (g-value *temp-win* :aggregate))
      (opal:destroy *temp-win*)
      (setf *temp-win* NIL)
    )
    T))
|#




(defun date-string ()
  (let (second minute hour date month year day daylight zone)
    (multiple-value-setq (second minute hour date month year day daylight zone)
      (get-decoded-time))
    (format nil "~d:~d, ~D ~D ~D"
	    hour
	    (cond ((= minute 0) "00")
		  ((< minute 10) (format nil "0~D" minute))
		  (t (format nil "~D" minute)))
	    (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	    date year)))


(defun write-ps-include-extra-info (file-name landscape-p
					      include-filename-and-date
					      page-comment)
  (when (or include-filename-and-date (> (length page-comment) 0))
	    
    (let ((file-date-string (convert-parentheses (format nil "~A   ~A" file-name (date-string)))))
							 
      (terpri)
      (format t "/Times-Roman findfont~%")
					; 6 is for font size in dots.
      (format t "6 scalefont setfont~%")
					; A4 page dimensions : 596x843, lower left is 0,0.
      (when page-comment
	(format t "35 0 moveto~%")	; Left hand side.
	(format t "(~A)" page-comment)
	(format t " show~%"))
	
      (when include-filename-and-date
	(format t "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
	(format t "%% Start of lower right corner filename and data....~%")
	(format t "~d 17 moveto~%"
		(round (-
			(if landscape-p *page-height* *page-width*)
			(* 3.0 (length file-date-string))))) ; 2.9 is approx char width for size 6.
	(format t "(~A)" file-date-string)
	(format t " show~%")
	(format t "%% End of lower right corner filename and data....~%")
	(format t "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~%")
	))))


      


(defun Write-PS-To-File (win file-name
			     include-filename-and-date
			     page-comment
			     left top
			     scale-x scale-y landscape-p
			     left-margin bottom-margin
			     borders-p subwindows-p clip-p
			     title creator for comment debug)
  (with-open-file (*standard-output* file-name :direction :output :if-exists :supersede)
    (header-comments title creator for comment)
    (prolog)
    (gsave)
    (when landscape-p
      (translate *page-height* 0)
      (ps-rotate 90))
    (translate left-margin bottom-margin)
    (scale scale-x scale-y)
    (translate left (- *print-area-height* top)) ; XXX
    (ps-window win borders-p subwindows-p clip-p)
    (grestore)
    (write-ps-include-extra-info
     file-name landscape-p include-filename-and-date page-comment)
    (trailer-comments debug)))
		  


;; LBG Modified 2/18/97 to handle individual window border specifications.
;; LBG Modified 3/16/98 to add filename and date to page
(defun Make-PS-File (windows file-name
			     &key left top scale-x scale-y landscape-p
			     include-filename-and-date
			     page-comment
			     (paper-size :letter)
			     (position-x :center) (position-y :center)
			     (left-margin *1-inch*) (right-margin *1-inch*)
			     (top-margin *1-inch*) (bottom-margin *1-inch*)
			     (borders-p :motif)
			     borders-p-for-windows
			     (subwindows-p T) (clip-p T) (color-p T)
			     (background-color opal:white)
			     (title (if (schema-p windows)
					(g-value windows :title)
					"Untitled"))
			     (creator (concatenate 'string
						   "Make-PS-File -- Garnet Version "
						   user::Garnet-Version-Number))
			     (for "")
			     (comment "")
			     ;; For debugging the bbox computations
					;(debug NIL)
			     )
  "Generate an Encapsulated PostScript file from Garnet windows.

Requires a Garnet WINDOW (or list of windows), and a PostScript
output FILE-NAME.  Optional arguments that affect the position
and appearance of the picture are:

POSITION-X - :LEFT, :CENTER, or :RIGHT. Default :CENTER.  

POSITION-Y - :TOP, :CENTER, or :BOTTOM.  Default :CENTER. 

LEFT-MARGIN, RIGHT-MARGIN, TOP-MARGIN, BOTTOM-MARGIN - Distance in
points, default 72.

LEFT, TOP - Distance in points, or default NIL to use POSITION-X
and POSITION-Y. 

SCALE-X, SCALE-Y - Scale factor for image.  Default is NIL, which means
the image will be automatically scaled to fit on the page.

LANDSCAPE-P - T or NIL, to rotate 90 degrees or portrait.  Default
is NIL.

PAPER-SIZE - :LETTER, :A4, or (WIDTH HEIGHT) in points specifies
page size. Default :LETTER.

SUBWINDOWS-P - T or NIL to include subwindows or not.  Default T.

BORDERS-P - T, NIL, :GENERIC, or :MOTIF, frames to print around windows.
Default :GENERIC.

BORDERS-P-FOR-WINDOWS - Appropriate when a list of windows is supplied
as the WINDOW arg. If supplied, BORDERS-P-FOR-WINDOWS is list of atoms
specifying whether or not to include frame around associated windows in
the WINDOW list. Otherwise, value of BORDERS-P is applied to all windows.

CLIP-P - T, NIL, or (LEFT TOP WIDTH HEIGHT) in screen coordinates,
controls clipping.  Default T.

COLOR-P - T or NIL controls use of color.  Default T.

BACKGROUND-COLOR - Opal color, background fill.  Default is opal:white.

TITLE, CREATOR, FOR, COMMENT - Strings for header comments. 
"
  ;; 2/18/97 LBG *temp-win* with multiple children has individual border info in :BORDERS-P slot.

  (if (not (listp windows))
      (setf windows (list windows)))
  (let* ((borders-p-for-windows (if (= (length borders-p-for-windows) (length windows))
				    borders-p-for-windows
				    (loop repeat (length windows) collect borders-p)))
	 (borders-p (loop for val in borders-p-for-windows when val return t))
	 region-left region-right region-top region-bottom
	 region-width region-height)
    ;;    (format t "borders-p = ~A, borders-p-for-windows = ~A~%" borders-p borders-p-for-windows)
	   

    ;; We call *temp-win* a "window", but it is really just a KR object
    ;; that has the same slots as a window.
    (setf *temp-win* (create-schema NIL
				    (:visible t)
				    (:background-color background-color)
				    (:border-width 0)
				    (:omit-title-bar-p t)
				    (:borders-p borders-p-for-windows)
				    (:child windows)))
    (cond
      ((and clip-p (listp clip-p))
       (setf region-left (first clip-p))
       (setf region-top (second clip-p))
       (setf region-width (third clip-p))
       (setf region-height (fourth clip-p))
       (setf clip-p T))			; Since the temp window has the same dimensions
					; as the clip region, just clip to the window
      (t
       (setf region-left opal:*screen-width*)
       (setf region-right 0)
       (setf region-top opal:*screen-height*)
       (setf region-bottom 0)
       ;; Figure maximum window size.
       ;; 2/18/97 LBG *temp-win* with multiple children has individual border info in :BORDERS-P slot.
       (loop for win in (g-value *temp-win* :child)
	     for borders-p in (g-value *temp-win* :borders-p) do

	     (let* ((agg (g-value win :aggregate))
		    (wl (+ (g-value win :left)
			   (if clip-p 0 (if agg (g-value agg :left) 0))))
		    (wt (+ (g-value win :top)
			   (if clip-p 0 (if agg (g-value agg :top) 0))))
		    (wr (+ wl (if clip-p (g-value win :width) (if agg (g-value agg :width) 0))))
		    (wb (+ wt (if clip-p (g-value win :height) (if agg (g-value agg :height) 0)))))
	       ;; Adjust for borders.
	       (multiple-value-bind (border-left border-right border-top border-bottom)
		   (window-borders win borders-p)

		 (setf region-left (min region-left wl))
		 (setf region-right (max region-right (+ wr border-left border-right)))
		 (setf region-top (min region-top (- wt border-top)))
		 (setf region-bottom (max region-bottom (+ wb border-bottom))))))

       (setf region-width (- region-right region-left))
       (setf region-height (- region-bottom region-top))))

    (s-value *temp-win* :left region-left)
    (s-value *temp-win* :top region-top)
    (s-value *temp-win* :width region-width)
    (s-value *temp-win* :height region-height)
    (s-value *temp-win* :border-width 0)
    (s-value *temp-win* :left-border-width 0)
    (s-value *temp-win* :right-border-width 0)
    (s-value *temp-win* :top-border-width 0)
    (s-value *temp-win* :bottom-border-width 0)
    (s-value *temp-win* :aggregate (create-schema NIL (:left 0) (:top 0) (:width region-width) (:height region-height)))

    (setf *clip-left* region-left)
    (setf *clip-right* region-right)
    (setf *clip-top* region-top)
    (setf *clip-bottom* region-bottom)
    (setf *color-p* color-p)
    (setf *font-list* NIL)

    ;; ** HERE IS THE CHANGE ** LBG 6/11/94
    ;;  (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn*))
    (setf *required-ps-fns* (list *fillshape-fn* *strokeshape-fn* *clip-fn* *line-fn* *text-fn* *polyline-fn*))

    (setf *image-list* NIL)
    (setf *image-cnt* 0)
    (setf *piximage-list* NIL)
    (setf *piximage-cnt* 0)
    (when borders-p
      (pushnew *generic-window-frame-fn* *required-ps-fns*)
      (when (eq borders-p :motif) (pushnew *motif-window-frame-fn* *required-ps-fns*)))
    (pushnew *rectangle-fn* *required-ps-fns*)

    (register-fns-in-win *temp-win* subwindows-p)

					; Deal with the paper size
    (cond
      ((listp paper-size)
       (setf *page-width* (car paper-size)
	     *page-height* (cadr paper-size)))
      ((eq paper-size ':letter)
       (setf *page-width* 612
	     *page-height* 792))
      ((eq paper-size ':a4)
       (setf *page-width* 594
	     *page-height* 842))
      (t
       (error "unknown paper-size -- try :paper-size (width height)")))

					; Rotate size 90 degrees if landscape.
    (if landscape-p
	(let ((temp *page-width*))
	  (setf *page-width* *page-height*)
	  (setf *page-height* temp)))

    ;; Adjust margins
    (setf *print-area-width* (- *page-width* left-margin right-margin))
    (setf *print-area-height* (- *page-height* top-margin bottom-margin))

    ;; Center window (or clipping region) on page by default
    ;; If not clipping, then look at the top-level aggregate instead of window
    (unless (and scale-x scale-y)
      (cond (scale-x
	     (setf scale-y (if (> region-height *print-area-height*)
			       (float (/ *print-area-height* region-height))
			       1)))
	    (scale-y
	     (setf scale-x (if (> region-width *print-area-width*)
			       (float (/ *print-area-width* region-width))
			       1)))
	    (t (setf scale-x (min 1 (float (/ *print-area-width*
					      region-width))
				  (float (/ *print-area-height*
					    region-height)))
		     scale-y scale-x))))

    ;; Adjust print area dimensions since it is getting scaled, too
    (setf *print-area-width* (ceiling (/ *print-area-width* scale-x)))
    (setf *print-area-height* (ceiling (/ *print-area-height* scale-y)))
    (unless left
      (setf left (case position-x
		   (:left (setf left 0))
		   (:center (setf left (round (- *print-area-width*
						 region-width) 2)))
		   (:right (setf left (- *print-area-width*
					 region-width))))))
    (unless top
      (setf top (case position-y
		  (:top (setf top 0))
		  (:center (setf top (round (- *print-area-height*
					       region-height) 2)))
		  (:bottom (setf top (- *print-area-height*
					region-height))))))

    ;; Compute compensation for windows with no title-bars
    (do* ((wins (g-value *temp-win* :child) (cdr wins))
	  (win (if wins (first wins)) (if wins (first wins)))
	  (this-left-thickness (if win (g-value win :left-border-width) 0)
			       (if win (g-value win :left-border-width) 0))
	  (this-top-thickness (if win (g-value win :top-border-width) 0)
			      (if win (g-value win :top-border-width) 0))
	  (left-thickness this-left-thickness
			  (max left-thickness this-left-thickness))
	  (top-thickness this-top-thickness
			 (max top-thickness this-top-thickness))
	  (left-different? NIL)
	  (top-different? NIL)
	  )
	 ((null wins) (progn
			(setf *thickest-left-border*
			      (if left-different? left-thickness 0))
			(setf *thickest-top-border*
			      (if top-different? top-thickness 0))))
      (unless (eq left-thickness this-left-thickness)
	(setf left-different? T))
      (unless (eq top-thickness this-top-thickness)
	(setf top-different? T)))

    ;; Compute boundingbox  llx = lower-left-x, urx = upper-right-x
    (cond
      (landscape-p
       (setf *llx* (round (+ left-margin (* top scale-y))))
       (setf *lly* (round (+ bottom-margin (* left scale-x))))
       (setf *urx* (round (+ *llx* (* region-height scale-y))))
       (setf *ury* (round (+ *lly* (* region-width scale-x)))))
      (t
       (setf *llx* (round (+ left-margin (* left scale-x))))
       (setf *lly* (round (+ bottom-margin (* (- *print-area-height*
						 (+ top region-height))
					      scale-y))))
       (setf *urx* (round (+ *llx* (* region-width scale-x))))
       (setf *ury* (round (+ *lly* (* region-height scale-y))))))

    ;; Now write everything to the file
    (Write-PS-To-File *temp-win* file-name
		      include-filename-and-date
		      page-comment
		      left top
		      scale-x scale-y landscape-p
		      left-margin bottom-margin
		      borders-p subwindows-p clip-p
		      title creator for comment NIL)
    ;; Reset color variable
    (setf *file-uses-color-p* NIL)

    ;; Clean up temporary window if one was allocated
    (when *temp-win*
      (s-value *temp-win* :child NIL)
      (opal:destroy (g-value *temp-win* :aggregate))
      (opal:destroy *temp-win*)
      (setf *temp-win* NIL)
      )
    T))





     
(defvar *PRINT-NON-VISIBLE-WINDOWS* t "When T PS-WINDOW ignores the :visible slot of a window when printing.")

;; LBG Modified 2/18/97 to handle individual window border specifications.
;;     Modified 1/7/99 to allow printing of non-visible windows, via global *PRINT-NON-VISIBLE-WINDOWS*.
(defun ps-window (win borders-p subwindows-p clip-p)
  (when (or *PRINT-NON-VISIBLE-WINDOWS* (g-value win :visible))
    (let* ((agg (g-value win :aggregate))
	   (natural-p (or clip-p (null agg)))
	   (left (+ (g-value win :left)
		    (if natural-p 0 (g-value agg :left))))
	   (top (+ (g-value win :top)
		   (if natural-p 0 (g-value agg :top))))
	   (width (if natural-p
		      (g-value win :width)
		      (g-value agg :width)))
	   (height (if natural-p
		       (g-value win :height)
		       (g-value agg :height)))
	   (top-level-p (member win (g-value *temp-win* :child)))
	   (old-clip-left *clip-left*)
	   (old-clip-right *clip-right*)
	   (old-clip-top *clip-top*)
	   (old-clip-bottom *clip-bottom*))
      (multiple-value-bind
	    (border-left border-right border-top border-bottom)
	  (window-borders win borders-p)

					; XXX: TODO: if window bbox (with borders) doesn't overlap
					; clip bbox, then don't draw

	;; LBG 1.12.20 So that we can trace individual windows in multi-window ps files.
	(format t "~A" (convert-parentheses (format nil "~%%~%% Begin new window ~A~%%~%" (g-value win :title))))

	(gsave)

	(if top-level-p
	    (translate (- (+ left border-left)
			  (g-value *temp-win* :left)
			  (if (and (g-value win :omit-title-bar-p)
				   (eq borders-p T))
			      *thickest-left-border*
			      0))
		       (- (g-value *temp-win* :top)
			  (- top (if (g-value win :omit-title-bar-p)
				     *thickest-top-border*
				     0))))
	    (unless (eq win *temp-win*)
	      (translate (+ left border-left)
			 (- border-top top)))
	    )

	(when (and borders-p
		   (not (eq win *temp-win*)))
	  (add-font-to-list *window-frame-font*)
	  (cond
	    ((or (eq borders-p t)
		 (g-value win :omit-title-bar-p)
		 (not top-level-p))
	     (format T "() 0 0 ~S ~S ~S ~S ~S ~S DrawGenericWindowFrame~%"
		     width height
		     border-left border-top border-right border-bottom))

	    ((eq borders-p :generic)
	     (format T "(~A) 0 0 ~S ~S ~S ~S ~S ~S DrawGenericWindowFrame~%"
		     (g-value win :title)
		     width height
		     border-left border-top border-right border-bottom))

	    ((eq borders-p :motif)
	     (format T "(~A) 0 0 ~S ~S ~S ~S ~S ~S DrawMotifWindowFrame~%"
		     (g-value win :title)
		     width height
		     border-left border-top border-right border-bottom))))

	;; Clip everything in this window (including subwindows) into the window
	(format T "0 0 ~S ~S ClipToRectangle~%" width height)
	(setf *clip-top* 0)
	(setf *clip-bottom* height)
	(setf *clip-left* 0)
	(setf *clip-right* width)

	;; Print the meat of the window
	(let ((top-agg (g-value win :aggregate)))
	  (gsave)
	  (if (not clip-p)
	      (translate (- (g-value top-agg :left))
			 (g-value top-agg :top)))
	  ;; maybe pass the window background color to the frame proc 
	  (print-window-background win)
	  (when top-agg
	    (kr-send top-agg :ps-object top-agg))
	  (grestore))

					;       (format t "(g-value win :borders-p) = ~A~%" (g-value win :borders-p))
	;; Print subwindows
	(when (or subwindows-p
		  (eq win *temp-win*))

	  ;; 2/18/97 LBG *temp-win* with multiple children has individual border info in :BORDERS-P slot.
	  (loop for child in (g-value win :child)
		for borders-p in (g-value win :borders-p) do
		(ps-window child borders-p subwindows-p clip-p)))

	(setf *clip-top* old-clip-top)
	(setf *clip-bottom* old-clip-bottom)
	(setf *clip-left* old-clip-left)
	(setf *clip-right* old-clip-right)

	(grestore)))))



(in-package :xlib)

;; (macroexpand '(set-buffer-offset current-boffset))

(defun xlib::radians->int16 (value)
  ;; Short floats are good enough
  (declare (type xlib::angle value))
  (declare (xlib::clx-values xlib::int16))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (kernel:%unary-round (/ (the float value) #.(float (/ pi 180.0s0 64.0s0) 0.0s0))))

(defun draw-arc (drawable gcontext x y width height angle1 angle2 &optional fill-p
			  full-circle-p	; LBG Add this flag to avoid expensive angle calculations.
			  )
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (optimize (speed 3) (safety 0)))
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x y)
	   (type card16 width height)
	   (type angle angle1 angle2)
	   (type generalized-boolean fill-p))
  (let ((display (drawable-display drawable))
	(request (if fill-p +x-polyfillarc+ +x-polyarc+)))
    (declare (type display display)
	     (type card16 request))
    (with-display
     (display)
     (force-gcontext-changes-internal gcontext)
     (with-buffer-output
      (display :length +requestsize+*)
      (let* ((last-request-byte (display-last-request display))
	     (current-boffset buffer-boffset))
	;; To append or not append, that is the question
	(if (and (not *inhibit-appending*)
		 last-request-byte
		 ;; Same request?
		 (= (aref-card8 buffer-bbuf last-request-byte) request)
		 (progn;; Set buffer pointers to last request
		   (set-buffer-offset last-request-byte)
		   ;; same drawable and gcontext?
		   (or (compare-request (4)
					(drawable drawable)
					(gcontext gcontext))
		       (progn;; If failed, reset buffer pointers
			 (set-buffer-offset current-boffset)
			 nil))))
	  ;; Append request
	  (progn
	    ;; Set new request length		
	    (card16-put 2 (index+ 3 (index-ash (index- current-boffset last-request-byte) -2)))
	    (set-buffer-offset current-boffset)
	    (put-items (0)		; Insert new point
		       (int16 x y)
		       (card16 width height)
		       (angle angle1 angle2))
	    (setf (display-boffset display) (index+ buffer-boffset 12)))
	  ;; New Request
	  (progn
	    (if full-circle-p
	      (progn
		(put-items
		 (4)
		 (code request)
		 (length 6)
		 (drawable drawable)
		 (gcontext gcontext)
		 (int16 x y)
		 (card16 width height))
		  
		;;  (CHECK-PUT 20 ANGLE1 ANGLE)
		(ASET-INT16 (THE INT16 0;; (RADIANS->INT16 ANGLE1)
				 )
			    (THE (SIMPLE-ARRAY INT16 (*)) BUFFER-BBUF)
			    (INDEX+ BUFFER-BOFFSET 20))
		;;  (CHECK-PUT 22 ANGLE2 ANGLE)
		(ASET-INT16 (THE INT16 23040;; (RADIANS->INT16 ANGLE2)
				 )
			    (THE (SIMPLE-ARRAY INT16 (*)) BUFFER-BBUF)
			    (INDEX+ BUFFER-BOFFSET 22)))
	      (put-items
	       (4)
	       (code request)
	       (length 6)
	       (drawable drawable)
	       (gcontext gcontext)
	       (int16 x y)
	       (card16 width height)
	       (angle angle1 angle2)))
	    (buffer-new-request-number display)
	    (setf (buffer-last-request display) buffer-boffset)
	    (setf (display-boffset display) (index+ buffer-boffset 24)))))))
    (display-invoke-after-function display)))



(defun draw-lines (drawable gcontext points &key relative-p fill-p (shape :complex))
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points) ;(repeat-seq (integer x) (integer y))
	   (type generalized-boolean relative-p fill-p)
	   (type (member :complex :non-convex :convex) shape))
;;  (format t "Drawing ~A~%" points)
  (if fill-p
      (fill-polygon drawable gcontext points relative-p shape)
    (with-buffer-request ((drawable-display drawable) +x-polyline+ :gc-force gcontext)
      ((data boolean) relative-p)
      (drawable drawable)
      (gcontext gcontext)
      ((sequence :format int16) points))))

(in-package :xlib)

;; (macroexpand '(set-buffer-offset current-boffset))

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
    (with-display (display)
      (force-gcontext-changes-internal gcontext)
      (with-buffer-output
       (display :length +requestsize+)
       (draw-arc-inner-foo ANGLE1 ANGLE2 BUFFER-BBUF BUFFER-BOFFSET DISPLAY DRAWABLE FULL-CIRCLE-P GCONTEXT HEIGHT REQUEST WIDTH X Y)		  

))
    (display-invoke-after-function display)))



(defun draw-arc-inner-foo (ANGLE1 ANGLE2 BUFFER-BBUF BUFFER-BOFFSET DISPLAY DRAWABLE FULL-CIRCLE-P GCONTEXT HEIGHT REQUEST WIDTH X Y)		  
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
      (draw-arc-inner-request ANGLE1 ANGLE2 BUFFER-BBUF BUFFER-BOFFSET CURRENT-BOFFSET DISPLAY HEIGHT LAST-REQUEST-BYTE WIDTH X Y)


      ;; New Request

      (progn
	(if full-circle-p

	  (new-full-circle-p-foo BUFFER-BBUF BUFFER-BOFFSET DRAWABLE GCONTEXT HEIGHT REQUEST WIDTH X Y)

		
	  (put-items
	   (4)
	   (code request)
	   (length 6)
	   (drawable drawable)
	   (gcontext gcontext)
	   (int16 x y)
	   (card16 width height)
	   (angle angle1 angle2)))
	(barfoo-buffer-new-request-number display buffer-boffset)


	      
	)


      )))


;(macroexpand '(set-buffer-offset current-boffset))

(defun draw-arc-inner-request (ANGLE1 ANGLE2 BUFFER-BBUF BUFFER-BOFFSET CURRENT-BOFFSET DISPLAY HEIGHT LAST-REQUEST-BYTE WIDTH X Y)
  (progn
    ;; Set new request length
					; (draw-arc-inner-request-1-foo BUFFER-BBUF BUFFER-BOFFSET CURRENT-BOFFSET LAST-REQUEST-BYTE)
    (card16-put 2 (index+ 3 (index-ash (index- current-boffset last-request-byte) -2)))
    (LET ((XLIB::.BOFFSET. XLIB::CURRENT-BOFFSET))
	 (DECLARE (TYPE XLIB:ARRAY-INDEX XLIB::.BOFFSET.))
	 (SETQ XLIB::BUFFER-BOFFSET XLIB::.BOFFSET.))
;    (set-buffer-offset current-boffset)
    (draw-arc-inner-request-1-bar ANGLE1 ANGLE2 BUFFER-BBUF BUFFER-BOFFSET DISPLAY HEIGHT WIDTH X Y)

))


#|

(macroexpand '(CHECK-PUT 0 X INT16))
(macroexpand '(CHECK-PUT 2 Y INT16))
(macroexpand '(CHECK-PUT 4 WIDTH CARD16))
(macroexpand '(CHECK-PUT 6 HEIGHT CARD16))
(macroexpand '(CHECK-PUT 8 ANGLE1 ANGLE))

(defun draw-arc-inner-request-1-bar (ANGLE1 ANGLE2 BUFFER-BBUF BUFFER-BOFFSET DISPLAY HEIGHT WIDTH X Y)
  (put-items (0)			; Insert new point
	     (int16 x y)
	     (card16 width height)
	     (angle angle1 angle2))
  (setf (display-boffset display) (index+ buffer-boffset 12)))




(defun new-full-circle-p-foo (BUFFER-BBUF BUFFER-BOFFSET DRAWABLE GCONTEXT HEIGHT REQUEST WIDTH X Y)
  (progn
    (put-items
     (4)
     (code request)
     (length 6)
     (drawable drawable)
     (gcontext gcontext)
     (int16 x y)
     (card16 width height)
     )
		  
    ;;  (CHECK-PUT 20 ANGLE1 ANGLE)
    (ASET-INT16 (THE INT16 0;; (RADIANS->INT16 ANGLE1)
		     )
		(THE (SIMPLE-ARRAY INT16 (*)) BUFFER-BBUF)
		(INDEX+ BUFFER-BOFFSET 20))
    ;;  (CHECK-PUT 22 ANGLE2 ANGLE)
    (ASET-INT16 (THE INT16 23040;; (RADIANS->INT16 ANGLE2)
		     )
		(THE (SIMPLE-ARRAY INT16 (*)) BUFFER-BBUF)
		(INDEX+ BUFFER-BOFFSET 22))

    ))



(defun barfoo-buffer-new-request-number (display buffer-boffset)
  (buffer-new-request-number display)
  (setf (buffer-last-request display) buffer-boffset)
  	      (setf (display-boffset display) (index+ buffer-boffset 24)))

;; (macroexpand '(index+ buffer-boffset 24))

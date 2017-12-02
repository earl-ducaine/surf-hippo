;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-

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

;; GUI Source file: windows-hack.lisp


(IN-PACKAGE "WINDOWS-HACK")


(defvar *delete-delay-after-print* 2 "Delay erasing PS files after printing, in seconds. Needs to be empirically determined.") 
(defvar *gc-every-print* nil)
(defvar *enable-printing* t)

(defvar *include-printer-flags* t)
;; For Solaris, *features* also has :sunos (as well as :solaris), so look for :solaris first.
(defvar *shell-lpr-command* (cond ((find :solaris *features*) "lp")
				  ((find :sunos *features*) "lpr")
				  (t "lpr")))
				  
(defvar *shell-lpr-printer-flag* (cond ((find :solaris *features*) "-d")
				       ((find :sunos *features*) "-P")
				       (t "-P"))) 

(defvar *PS-FILENAME-SUFFIX* "" "Added to .ps filenames")
(defvar *maximum-ps-filename-length* nil "When NIL, no limit. Some problems with VMS server based printers.suggest setting this to 32.")

;; Put all the garnet windows in the print menu, otherwise only the windows in *output-windows*.
(defvar *all-windows-to-print-window-menu* nil)

(defvar *print-now* t)
(defvar *print-windows-what-to-do* :print-now)
(defvar *print-together* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;   Some Postscript file creation variables.   ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *ps-landscape-p* nil "Sets landscape [T] or portrait [NIL] for PS files.")
(defvar *ps-borders-p* t "T, NIL, :GENERIC, or :MOTIF, frames to print around windows.")
(defvar *ps-color-p* t "Enable color information in PS file.")
(defvar *ps-left-margin* 72 "Distance in points for left margin in PS files.")
(defvar *ps-right-margin* 72 "Distance in points for right margin in PS files.")
(defvar *ps-top-margin* 72 "Distance in points for top margin in PS files.")
(defvar *ps-bottom-margin* 72 "Distance in points for bottom margin in PS files.")

;; Empirical values for "killing" the ps margins with *KILL-PS-MARGINS*.
(defvar *minimum-ps-left-margin* 10)
(defvar *minimum-ps-right-margin* 10)
(defvar *minimum-ps-top-margin* 10)
(defvar *minimum-ps-bottom-margin* 10)	; To allow room for the filename-and-date-in-ps-files



(defvar *ps-header-print-p* nil)	;add header page to printed ps files.
(defvar *lpr-paper-size* :a4 ":LETTER, :A4, or (WIDTH HEIGHT) in points specifies page size.")
(defvar *lpr-paper-width* 594)		; for a4, letter is 612
(defvar *lpr-paper-height* 842)		; for a4, letter is 792
(defvar *ps-position-x* :center ":LEFT, :CENTER, or :RIGHT.")
(defvar *ps-position-y* :center ":TOP, :CENTER, or :BOTTOM.")
(defvar *ps-scale-x* nil "X scale factor for image for making PS files. Default is NIL, which means the image will be automatically scaled to fit on the page.") 
(defvar *ps-scale-y* nil "Y scale factor for image for making PS files. Default is NIL, which means the image will be automatically scaled to fit on the page.")
(defvar *ps-file-page-comment* "" "A string added to the lower left corner of all PS files.")
(defvar *kill-ps-margins* nil "When T minimize all margins for PS files.")
(defvar *include-filename-and-date-in-ps-files* t "Include filename and date in lower right corner of PS files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *kill-multiple-window-printing* nil) ; In some setups printing more than one window at a time may crash the system.
(defvar *flag-windows-for-borders* nil)

;; List of strings for which the titles of windows passed to PRINT-WINDOWS are checked - if a title contains one of the strings, then that window is not printed. 
(defvar *print-windows-exclusion-list* nil)

;; List of strings for which the titles of windows passed to PRINT-WINDOWS are checked - if a title contains one of the strings, then that window is printed. 
(defvar *print-windows-inclusion-list* nil)
(defvar *print-windows-include-title* nil)

;; (defvar *hard-copy-screen nil)

(defun print-window-menu (&optional pre-selected-window-list)
  ;; Returns a list of windows selected from all the opal windows except for menu windows.
  (let ((candidates (if *all-windows-to-print-window-menu* (garnet-debug:windows) *output-windows*)))
    (if (and (length candidates) (> (length candidates) 1))
	(window-selection-menu "Select Windows For Saving As .PS Files" candidates pre-selected-window-list)
	candidates)))

(defun make-ps-file (window-or-window-list filename &optional borders-p-for-windows)
  (let (*print-pretty*)
    (opal:make-ps-file window-or-window-list filename
		       :include-filename-and-date *include-filename-and-date-in-ps-files*
		       :page-comment *ps-file-page-comment*
		       :borders-p-for-windows borders-p-for-windows
		       :position-x *ps-position-x*
		       :position-y *ps-position-y*
		       :landscape-p *ps-landscape-p*
		       :borders-p *ps-borders-p*
		       :color-p *ps-color-p*
		       :left-margin *ps-left-margin*
		       :right-margin *ps-right-margin*
		       :top-margin *ps-top-margin*
		       :bottom-margin *ps-bottom-margin*
		       :paper-size *lpr-paper-size*
		       :SCALE-X *ps-scale-x*   :SCALE-Y *ps-scale-y*)
    (unless (probe-file filename) (sim-error (format nil "PS file ~A not created!" filename)))))

(defun mouse-print-window (filename) (make-ps-file (nth 1 (garnet-debug::ident)) filename))

(defun ps-file-options-menu ()
  (let ((dummy1 (if (numberp *ps-scale-x*) *ps-scale-x* 1.0))
	(dummy2 (numberp *ps-scale-x*))
	(dummy3 (if (numberp *ps-scale-y*) *ps-scale-y* 1.0))
	(dummy4 (numberp *ps-scale-y*))
	(dummy5 (case *lpr-paper-size*
		  ((:letter :a4) *lpr-paper-size*)
		  (t :arbitrary)))
	(dummy6 (if *ps-landscape-p* :landscape :portrait)))
    (choose-variable-values
     `(					; (*ps-header-print-p* "Print with header page" :boolean)
       (dummy6 "Image orientation:" :choose (:landscape :portrait) :label-left)
       (*ps-borders-p* "Include window borders" :boolean)
       (*flag-windows-for-borders* "Flag individual windows for borders" :boolean)
       (*ps-color-p* "Include colors in PS file" :boolean)
       (*printer* "Specify printer" :string)
       (*kill-ps-margins* "Kill margins" :boolean)
       (*shell-lpr-command* "Shell line printer command" :string)
       (dummy2 "Set X scale to number below" :boolean)
       (dummy1 "Scale X:" :float)
       (dummy4 "Set Y scale to number below" :boolean)
       (dummy3 "Scale Y:" :float)
       (*include-printer-flags* "Include printer command line flags" :boolean)
       (*include-filename-and-date-in-ps-files* "Include filename and date in ps files" :boolean)
       (*ps-file-page-comment* "Comment to add to lower left hand corner of ps files" :string)
       (dummy5 "Paper size:" :choose (:letter :a4 :arbitrary) :label-left)
       (*lpr-paper-width* "Paper width in points (when Size = Arbitrary)" :integer)
       (*lpr-paper-height* "Paper height in points (when Size = Arbitrary)" :integer))		      
     :label "Postscript File Options")
    (setq *ps-landscape-p* (eq dummy6 :landscape))
    (setq *lpr-paper-size*
	  (case dummy5
	    ((:letter :a4)  dummy5)
	    (:arbitrary (list *lpr-paper-width*   *lpr-paper-height*))))
    (setq *ps-scale-x* (and dummy2 dummy1)
	  *ps-scale-y* (and dummy4 dummy3))
    nil))

(defun select-and-filter-printed-windows (windows-to-print exclusion-list inclusion-list)
  (cond-every (exclusion-list
	       (setq windows-to-print 
		     (loop for win in windows-to-print collect
			   (loop for exclude-title-fragment in exclusion-list 
				 when (search exclude-title-fragment (gv win :title)) do (return nil)
				 finally (return win)))))
	      (inclusion-list
	       (setq windows-to-print 
		     (loop for win in windows-to-print collect
			   (loop for include-title-fragment in inclusion-list 
				 when (search include-title-fragment (gv win :title)) do (return win)
				 finally (return nil))))))
  (clean-up-list windows-to-print))

(defun print-window-options-comment ()
  (concatenate 'string
	       (format nil
		       " -- Current Settings --~% ~A mode~% ~A ~% ~A~% Print shell command \"~A\"~% ~A~% Printer: ~A"
		       (if *ps-landscape-p* "Landscape" "Portrait")
		       (if *ps-borders-p* "Include window borders" "Omit window borders")
		       (if *include-printer-flags* "Include shell command flags" "Omit shell command flags")
		       *shell-lpr-command*
		       (if *kill-ps-margins* "Kill ps margins" "Keep ps margins")
		       (or *printer* "default"))
	       (if (or *ps-scale-x* *ps-scale-y*)
		   (format nil "~%Scale-x: ~A, SCALE-Y: ~A" *ps-scale-x* *ps-scale-y*)
		   "")))

(defun reorder-windows-to-print (windows-to-print)
  (let ((reordered-list (reorder-list-menu windows-to-print (loop for win in windows-to-print collect (gv win :title)) "Restack Printed Windows: 1 => Bottom")))
    (setq *output-windows* (delete-duplicates (nconc *output-windows* reordered-list)))
    reordered-list))

(defun determine-borders-p-for-windows (windows-to-print)
  (when *flag-windows-for-borders*
    (let ((wins-w-borders (window-selection-menu "Select Windows with Printed Borders" windows-to-print (when *ps-borders-p* windows-to-print))))
      (loop for win in windows-to-print collect (true-p (member win wins-w-borders))))))

(defun get-candidate-ps-windows-filename (pathname-directory windows-to-print filename-extra)
  (when windows-to-print
    (let* ((windows-to-print (coerce-to-list windows-to-print))
	   (top-window (car windows-to-print))
	   (candidate (concatenate 'string  
				  (if (and (> (length windows-to-print) 1)
					   (> (length (gv top-window :session-name)) 1))
				      (gv top-window :session-name)
				      (strip-displayed-host-name-from-title (gv top-window :title)))
				  filename-extra)))
      (concatenate 'string
		   pathname-directory
		   (make-nice-filename
		    (string-tail candidate *MAXIMUM-PS-FILENAME-LENGTH*))
		   ".ps"))))

(defun generate-ps-files (windows-to-print &key directory filename-extra print-together-filename)
  (when windows-to-print
    (let* ((pathname-directory (fixup-pathname-directory directory))
	   (borders-p-for-windows (determine-borders-p-for-windows windows-to-print))
	   (multiple-windows-p (> (length windows-to-print) 1)))
      (if (or *print-together* print-together-filename (not multiple-windows-p))
	  (let ((dummy1 (or (when (and (stringp print-together-filename)
				       (> (length print-together-filename) 0))
			      (concatenate 'string pathname-directory (make-nice-filename print-together-filename) ".ps"))
			    (get-candidate-ps-windows-filename pathname-directory windows-to-print filename-extra)))
		dummy2)
	    (choose-variable-values `((dummy1 "Filename" :string)
				      (dummy2 "CANCEL" :boolean))
				    :label "Postscript Filename")
	    (unless dummy2
	      (when (write-file-overwrite-authorization dummy1)
		(unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
		(when (ext:unix-namestring pathname-directory nil)
		  (make-ps-file windows-to-print dummy1 borders-p-for-windows)
		  (format t ";; File ~a written~%" dummy1)
		  (list dummy1)))))
	  (no-nils
	   (loop for window-to-print in windows-to-print
		 for i upfrom 0
		 collecting
		 (let ((filename (get-candidate-ps-windows-filename pathname-directory window-to-print filename-extra)))
		   (when (write-file-overwrite-authorization filename)
		     (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
		     (when (ext:unix-namestring pathname-directory nil)
		       (make-ps-file window-to-print filename)
		       (format t ";; File ~a written~%" filename)
		       filename)))))))))

(defun make-lpr-flags-string ()
  (concatenate `string			; (if (not *ps-header-print-p*) "-h ")
	       (when (> (length *printer*) 0)
		 (format nil "~a ~a" *shell-lpr-printer-flag* (string *printer*)))))

(defun print-windows-convert-what-to-do-to-choose-symbol (what-to-do)
  (case what-to-do
    (:JUST-WRITE-&-DESTROY :JUST_WRITE_&_DESTROY)
    (:Just-write :Just_write)
    (:Print-now :Print_now)
    (:print-&-destroy :print_&_destroy)
    (t :NO_FILE/PRINT)))

(defun print-windows-convert-choose-symbol-to-what-to-do (choose-symbol)
  (case choose-symbol
    (:JUST_WRITE_&_DESTROY :JUST-WRITE-&-DESTROY)
    (:Just_write :Just-write)
    (:Print_now :Print-now )
    (:print_&_destroy :print-&-destroy)
    (t :NO_FILE/PRINT)))

(defvar *print-windows-use-automatic-plot-directory* t)

(defun print-windows (&key (windows :ALL)
		      (kill-ps-margins *kill-ps-margins*)
		      (landscape *ps-landscape-p*)
		      (include-title *print-windows-include-title*)
		      (what-to-do *print-windows-what-to-do*)
		      (printer *printer*)
		      (print-together *print-together*)
		      directory (filename-suffix *PS-FILENAME-SUFFIX*)
		      arrange
		      erase-files
		      HARD-COPY-SCREEN
		      use-menu
		      (exclusion-list *print-windows-exclusion-list*)
		      (inclusion-list *print-windows-inclusion-list*))
  "Generates and prints PS files of selected WINDOWS on PRINTER [default given by *PRINTER*].  WHAT-TO-DO options [default given by
*PRINT-WINDOWS-WHAT-TO-DO*] include:

  :PRINT-NOW       => Send PS files to PRINTER.
  :PRINT-&-DESTROY => Destroy windows after writing and printing PS files.
  :JUST-WRITE      => Only write PS files.
  :JUST-WRITE-&-DESTROY => Only write PS files, then destroy windows.
  :NO_FILE/PRINT   => No files are generated. Used to access auxilary
                      functions such as arranging the selected windows, etc.

DIRECTORY applies to the PS files. If not supplied, the default file directory will be the current value of the global variable *PLOT-DIRECTORY*, and if
supplied will set the value of *PLOT-DIRECTORY*. When WINDOWS is :ALL, then all visible windows are selected for printing according to the other
options. Additional options include:

  ERASE-FILES      => PS files will be erased after printing [default NIL].
                      BUG ALERT: This may erase file before printing occurs.
  HARD-COPY-SCREEN => A PS file including all visible windows is printed [default NIL].
  PRINT-TOGETHER   => All selected windows are printed in one file [default
                      given by *PRINT-TOGETHER*]. If set to a filename string, this is
                      used to name the PS file. Otherwise an ad-hoc filename is
                      constructed from the selected windows.
  ARRANGE          => Arrange selected windows so that they don't overlap [default NIL].
                      If this is a number, then ARRANGE specifies the number of windows
                      per row, used by ARRANGE-WINDOWS.
  FILENAME-SUFFIX  => If non-NIL, this will be added to the end of the filename for all
                      created .ps files. The default value is given by *PS-FILENAME-SUFFIX*.
                      Ignored if PRINT-TOGETHER specifies an explicit filename.

"
  (let* ((*automatic-run* (or (not use-menu) *automatic-run*))
	 (windows-to-print (select-and-filter-printed-windows
			    (print-window-menu (if (eq :all windows) :all (coerce-to-list windows)))
			    exclusion-list inclusion-list))
	 (num-windows-to-print (length windows-to-print))
	 (multiple-windows-p (> num-windows-to-print 1))
	 ;; Unix bug causes crash if we try to print out more than one file from this function.
	 (automatic-directory (get-plot-directory))
	 (dummy1 (and *kill-multiple-window-printing* multiple-windows-p))
	 (dummy2 (print-windows-convert-what-to-do-to-choose-symbol what-to-do))
	 (dummy3 erase-files)
	 (dummy4 hard-copy-screen)
	 dummy5 (dummy6 include-title)
	 dummy7 dummy8 (dummy10 filename-suffix) dummy11 dummy12
	 (dummy13 (true-p print-together))
	 (dummy14 arrange)
	 (dummy15 (if *print-windows-use-automatic-plot-directory* "Use automatic plot directory" "Use *PLOT-DIRECTORY*")) 
	 dummy16)
    (setq *printer* printer
	  *kill-ps-margins* kill-ps-margins 
	  *ps-landscape-p* landscape)
    (unless *plot-directory* (setq *plot-directory* *Surf-user-home*))
    (when (opal:directory-p DIRECTORY) (setq *plot-directory* DIRECTORY))
    (when (or windows-to-print *automatic-run*)
      (choose-variable-values
       `((dummy2 "What to do:" :choose (:Print_now :Just_write :print_&_destroy :No_file/print :JUST_WRITE_&_DESTROY) :label-left)
	 ,(when multiple-windows-p `(dummy13 ,(format nil "Put all ~d selected windows together" num-windows-to-print) :boolean))
	 (dummy6 "Include all window titles for printing" :boolean)
	 (dummy3 "Erase PS files after printing them" :boolean)
	 ;; (:comment ,(print-window-options-comment))
	 (dummy8 ,(format nil "Change PS options~%~A" (print-window-options-comment)) :boolean)
	 ,(when multiple-windows-p `(dummy11 "Set window order" :boolean))
	 ,(when multiple-windows-p `(dummy14 "Retile selected windows" :boolean))
	 (dummy4 "Hardcopy all the windows now" :boolean)
	 (dummy15 ,(format nil "Use *PLOT-DIRECTORY* (currently ~A) for output,~%or automatic directory (currently ~A)" *PLOT-DIRECTORY* automatic-directory)
	  :choose ("Use *PLOT-DIRECTORY*" "Use automatic plot directory"))
	 (dummy16 "Edit *PLOT-DIRECTORY*" :boolean)
	 (dummy10 "Add this string to the filename(s) created" :string))
       :text (if (= 1 (length windows-to-print))
		 (format nil "Print menu for window ~A" (gv (car windows-to-print) :title))
		 (format nil "Print menu for ~d selected windows" (length windows-to-print)))
       :label "Window Printing Menu")
      (when dummy8 (ps-file-options-menu))
      (when dummy2
	(when dummy16
	  (let ((directory-path (directory-browser "Select directory for *PLOT-DIRECTORY*" *PLOT-DIRECTORY*)))
	    (when directory-path (setq *plot-directory* (namestring directory-path)))))
	(cond-every
	 (dummy11 (setq windows-to-print
			(reorder-windows-to-print windows-to-print)))
	 (dummy14 (setq windows-to-print
			(arrange-windows :use-menu t :windows-per-row (if (numberp arrange) arrange *arrange-windows-per-row*) :windows (reverse windows-to-print))))
	 (*KILL-PS-MARGINS* (setq *ps-left-margin* *minimum-ps-left-margin*  *ps-right-margin* *minimum-ps-right-margin*
				  *ps-top-margin* *minimum-ps-top-margin* *ps-bottom-margin* *minimum-ps-bottom-margin*))
	 (dummy4 (setq windows-to-print (visible-windows))))
	(let ((dummy2 (print-windows-convert-choose-symbol-to-what-to-do dummy2))
	      (*print-together* (or dummy4 dummy13))
	      ;; Make title on window visible for the ps file.
	      (windows-w-new-titles (when dummy6 (add-titles-to-those-without windows-to-print))))
	  (setq *print-windows-what-to-do* dummy2
		*print-windows-include-title* dummy6)
	  (loop for win in windows-to-print do (update-title win))

	  (setq *print-windows-use-automatic-plot-directory* (not (string= dummy15 "Use *PLOT-DIRECTORY*")))
	  (case dummy2
	    ((:print-now :print-&-destroy :just-write :JUST-WRITE-&-DESTROY)
	     (loop for ps-file in (generate-ps-files windows-to-print
						     :directory (if (string= dummy15 "Use *PLOT-DIRECTORY*") *PLOT-DIRECTORY*  automatic-directory)
						     :filename-extra dummy10
						     :print-together-filename print-together)
		   when (or (eq dummy2 :print-now) (eq dummy2 :print-&-destroy)) do (print-ps-file ps-file (make-lpr-flags-string))
		   when dummy3 do (sleep *delete-delay-after-print*) (delete-file ps-file))))
	  (if (or (eq dummy2 :print-&-destroy) (eq dummy2 :JUST-WRITE-&-DESTROY))
	      (progn (unlock-windows windows-to-print) (mapcar 'clear-window windows-to-print))
	      (mapcar 'remove-title windows-w-new-titles))) ; Remove any new titles, since we also have title on window manager title bar.
	(reset-*ps-margin*s)
	(setq *print-together* dummy13)))))

(defun hard-copy-screen ()
  (let ((*automatic-run* t))
    (print-windows :what-to-do :print-now :erase-files t :hard-copy-screen t)))

(defun reset-*ps-margin*s ()
  (setq *ps-left-margin* 72 *ps-right-margin* 72 *ps-top-margin* 72 *ps-bottom-margin* 72)
  nil)

(defun print-ps-file (filename &optional lpr-flags-string)
  (when *gc-every-print* (ext:gc))
  (when *enable-printing*
    (let ((*announce-shell-exec* t))
      (shell-exec (concatenate `string
			       *shell-lpr-command*
			       " "
			       ;;	   " -s "
			       (when *include-printer-flags* lpr-flags-string)
			       (format nil "  '~A'" filename))))))

(defun arrange-plot-and-clear-standard-plots (&key
					      (print-together *print-together*)
					      (landscape-p *ps-landscape-p*)
					      (borders-p *ps-borders-p*)
					      (wins-per-row *arrange-windows-per-row*) ; :max-pack
					      (window-tile-x-gap *window-tile-x-gap*) (window-tile-y-gap *window-tile-y-gap*)
					      (start-x *window-tile-start-x*) (start-y *window-tile-start-y*)
					      break-after-plot)
  (let* ((*print-together* print-together) 
	 (*ps-borders-p* borders-p)
	 (*ps-landscape-p* landscape-p)
	 (wins (reverse (windows-of-mode :standard-plot))) ; Put first win first.
	 (wins-per-row (case wins-per-row
			 (:max-pack (round (sqrt (length wins))))
			 (t (round wins-per-row)))))
    (arrange-windows :windows-per-row wins-per-row :windows wins :window-tile-x-gap window-tile-x-gap :window-tile-y-gap window-tile-y-gap :start-x start-x :start-y start-y)
    (print-windows :windows wins)
    (when break-after-plot (break))
    (clear-windows (windows-of-mode :standard-plot) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*PS-FILENAME-SUFFIX*
	  *ps-position-x* *ps-position-y* *ps-scale-x* *ps-scale-y*
	  *DELETE-DELAY-AFTER-PRINT*
	  *include-filename-and-date-in-ps-files*
	  *PS-FILE-PAGE-COMMENT*
			  
	  *ps-landscape-p* *ps-borders-p* *ps-color-p* *ps-left-margin* *ps-right-margin*
	  *ps-top-margin* *ps-bottom-margin* *ps-header-print-p* *lpr-paper-size*

	  *minimum-ps-left-margin* *minimum-ps-right-margin* *minimum-ps-top-margin* *minimum-ps-bottom-margin*

	  *lpr-paper-width* *lpr-paper-height*

	  *print-now* *PRINT-WINDOWS-WHAT-TO-DO*
	  *gc-every-print* *enable-printing* 
	  *print-together* *print-windows-inclusion-list* *print-windows-exclusion-list*
	  *print-windows-include-title*
	  *MAXIMUM-PS-FILENAME-LENGTH*

	  *shell-lpr-command*
	  *shell-lpr-printer-flag*
	  *include-printer-flags*
	  *flag-windows-for-borders*  
	  *kill-ps-margins*
	  *kill-multiple-window-printing*
	  
	  print-windows make-ps-file
	  HARD-COPY-SCREEN
	  PRINT-WINDOW-MENU
	  PS-FILE-OPTIONS-MENU

	  arrange-plot-and-clear-standard-plots))



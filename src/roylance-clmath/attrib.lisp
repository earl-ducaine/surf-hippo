;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FS -*-

;;;; File Attribute Lists

;;;  (c) Copyright Gerald Roylance 1987
;;;      All Rights Reserved.
;;;  No part of this material may be used, photocopied,
;;;  reproduced or committed to magnetic, electronic or any other
;;;  form of media without prior written consent.

;;; Bugs and Fixes
;;;   *** doesn't jimmy the pathname object

(in-package :fs)

(export '(parse-attribute-list pathname-attribute-list))

;;; require
;;; use-package
;;; import
;;; commands

;;; Known attributes
;;;   mode, package, base, syntax, lowercase, fonts, backspace, patch-file
;;;
;;; -*- name1 : value1, value2, value3 ; name2 : value4 -*-
;;; -> (:name1 (:value1 :value2 :value3) :name2 :value4)


;;;; Interpretation of Package Attributes

;;;   -*- Package: Name -*-                           err if package doesn't exist
;;;   -*- Package: (Name) -*-                         make it if it doesn't exist
;;;   -*- Package: (Name use) -*-                     make and use use
;;;   -*- Package: (Name use size) -*-                more options
;;;   -*- Package: (Name :key value :key value) -*-   args to (make-package ...)

;;;  the last line can be ambiguous with the previous lines.
;;;  (make-package name &key (use "LISP") (nicknames NIL))
;;;    name is symbol or string
;;;    use is list of symbols or strings
;;;    nicknames is list of strings


;;;; Parse a File Attribute List

;; LG change 04.04.2016
;; (defun fs:parse-attribute-list
(defun parse-attribute-list
 (line &optional
				(start 0)
				(end   #-MACLISP (length line)
				       #+MACLISP (flatc  line)))

    (let ((attr nil)
	  (*package* (find-package "KEYWORD"))
	  (left (search "-*-" line :start2 start :end2 end)))

      (if (not (null left))
	  (let ((right (search "-*-" line :start2 (+ left 3) :end2 end)))
	    (if (not (null right))
		;; got it -- now look for ; and parse each group
		(do ((i (+ left 3) (1+ j))
		     (j 0))
		    ((>= j right))
		  (setq j (or (search ";" line :start2 i :end2 right) right))

		  ;; i and j now delimit a group
		  (let ((i0 (search ":" line :start2 i :end2 j))	; find the :
			(name :MODE))		; default to :MODE
		    (if (null i0)
			(setq i0 (1- i))
			(setq name (read-from-string line t nil :start i :end i0)))
		    (push name attr)		; the name
		    (do ((k0   (1+ i0) (1+ k1))	; find the values
			 (k1   0)
			 (vals nil))
			((>= k0 j)
			 (push (if (null (cdr vals))
				   (car vals)
				   (reverse vals))
			       attr))
		      (setq k1 (or (search "," line :start2 k0 :end2 j) j))
		      (push (read-from-string line t nil :start k0 :end k1)
			    vals))
		    ))
		)))
      (nreverse attr)))


;;;; Get the line from a file and parse it

(defun fs::pathname-attribute-list (pathname)
  (with-open-file (file pathname)
    (parse-attribute-list (read-line file))))

#|

(fs:pathname-attribute-list "oz:<glr.lisp>attrib.lsp")
(fs:pathname-attribute-list "oz:<glr.papers>math.tex")

|#


;;;; File Attribute Lists

;;; pathname should be a real pathname
;;; stream should be an open file

#|

(defun fs:read-attribute-list (pathname stream)
  (declare (ignore pathname))			; ***

  (let* ((line (read-line stream))
	 (attr (fs:parse-attribute-list line)))

    (file-position stream 0)			; reset the stream

    ;; *** should jimmy the pathname

    attr))

|#

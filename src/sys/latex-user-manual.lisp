;; Functions to be used when making a new user or reference manual
;; See also /usr/local/surf-hippo/doc/latex/How-To-Do-It-Notes

(defun gen-whatevers-latex ()
  (loop for name in (loop for model in (models) collect (model-name model))
	when (fboundp (read-from-string (format nil "~As" name)))
	do
	(let* ((whatevers-doc-format-string (concatenate 'string
							 "Returns a list of all ~(~A~)s associated with the cell elements referenced by ELEMENT, "
							 "if supplied; otherwise, all ~(~A~)s in circuit."))
	       (whatevers-symbol-collector-function (format nil "~As" name))
	       (whatevers-function-doc-string (format nil whatevers-doc-format-string name name))
	       (tex-filename (format nil "/usr/local/surf-hippo/doc/latex/source-tex-files/forms/~:@(~a~)-Function.lisp.tex" whatevers-symbol-collector-function)))
	  (with-open-stream
	   (stream (open tex-filename :direction :output :if-exists :supersede))
	   (format stream
		   "~%
\\index{~:@(~a~)}
\\begin{lisp:nodocumentation}{~(~A~)}{Function}{{\\sf \\&optional} (element nil element$-$supplied-p)}
\\end{lisp:nodocumentation}~%"
		   whatevers-symbol-collector-function
		   whatevers-symbol-collector-function))
;; Now output documentation version to the screen for pasting into the appropriate file.
	  (format t
		  "~%
\\index{~:@(~a~)}
\\begin{lisp:documentation}{~(~A~)}{Function}{{\\sf \\&optional} (element nil element$-$supplied-p)}
~A
\\end{lisp:documentation}~%"
		  whatevers-symbol-collector-function
		  whatevers-symbol-collector-function
		  whatevers-function-doc-string))))

(defun type-def-documentation ()
  (loop for model in (models) do
	(let ((name (model-name model)))
	  (when (model-child-structure-type model)
	    (loop for type-def-macro-name in
		  (list (read-from-string (format nil "~A-def" name))
			(read-from-string (format nil "~A-quoted-def" name)))
		  do
		   
		  (let* ((tex-filename (format nil "/usr/local/surf-hippo/doc/latex/source-tex-files/forms/~:@(~a~)-Macro.lisp.tex" type-def-macro-name))
			 (type-def-macro-doc-string (documentation type-def-macro-name 'function)))
		    (with-open-stream
			(stream (open tex-filename :direction :output :if-exists :supersede))
		      (format stream
			      "~%
\\index{~:@(~a~)}
\\begin{lisp:nodocumentation}{~(~A~)}{Macro}{{\\sf body}}
\\end{lisp:nodocumentation}~%"
			      type-def-macro-name
			      type-def-macro-name))

		    ;; Now output documentation version to the screen for pasting into the appropriate file.
		    (when (> (length type-def-macro-doc-string) 0)
		      (format t
			      "~%
\\index{~:@(~a~)}
\\begin{lisp:documentation}{~(~A~)}{Macro}{{\\sf body}}
~a
\\end{lisp:documentation}~%"
			      type-def-macro-name
			      type-def-macro-name
			      type-def-macro-doc-string))))))))



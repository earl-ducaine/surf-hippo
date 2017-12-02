(deftype circuit-object-type-2 () "The union of all circuit object types." `(or ,@*element-structure-types*))


(macrolet ((hash-table-functions () "A macro for hash table functions."

				 `(progn ,@(mapcar #'(lambda (name)
						       (let ((fun-name (read-from-string (format nil "~A-hash-table" name))))
							 `(defun ,fun-name ()
							    ,(format nil "A foobar for all ~as" name)
							    (model-hash-table (get (quote ,name) (quote model))))))
						   '(segment)
					; *model-names*
						   ))))
  (hash-table-functions))




#|
(eval
  `(progn 
     ,@(mapcar
	#'(lambda (thing) `(defun ,(read-from-string (format nil "~A-foo" thing))
			     (arg-1 &key (,thing t)) ,(format nil "This 1 libas all ~As...." thing) (foo arg-1 ,thing)))
	*model-names*)))
		     


(defun foo (one two &optional three (four baz) &key (bar t) bim bam)
  "foo bar bam maam"
  )


(eval `(progn ,@(mapcar
		 #'(lambda (plot-list-info)
		     `(defvars-w-value
			(,(plot-list-info-names plot-list-info) nil)
			(,(plot-list-info-enable-var plot-list-info) nil)
			(,(plot-list-info-structures  plot-list-info) nil)))
		 *plot-lists-info*)))



(eval-when (compile load eval) `(progn 
	      ,@(mapcar
		 #'(lambda (thing)
		     (let* ((thing-string (string-downcase (string thing)))
			    (elt-type-sym (read-from-string (format nil "~A-type" thing-string)))
			    (elt-type-sym-fboundp (fboundp elt-type-sym))
			    (args-symbol (read-from-string (format nil "(element~A)" (if elt-type-sym-fboundp (format nil " &optional ~A" elt-type-sym) ""))))
			    (doc-string (format nil "Return all ~As associated with the cell element of ELEMENT.~a"
						thing-string
						(if elt-type-sym-fboundp
						  (format nil " If ~A is supplied, then only return ~As that match ~A." elt-type-sym thing-string elt-type-sym)
						  ""))))
		       `(defun ,(read-from-string (format nil "node-~as" thing)) ,args-symbol
			  ,doc-string
			  ,(if (not elt-type-sym-fboundp)
			     `(cell-element-elements element ',thing)
			     `(let* ((all-elts-of-type (cell-element-elements element ',thing))
				     (element-type (element ,elt-type-sym)))
				(if ,elt-type-sym (delete-if-not #'(lambda (obj) (eq (,elt-type-sym obj) element-type)) all-elts-of-type) all-elts-of-type))))))
		 ;; *model-names*
		 '(soma)
		 )))


|#


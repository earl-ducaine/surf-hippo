(in-package "GARNET-GADGETS")

;; Use CMUCL compatible DIRECTORY function
(defun put-filenames-in-menu (save-gad dir-name)
  (let ((file-list NIL)
	(dir (directory dir-name))
	(save-win (g-value save-gad :window)))
    (dolist (name dir)
      (setf file-list (cons (string-left-trim #+apple '(#\:)
					      #-apple '(#\/)
					      (enough-namestring name (truename dir-name)))
			    file-list)))
    (setf file-list (sort file-list #'(lambda (x y) (string< x y))))
    (push #+apple ":"
          #-apple ".." 
          file-list)
    (s-value (g-value save-gad :file-menu) :items file-list)
    (s-value (g-value save-gad :file-menu) :selected-ranks NIL)
    (s-value (g-value save-gad :message) :string "")
    (s-value (g-value save-gad :file-menu :scroll-bar) :value 0)
    (opal:update save-win)))
(defun DEMO-FILE-BROWSER::DIRECTORY-FN (namestring)
  (let ((dir (directory #+cmu namestring
			#+apple (concatenate 'string (namestring namestring) "*")
                        #-(or cmu apple) 
                              (concatenate 'string (namestring namestring) "/")
			      ;; Added opt out for cmucl
                          #-(or cmu clisp) :directories #-(or cmu clisp) t)))
    (if (or (null dir) (equal (car dir) namestring)) NIL dir)))






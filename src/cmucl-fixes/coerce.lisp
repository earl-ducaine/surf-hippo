(in-package "LISP")

;; same as original, but need this for this compile?
(defmacro type-specifier-atom (type)
  "Returns the broad class of which TYPE is a specific subclass."
  `(if (atom ,type) ,type (car ,type)))

(defun coerce (object output-type-spec)
  "Coerces the Object to an object of type Output-Type-Spec."
  (cond
   ((typep object output-type-spec)
    object)
   ((eq output-type-spec 'character)
    (character object))
   ((eq output-type-spec 'function)
    (eval `#',object))
   ((numberp object)
    (case output-type-spec
      ((short-float single-float float)
       (%single-float object))
      ((double-float long-float)
       (%double-float object))
;; LG fix 19.08.2016
      ((double-double-float)
       (%double-double-float object))
      (complex
       (complex object))
      (t
       (error "~S can't be converted to type ~S." object output-type-spec))))
   (t
    (typecase object
      (list
       (case (type-specifier-atom output-type-spec)
	 ((simple-string string simple-base-string base-string)
	  (list-to-string* object))
	 ((simple-bit-vector bit-vector) (list-to-bit-vector* object))
	 ((simple-vector vector array simple-array)
	  (list-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (simple-string
       (case (type-specifier-atom output-type-spec)
	 ((cons list) (vector-to-list* object))	; Added CONS output-type-spec LBG May 9 2001
	 ;; Can't coerce a string to a bit-vector!
	 ((simple-vector vector array simple-array)
	  (vector-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (simple-bit-vector
       (case (type-specifier-atom output-type-spec)
	 ((cons list) (vector-to-list* object))	; Added CONS output-type-spec LBG May 9 2001
	 ;; Can't coerce a bit-vector to a string!
	 ((simple-vector vector array simple-array)
	  (vector-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (simple-vector
       (case (type-specifier-atom output-type-spec)
	 ((cons list) (vector-to-list* object))	; Added CONS output-type-spec LBG May 9 2001
	 ((simple-string string simple-base-string base-string)
	  (vector-to-string* object))
	 ((simple-bit-vector bit-vector) (vector-to-bit-vector* object))
	 ((vector array simple-array) (vector-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (string
       (case (type-specifier-atom output-type-spec)
	 ((cons list) (vector-to-list* object))	; Added CONS output-type-spec LBG May 9 2001
	 ((simple-string simple-base-string)
	  (string-to-simple-string* object))
	 ;; Can't coerce a string to a bit-vector!
	 ((simple-vector vector simple-array array)
	  (vector-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (bit-vector
       (case (type-specifier-atom output-type-spec)
	 ((cons list) (vector-to-list* object))	; Added CONS output-type-spec LBG May 9 2001
	 ;; Can't coerce a bit-vector to a string!
	 (simple-bit-vector (bit-vector-to-simple-bit-vector* object))
	 ((simple-vector vector array simple-array)
	  (vector-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (vector
       (case (type-specifier-atom output-type-spec)
	 ((cons list) (vector-to-list* object))	; Added CONS output-type-spec LBG May 9 2001
	 ((simple-string string base-string simple-base-string)
	  (vector-to-string* object))
	 ((simple-bit-vector bit-vector) (vector-to-bit-vector* object))
	 ((simple-vector vector array simple-array)
	  (vector-to-vector* object output-type-spec))
	 (t (error "Can't coerce ~S to type ~S." object output-type-spec))))
      (t (error "~S is an inappropriate type of object for coerce." object))))))

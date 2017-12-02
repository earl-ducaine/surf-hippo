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

;; GUI Source file: sequences.lisp

(IN-PACKAGE "WINDOWS-HACK")

;; Various useful functions for handling sequences, many of which probably can be done better if I read CLTL more
;; thoroughly. Functions specialized for strings are also found in strings.lisp.  

;; *********************************
;; Hash Tables
;; *********************************

(defun hash-table-list (table) (when table (loop for elt being the hash-value of table collect elt)))
(defun hash-table-empty (table) (= 0 (hash-table-count table)))
   
;;; PRINT-TABLE-KEYS and PRINTIT Functions to print out the keys of a hash table.
(defun print-table-keys (table) (maphash 'printit table))

(defun printit (name nd)
  (declare (ignore nd))
  (print name))

(defun named-structure-symbol (st) (type-of st))

;; *********************************
;; Lists, Arrays And Sequences
;; *********************************

(defun remove-all (elements-to-remove sequence)
  (if elements-to-remove
    (remove-all (cdr elements-to-remove) (remove (car elements-to-remove) sequence))
    sequence))
	
(defun find-closest-list-value (numeric-list number)
  "Return as values the value in NUMERIC-LIST that is arithmetically closest to NUMBER, and the index of that value in NUMERIC-LIST."
  (VALUES-list (cdar (sort
		      (loop for val in numeric-list
			    for index from 0
			    collect (list (abs (- val number)) val index)) '< :key 'car))))

(defun normalize-list-lengths (lists) (mapcar #'(lambda (list) (list-head list (apply 'min (mapcar 'length lists)))) lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math on numeric lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negate (thing)
  "Given a number or numeric sequence THING, return the negative version."
  (if (numberp thing) (- thing) (map (TYPE-OF-SEQ thing) `- thing)))

(defun negate-single-float (thing)
  "Given a single float number or single float numeric sequence THING [list or vector], return the negative version."
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (typecase thing
    (number (- (the sf thing)))
    (array (loop for val single-float across (the (vector SINGLE-FLOAT) thing)
		 for index fixnum from 0
		 do (setf (aref (the (vector SINGLE-FLOAT) thing) index) (- (the sf val)))))
    (cons (loop for val single-float in thing
		collect (- (the sf val))))))


;; (reduce '+ (map 'cons `* df-listx df-listy))
(defun sum-double-float-listx*listy (df-listx df-listy)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x double-float in df-listx
	for y double-float in df-listy
	sum (* x y) into out double-float
	finally (return out)))
	
(defun sum-double-float-listx*listx (df-listx)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x double-float in df-listx
	sum (* x x) into out double-float
	finally (return out)))

(defun sum-double-float-arrayx*arrayy (df-arrayx df-arrayy)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x double-float across (the (vector double-float) df-arrayx)
	for y double-float across (the (vector double-float) df-arrayy)
	sum (* x y) into out double-float
	finally (return out)))
	
(defun sum-double-float-arrayx*arrayx (df-arrayx)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x double-float across (the (vector double-float) df-arrayx)
	sum (* x x) into out double-float
	finally (return out)))

(defun sum-single-float-arrayx*arrayy (df-arrayx df-arrayy)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x single-float across (the (vector single-float) df-arrayx)
	for y single-float across (the (vector single-float) df-arrayy)
	sum (* x y) into out single-float
	finally (return out)))
	
(defun sum-single-float-arrayx*arrayx (df-arrayx)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x single-float across (the (vector single-float) df-arrayx)
	sum (* x x) into out single-float
	finally (return out)))

				;; (defun sum-double-float-listx*listx (df-listx) (sum-double-float-listx*listy df-listx df-listx))

(defun sum-double-float-list (list)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x double-float in list sum x into out double-float finally (return out)))

(defun sum-single-float-array (array)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x single-float across (the (vector single-float) array) sum x into out single-float finally (return out)))

(defun sum-double-float-array (array)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (loop for x double-float across (the (vector double-float) array) sum x into out double-float finally (return out)))

(defun equal-double-float-array (array)
  (declare (optimize (safety 2) (speed 3) (space 0))
	   (type (vector double-float) array))
  (let ((test (aref array 0)))
    (loop for x double-float across array unless (= test x) do (return nil) finally (return t))))

(defun max-of-seq (seq) (reduce 'max seq))
(defun min-of-seq (seq) (reduce 'min seq))

;;; MAXIMIZE-VAL-OR-SEQ, MINIMIZE-VAL-OR-SEQ, A-BIT-MORE, A-BIT-LESS
;;; VAL-OR-SEQ can be a single number, an array, a list, a list of lists, a list of arrays, an
;;; array of lists, etc, etc. All arrays must be 1D.

(defmacro dispatch-extrema-val-or-seq (function)
  `(typecase val
     (number val)
     (list (,function val))
     (t 0.0)))

(defun maximize-val-or-seq (val-or-seq)
  (typecase val-or-seq
    (number val-or-seq)
    (array (loop for val across val-or-seq maximizing (dispatch-extrema-val-or-seq maximize-val-or-seq)))
    (t (loop for val in val-or-seq maximizing (dispatch-extrema-val-or-seq maximize-val-or-seq)))))

(defun minimize-val-or-seq (val-or-seq)
  (typecase val-or-seq
    (number val-or-seq)
    (array (loop for val across val-or-seq minimizing (dispatch-extrema-val-or-seq minimize-val-or-seq)))
    (t (loop for val in val-or-seq minimizing (dispatch-extrema-val-or-seq minimize-val-or-seq)))))

(defun sequence-max (seq) (maximize-val-or-seq seq))
(defun sequence-min (seq) (minimize-val-or-seq seq))
(defun max-of-list (list) (maximize-val-or-seq list))
(defun min-of-list (list) (minimize-val-or-seq list))

(defun a-bit-more (val-or-seq &optional (how-much 0.0) fix-it)
  (let* ((max (maximize-val-or-seq val-or-seq))
	 (adjusted (+ max (abs (* how-much max)))))
    (if fix-it (ceiling adjusted) adjusted)))

(defun a-bit-less (val-or-seq &optional (how-much 0.0) fix-it)
  (let* ((min (minimize-val-or-seq val-or-seq))
	 (adjusted (- min (abs (* how-much min)))))
    (if fix-it (floor adjusted) adjusted)))

(defun normalize-sequence (sequence &optional (min-is-zero-p t))
  "Return a list of normalized values derived from the numbers in the 1d SEQUENCE. Minimum value is 0 when MIN-IS-ZERO-P is true
 [default], otherwise taken as minimum of sequence."
  (let ((max (sequence-max sequence))
	(min (if min-is-zero-p 0.0 (sequence-min sequence))))
    (map (TYPE-OF-SEQ sequence) #'(lambda (val) (/ (- val min) (- max min))) sequence)))

(defun add-val-to-float-list (val list)
  "Return a single float list whose members are given by the sums of VAL and each member of LIST, all of which must be single floats."
  (sequence-to-float-array-w-offset list val))

(defun scale-float-list (scale list)
  "Return a single float list whose members are given by the products of SCALE and each member of LIST, all of which must be
single floats."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float scale))
  (loop for val single-float in list collect (* scale val)))

(defun scale-and-offset-float-list (scale offset list)
  "Return a single float list whose members are given by the products of SCALE and the sum of each member of LIST and OFFSET, all of which must be
single floats."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float scale offset))
  (loop for val single-float in list collect (* scale (+ val offset))))

(defun scale-float-array (scale array)
  "Return a single float array whose members are given by the products of SCALE and each member of ARRAY, all of which must be
single floats."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float scale))
  (loop for val single-float across array
	for index fixnum from 0
	do (setf (aref array index) (* scale (aref array index)))))

(defun scale-and-offset-float-array (scale offset array)
  "Return a single float array whose members are given by the products of SCALE and the sum of each member of ARRAY and OFFSET, all of which must be
single floats."
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (single-float scale offset))
  (loop for val single-float across array
	for index fixnum from 0
	do (setf (aref array index) (* scale (+ (aref array index) offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-member-equal (item list)
  ;; "Returns tail of list beginning with first element satisfying EQLity, :test, or :test-not with a given item."
  (do ((list list (cdr list)))
      ((null list) nil)
    (let ((car (car list)))
      (when (equal item car) 
	(return list)))))

(defun my-delete (elt list)
  ;; Guaranteed to destructively remove ELT from LIST, as long as ELT is not the car of LIST.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (do ((list list (cdr list)))
      ((or (eq elt (cadr list)) (null list))
       (when list (rplacd list (cddr list)))))
  nil)

(defun fast-sort-delete-duplicates (num-list)
  ;; For floats. Non-destructive since SORT is called on copy of NUM-LIST.
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let ((out '())
	(temp-list (copy-list num-list)))
    (do ((sorted-num-list
	  (sort temp-list #'(lambda (val1 val2) (> (the single-float val1)(the single-float val2))))
	  (cdr sorted-num-list)))
	((null (cadr sorted-num-list))
	 (push (car sorted-num-list) out))
      (unless (= (the sf (car sorted-num-list))
		 (the sf (cadr sorted-num-list)))
	(push (car sorted-num-list) out)))
    out))

(defun double-lists (list-of-lists) (nconc (copy-list list-of-lists) list-of-lists))

;;(defun add-to-end (list last) (reverse (cons last (reverse list))))
(defun add-to-end (list last) (append list (list last)))

(defun atomize-list (thing)
  "If THING is an atom, returns THING; if THING is a list with one element, returns that element, otherwise returns THING."
  (if (atom thing)
    thing
    (if (= 1 (length thing))
      (car thing)
      thing)))

(defun generic-intersection (foo bar)
  ;;  "Returns as a list the commonality between the atoms or lists FOO and BAR."
  (cond
   ((and (consp foo)
	 (consp bar))
    (intersection foo bar))
   ((consp bar) (list (find foo bar)))
   ((consp foo) (list (find bar foo)))
   ((eq foo bar) (list foo))))

(defun rem-not-in-keys (list keys)
  ;; Returns a version of LIST that includes only items in LIST that appear in KEYS.
  (intersection list keys)
					; (loop for item in list when (member item keys :test #'equal) collect item)
  )

(defun rem-in-keys (list keys)
  ;; Returns a version of LIST that includes only items in LIST that do not appear in KEYS.
  (set-difference list keys)
					; (loop for item in list when (not (member item keys :test #'equal)) collect item)
  )

(defun thing-in-array-p (thing array) (loop for elt across array thereis (eq thing elt)))

(defun boolean-list (value length) (loop for count from 1 to length collect (true-p value)))

(defun list-of-nums-start-end (&key (start 0.0) end (increment 1.0))
  "Return a list of LENGTH of increasing numbers (type determined by START [default 0.0] and INCREMENT [default 1.0], until reaching END. The number type will be
determined by the type of START and INCREMENT."
  (loop for i from start by increment until (> i end) collect i))

(defun list-of-nums (length &optional (start 0.0) (increment 1.0))
  "Return a list of LENGTH of increasing numbers (type determined by START [default 0.0] and INCREMENT [default 1.0]. The number type will be
determined by the type of START and INCREMENT."
  (loop for i from start by increment for count from 0 to (1- (round length)) collect i))

(defun list-of-sf-nums (length &optional (start 0.0) (increment 1.0))
  "As LIST-OF-NUMS with returned list of single floats."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((start (s-flt start))
	(increment (s-flt increment)))
    (loop for i single-float from start by increment
	  for count fixnum from 0 to (1- (the fn (round length)))
	  collect i)))

(defun list-of-ints (length &optional (start 0) (increment 1))
  "As LIST-OF-NUMS with returned list of integers."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((start (round start))
	(increment (round increment)))
    (loop for i fixnum from start by increment
	  for count fixnum from 0 to (1- (the fn (round length)))
	  collect i)))

(defun array-of-nums (length &optional (start 0.0) (increment 1.0))
  "Return an array of LENGTH increasing numbers (type determined by START [default 0.0] and INCREMENT [default 1.0]."
  (list-to-array (list-of-nums length start increment)))

(defun string-remove (string list) (remove string list :test #'equal))

(defun push-lists-onto-list (lists list2)
  (loop for list in (reverse lists) do (push list list2))
  list2)

(defun every-nth (list n &optional (offset 0))
  (loop for val in list
	for count from 0
	when (= (mod (- count offset) n) 0)
	collect val))

(defun chop-list (list chop-length)
  (let ((count 0)
	(partial-out '())
	(out '()))
    (dotimes (i (length list))
      (push (car list) partial-out)
      (setq list (cdr list)
	    count (1+ count))
      (cond ((= (+ count 2) chop-length)
	     (push (reverse partial-out) out)
	     ;; Retain last xy values so that lines are joined.
 	     (setq partial-out (list (car partial-out)(cadr partial-out))
		   count 0))))
    (if (> (length partial-out) 2) (push (reverse partial-out) out))
    (reverse out)))

(defun order-list-from-key-list (list key-list &optional other-list)
  (let ((output '()))
    (loop for key in key-list do
	  (let ((search-result-list (member key list :test 'equal)))
	    (when search-result-list
	      (setq output (concatenate 'list (list (nth (- (length list) (length search-result-list)) (or other-list list))) output)))))
    (reverse output)))

(defun split-up (list split-length)
  (let (sub)
    (loop for val in list
	  for count from 1
	  do (push val sub)
	  when (or (= count (length list))
		   (= 0 (mod count split-length)))
	  collect (reverse sub) into out
	  and do (setq sub nil)
	  finally (return out))))

(defun concatenate-to-sf-array (&rest things)
  (declare (optimize (safety 2) (speed 3) (space 0)))
  (let* ((array-length (apply `+ (mapcar #'(lambda (thing) (if (consp thing) (length thing) 1)) things)))
	 (array (make-array (list array-length) :element-type 'dft-float))
	 (index -1))
    (declare (fixnum index))
    (loop for thing in things
	  when (atom thing) do (setf (aref array (incf index)) thing)
	  else do (mapcar #'(lambda (val) (setf (aref array (incf index)) val)) thing))
    array))

(defun last-element (seq) (elt seq (1- (length seq))))
(defun first-element (seq) (elt seq 0))

;; Taken from Common Lisp by Paul Graham, page 49. (he calls this PROPER-LIST?, but the "?" screws up the user manual files).
(defun proper-list-p (x)
  (or (null x)
      (and (consp x)
	   (proper-list-p (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sequence Predicates
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-or-symbol-or-string-list-p (list)
  (when (listp list)
    (loop for item in list unless (or (numberp item) (stringp item) (symbolp item)) do (return nil) finally (return t))))

(defmacro sequence-predicate-iterator (sequence predicate-function loop-conditional)
  `(let ((internal-sequence ,sequence)
	 (internal-predicate-function ,predicate-function))
     (typecase internal-sequence
       (cons (loop for val in internal-sequence ,loop-conditional (funcall internal-predicate-function val)))
       (array (loop for val across internal-sequence ,loop-conditional (funcall internal-predicate-function val))))))
	

(defmacro sequence-every-predicate-iterator (sequence predicate-function)
  `(sequence-predicate-iterator ,sequence ,predicate-function always))

(defmacro sequence-some-predicate-iterator (sequence predicate-function)
  `(sequence-predicate-iterator ,sequence ,predicate-function thereis))

(defmacro sequence-never-predicate-iterator (sequence predicate-function)
  `(sequence-predicate-iterator ,sequence ,predicate-function never))

(defun sequencep (thing) (or (consp thing) (arrayp thing)))
;; (defun numeric-sequence-p (seq) (every 'numberp seq)) ; nice but inefficient
(defun numeric-sequence-p (seq)
  (let ((sequence-type (type-of seq)))
    (or (true-p (and (arrayp seq) (or (member 'single-float sequence-type) (member 'double-float sequence-type))))
	(SEQUENCE-every-PREDICATE-ITERATOR seq 'numberp))))

(defun no-numerics-sequence-p (seq)
  (let ((sequence-type (type-of seq)))
    (if (and (arrayp seq) (or (member 'single-float sequence-type) (member 'double-float sequence-type)))
      nil
      (SEQUENCE-never-PREDICATE-ITERATOR seq 'numberp))))

(defun null-sequence-p (seq) (SEQUENCE-every-PREDICATE-ITERATOR seq 'null))
(defun no-nulls-sequence-p (seq) (SEQUENCE-never-PREDICATE-ITERATOR seq 'null))
(defun null-in-sequence-p (seq) (SEQUENCE-some-PREDICATE-ITERATOR seq 'null))
(defun null-list-p (list) (null-sequence-p list))
(defun sequence-null-p (seq) (null-sequence-p seq))
(defun sequence-not-all-null-p (seq) (not (null-sequence-p seq)))

(defun zero-sequence-p (seq) (SEQUENCE-every-PREDICATE-ITERATOR seq 'zerop))
(defun sequence-zerop (seq) (zero-sequence-p seq))
(defun sequence-not-all-zero-p (seq) (not (zero-sequence-p seq)))
(defun array-zero-p (array) (zero-sequence-p array))
(defun array-not-all-zero-p (array) (sequence-not-all-zero-p array))

(defun single-float-sequence-p (seq) (SEQUENCE-every-PREDICATE-ITERATOR seq #'(lambda (elt) (typep elt 'single-float))))
(defun single-float-in-sequence-p (seq) (SEQUENCE-some-PREDICATE-ITERATOR seq #'(lambda (elt) (typep elt 'single-float))))
(defun sequence-single-float-p (seq) (single-float-sequence-p seq))
(defun single-float-list-p (list) (single-float-sequence-p list))
(defun single-float-in-list-p (list) (single-float-in-sequence-p list))

(defun double-float-sequence-p (seq) (SEQUENCE-every-PREDICATE-ITERATOR seq  #'(lambda (elt) (typep elt 'double-float))))
(defun double-float-in-sequence-p (seq) (SEQUENCE-some-PREDICATE-ITERATOR seq  #'(lambda (elt) (typep elt 'double-float))))
(defun sequence-double-float-p (seq) (double-float-sequence-p seq))
(defun double-float-list-p (list) (double-float-sequence-p list))
(defun double-float-in-list-p (list) (double-float-in-sequence-p list))

(defun complement-sequence (seq)
  "Returns a BOOLEAN complement of the elements in SEQ."
  (map (type-of-seq seq) #'(lambda (elt) (null (true-p elt))) seq))

(defun complement-list (list) (complement-sequence list))

(defun flatten-no-nils (list &rest things)
  "Remove all NILs in LIST (consing any THINGS), and return one list. Uses FLATTEN-LIST."
  (loop for x in (flatten-list list things) when x collect x))

(defun no-nils (list)
  "Remove all NILs in LIST."
  (remove-if 'not list)
  ;; (loop for x in list when x collect x)
  )

(defun clean-up-list (list)
  "Remove all NILs in LIST and delete duplicates."
  (delete-duplicates (no-nils list)))

(defun replace-dotted-pair-with-list (list)
  (cond
   ((proper-list-p list) list)
   ((consp list) (cons (car list) (replace-dotted-pair-with-list (cdr list))))
   (t (list list))))

(defun flatten-list (thing &rest other-things)
  "Convert atoms and arrays to lists, and dotted-pairs into proper lists."
  (when (or thing (car other-things))
    (let ((thing (typecase thing
		   (string thing)
		   (array (array-to-list thing))
		   (list (REPLACE-DOTTED-PAIR-WITH-LIST thing))
		   (t thing)))
	  (other-things (if (equal other-things '(nil)) nil other-things)))
      (if (and (atom thing) (not other-things))
	  (list thing)
	  (loop for elt in (concatenate 'list (coerce-to-list thing) other-things)
		nconcing (if (sequencep elt) (flatten-list elt) (list elt)))))))

(defun flatten (the-list) (mapcan #'(LAMBDA (x) (IF (LISTP x) (flatten x) (LIST x))) the-list))

(defun flatten-sequence (sequence &rest other-sequences) (flatten-list sequence other-sequences))
(defun flatten-sequence-to-list (sequence &rest other-sequences) (flatten-list sequence other-sequences))
(defun flatten-no-nils-list (sequence &rest other-sequences) (no-nils (flatten-list sequence other-sequences)))
(defun flatten-no-nils-sequence (sequence &rest other-sequences) (flatten-no-nils-list sequence other-sequences))
(defun flatten-no-nils-sequence-to-list (sequence &rest other-sequences) (flatten-no-nils-list sequence other-sequences))
(defun flatten-no-nils-no-dups-list (sequence &rest other-sequences) (delete-duplicates (no-nils (flatten-list sequence other-sequences))))

(defun flatten-if-list (thing &rest other-things)
  (if (or other-things (consp thing)) (flatten-list thing other-things) thing))

(defun flatten-if-list-greater-than-1 (thing &rest other-things)
  (if (or other-things (and (consp thing) (> (length thing) 1)))
    (flatten-list thing other-things)
    (if (consp thing) (car thing) thing)))

(defun atomize-delete-duplicates-flatten-no-nils-list (list)
  (atomize-list (delete-duplicates (flatten-no-nils-list list))))

(defun atom-or-car (atom-or-list) (if (listp atom-or-list) (car atom-or-list) atom-or-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun df-zero-2d-array (array)
  (loop for x from 0 to (1- (car (array-dimensions array))) do
	(loop for y from 0 to (1- (cadr (array-dimensions array)))
	      do (setf (aref array x y) 0.0d0))))

(defun get-key-value-from-list (list target-key)
  (let ((signal nil))
    (loop for symbol in list
	  when signal do (return symbol)
	  when (eq symbol target-key)
	  do (setq signal t))))

(defun pad-end (list desired-length)
  ;; Return a list of length DESIRED-LENGTH whose head consists of LIST followed by the necessary number of duplicates of the last
  ;; value of the LIST.
  (let ((last-val (car (last list))))
    (append list (list-of-nums (- desired-length (length list)) last-val 0.0))))

(defun sequence-segment (sequence start end)
  (typecase sequence
    (array (let ((out (make-array (- end start))))
	     (loop for count from start to end
		   do (setf (aref out count) (aref sequence count)))
	     out))
    (cons (loop for count from 0 to end
		for val in sequence
		when (>= count start) collect val))))


(defun sequence-head (sequence &optional head-length)
  (if (and head-length (< head-length (length sequence)))
    (typecase sequence
      (array (let ((out (make-array head-length)))
	       (loop for count from 0 to (1- head-length)
		     do (setf (aref out count) (aref sequence count)))
	       out))
      (cons (loop for count from 0 to (1- head-length)
		  for val in sequence
		  collect val)))
    sequence))

(defun sequence-tail (sequence &optional tail-length)
  (if tail-length
    (let* ((length (length sequence))
	   (start (- length tail-length)))
      (typecase sequence
	(array (let ((out (make-array tail-length)))
		 (loop for count from 0 to (1- length)
		       when (>= count start)
		       do (setf (aref out (- count start)) (aref sequence count)))
		 out))
	(cons (loop for count from 0 to (1- length)
		    for val in sequence
		    when (>= count start)
		    collect val))))
    sequence))

(defun array-head (array &optional head-length) (sequence-head array head-length))
(defun array-tail (array &optional tail-length) (sequence-tail array tail-length))
(defun list-head (list &optional head-length) (sequence-head list head-length))
(defun list-tail (list &optional tail-length) (sequence-tail list tail-length))

;; From a flat MOTHER list, return a list of NUMBER-OF-CHILDREN lists, each made up of the ordered
;; members of the original list, taken mod NUMBER-OF-CHILDREN. Thus, with NUMBER-OF-CHILDREN = 4,
;;
;;   '(a b c d e f g) -> '((a d) (b e) (c g) (d))

(defun distribute-into-lists (mother number-of-children &optional return-as-values)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum number-of-children))
  (let ((result (loop for count fixnum from 1 to number-of-children collect '())))
    (loop for value in mother
	  for count fixnum from 0
	  do (push value (nth (mod count number-of-children) result))
	  finally (return (let ((dummy (loop for child in result collect (reverse child))))
			    (if return-as-values (values-list dummy) dummy))))))

;; *********************************
;; Sequence Conversions
;; *********************************

;; Consider VECTOR-TO-LIST* in /usr/src/cmucl/cmucl/src/code/seq.lisp

(defun coerce-to-list (stuff &rest rest)
  "If STUFF is an atom, return (LIST STUFF). If REST, then (LIST STUFF REST)."
  (when stuff
    (if (and (consp stuff) (not rest))
      stuff
      (if (and (sequencep stuff) (not (stringp stuff)))
	(concatenate 'list (coerce stuff 'cons) rest)
	(cons stuff rest)))))

(defun collect-to-array (thing &rest rest)
  "If THING is an atom, return #(THING). If REST, then #(THING REST)."
  (sequence-to-array (cons thing rest)))

;; (defun sequence-to-array (sequence) (coerce sequence 'simple-array))
(defun sequence-to-array (sequence) (coerce sequence 'vector))
(defun list-to-array (list) (sequence-to-array list))
(defun sequence-to-array-generic (sequence) (sequence-to-array sequence))
(defun sequence-to-gen-array (sequence) (sequence-to-array sequence))

(defun sequence-to-fixnum-array (sequence) (fix-array sequence))
;; LG 28.08.2016 CMUCL doesn't let MAP work with SIMPLE-ARRAY
;; (defun sequence-to-float-array-w-offset (sequence offset) (map 'simple-array #'(lambda (val) (s-flt (+ val offset))) sequence))
(defun sequence-to-float-array-w-offset (sequence offset) (map 'vector #'(lambda (val) (s-flt (+ val offset))) sequence))
;; (defun sequence-to-float-array-w-scale-and-offset (sequence scale offset) (map 'simple-array #'(lambda (val) (s-flt (* scale (+ val offset)))) sequence))
(defun sequence-to-float-array-w-scale-and-offset (sequence scale offset) (map 'vector #'(lambda (val) (s-flt (* scale (+ val offset)))) sequence))
(defun sequence-to-float-array (sequence) (s-flt-array sequence))
(defun sequence-to-double-float-array (sequence) (d-flt-array sequence))

(defun sequence-to-list (sequence) (coerce sequence 'cons))
(defun array-to-list (seq) (sequence-to-list seq))

(defun sequence-to-fixnum-list (sequence) (fix-list sequence))
(defun sequence-to-float-list-w-offset (sequence offset) (map 'list #'(lambda (val) (s-flt (+ val offset))) sequence))
(defun sequence-to-float-list-w-scale-and-offset (sequence scale offset) (map 'list #'(lambda (val) (s-flt (* scale (+ val offset)))) sequence))


(defun sequence-to-double-float-list (sequence) (d-flt-list sequence))

;; Only in case these are reference somewhere in old code. May 9 2001
(defun list-to-array-single (list) (s-flt-array list))
(defun list-to-array-double (list) (d-flt-array list))
(defun list-to-array-fix (list) (fix-array list))
(defun list-to-array-generic (list) (sequence-to-array list))

(defun array-to-float-list (seq) (s-flt-list seq))
(defun float-list (list) (s-flt-list list))
(defun single-float-list (list) (s-flt-list list))
(defun double-float-list (list) (d-flt-list list))
(defun single-float-sequence (sequence) (s-flt-gen sequence))
(defun double-float-sequence (sequence) (d-flt-gen sequence))

(defun move-list-to-array (list array)
  (loop for i from 0 to (1- (array-dimension array 0))
	for val in list do
	(setf (aref array i) val)))


(defun 2darray-to-list (seq)
  (loop for j from 0 to 1 collect
	(loop for i from 0 to (1- (nth 1 (array-dimensions seq)))
	      collect (aref seq j i))))

(defun float-2dlist-to-array (seq)
  (let* ((dim-0 (length seq))
	 (dim-1 (length (car seq)))
	 (array (make-array (list dim-0 dim-1) :element-type 'single-float)))
    (loop for list in seq
	  for j from 0
	  do
	  (loop for val in list
		for i from 0
		do (setf (aref array j i) val)))
    array))

(defun double-float-2darray (array)
  (let* ((dim-0 (car (array-dimensions array)))
	 (dim-1 (cadr (array-dimensions array)))
	 (new-array (make-array (list dim-0 dim-1) :element-type `double-float)))
    (loop for i from 0 to (1- dim-0) do
	  (loop for j from 0 to (1- dim-1) do
		(setf (aref new-array i j) (coerce (aref array i j) 'double-float))))
    new-array))

(defun single-float-2darray (array)
  (let* ((dim-0 (car (array-dimensions array)))
	 (dim-1 (cadr (array-dimensions array)))
	 (new-array (make-array (list dim-0 dim-1) :element-type `single-float)))
    (loop for i from 0 to (1- dim-0) do
	  (loop for j from 0 to (1- dim-1) do
		(setf (aref new-array i j) (coerce (aref array i j) 'single-float))))
    new-array))

(defun 2d-array-max-min (array)
  ;; Return  as values the max and min values of the 2d numeric ARRAY.
  (let (max min)
    (loop for x-index from 0 to (1- (array-dimension array 0)) do
	  (loop for y-index from 0 to (1- (array-dimension array 1)) do
		(let ((val (aref array x-index y-index)))
		  (unless min (setq min val) (setq max min))
		  (cond-every ((> val max) (setq max val))
			      ((< val min) (setq min val))))))
    (values max min)))

(defun 2d-array-max (array)
  (let ((max))
    (dotimes (x (array-dimension array 0))
      (dotimes (y (array-dimension array 1))
	(setq max (max (or max (aref array x y)) (aref array x y)))))
    max))

(defun 2d-array-min (array)
  (let ((min))
    (dotimes (x (array-dimension array 0))
      (dotimes (y (array-dimension array 1))
	(setq min (min (or min (aref array x y)) (aref array x y)))))
    min))

(defun normalize-2darray (array &optional invert (dynamic-range 100) (base-offset 100))
  (let* ((max (2d-array-max array))
	 (min (2d-array-min array))
	 (amp (- max min)))
    (loop for i from 0 to (1- (car (array-dimensions array))) do
	  (loop for j from 0 to (1- (cadr (array-dimensions array))) do
		(setf (aref array i j)
		      (let ((val (* (/ dynamic-range 100)
				    (+ (/ base-offset 100) (/ (- (aref array i j) min) amp)))))
			(if invert (- 1 val) val)))))
    array))
  
(defun 2dseq-to-array (seq)
  (let* ((dim-0 (length seq))
	 (dim-1 (length (car seq)))
	 (array (make-array (list dim-0 dim-1))))
    (typecase seq
      (array)
      (cons
       (loop for list in seq
	     for j from 0
	     do
	     (loop for val in list
		   for i from 0
		   do
		   (setf (aref array j i) val)))
       array))))

(defun make-2d-histo-array (x-data y-data x-incs y-incs
				   &key
				   print-out-max-mins
				   x-min x-max y-min y-max
				   (increment 1) (base-offset 0))
  (let* ((density-array (make-array (list x-incs y-incs) :element-type 'single-float))
	 (max-x-indice (1- x-incs))
	 (max-y-indice (1- y-incs))
	 (max-x (or x-max (sequence-max x-data)))
	 (min-x (or x-min (sequence-min x-data)))
	 (max-y (or y-max (sequence-max y-data)))
	 (min-y (or y-min (sequence-min y-data))))
    (loop for x in x-data
	  for y in y-data
	  do (let ((x-indice
		    (floor (* (+ 1 max-x-indice)
			      (/ (- x min-x)
				 (- max-x min-x)))))
		   (y-indice
		    (- max-y-indice
		       (floor (* (+ 1 max-y-indice)  (/ (- y min-y)
							(- max-y min-y)))))))
	       (when (and (<= 0 x-indice max-x-indice) (<= 0 y-indice max-y-indice))
		 (when (= (aref density-array x-indice y-indice) 0)
		   (setf (aref density-array x-indice y-indice) (s-flt base-offset)))
		 (setf (aref density-array x-indice y-indice)
		       (+ (s-flt increment) (aref density-array x-indice y-indice))))))
    (when print-out-max-mins (format t "Maximum/Minimum Values: ~A/~A~%"
				     (2d-array-max density-array)
				     (2d-array-min density-array)))
					;    (break)
    density-array))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ARRAY-SEARCH functions: Given a target value, find corresponding index of element in arrays.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-array-search (target function-array)
  (loop for index from 0
	when (or (>= index (array-dimension function-array 0))
		 (>= (aref function-array index) target))
	do (return index)))
  
;; The array index with an entry that is the closest from below to TARGET in FUNCTION-ARRAY is returned. FUNCTION-ARRAY must hold
;; a monotonically increasing function. Uses a recursive (binary tree?) search.
(defun array-search (target function-array &optional guess-index last-guess-index)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((array-dimension (array-dimension function-array 0)))
    (unless guess-index (setq guess-index (round (/ array-dimension 2))))
    (if (or (>= guess-index array-dimension) (= (aref function-array guess-index) target))
      guess-index
      (if (and last-guess-index (= (the fn guess-index) (the fn last-guess-index)))
	guess-index
	(array-search
	 target function-array
	 (+ (the fn guess-index)
	    (if (< (aref function-array guess-index) target)
	      (abs (round (/ (- guess-index (the fn (or last-guess-index array-dimension))) 2)))
	      (- (abs (round (/ (- (the fn guess-index) (the fn (or last-guess-index 0))) 2))))))
	 guess-index)))))

;; Double float (both TARGET and FUNCTION-ARRAY) version of ARRAY-SEARCH.
(proclaim '(inline array-search-double))
(defun array-search-double (target function-array &optional guess-index last-guess-index)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float target))
  (let* ((array-dimension (array-dimension (the (vector double-float) function-array) 0))
	 (this-guess-index (or guess-index (round (/ (the (UNSIGNED-BYTE 32) array-dimension) 2)))))
    (declare (fixnum this-guess-index))
    (if (or (>= (the fn this-guess-index) array-dimension)
	    (= (aref (the (vector double-float) function-array) this-guess-index) target))
      this-guess-index
      (if (and last-guess-index (= (the fn this-guess-index) (the fn last-guess-index)))
	this-guess-index
	(array-search
	 target function-array
	 (+ (the fn this-guess-index)
	    (if (< (aref (the (vector double-float) function-array) this-guess-index) target)
	      (the fn (round (abs (the sf (/ (- this-guess-index (the fn (or last-guess-index array-dimension))) 2.0)))))
	      (the fn (- (the fn (round (abs (the sf (/ (- (the fn this-guess-index) (the fn (or last-guess-index 0))) 2.0)))))))))
	 this-guess-index)))))

#|
********************************************************

In article <44chch$g58@newsbf02.news.aol.com>,
JScheller <jscheller@aol.com> wrote:
>How does one copy the values in an array to a new, independent array?

If it's a one-dimensional array (i.e. a vector), you can use COPY-SEQ.

Otherwise, you have to write a function that copies element-by-element.
Here's a function that should do it (it's untested):

|#
(defun copy-array (old-array &optional new-array)
  (let* ((rank (array-rank old-array))
	 (index-list (make-list rank :initial-element 0))
         (dimensions (apply #'vector (array-dimensions old-array))))
    (unless new-array
      (setq new-array
	    (make-array (apply #'list (array-dimensions old-array))
			:element-type (array-element-type old-array)
					;; :adjustable (array-adjustable-p old-array)
			:fill-pointer (and (array-has-fill-pointer-p old-array)
					   (fill-pointer old-array)))))
    (loop
     ;; copy an element
     (setf (apply #'aref new-array index-list)
	   (apply #'aref old-array index-list))
     ;; Increment the index
     (let (temp
	   (index-pos (1- rank)))	; start with last dimension
       (loop
	(when (minusp index-pos)
	  (return-from copy-array new-array))
	(setq temp (incf (nth index-pos index-list)))
	(if (>= temp (svref dimensions index-pos))
	  ;; carry into higher dimension
	  (progn
	    (setf (nth index-pos index-list) 0)
	    (decf index-pos))
	  ;; return from the increment loop
	  (return nil)))))))
#|
-- 
Barry Margolin
BBN PlaNET Corporation, Cambridge, MA
barmar@bbnplanet.com
Phone (617) 873-3126 - Fax (617) 873-6351
********************************************************
********************************************************
|#

#|
(defun copy-array (array)
  (adjust-array (make-array (array-dimensions array)
			    :displaced-to array
			    :element-type (array-element-type array))
		(array-dimensions array)
		:displaced-to nil))
|#

#|
   From: Erik Naggum <erik@naggum.no>
   Newsgroups: comp.lang.lisp
   Date: 06 Feb 1996 16:27:30 +0000
   Organization: Naggum Software; +47 2295 0313
|#

(defun insert-after (newelt list index)
  "Insert NEWELT in LIST after the INDEXth cell.
   Returns LIST."
  (let ((cell (nthcdr index list)))
    (setf (cdr cell) (cons newelt (cdr cell))))
  list)

#|
Beautiful solution that can be rewritten to an even more concise form:
|#

(defun insert-after (newelt list index)
  "Insert NEWELT in LIST after the INDEXth cell.
  Returns LIST."
  (push newelt (cdr (nthcdr index list)))
  list)

#|
Bernhard Pfahringer
Austrian Research Institute for  http://www.ai.univie.ac.at/~bernhard/
Artificial Intelligence          bernhard@ai.univie.ac.at 
|#

(export '(SUM-SINGLE-FLOAT-ARRAYX*ARRAYX SUM-SINGLE-FLOAT-ARRAYX*ARRAYY
					 SUM-SINGLE-FLOAT-ARRAY SUM-DOUBLE-FLOAT-ARRAY SUM-DOUBLE-FLOAT-ARRAYX*ARRAYY
					 SUM-DOUBLE-FLOAT-ARRAYX*ARRAYX EQUAL-DOUBLE-FLOAT-ARRAY)) 


(export '(NEGATE NEGATE-SINGLE-FLOAT
	  NORMALIZE-LIST-LENGTHS
	  insert-after
	  find-closest-list-value
	  
	  DISTRIBUTE-INTO-LISTS
	  NUM-OR-SYMBOL-OR-STRING-LIST-P 
	  proper-list-p SEQUENCEP
	  numeric-sequence-p no-numerics-sequence-p
	  null-sequence-p no-nulls-sequence-p null-in-sequence-p null-list-p sequence-null-p sequence-not-all-null-p
	  zero-sequence-p SEQUENCE-ZEROP sequence-not-all-zero-p array-zero-p array-not-all-zero-p
	  single-float-sequence-p single-float-in-sequence-p sequence-single-float-p single-float-list-p single-float-in-list-p
	  double-float-sequence-p double-float-in-sequence-p sequence-double-float-p double-float-list-p double-float-in-list-p
	  
	  COMPLEMENT-LIST COMPLEMENT-sequence

	  flatten flatten-sequence FLATTEN-SEQUENCE-TO-LIST replace-dotted-pair-with-list FLATTEN-LIST
	  no-nils CLEAN-UP-LIST flatten-no-nils-list flatten-no-nils-sequence flatten-no-nils-sequence-to-list
	  flatten-no-nils-no-dups-list FLATTEN-IF-LIST flatten-if-list-greater-than-1
	  atomize-delete-duplicates-flatten-no-nils-list
	  atom-or-car


	  REDUCE-REPEATS
	  
	  add-to-end
	  
	  FLOAT-2DLIST-TO-ARRAY

	  hash-table-list
	  print-table-keys
	  hash-table-empty
	  printit
	  named-structure-symbol

	  FIRST-ELEMENT LAST-ELEMENT
	  my-delete
	  FAST-SORT-DELETE-DUPLICATES
	  
	  get-key-value-from-list
	  
	  push-lists-onto-list chop-list
	  every-nth
	  order-list-from-key-list
	  replace-dotted-pair-with-list

	  sum-double-float-listx*listy sum-double-float-listx*listx sum-float-list sum-double-float-list
	  
	  split-up 	  concatenate-to-sf-array
	  2darray-to-list MOVE-LIST-TO-ARRAY
	  copy-array


	  
	  sequence-to-array SEQUENCE-TO-ARRAY-GENERIC sequence-to-gen-array list-to-array list-to-array-generic
	  sequence-to-fixnum-array list-to-array-fix

	  sequence-to-float-array-w-offset sequence-to-float-array-w-scale-and-offset
	  SEQUENCE-TO-FLOAT-ARRAY sequence-to-double-float-array LIST-TO-ARRAY-DOUBLE
	  COLLECT-TO-ARRAY
	  sequence-to-list array-to-list coerce-to-list
	  sequence-to-fixnum-list
	  float-list array-to-float-list
	  sequence-to-float-list-w-offset sequence-to-float-list-w-scale-and-offset
	  sequence-to-double-loat-list double-float-list
	  2DSEQ-TO-ARRAY DOUBLE-FLOAT-2DARRAY single-float-2darray

	  boolean-list list-of-nums-start-end list-of-nums list-of-sf-nums array-of-nums LIST-OF-INTS
	  DF-ZERO-2D-ARRAY
	  THING-IN-ARRAY-P
	  2d-array-max-min
	  2D-ARRAY-MAX
	  2D-ARRAY-Min
	  NORMALIZE-2DARRAY
	  make-2d-histo-array

	  atomize-list
	  generic-intersection
	  REM-NOT-IN-KEYS  REM-IN-KEYS
	  LIST-HEAD
	  SEQUENCE-SEGMENT
	  SEQUENCE-HEAD
	  ARRAY-HEAD
	  ARRAY-TAIL SEQUENCE-TAIL

	  PAD-END
	  
	  LIST-TAIL
	  normalize-sequence
	  ADD-VAL-TO-FLOAT-LIST SCALE-FLOAT-LIST
	  	  SCALE-AND-OFFSET-FLOAT-LIST
	  SCALE-AND-OFFSET-FLOAT-ARRAY
	  SCALE-FLOAT-ARRAY

	  
	  
	  DOUBLE-LISTS
	  MAXIMIZE-VAL-OR-SEQ MINIMIZE-VAL-OR-SEQ A-BIT-MORE A-BIT-LESS
	  max-of-list min-of-list sequence-max sequence-min 
	  remove-all
	  simple-array-search array-search array-search-double
	  ))  



	  
	  
         



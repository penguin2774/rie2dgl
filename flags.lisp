(in-package :rie2dgl)

(defmacro def-flag-fn (name &rest flags)
  "Defines a function for working with flags (see functions documentation for usage."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
		(defun ,name (op &rest flags)
		  ,(format nil "Performs bitwise operations with a set of keywords treated as bits of a byte.
Operations may be:
:ior - inclusive or on all 'flags'
:and - ands first 'flag' (as integer) with following keywords.
keyword - returns individual integer for that flag
keyword* - returns ior of all keywords
Possible keywords are (in order) ~{~a~^, ~}." flags)
		  (declare (optimize (safety 0) (speed 3))
			   (keyword op))
		  (labels ((key-to-flag (key) 
			     (declare (keyword key))
			     (ecase key
			       ,@(loop for j in flags
				    for c from 0
			 
				    collect `(,j ,(ash 1 c)))))
			   (ior-flags (flags)
			     (apply #'logior (loop for i in  flags
						collect (the fixnum (key-to-flag i))))))
		    (case op 
		      (:ior
		       (ior-flags flags))
		      (:and
		       (logand (the fixnum (car flags)) (the fixnum (ior-flags flags)) ))
		      (otherwise
		       (if flags
			   (ior-flags (cons op flags))
			   (key-to-flag op))))))))


(defmacro test-flags (fn integer &rest flags)
  "Test to see if all FLAGS, using flag-fn FN, are set in INTEGER. Atempts to do all computation at compile time if FLAGS are all keywords. 
 (note: will evaluate FN if that is the case)"
  (if (every #'keywordp flags)
      (let ((pre-computed-flag-integer (apply (eval fn) :ior flags)))
	`(equal (logand ,pre-computed-flag-integer ,integer) ,pre-computed-flag-integer))
      `(let ((result (apply ,fn :ior flags)))
	 (equal (logand result ,integer) result))))
      
	      
	      
	  
       


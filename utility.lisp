(in-package :rie2dgl)

(defmacro def-accessor-methods (class &rest vars)
  `(progn ,@(loop for foreign-slot in vars
	      append `((ensure-generic-function  ',foreign-slot :lambda-list '(object))
		       (defmethod ,foreign-slot ((self ,class))
			 (,(intern (concatenate 'string (symbol-name class) "-GET-" (symbol-name foreign-slot)) 'backend) (fp self)))
		       (ensure-generic-function '(setf ,foreign-slot) :lambda-list  '(value obj))
		       (defmethod (setf ,foreign-slot) (value (self ,class))
			 (,(intern (concatenate 'string (symbol-name class) "-CHANGE-" (symbol-name foreign-slot)) 'backend) (fp self) value))))))

(defmacro def-reader-methods (class &rest vars)
  `(progn ,@(loop for foreign-slot in vars
	       append `((ensure-generic-function ',foreign-slot :lambda-list  '(object))
		       (defmethod ,foreign-slot ((self ,class))
			 (,(intern (concatenate 'string (symbol-name class) "-GET-" (symbol-name foreign-slot)) 'backend) (fp self))) ))))

(defmacro def-writer-methods (class &rest vars)
  `(progn ,@(loop for foreign-slot in vars
	      append `((ensure-generic-function (setf ',foreign-slot) :lambda-list '(value obj))
		       (defmethod (setf ,foreign-slot) (value (self ,class))
			 (,(intern (concatenate 'string (symbol-name class) "-CHANGE-" (symbol-name foreign-slot)) 'backend) (fp self) value))))))
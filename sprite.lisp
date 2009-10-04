(in-package rie2dgl)




(defclass sprite (animation)
  ((texture-dict
   :reader texture-dict)
   (current
    :reader current)))




(defmethod initialize-instance  ((self sprite) &rest initargs &key texture first &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (texture-dict) self
    (if (not (loaded? texture))
	(error "Frames texture ~a not loaded." texture))
    (if (not first)
	(error "Need a texture keyword for sprite ~a." self))
    (if (not (typep texture 'texture-dict))
	(error "Expected a :texture of type TEXTURE-DICT got one of type ~a" (type-of texture)))
    (setf texture-dict texture)
    (when (not (slot-boundp self 'texture))
      (if (listp first)
	  (apply #'change self first)
	  (change self first)))
    
    (call-next-method)))


(defgeneric change (object &rest new))
(defmethod change ((self sprite) &rest new)

  (labels ((make-change (new-texture new)
	     
	     (etypecase new-texture
	       (texture-dict
		(if (cdr new)
		    (make-change (reference-texture new-texture (second new)) (cdr new))
		    (error "change referenced a texture-dict but had no more keywords.")))
	       (texture-list
		(if (and (cdr new) (not (typep (second new) '(or ratio integer))))
		    (error "Got a texture-list, but had more keywords left: ~{~a~^, ~}." (cdr new))
		    (if (typep (second new) '(or ratio integer))
			(change-frames self new-texture (second new))
			(change-frames self new-texture))))
	       (texture
		(if (cdr new)
		    (error "Got a texture, but had more keywords left: ~{~a~^, ~}." (cdr new))
		      
		    (change-texture self new-texture))))))
    (make-change (reference-texture (texture-dict self) (car new)) new)
    (setf (slot-value self 'current) (loop for i in new
					if (symbolp i)
					collect i))))
       
       
	
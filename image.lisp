(in-package :rie2dgl)


    

(defclass image ()
  ((texture
    :initarg texture
    :reader texture
    :documentation "The texture object, can be changed with CHANGE-TEXTURE.")
   (location
    :reader location
    :initform (make-array '(3)  :element-type 'single-float :initial-element 0.0)
    :type (vector single-float 3)
    :documentation "A 3 element wide vector for 3d possitioning (third dimention only relevent in prospective mode)")
   (render-spec
    :reader render-spec
    :documentation "Holds a custom RENDER-SPEC if center is changed, or other changes to the rectangle are performed.")
   (scale
    :reader scale
    :initarg :scale
    :type (or single-float simple-vector)
    :initform 1.0
    :documentation "Scale value of the image, should be a float, if 1.0, is ignored.")
   (rotation
    :reader rotation
    :type single-float
    :initform 0.0
    :documentation "Rotation along z axis, changeable with ROTATE.")
   (width 
    :accessor width
    :type integer
    :documentation "Width of the texture (scale not taken into account).")
   (height
    :accessor height
    :type integer
    :documentation "Height of the texture (scale not taken into account).")
   (sub-images
    :reader sub-images
    :initarg :sub-images
    :initform (list)
    :documentation "Sub Images of the IMAGE, they are renderd immediatly after the IMAGE and within its transformation matrix")))


(defmethod (setf scale) (value (self image))
  (declare (optimize (speed 3) (safety 1))
	   ((or real simple-vector) value))
  (etypecase value
    (simple-vector
     (setf (slot-value self 'scale) value))
    (single-float
     (setf (slot-value self 'scale) value))
    (number
     (setf (slot-value self 'scale) (coerce value 'single-float)))))
	
    
(defmethod initialize-instance ((self image) &rest initargs &key center texture (location '(0 . 0)) scale &allow-other-keys)
  (declare (ignore initargs))
  
  (let ((loc location))
    (if (not (slot-boundp self 'texture))
	(if (not (loaded? texture))
	    (error "Texture ~a not loaded." texture)
	    (setf (slot-value self 'texture) texture)))
    (setf (width self) (width (slot-value self 'texture)))
    (setf (height self) (height (slot-value self 'texture)))
    (setf (scale self) (or scale 1.0))
    (call-next-method)
    (with-xyz-slots ((location 3)) self
      (typecase location
	(list
	 (relocate self 
	       (car loc)
	       (if (consp (cdr loc))
		   (second loc)
		   (cdr loc)) 
	       (or (third loc) 0.0)))
	(sequence
	 (relocate self (elt loc 0) (elt loc 1) (if (= (length loc) 3)
						(elt loc 2)
						0.0)))))
    (if center
	(set-center self (car center) (if (consp (cdr center))
					  (second center)
					  (cdr center))))))
  

  



(defmethod cache ((self image))
  "Calls CACHE on RENDER-SPEC if it exists otherwise on TEXTURE"
  (declare (inline cache))
  (if (render-spec self)
      (cache (render-spec self))
      (cache (texture self))))

(defgeneric render (image))

(defmethod render ((image image))
  (with-slots (texture scale rotation  location sub-images width height) image 
     (declare (optimize (speed 3) (safety 1))
	     (texture texture)
	     (type (or single-float simple-vector) scale)
	     (single-float rotation)
	     ((vector single-float 3) location)
	     (list sub-images)
	     (integer width)
	     (integer height))
    (gl:enable :blend :texture-2d)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    
    (gl:with-pushed-matrix
      ;(with-xyzs ((location 3))
;	(declare (single-float location.x location.y location.z))
	(gl:translate (aref location 0) (aref location 1) (aref location 2));)
      
	(if (not (zerop rotation)) 
	  (gl:rotate rotation 0.0 0.0 1.0))

     (if (not (equal scale 1.0))
	  (if (vectorp scale)
	      (with-xyzs ((scale 2))
		(declare (single-float scale.x scale.y))
		(gl:scale scale.x scale.y 1.0))
	      (gl:scale scale scale 1.0)))
     
      (gl:bind-texture :texture-2d (name texture))
      (if (slot-boundp image 'render-spec)
	  (render (render-spec image))
	  (render (render-spec texture)))
      (loop for i in sub-images
	 do (render i))))
  (gl:disable :blend :texture-2d))

(defmethod set-center ((self image) w-ratio h-ratio)
  (if (slot-boundp self 'render-spec)
      (set-center (render-spec self) w-ratio h-ratio)
      (progn
	(setf (slot-value self 'render-spec) 
	      (make-instance 'render-spec :width (width (texture self)) :height (height (texture self))))
	(set-center (render-spec self) w-ratio h-ratio))))

(defgeneric add-subimage (image sub-image))

(defmethod add-subimage ((image image) sub-image)
  (push sub-image (slot-value image 'sub-images)))

(defgeneric rotate (image degrees))

(defmethod rotate ((image image) degrees)
  (declare (optimize (speed 3))
	   (real degrees))
  (incf (the single-float (slot-value image 'rotation)) (coerce degrees 'single-float)))

(defgeneric move (object x y &optional z))
(defmethod move ((self image) x y &optional (z 0.0))
    (with-slots (location) self
    (declare (optimize (speed 3) (safety 1))
	     (real x y z)
	     ((vector single-float 3) location))
    (incf (aref location 0) (coerce x 'single-float))
    (incf (aref location 1) (coerce y 'single-float))
    (incf (aref location 2) (coerce z 'single-float))))

       
(defgeneric relocate (object x y &optional z))
(defmethod relocate ((self image) x y &optional ( z 0.0))
  (with-slots (location) self
    (declare (optimize (speed 3) (safety 1))
	     (real x y z)
	     ((vector single-float 3) location))
    (setf (aref location 0) (coerce x 'single-float))
    (setf (aref location 1) (coerce y 'single-float))
    (setf (aref location 2) (coerce z 'single-float))))
  
     
(defgeneric (setf location) (value object))
(defmethod (setf location) (value (self image))
  (declare (inline (setf location)))
  (with-xyzs ((value 3))
    (relocate self value.x value.y value.z)))
  

(defgeneric change-texture (object new-texture))
(defmethod change-texture ((self image) new-texture)
  (declare (optimize (speed 3) (safety 1))
	   (texture new-texture))
  (if (slot-value new-texture 'name)
      (with-slots (texture width height) self
	(setf texture new-texture)
	(setf width (width new-texture))
	(setf height (height new-texture)))
      (error "New texture does not have a texture name")))
      
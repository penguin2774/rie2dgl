(in-package :rie2dgl)


    

(defclass image ()
  ((fp
    :reader fp
    :documentation "Foreign pointer to the image structure")
   (sprite-fp
    :reader sprite
    :documentation "Foreign pointer to the sprite wraper structure"))
  )



  
	
    
(defmethod initialize-instance ((self image) &rest initargs &key  texture x y z (scale 1.0) (rot 0.0) &allow-other-keys)
  (declare (ignore initargs))
  (let* ((img (backend:make-image (fp texture) x y z scale rot))
	 (sprite (backend:make-sprite img :image)))
    (tg:finalize self (lambda ()
			(backend:free-sprite sprite)))
    (setf (slot-value self 'fp) img)
    (setf (slot-value self 'sprite-fp) sprite)))
  

(defgeneric scale (object value))

(defmethod scale ((self image) value)
  (backend:scale-image self value))



(defmethod cache ((self image))
  "Calls CACHE on RENDER-SPEC if it exists otherwise on TEXTURE"
  (backend:cache-image (fp self)))

(defgeneric render (image))

(defmethod render ((image image))
  (backend:render-image (fp image)))

(defmethod set-center ((self image) w-ratio h-ratio)
  (backend:set-center (fp self) w-ratio h-ratio))

(defgeneric push-subimage (image sub-image))

(defmethod push-subimage ((self image) sub-image)
  (backend:push-subimage (fp self) (sprite sub-image)))

(defgeneric pop-subimage (image))

(defmethod pop-subimage ((self image))
  (backend:pop-subimage (fp self)))

(defgeneric rem-subimage (self target))

(defmethod rem-subimage ((self image) target)
  (backend:rem-subimage (fp self) (sprite target)))


(defgeneric rotate (image degrees))

(defmethod rotate ((self image) degrees)
  (backend:rotate-image (fp self) degrees))

(defgeneric move (object x y &optional z))
(defmethod move ((self image) x y &optional (z 0.0))
  (backend:move-image (fp self) x y z))

 
(defgeneric relocate (object x y &optional z))
(defmethod relocate ((self image) x y &optional ( z 0.0))
  (backend:relocate-image (fp self) x y z))
  
     
(defgeneric change-texture (object new-texture))
(defmethod change-texture ((self image) new-texture)
  (backend:change-image-texture (fp self) (fp new-texture)))
      
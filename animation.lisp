(in-package rie2dgl)


(defclass animation ()
  ((fp 
    :reader fp
    :documentation "Hold the foreign pointer.")
   (sprite-fp
    :reader sprite
    :documentation "Holds the foreign pointer to the sprite structure.")))

(def-flag-fn animation-flags :only-once)

(defmethod initialize-instance  ((self animation) &rest initargs &key texture (frame-rate 1) (flags 0) x y (z 0.0) (scale 1.0) (rot 0.0) &allow-other-keys)
  (declare (ignore initargs))
  (let* ((anim (etypecase texture
		 (texture
		  (backend:make-disabled-animation (fp texture) (float frame-rate) flags (float x) (float y) (float z) (float scale) (float rot)))
		 (texture-list
		  (backend:make-animation (fp texture) (len texture) (float frame-rate) flags (float x) (float y) (float z) (float scale) (float rot)))))
	 (sprite (backend:make-sprite anim :animation)))
    (tg:finalize self (lambda ()
			(backend:free-sprite sprite)))
    (setf (slot-value self 'fp) anim)
    (setf (slot-value self 'sprite-fp) sprite)))

(defgeneric disabled? (object))
(defmethod disabled? ((self animation))
  (backend:animation-disabledp (fp self)))

(defgeneric stopped? (object))
(defmethod stopped? ((self animation))
  (backend:stoppedp (fp self)))

(defmethod scale ((self animation) value)
  (backend:scale-image (backend:get-image-data (fp self)) (float value)))


(defmethod outside-bounds? ((self animation) x1 y1 x2 y2)
  (backend:rect2d-outside-test (backend:get-image-data (fp self)) x1 y1 x2 y2))

(def-accessor-methods animation
    ticks frame-rate current-frame)

  

(defmethod render ((self animation))
  
  (backend:render-animation (fp self)))


(defmethod set-center ((self animation) w-ratio h-ratio)
  (backend:set-center (backend:get-image-data (fp self)) w-ratio h-ratio))



(defmethod push-subimage ((self animation) sub-image)
  (backend:push-subimage (backend:get-image-data (fp self)) (sprite sub-image)))



(defmethod pop-subimage ((self animation))
  (backend:pop-subimage (backend:get-image-data (fp self))))



(defmethod rem-subimage ((self animation) target)
  (backend:rem-subimage  (backend:get-image-data (fp self)) (sprite target)))





(defmethod rotate ((self animation) degrees)
  (backend:rotate-image (backend:get-image-data (fp self)) degrees))


(defmethod move ((self animation) x y &optional (z 0.0))
  (backend:move-image (backend:get-image-data (fp self)) x y z))

 

(defmethod relocate ((self animation) x y &optional ( z 0.0))
  (backend:relocate-image (backend:get-image-data (fp self)) x y z))
  

(defgeneric x (object))
(defmethod x ((self animation))
  (cffi:mem-aref (backend:image-get-loc (backend:get-image-data (fp self))) :float 0))

(defgeneric y (object))
(defmethod y ((self animation))
  (cffi:mem-aref (backend:image-get-loc (backend:get-image-data (fp self))) :float 1))

(defgeneric z (object))
(defmethod z ((self animation))
  (cffi:mem-aref (backend:image-get-loc (backend:get-image-data (fp self))) :float 2))


     
(defgeneric change-frames (object texture-list &optional new-frame-rate new-flags))
(defmethod change-frames ((self animation) texture-list &optional new-frame-rate new-flags)

  (change-texture self texture-list)
  (if new-frame-rate
      (backend:animation-change-frame-rate (fp self) new-frame-rate))
  (if new-flags
      (backend:animation-change-flags (fp self) new-flags)))

(defmethod change-texture ((self animation) new-texture)
  (etypecase new-texture 
    (texture
     (backend:change-frames-disable (fp self) (fp new-texture)))
    (texture-list

     (backend:change-frames  (fp self) (fp new-texture) (len new-texture)))))
      

(defgeneric toggle (object))
(defmethod toggle ((self animation))
  (backend:toggle (fp self)))
	       

(defgeneric start (object))
(defmethod start ((self animation))
  (backend:start (fp self)))

      
(defgeneric stop (object))
(defmethod stop ((self animation))
  (backend:stop (fp self)))

(defgeneric reset-ticks (object))
(defmethod reset-ticks ((self animation))
  (backend:reset-ticks (fp self)))
	


	  


(defgeneric next-frame (object))
(defmethod next-frame ((self animation))
  (backend:next-frame (fp self)))
  
(defgeneric prev-frame (object))
(defmethod prev-frame ((self animation))
  (backend:prev-frame (fp self)))

(defgeneric set-frame (object num))

(defmethod set-frame ((self animation) num)
  (backend:set-frame (fp self) num))

(defgeneric first-frame (object))
(defmethod first-frame ((self animation))
  (backend:first-frame (fp self)))

(defgeneric last-frame (object))
(defmethod last-frame ((self animation))
  (backend:last-frame (fp self)))





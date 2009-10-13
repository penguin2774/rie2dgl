(defpackage :backend
  (:use :cl :cffi))


(in-package :backend)

(define-foreign-library backend
    (:unix "./backend.so")
    (t (:default "backend")))


(use-foreign-library backend)

(defcstruct render-spec
  (texture-rect :float :count 4)
  (quad-points :float :count 4)
  (center-ratio :float :count  2)
  (list-cache :unsigned-int)
  (w :int)
  (h :int))


(defcstruct texture 
  (name :unsigned-int)
  (spec (:pointer render-spec)))

(defcstruct sprite-cons
  (data :pointer)
  (next :pointer))

(defcstruct image
  (texture (:pointer texture))
  (loc :float :count 3)
  (spec (:pointer render-spec))
  (scale :float)
  (rot :float)
  (w :int)
  (h :int)
  (subs (:pointer sprite-cons)))

(defcstruct animation 
  (image (:pointer image))
  (texture (:pointer :pointer))
  (ticks :float)
  (frame-rate :float)
  (frame-count :int)
  (current-frame :int)
  (flags :int))
  
(defcenum sprite-type
  :image
  :animation)

(defcunion sprite-union
  (image (:pointer image))
  (anim (:pointer animation))
  (raw (:pointer)))

(defcstruct sprite
  (type sprite-type)
  (data sprite-union))
    

(defconstant anim-stopped (ash 1 0))
(defconstant anim-loop (ash 1 1))

(defcfun make-render-spec (:pointer render-spec)
  (w :int)
  (h :int))



(defcfun set-center (:pointer render-spec)
  (spec (:pointer render-spec))
  (w_ratio :int)
  (h_ratio :int))

(defcfun recenter (:pointer render-spec)
  (spec (:pointer render-spec)))


(defcfun cachedp :int
  (spec (:pointer render-spec)))

(defcfun cache (:pointer render-spec)
  (spec (:pointer render-spec)))

(defcfun free_cache (:pointer render-spec)
  (spec (:pointer render-spec)))


(defcfun render-render-spec :void
  (spec (:pointer render-spec)))
  
(defcfun free-render-spec :void
  (spec (:pointer render-spec)))


; ####################################### texture functions ##################

(defcfun loadedp :int
  (tex (:pointer texture)))

(defcfun make-texture (:pointer texture)
  (name :unsigned-int)
  (spec (:pointer render-spec)))
  
(defcfun free-texture :void
  (tex (:pointer texture)))



; ####################################### image functions 

(defcfun make-image (:pointer image)
  (tex (:pointer texture))
  (x :float)
  (y :float)
  (z :float)
  (scale :float)
  (rot :float))


(defcfun free-image :void
  (img (:pointer image)))

(defcfun cache-image (:pointer image)
  (img (:pointer image)))


(defcfun render-image :void
  (img (:pointer image)))




(defcfun set-image-center (:pointer image)
  (image (:pointer image)
	 (w-ratio :float)
	 (h-ratio :float)))


(defcfun push-subimage (:pointer image)
  (image (:pointer image))
  (sub (:pointer sprite)))

;; no way of checking success...
(defcfun rem-subimage (:pointer image) 
  (image (:pointer image))
  (sub (:pointer sprite)))


(defcfun pop-subimage (:pointer sprite)
  (image (:pointer image)))


(defcfun rotate-image (:pointer image)
  (image (:pointer image))
  (radians :float))


(defcfun scale-image (:pointer image)
  (image (:pointer image))
  (scale :float))

(defcfun move-image (:pointer image)
  (image (:pointer image))
  (x :float)
  (y :float)
  (z :float))

(defcfun relocate-image (:pointer image)
  (image (:pointer image))
  (x :float)
  (y :float)
  (z :float))



(defcfun change-image-texture (:pointer image)
  (image (:pointer image))
  (texture (:pointer texture)))
    

;; #################################### Animation Fuctions


(defcfun  make-animation (:pointer animation)
  (texs (:pointer :pointer))
  (count :unsigned-int)
  (frame-rate :float)
  (flags :long)
  (x :float)
  (y :float)
  (z :float)
  (scale :float)
  (rot :float))

(defun make-animation-from-list (texs frame-rate flags x y z  scale rot)
  (let ((array (foreign-alloc '(:pointer texture) :count (length texs))))
    (loop for i in texs
	 for j from 0
	 do (setf (mem-aref array '(:pointer texture) j) i))
  (make-animation array (length texs) frame-rate flags x y z scale rot)))

(defcfun free-animation :void
  (anim (:pointer animation)))


(defcfun stoppedp :int
    (anim (:pointer animation)))

(defcfun last-framep(:pointer animation)
    (anim (:pointer animation)))


(defcfun toggle (:pointer animation)
    (anim (:pointer animation)))


(defcfun start (:pointer animation)
    (anim (:pointer animation)))


(defcfun stop (:pointer animation)
    (anim (:pointer animation)))


(defcfun reset-ticks (:pointer animation)
    (anim (:pointer animation)))


(defcfun change-frames  (:pointer animation)
    (anim (:pointer animation))
    (texs (:pointer texture))
    (count :int))


(defcfun next-frame  (:pointer animation)
    (anim (:pointer animation)))

(defcfun prev-frame (:pointer animation)
    (anim (:pointer animation)))

(defcfun set-frame  (:pointer animation)
    (anim (:pointer animation))
    (frame :unsigned-int))

(defcfun first-frame  (:pointer animation)
    (anim (:pointer animation)))

(defcfun last-frame  (:pointer animation)
    (anim (:pointer animation)))


(defcfun render-animation :void
    (anim (:pointer animation)))
;; #################################### Sprite Functions

(defcfun make-sprite (:pointer sprite)
  (data :pointer)
  (type sprite-type))


(defcfun free-sprite :void
  (sprite (:pointer sprite)))

(defcfun render-sprite :void
  (sprite (:pointer sprite)))

(defcfun render-sprites :void
  (sprites (:pointer :pointer))
  (count (:unsigned-int)))


(defun get-sprite-data (sprite)
  (ecase (foreign-slot-value sprite 'sprite 'type)
    (:image
     (foreign-slot-value (foreign-slot-value sprite 'sprite 'data) 'sprite-union 'image))
    (:animation
     (foreign-slot-value (foreign-slot-value sprite 'sprite 'data) 'sprite-union 'anim))))

(defun get-sprite-image-data (sprite)
  (ecase (foreign-slot-value sprite 'sprite 'type)
    (:image
     (foreign-slot-value (foreign-slot-value sprite 'sprite 'data) 'sprite-union 'image))
    (:animation
     (foreign-slot-value (foreign-slot-value (foreign-slot-value sprite 'sprite 'data) 'sprite-union 'anim) 'animation 'image))))
    
    

(defparameter *SPRITES* 10000)
(defun standard-bind (texture surface)
  
  (gl:bind-texture :texture-2d texture)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-image-2d :texture-2d 0
		   :rgba (sdl:width surface) (sdl:height surface)
		   0 :rgba :unsigned-byte (sdl-base::with-pixel (pixels (sdl::fp surface))
					    (sdl-base::pixel-data pixels))))



(defun setup-RC ()
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  (gl:disable :depth-test)
  (gl:disable :lighting)
  (gl:disable :dither)
  (gl:disable :blend))

   

(defun clear-scene ()
  (gl:clear :color-buffer)
  (gl:color 1.0 1.0 1.0))
  

(defun resize-window (w h)
  (when (zerop h)
    (setf h 1))
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d  0.0 (float w) 0.0 (float h))
  (gl:matrix-mode :modelview)
  
  (gl:load-identity))

(defparameter +SPRITES+ 500)
(defun make-bounce-fn (low high &key (step 1) start-value (start-dir :up))
  (assert (or (eq start-dir :up)
	      (eq start-dir :down)))
  (let ((var (or start-value low))
	(dir start-dir))
    (lambda ()
      (if (eq dir :up)
	  (when (>= (incf var step) high)
	    (setf var high)
	    (setf dir :down))

	  (when (<= (decf var step) low)
	    (setf var low)
	    (setf dir :up)))
      var)))

(defun bind-textures-to-files (&rest files)
  
  (loop for file in files
       for tex in (gl:gen-textures (length files))
       collect 
       (sdl:with-surface (surf (sdl-image:load-image file))
	 (let ((result (make-texture tex (make-render-spec (sdl:width surf)
							   (sdl:height surf)))))
	   (standard-bind tex surf)
	   (gl:disable :texture-2d)
	   result))))
       

(defun test ()
  (sdl:with-init (sdl-cffi::sdl-init-video )
    
      (let ((w (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Testing!"))
	    (textures (bind-textures-to-files  "/home/nathan/prj/rie2dgl/test-images/cow-1.png"
					       "/home/nathan/prj/rie2dgl/test-images/cow-2.png")) sprites start-p)
	    
	(print (length textures))
	
	
	(setf sprites (foreign-alloc '(:pointer sprite) :count +SPRITES+))
	(loop for i from 0 below +SPRITES+
	   do (setf (mem-aref sprites '(:pointer sprite) i) (make-sprite (make-animation-from-list textures (float 1/20) ANIM-LOOP (random 512.0) (random 512.0)  0.0 1.0 0.0) :animation)))
;; 	(setf sprites (list (make-image texture 0.0 0.0 0.0 1.0 0.0)
;; 			    (make-image texture 0.0 512.0 0.0 1.0 0.0)
;; 			    (make-image texture 512.0 512.0 0.0 1.0 0.0)
;; 			    (make-image texture 512.0 0.0 0.0 1.0 0.0)))
	
	
	(resize-window 512 512)
	(setup-RC)
	
	(setf (sdl:frame-rate) 45)

	(sdl:with-events ()
	  (:quit-event ()
			(loop for i from 0 below +SPRITES+
			   do (free-sprite (mem-aref sprites '(:pointer sprite) i)))
			(loop for texture in textures
			     do (free-texture texture))
			   t)
	  (:idle ()
		 (clear-scene)
		 
 		 (if start-p 
		     (loop for i from 0 below +SPRITES+
			for sprite = (get-sprite-image-data (mem-aref sprites '(:pointer sprite) i))
			do  
			  
			  (move-image  sprite (- (random 10.0) 5.0) (- (random 10.0) 5.0) 0.0)
			  ))
		 (render-sprites sprites +SPRITES+)


		 (gl:flush)
		 (sdl:update-display))
	  (:key-down-event (:key key)
			   (when (sdl:key= key :SDL-KEY-SPACE)
			     (setf start-p (not start-p))
			     (if start-p
				 (loop for i from 0 below +SPRITES+
				    for sprite = (get-sprite-data (mem-aref sprites '(:pointer sprite) i))
				    do (set-frame sprite 0)
				      (stop sprite)
				      )
				 (loop for i from 0 below +SPRITES+
				    for sprite = (get-sprite-data (mem-aref sprites '(:pointer sprite) i))
				    do (start sprite)
				      )))))


	
	)))

    
    
  
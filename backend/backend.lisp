(defpackage :backend
  (:use :cl :cffi)
  (:export render-spec
	   texture
	   sprite-cons
	   image
	   animation
	   sprite-type
	   sprite-union
	   sprite
	   anim-stopped
	   anim-loop

	   make-render-spec   
	   clone-render-spec
	   flip-axises
	   set-center
	   recenter
	   cachedp
	   cache	   
	   free-cache
	   render-render-spec
	   free-render-spec
	   render-spec-get-texture-rect
	   render-spec-get-quad-points
	   render-spec-get-center-ratio
	   render-spec-get-w
	   render-spec-get-h
	  
	   loadedp
	   make-texture
	   free-texture
	   free-texture-clone
	   texture-clone-op
	   clone-texture
	   make-texture-array
	   texture-get-name
	   
	   make-image
	   free-image
	   cache-image
	   render-image
	   set-scale
	   set-image-center
	   push-subimage
	   rem-subimage
	   pop-subimage
	   rotate-image
	   scale-image
	   move-image
	   relocate-image
	   rect2d-outside-test
	   change-image-texture
	   image-get-loc
	   image-get-scale
	   image-get-rot
	   image-get-w
	   image-get-h

	   make-animation
	   make-disabled-animation
	   animation-disabledp
	   make-animation-from-list
	   free-animation
	   get-image-data
	   change-frame-rate
	   change-flags
	   stoppedp
	   last-framep
	   toggle
	   start
	   stop
	   reset-ticks
	   change-frames
	   change-frames-disable
	   disable-animation
	   next-frame
	   prev-frame
	   set-frame
	   first-frame
	   last-frame
	   render-animation
	   animation-get-ticks
	   animation-change-ticks
	   animation-get-frame-rate
	   animation-change-frame-rate
	   animation-get-frame-count
	   animation-get-current-frame
	   animation-change-flags
	   animation-get-flags
	   
	   make-sprite
	   free-sprite
	   render-sprite
	   render-sprites
	   with-sprite-array
	   get-sprite-data
	   get-sprite-image-data))
	   

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
  (flags :int)
  (flip-callback :pointer))
  
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

(defmacro def-get-fns (struct &rest vars)
  `(progn ,@(loop for foreign-slot in vars
	       collect `(defun ,(intern (concatenate 'string (symbol-name struct) "-GET-" (symbol-name foreign-slot))) (struct)
			  (foreign-slot-value struct ',struct ',foreign-slot)))))

(defmacro def-change-fns (struct &rest vars)
  `(progn ,@(loop for foreign-slot in vars
	       collect `(defun ,(intern (concatenate 'string (symbol-name struct) "-CHANGE-" (symbol-name foreign-slot))) (struct value)
			  (setf (foreign-slot-value struct ',struct ',foreign-slot) value)))))

;; ######################################## Render Spec Functions

(defcfun make-render-spec (:pointer render-spec)
  (w :int)
  (h :int))

(defcfun clone-render-spec (:pointer render-spec)
  (spec (:pointer render-spec)))

(defcenum flip-axises
  :h
  :v
  :hv)

(defcfun set-center (:pointer render-spec)
  (spec (:pointer render-spec))
  (w_ratio :float)
  (h_ratio :float))

(defcfun recenter (:pointer render-spec)
  (spec (:pointer render-spec)))


(defcfun cachedp :boolean
  (spec (:pointer render-spec)))

(defcfun cache (:pointer render-spec)
  (spec (:pointer render-spec)))

(defcfun free-cache (:pointer render-spec)
  (spec (:pointer render-spec)))


(defcfun render-render-spec :void
  (spec (:pointer render-spec)))
  
(defcfun free-render-spec :void
  (spec (:pointer render-spec)))

(def-get-fns render-spec
    texture-rect quad-points center-ratio w h)



; ####################################### texture functions ##################

(defcfun loadedp :boolean
  (tex (:pointer texture)))

(defcfun make-texture (:pointer texture)
  (name :unsigned-int)
  (spec (:pointer render-spec)))
  
(defcfun free-texture :void
  (tex (:pointer texture)))

(defcfun free-texture-clone :void
  (tex (:pointer texture)))


(defcenum texture-clone-op
  :noop
  :flip-h
  :flip-v
  :flip-hv)

(defcfun clone-texture (:pointer texture)
  (tex (:pointer texture))
  (op texture-clone-op))

(def-get-fns texture name)

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


(defun set-scale (image scale)
  (setf (foreign-slot-value image 'image 'scale) scale))

(defcfun set-image-center (:pointer image)
  (image (:pointer image))
  (w-ratio :float)
  (h-ratio :float))


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

(defbitfield rect2d-clip-result
  :left 
  :right
  :top
  :bottom)
  
(defcfun rect2d-outside-test rect2d-clip-result
  (image (:pointer image))
  (x1 :float)
  (y1 :float)
  (x2 :float)
  (y2 :float))


(defcfun change-image-texture (:pointer image)
  (image (:pointer image))
  (texture (:pointer texture)))


      
    
(def-get-fns image
    loc scale rot w h)



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

(defcfun  make-disabled-animation (:pointer animation)
  (texs (:pointer texture))
  (frame-rate :float)
  (flags :long)
  (x :float)
  (y :float)
  (z :float)
  (scale :float)
  (rot :float))

(defcfun animation-disabledp (:boolean)
  (anim (:pointer animation)))


(defun get-image-data (anim)
  (foreign-slot-value anim 'animation 'image))


(defun get-frame-rate (anim)
  (foreign-slot-value anim 'animation 'frame-rate))



(defmacro change-flip-callback (anim fn)
  `(setf (foreign-slot-value ,anim 'animation 'flip-callback) (callback ,fn)))

(defun make-animation-from-list (texs frame-rate flags x y z  scale rot)
  (let ((array (foreign-alloc '(:pointer texture) :count (length texs))))
    (loop for i in texs
	 for j from 0
	 do (setf (mem-aref array '(:pointer texture) j) i))
  (make-animation array (length texs) frame-rate flags x y z scale rot)))

(defcfun free-animation :void
  (anim (:pointer animation)))


(defcfun stoppedp :boolean
    (anim (:pointer animation)))

(defcfun last-framep :boolean
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
  (texs (:pointer :pointer))
  (count :int))

(defcfun change-frames-disable (:pointer animation)
  (anim (:pointer animation))
  (tex (:pointer texture)))

(defcfun disable-animation (:pointer animation)
  (anim (:pointer animation)))

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


(def-get-fns animation 
    ticks frame-rate frame-count current-frame flags)

(def-change-fns animation 
    ticks frame-rate  current-frame flags)

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

(defmacro with-sprite-array ((name count load-fn) &body body )
  (let ((load-fn-gs (gensym "load-fn-"))
	(count-gs (gensym "count-")))
  `(let ((,load-fn-gs ,load-fn)
	 (,count-gs ,count))
    (with-foreign-object (,name '(:pointer sprite) ,count-gs)
    
     (flet ((,(intern (concatenate 'string "RENDER-" (symbol-name name) "-SPRITES")) ()
	      (render-sprites ,name ,count-gs)))
       (loop for i from 0 below ,count-gs
	    do (setf (mem-aref ,name '(:pointer sprite) i) (let ((result (funcall ,load-fn-gs i)))
							     (rie2dgl:sprite result))))
       (progn ,@body))))))
  
					   
	 


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

(defun print-texture-quadpoints (&rest textures)
  (labels ((print-texture-quad (tex)
	     (let* ((spec (foreign-slot-value tex 'texture 'spec))
		    (quads (foreign-slot-value spec 'render-spec 'texture-rect)))
	       (format t "texture-rect ~a spec ~a quads ~a [~a ~a ~a ~a]~%" tex spec quads  (mem-aref quads :float 0)
		      (mem-aref quads :float 1)
		      (mem-aref quads :float 2)
		      (mem-aref quads :float 3)))))
    (loop for i in textures
       if (listp i)
       do (apply #'print-texture-quadpoints i)
       else
       do (print-texture-quad i))))

(defun test-vibrating-cows ()
  (sdl:with-init (sdl-cffi::sdl-init-video )
    (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Testing!")
      (let* ((textures-l (bind-textures-to-files  "/home/nathan/prj/rie2dgl/test-images/cow-1.png"
					       "/home/nathan/prj/rie2dgl/test-images/cow-2.png"))
	     (textures-r (loop for tex in textures-l
			      collect (clone-texture tex :flip-h)))
	     sprites start-p)
	
	;(print (length textures))
	
	(print-texture-quadpoints  textures-l textures-r)
	(setf sprites (foreign-alloc '(:pointer sprite) :count +SPRITES+))
	(loop for i from 0 below (/ +SPRITES+ 2)
	   do (setf (mem-aref sprites '(:pointer sprite) i) (make-sprite (make-animation-from-list textures-l (float 1/20) ANIM-LOOP (random 512.0) (random 512.0)  0.0 1.0 0.0) :animation)))
	(loop for i from (/ +SPRITES+ 2) below +SPRITES+
	   do (setf (mem-aref sprites '(:pointer sprite) i) (make-sprite (make-animation-from-list textures-r (float 1/20) ANIM-LOOP (random 512.0) (random 512.0)  0.0 1.0 0.0) :animation)))
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
			(loop for texture in (append textures-l textures-r)
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

(defun make-texture-array (texs)
  (let ((result (foreign-alloc '(:pointer texture) :count (length texs))))
    (loop for i in texs
       for j from 0
       do (setf (mem-aref result '(:pointer texture) j) i))
    result))


    
(defvar +image-path+ #p"/home/nathan/prj/rie2dgl/test-images/")

(defun choose (&rest options)
  (nth  (random (length options)) options))

 


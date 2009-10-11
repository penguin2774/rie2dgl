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

(defcunion sprite
  (type sprite-type)
  (image (:pointer image))
  (anim (:pointer animation)))

(defconstant anim-stopped (ash 1 0))
(defconstant anim-loop (ash 1 1))

(defcfun make-render-spec (:pointer render-spec)
  (w :int)
  (h :int))



(defcfun set_center (:pointer render-spec)
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


(defcfun free-sprite :void
  (sprite (:pointer sprite)))

(defcfun render :void
  (sprite (:pointer sprite)))


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
  (gl:disable :blend)
  )

   

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

(defconstant +SPRITES+ 1000)

(defun test ()
  (sdl:with-init ()
    
      (let ((w (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Testing!"))
	    texture sprites)
	    
	(let ((tex (first (gl:gen-textures 1))))
	  (sdl:with-surface (alien-surf (sdl-image:load-image "/home/nathan/prj/rie2dgl/test-images/1eyed_alien_stance-2.png"))
	    
	    (gl:enable :texture-2d)
	    (setf texture (make-texture tex (make-render-spec (sdl:width alien-surf)
					     (sdl:height alien-surf))))
	    
	    (standard-bind tex alien-surf)
	    (gl:disable :texture-2d)))
	
	
	(setf sprites (loop repeat +SPRITES+
	  
			 collect (make-image texture (random 512.0) (random 512.0) 0.0 1.0 0.0)))
	
	
	(resize-window 512 512)
	(setup-RC)
	
	(setf (sdl:frame-rate) 45)

	(sdl:with-events ()
	  (:quit-event ()
			(loop for sprite in sprites
			      do (free-image sprite))
			   (free-texture texture)
			   t)
	  (:idle ()
		 (clear-scene)

		 (loop for i in sprites
		      do (render-image i))

		 (gl:flush)
		 (sdl:update-display)))

	
	)))

    
    
  
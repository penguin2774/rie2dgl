(defpackage :backend
  (:use :cl :cffi))


(in-package :backend)

(define-foreign-library backend
    (:unix "./backend.so")
    (t (:default "backend")))


(use-foreign-library backend)


(defcstruct texture 
  (w :int)
  (h :int)
  (name :unsigned-int))

(defcstruct sprite
  (x :float)
  (y :float)
  (tex :pointer))

(defcfun "load_texture" :void
  (filename :string)
  (tex :pointer))

(defcfun "setup_RC" :void)

(defcfun "render_sprite" :void
  (sprite :pointer))

(defcfun "render_sprites" :void
  (sprites :pointer)
  (count :int))

(defcfun "resize_window" :void
  (w :int)
  (h :int))

(defparameter *SPRITES* 10000)

(defun test ()
  (sdl:with-init ()
    (with-foreign-objects ((tex 'texture)
			   (sprites 'sprite *SPRITES*)
			   (event 'sdl-cffi::sdl-event))
      (loop for i from 0 below *SPRITES*
	   for sprite = (mem-aref sprites 'sprite i)
	   do (setf (foreign-slot-value sprite 'sprite 'tex) tex
		    (foreign-slot-value sprite 'sprite 'x) (random 512.0)
		    (foreign-slot-value sprite 'sprite 'y) (random 512.0)))
	   
      
      (let ((w (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Testing!")))
	
	(declare  ;(optimize (speed 3) (safety 0) (debug 0))
	 (fixnum *SPRITES*))
	(resize-window 512 512)
	(setup-RC)
	(load-texture "/home/nathan/prj/rie2dgl/test-images/1eyed_alien_stare.png" tex)
	(setf (sdl:frame-rate) 45)

	(loop until (equal (foreign-enum-keyword 'sdl-cffi::sdl-event-type (foreign-slot-value event 'sdl-cffi::sdl-event 'type)) :sdl-quit-event)
	   do
	     (render-sprites sprites *SPRITES*)
	     (gl:flush)
	     (sdl:update-display)
	     (sdl-cffi::sdl-delay 22)
	     (sdl-cffi::sdl-poll-event event)
	     )

	     
	))))

    
    
  
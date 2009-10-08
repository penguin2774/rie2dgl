(defpackage :render-test
  (:use :cl)
  (:export :run))

(in-package :render-test)



(defstruct texture 
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  (name 0 :type fixnum))

(defstruct sprite
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (texture nil :type texture))


(defun load-texture (tex)
  (let ((name (first (gl:gen-textures 1)))
	(surface (sdl-image:load-image "/home/nathan/prj/rie2dgl/test-images/1eyed_alien_stare.png")))
    (declare		    (optimize (speed 3) (safety 0) (debug 0))
     (texture tex)
     (fixnum name))
    (gl:bind-texture :texture-2d name)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  
    (gl:tex-image-2d :texture-2d 0 :rgba (sdl:width surface) (sdl:height surface)
		     0 :rgba :unsigned-byte (sdl-base::with-pixel (pixels (sdl::fp surface))
					      (sdl-base::pixel-data pixels)))
    (setf (texture-w tex) (sdl:width surface))
    (setf (texture-h tex) (sdl:height surface))
    (setf (texture-name tex) name)
    tex))
  
(defun setup-rc ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (gl:clear-color 0.0 0.0 0.0 0.0))

(defun render-sprite (s)
  (let ((tex (sprite-texture s)))
    (declare		    (optimize (speed 3) (safety 0) (debug 0))
     (sprite s)
     (texture tex))
    (gl:enable :texture-2d)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
  
    (gl:color 1.0 1.0 1.0)
    (gl:bind-texture :texture-2d (texture-name tex))
    (gl:with-pushed-matrix 
      (gl:translate (sprite-x s) (sprite-y s) 0.0)
      (gl:with-primitive :quads
	(gl:tex-coord 0.0 0.0)
	(gl:vertex 0.0 0.0 0.0)

	(gl:tex-coord 1.0 0.0)
	(gl:vertex (texture-w tex) 0.0 0.0)

	(gl:tex-coord 1.0 1.0)
	(gl:vertex (texture-w tex) (texture-h tex) 0.0)

	(gl:tex-coord 0.0 1.0)
	(gl:vertex 0.0 (texture-h tex) 0.0)))
    (gl:disable :texture-2d :blend)))


(defun resize-window (w h)
  (declare		    (optimize (speed 3) (safety 0) (debug 0))
   (fixnum w h))
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0.0 w 0.0 h)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defparameter *SPRITES* 1000)

(defun run ()
  (sdl:with-init ()
    (let* ((w (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Testing!"))
	  (tex (load-texture (make-texture)))
	  (sprites (make-array *SPRITES* :element-type 'sprite
			      :initial-contents (loop repeat *SPRITES*
						     collect (make-sprite :x (random 512.0) :y  (random 512.0) :texture tex)))))
      (declare		    (optimize (speed 3) (safety 0) (debug 0))
       (simple-vector sprites)
       (texture tex)
       (fixnum *SPRITES*))
      (resize-window 512 512)

      (setup-rc)
      (setf (sdl:frame-rate) 45)
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	 (loop for i from 0 below *SPRITES*
	      do (render-sprite (svref sprites i)))
	 (gl:flush)
	 (sdl:update-display))))))


   

  
  

     
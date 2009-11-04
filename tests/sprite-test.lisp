;;;
;;; Rendering a square with an image on it.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;
 
 
(defpackage :sprite-test
  (:use :cl :rie2dgl)
  (:export :run))
 
(require 'lispbuilder-sdl)
(require 'lispbuilder-sdl-image)
(require 'cl-opengl)
(require 'cl-glu)
 
 
 
(in-package :sprite-test)
 
(defclass window ()
  ((rot-x
    :initform 0.0
    :accessor rot-x)
   (rot-y
    :initform 0.0
    :accessor rot-y)
   (drag-point
    :initform nil
    :accessor drag-point)))
 
 
(def-texture dwarf-frigate texture "./test-images/dwarf-frigit-no-turret.png")
(def-texture dwarf-turret texture "./test-images/dwarf-frigit-turret.png")
 
(def-texture-list alien-look texture-list "./test-images/1eyed_alien_stance-~a.png" :start 1 :end 3)
(def-texture alien-stare texture "./test-images/1eyed_alien_stance-2.png")
(def-texture-dict alien
  (:stare texture
     "./test-images/1eyed_alien_stare.png")
  (:look texture-list
   "./test-images/1eyed_alien_stance-~a.png" :start 1 :end 3)
  (:walk
   (:right texture-list
    "./test-images/1eyed_alien_walking-~a.png" :start 1 :end 2)
   (:left texture-list
    "./test-images/1eyed_alien_walking-left-~a.png" :start 1 :end 2)))
 
 
 
 
(defgeneric setup-RC (w))
 
(defmethod setup-RC ((w window))
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  (gl:disable :depth-test)
  (gl:disable :lighting)
  (gl:disable :dither)
  (gl:disable :blend)
  )
 
   
 
 
 
 
(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  (gl:clear :color-buffer)
  (gl:color 1.0 1.0 1.0))
  
 
 
 
(defgeneric resize-window (window w h))
(defmethod resize-window ((win window) w h)
  (when (zerop h)
    (setf h 1))
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0.0 (float w) 0.0 (float h))
  (gl:matrix-mode :modelview)
  
  (gl:load-identity))
 
 
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
(defun make-slide-up-fn (low high &key (step 1) start-value)
  
  (let ((var (or start-value low))
	(dir :up))
    (lambda ()
      (if (eq dir :up)
	  (when (>= (incf var step) high)
	    (setf var high)
	    (setf dir :stop))
	  nil)
      var)))
 
 

  
 
(defun run ()
  (let ((w (make-instance 'window))
	(bounce-fn (make-slide-up-fn 0.0 3.0 :step 0.005
				     :start-value 0.0))
	

	
	(sprite-count 1000)
	  
	(count (* 7 40))
	(start-p nil))
    (sdl:with-init ()
      (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
      (resize-window w 512 512)
					;      (sdl:set-gl-attribute :sdl-gl-doublebuffer 1)
      (setup-RC w)
					;      (gl:tex-env :texture-env :texture-env-mode :replace)
      (with-textures (alien)
	(let ((alien-sprites (loop for i from 0 repeat sprite-count
				collect (make-instance 'sprite :texture (reft alien) :first :look :x  (random 512.0) :y (random 512.0) :frame-rate 1/8 :flags  backend:anim-loop ))))

	  (backend:with-sprite-array (aliens sprite-count (lambda (i)
						    (nth i alien-sprites)))
	  
	    (setf (sdl:frame-rate) 45)
 
 
	    (sdl:with-events ()
	      (:quit-event () t)
	      (:idle ()
 
		 
		   
		     (render-scene w)
		   
 		     (loop for alien in alien-sprites 
 			with bounce = (funcall bounce-fn)
			
 			if (and (eq (first (current alien)) :look) (>= 0 count))
 			do (change alien :walk :right)
 			if (>= 0 count)
 			do (if (stopped? alien)
 			       (start alien))
 			(move alien 1.0 (- 1.0 (random 2.0)))
 			else
 			do (scale alien bounce)
			
 			finally
 			(decf count))
		     (render-aliens-sprites)
					;		      finally
					;		      (decf count))
 
 
 
 
 
 
		  
		     (gl:flush)
		     (sdl:update-display)
		     )
	      (:video-expose-event () (sdl:update-display))
	      (:key-down-event (:key key)
			       (when (sdl:key= key :SDL-KEY-SPACE)
				 (setf start-p t)))
	      )))))))
 
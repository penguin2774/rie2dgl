;;;
;;; Chris dancing to the six flags song!
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;
 
 
(defpackage :chris-test
  (:use :cl :rie2dgl)
  (:export :run))
 
(require 'lispbuilder-sdl)
(require 'lispbuilder-sdl-image)
(require 'cl-opengl)
(require 'cl-glu)
 
 
 
(in-package :chris-test)
 
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
 
 

(def-texture-dict chris
    (:dance-1
     (:left back-and-forth
	    "./test-images/dance/dance1-~a.png" :start 1 :end 2)
     (:right back-and-forth 
	     (chris :dance-1 :left) :end 2 :texture '(texture-clone :clone-op :flip-h)))
  (:dance-2
   (:left back-and-forth
	  "./test-images/dance/dance2-~a.png" :start 1 :end 3)
   (:right back-and-forth 
	   (chris :dance-2 :left) :end 3 :texture '(texture-clone :clone-op :flip-h)))
  (:dance-3
   (:left back-and-forth
	  "./test-images/dance/dance3-~a.png" :start 1 :end 3)
   (:right back-and-forth 
	   (chris :dance-3 :left) :end 3 :texture '(texture-clone :clone-op :flip-h)))
  (:dance-4
   (:left back-and-forth
	  "./test-images/dance/dance4-~a.png" :start 1 :end 3)
   (:right back-and-forth 
	   (chris :dance-4 :left) :end 3 :texture '(texture-clone :clone-op :flip-h)))
  (:dance-5 back-and-forth
	    "./test-images/dance/dance5-~a.png" :start 1 :end 5))
   
 
 
 
 
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

(defun choose (&rest options)
  (nth  (random (length options)) options))

 
(defun make-chris-fn ()
  (let ((sprite (make-instance 'sprite :texture (reft chris) :first (choose
								     '(:dance-1 :left)
								     '(:dance-1 :right)
								     '(:dance-2 :left)
								     '(:dance-2 :right)
								     '(:dance-3 :left)
								     '(:dance-3 :right)
								     '(:dance-4 :left)
								     '(:dance-4 :right)
								     :dance-5)
			        :x  (random 512.0) :y (random 512.0) :frame-rate 1/8 :flags  backend:anim-loop ))
	(ski 0.0))
    (lambda ()
      (let ((ski? (eq (first (current sprite)) :dance-1) )
	    
	    (speed 14.0))
      (when (>= (+ (ticks sprite) (frame-rate sprite)) 1.0) ; frame changes next tick
	(cond 
	  ((= (random 25) 1)
	   (apply #'change sprite (choose
				   '(:dance-1 :left)
				   '(:dance-1 :right)
				   '(:dance-2 :left)
				   '(:dance-2 :right)
				   '(:dance-3 :left)
				   '(:dance-3 :right)
				   '(:dance-4 :left)
				   '(:dance-4 :right)
				   '(:dance-5))))
	  ((and ski?  (= (current-frame sprite) 1))
	   (setf ski 0.95))))
      (when ski?
	(let ((clip (outside-bounds? sprite 0.0 0.0 512.0 512.0)))
	  (case (first clip)
	    (:left
	     (change sprite :dance-1 :right))
	    (:right
	     (change sprite :dance-1 :left))))
	     
	    
	(move sprite (* ski (if (eq (second (current sprite)) :left)
				(- speed)
				speed)) 0.0)
	(setf ski (* ski ski))))
      (render sprite))))
  
 

  
 
(defun run ()
  (let ((w (make-instance 'window))
	
	

	
	(sprite-count 50))
    (sdl:with-init ()
      (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
      (resize-window w 512 512)
					;      (sdl:set-gl-attribute :sdl-gl-doublebuffer 1)
      (setup-RC w)
					;      (gl:tex-env :texture-env :texture-env-mode :replace)
      (with-textures (chris)
	(let ((chrises (loop for i from 0 repeat sprite-count
				collect (make-chris-fn) )))

	  	  
	    (setf (sdl:frame-rate) 45)
 
 
	    (sdl:with-events ()
	      (:quit-event () t)
	      (:idle ()
 
		 
		   
		     (render-scene w)
		   
 		     (loop for i in chrises
			  do (funcall i))
 
 
 
		  
		     (gl:flush)
		     (sdl:update-display)
		     )
	      (:video-expose-event () (sdl:update-display))
	      (:key-down-event (:key key)
			       (when (sdl:key= key :SDL-KEY-SPACE)
				 (print key)))))))))
 
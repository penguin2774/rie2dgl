(defpackage rielib
  (:use :cl)
  (:export 
					; texture-db.lisp
   :name
   :file
   :width
   :height
   :bind-fn
	   
   :sub-textures
   :len
	   
   :loaded?
   :standard-bind
   :bind
   :free
   :reference-texture
   :reft
   :def-texture
   :def-texture-list
   :def-texture-dict
   :with-textures
					; image.lisp
   :image
   :texture
   :scale
   :rotation
   :rotate
	
   :texture-rect
   :location
   :set-center
   :sub-images
   :load-texture
   :render
   :add-subimage
   :x1 :y1 :x2 :y2 :x :y))

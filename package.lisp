(in-package :cl-user)

(defpackage rie2dgl
  (:use :cl)
  (:export 
   :fp
					; texture-db.lisp
   :name
   :file
   :width
   :height
   :bind-fn
   :render-spec
   :quad-points
   :texture-points
   :center-ratio
   :list-cache
   :sub-textures
   :len
	   
   :loaded?
   :standard-bind
   :bind
   :free
   :reference-texture
   :texture
   :texture-clone
   :texture-proxy
   :texture-list
   :back-and-forth
   :texture-dict
   :reft
   :def-texture
   :def-texture-list
   :def-texture-dict
   :with-textures
   :sub-textures
					; image.lisp
   :image
   :cache
   :texture
   :scale
   :rotation
   :rotate
   :move
   :relocate
   :outside-bounds?
   :texture-rect
   :location
   :set-center
   :sub-images
   :push-subimage
   :pop-subimage
   :rem-subimage
   :load-texture
   :render
   :add-subimage
   :x1 :y1 :x2 :y2 :x :y
					; animation.lisp
   :animation
   :frame-rate
   :ticks
   :frames
   :frames-left
   :stopped?
   :toggle
   :start
   :stop
   :ticks
   :frame-rate
   :change-frames
   :current-frame
   :next-frame
   :prev-frame
   :set-frame
   :first-frame
   :last-frame
   :reset-ticks
					; sprite
   :sprite
   :change
   :current
   :texture-dict
					; math3d
   :m3d-pi
   :deg-to-rad
   :rad-to-deg
   :hr-to-deg
   :hr-to-rad
   :deg-to-hr
   :rad-to-hr
   :load-identity44
   :rotation-matrix44
   :transform-vector3
   :draw-torus
   :with-xyzs
   :with-matrix-xyzs
   :with-xyz-slots
   :cross-product
   :set-matrix-column44
   :set-matrix-column33
   :get-matrix-column44
   :get-matrix-column33
   :scale-vector2
   :scale-vector3
   :vector-length-squared
   :vector-length
   :normalize-vector
   :load-vector
   :mulf
   :divf
   :invert-matrix44
   :find-normal
   :normalized-vertices
   :normalize
   
   ))

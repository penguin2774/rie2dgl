(in-package :rie2dgl)


(defclass texture ()
  ((name
    :reader name
    :initform nil
    :documentation "Holds the GL Texture Name produced with gl:gen-textures")
   (file
    :reader file
    :initarg :file
    :documentation "Holds the file that will be loaded when (bind) is called")
   (render-spec
    :reader render-spec)
   (bind-fn
    :accessor bind-fn
    :initarg :bind-fn
    :initform #'standard-bind
    :documentation "The binding function called to bind the texture to the surface. See #'standard-bind documentation")
    ))

(defclass texture-list ()
  ((sub-textures
    :reader sub-textures
    :initarg :sub-textures
    :documentation "A list of all sub textures.")
   (length
    :reader len
    :initarg :length
    :documentation "The length of the list (shouldn't change...)")))



(defclass texture-dict ()
  ((sub-textures
    :reader sub-textures
    :initarg :sub-textures
    :documentation "Hash table holding the sub textures")))

(defclass render-spec ()
  ((texture-rect
    :reader texture-rect
    :initform (vector 0.0 0.0 1.0 1.0))
   (quad-points
    :reader quad-points
    :initform (vector 0.0 0.0 0.0 0.0))
   (center-ratio
    :accessor center-ratio
    :initform (vector 0.5 0.5))
   (list-cache
    :reader list-cache)
  (width
    :reader width
    :initform nil
    :initarg :width
    :documentation "Width of the texture.")
   (height
    :reader height
    :initform nil
    :initarg :height
    :documentation "Height of the texture.")))


  


(defvar *texture-database* (make-hash-table :test #'eq))
(defvar *loaded-textures* (list))

(defmethod initialize-instance  ((self render-spec) &rest initargs &key width height &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method)
  (with-slots ((w width) (h height)) self
    (setf w width)
    (setf h height))
  (recenter self))


(defmethod width ((self texture))
  (declare (inline width))
  (width (render-spec self)))

(defmethod height ((self texture))
  (declare (inline height))
  (height (render-spec self)))

(defgeneric set-center (image w-ratio h-ratio))
(defmethod set-center ((self render-spec) w-ratio h-ratio)
  (with-slots (center-ratio) self
    (setf (elt center-ratio 0) w-ratio)
    (setf (elt center-ratio 1) h-ratio))
  (recenter self))

(defgeneric recenter (object))
(defmethod recenter ((self render-spec))
  (with-slots (width height center-ratio) self
    (let ((w-ratio (elt center-ratio 0))
	  (h-ratio (elt center-ratio 1)))
    (with-xyz-slots ((quad-points 22)) self
      (setf quad-points.x1 (- (* width w-ratio) width))
      (setf quad-points.x2 (* width w-ratio))
      (setf quad-points.y1 (- (* height h-ratio) height))
      (setf quad-points.y2 (* height h-ratio))))))

  
(defmethod render ((self render-spec))
  (with-slots (list-cache texture-rect quad-points) self
    (declare (optimize (safety 1) (speed 3))
	     (simple-vector texture-rect)
	     (simple-vector quad-points))
    (if (cached? self)
	(gl:call-list list-cache)
	(with-xyzs ((texture-rect 22)
		    (quad-points 22))
	  (declare (single-float texture-rect.x1 texture-rect.y1
				 texture-rect.x2 texture-rect.y2
				 quad-points.x1 quad-points.y1
				 quad-points.x2 quad-points.y2))
				 
	  (gl:with-primitive :quads
					;  SDL orientates top to bottom, where as GL orientates bottom to top. We flip the texture coordinates to fix that.
	     (gl:tex-coord texture-rect.x1 texture-rect.y2)
	    (gl:vertex quad-points.x1 quad-points.y1)

	    (gl:tex-coord texture-rect.x2 texture-rect.y2)
	    (gl:vertex quad-points.x2 quad-points.y1)

	
	    (gl:tex-coord texture-rect.x2 texture-rect.y1)
	    (gl:vertex quad-points.x2 quad-points.y2)

	
	    (gl:tex-coord texture-rect.x1 texture-rect.y1)
	    (gl:vertex quad-points.x1 quad-points.y2))))))

(defgeneric cached? (object))
(defmethod cached? ((self render-spec))
  (declare (inline cached?))
  (if (and (slot-boundp self 'list-cache) (list-cache self) (gl:is-list (list-cache self)))
      t nil))

(defgeneric cache (object))
(defmethod cache ((self render-spec))
  (with-slots (texture-rect quad-points) self
    
    (let ((name (gl:gen-lists 1)))
      
      (with-xyzs ((texture-rect 22)
		  (quad-points 22))
	(gl:with-new-list (name :compile)
	  (gl:with-primitive :quads
					;  SDL orientates top to bottom, where as GL orientates bottom to top. We flip the texture coordinates to fix that.
	    (gl:tex-coord texture-rect.x1 texture-rect.y2)
	    (gl:vertex quad-points.x1 quad-points.y1)

	    (gl:tex-coord texture-rect.x2 texture-rect.y2)
	    (gl:vertex quad-points.x2 quad-points.y1)

	
	    (gl:tex-coord texture-rect.x2 texture-rect.y1)
	    (gl:vertex quad-points.x2 quad-points.y2)

	
	    (gl:tex-coord texture-rect.x1 texture-rect.y1)
	    (gl:vertex quad-points.x1 quad-points.y2))))
      (setf (slot-value self 'list-cache) name))))

(defmethod cache ((self texture))
  (cache (slot-value self 'render-spec)))

(defmethod cache ((self texture-list))
  (loop for texture in (sub-textures self)
       do (cache texture)))

(defmethod cache ((self texture-dict))
  (loop for texture being the hash-values in (sub-textures self)
       do (cache texture)))

(defmethod free ((self render-spec))
  (when (cached? self)
      (gl:delete-lists (slot-value self 'list-cache) 1)
      (setf (slot-value self 'list-cache) nil)))

(defgeneric loaded? (texture))

(defmethod loaded? ((self texture))
   (declare (inline loaded?)
	   (optimize (speed 3) (safety 1)))
	   
  (and (name self)
       (%gl:is-texture (name self))))

(defmethod loaded? ((self texture-list))
  (declare (inline loaded?)
	   (optimize (speed 3) (safety 1)))
  (loop for i in (sub-textures self)
     if (not (loaded? i))
     return nil
     finally
       (return t)))

(defmethod loaded? ((self texture-dict))
  (declare (inline loaded?)
	   (optimize (speed 3) (safety 1)))
  (loop for i being the hash-values in (sub-textures self)
     if (not (loaded? i))
     return nil
     finally
       (return t)))

  

(defun standard-bind-nearest (texture surface)
  
  (gl:bind-texture :texture-2d texture)
  (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-image-2d :texture-2d 0
		   :rgba (sdl:width surface) (sdl:height surface)
		   0 :rgba :unsigned-byte (sdl-base::with-pixel (pixels (sdl::fp surface))
					    (sdl-base::pixel-data pixels))))

(defun standard-bind (texture surface)
  
  (gl:bind-texture :texture-2d texture)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-image-2d :texture-2d 0
		   :rgba (sdl:width surface) (sdl:height surface)
		   0 :rgba :unsigned-byte (sdl-base::with-pixel (pixels (sdl::fp surface))
					    (sdl-base::pixel-data pixels))))


(defgeneric bind (texture))

(defmethod bind ((self texture))
  (with-slots (name bind-fn render-spec) self
    (labels ((convert-image-for-gl (surface)
	       (let ((result (sdl:create-surface (sdl:width surface) (sdl:height surface)
						 :bpp 32 :pixel-alpha t)))
		 (sdl:blit-surface surface result)
		 result)))
      (sdl:with-surfaces  ((loaded-surface (lispbuilder-sdl-image:load-image (file self)))
			   (converted-surface (convert-image-for-gl loaded-surface)))
	(let ((texture (first (gl:gen-textures 1))))
	
	  (funcall bind-fn texture converted-surface)
	  (setf name texture)

	  
	  (setf render-spec (make-instance 'render-spec :width (sdl:width converted-surface) :height (sdl:height converted-surface)))
	  name)))))

(defmethod bind ((self texture-list))
  (loop for i in (sub-textures self)
     collect (bind i)))

(defmethod bind ((self texture-dict))
  (loop for i being the hash-values in (sub-textures self)
     do (bind i)))


       
    
      

(defgeneric free (texture))

(defmethod free ((self texture))
  (with-slots (width height name render-spec) self
    (gl:delete-textures (list name))
    (free render-spec)
    (setf name (setf render-spec nil))))

(defmethod free ((self texture-list))
  (loop for i in (sub-textures self)
       do (free i)))

(defmethod free ((self texture-dict))
  (loop for i being the hash-values in (sub-textures self)
       do (free i)))


		 
(defgeneric reference-texture (texture &rest args))

(defmethod reference-texture ((self texture) &rest args)

  (if args
      (error "Texture ~a is just a texture, but wanted ~a sub-texture." self args)
      self))

(defmethod reference-texture ((self texture-list) &rest args)
  (if args
      (apply #'reference-texture (or (nth (car args) (sub-textures self)) (error "There is no ~:R element to ~a's list" (car args) self)) 
	     (cdr args))
      self))

(defmethod reference-texture ((self texture-dict) &rest args)
  (if args
      (apply #'reference-texture (or (gethash (car args) (sub-textures self)) (error "No such texture ~a in ~a." (car args) self)) 
	     (cdr args))
      self))


	     

(defmethod reference-texture ((self symbol) &rest args)
  (if self
      (apply #'reference-texture (or (gethash self *texture-database*) (error "No such texture ~a in database." (car args))) args)
      (error "reference-texture called with nil symbol")))
  
	  
;; reft causes all args to be evaluated except symbols, which it quotes automaticly.
;; but its prevents variable reference. fix?	  
(defmacro reft (&rest args)
  `(reference-texture ,@(loop for i in args
			     if (symbolp i)
			     collect `(quote ,i)
			     else
			     collect i)))
    


(defmacro def-texture (name path)
 `(setf (gethash ',name *texture-database*) (make-instance 'texture :file ,path)))

(defmacro def-texture-list (name path &key (start 0) end bind-fn)
  (let ((start-g (gensym "start-"))
	(end-g (gensym "end-"))
	(bind-fn-g (gensym "bind-fn-"))
	(path-g (gensym "path-")))
    `(let ((,start-g ,start)
	   (,end-g ,end)
	   (,bind-fn-g ,bind-fn)
	   (,path-g ,path))
       (setf (gethash ',name *texture-database*)
	     
	     (make-instance 'texture-list :sub-textures (loop for i from ,start-g to ,end-g
							   collect (make-instance 'texture :file (format nil ,path-g i)
										  :bind-fn (or ,bind-fn-g #'standard-bind)))
			    :length (- ,end-g ,start-g))))))


	      

(defun make-textures (path &key start end bind-fn)
  (if (or start end)
      (make-instance 'texture-list :sub-textures (loop for i from (or start 0) to end
						      collect (make-instance 'texture :file (format nil path i) :bind-fn (or bind-fn #'standard-bind)))
		     :length (- end (or start 0)))
      (make-instance 'texture :file path :bind-fn (or bind-fn #'standard-bind))))

(defmacro def-texture-dict (name &rest spec)
 
   (labels ((make-dict-instance (spec)
	      (let ((hash-g (gensym "hash-")))
		`(make-instance 'texture-dict :sub-textures 
				(let ((,hash-g (make-hash-table :test 'eq)))
				  ,@(loop for (subname . args) in spec
				       collect (if (listp (first args))
						   `(setf (gethash ',subname ,hash-g)
							  ,(make-dict-instance args))
						   `(setf (gethash ',subname ,hash-g)
							  (make-textures ,@args))))
				  ,hash-g)))))
    `(setf
      (gethash ',name *texture-database*)
      ,(make-dict-instance spec)
       )))
			 

(defun bind-textures (&rest textures)
  (loop for i in textures 
     if (listp i) 
     do (apply #'bind-textures i)
     else 
     do (bind (gethash i *texture-database*) )
     (push i *loaded-textures*)))

(defun free-textures (&rest textures)
  (loop for i in textures
       if (listp i)
       do (apply #'free-textures i)
     else
       do (free (gethash i *texture-database*))
       (setf *loaded-textures* (delete i *loaded-textures*))))

(defmacro with-textures ((&rest textures) &body body)
  `(unwind-protect 
       (progn (bind-textures ,@(loop for i in textures
				    collect `(quote ,i)))
	 ,@body)
    (free-textures ,@(loop for i in textures
			  collect `(quote ,i)))))



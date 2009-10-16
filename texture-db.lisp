(in-package :rie2dgl)


(defclass texture ()
  ((fp
    :reader fp
    :documentation "Holds the foreign pointer to the texture structure")
   (file
    :reader file
    :initarg :file
    :documentation "Holds the file that will be loaded when (bind) is called")
   (render-spec
    :reader render-spec
    :documentation "Holds the renderspec foreign pointer.")
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
    :documentation "A foreign array of textures")
   (length
    :reader len
    :initarg :length
    :documentation "Length of the foreign array.")))



(defclass texture-dict ()
  ((sub-textures
    :reader sub-textures
    :initarg :sub-textures
    :documentation "Hash table holding the sub textures")))

(defclass render-spec ()
  ((fp
    :reader fp
    :documentation "Foreign pointer to the render spec structure.")))


  


(defvar *texture-database* (make-hash-table :test #'eq))
(defvar *loaded-textures* (list))



(defgeneric set-center (object w-ratio h-ratio))
(defmethod set-center ((self render-spec) w-ratio h-ratio)
  (backend:set-center (fp self) w-ratio h-ratio))

(defgeneric recenter (object))
(defmethod recenter ((self render-spec))
  (backend:recenter (fp self)))

  

(defgeneric cached? (object))
(defmethod cached? ((self render-spec))
  (declare (inline cached?))
  (backend:cachedp (fp self)))

(defgeneric cache (object))
(defmethod cache ((self render-spec))
  (backend:cache (fp self)))

(defmethod cache ((self texture))
  (declare (inline cache))
  (cache (render-spec self)))

(defmethod cache ((self texture-list))
  (loop for texture in (sub-textures self)
       do (cache texture)))

(defmethod cache ((self texture-dict))
  (loop for texture being the hash-values in (sub-textures self)
       do (cache texture)))

(defmethod free ((self render-spec))
  "Frees the render-spec, note: is automaticly called by (free texture)."
  (backend:free-render-spec (fp self))
  (setf (slot-value self 'fp) nil))

(defgeneric loaded? (texture))

(defmethod loaded? ((self texture))
   (declare (inline loaded?))
   (backend:loadedp (fp self)))

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
  (sdl:with-surface (surf (sdl-image:load-image (file self)))
    (gl:enable :texture-2d)
    (let* ((tex (gl:gen-textures 1))
	  (result (backend:make-texture tex (backend:make-render-spec (sdl:width
								       surf)
								      (sdl:height
								       surf)))))
      (funcall (bind-fn self) tex surf)
      (gl:disable :texture-2d)
      result)))
      
    
    

(defmethod bind ((self texture-list))
  (loop for i in (sub-textures self)
     collect (bind i)))

(defmethod bind ((self texture-dict))
  (loop for i being the hash-values in (sub-textures self)
     do (bind i)))


       
    
      

(defgeneric free (texture))

(defmethod free ((self texture))
  (backend:free-texture (fp self))
  (setf (slot-value self 'fp) nil))

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
    

;; this is going to be tricky
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



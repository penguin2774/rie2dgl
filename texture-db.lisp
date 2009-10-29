(in-package :rie2dgl)

(defclass texture ()
  ((fp
    :reader fp
    :initform nil
    :documentation "Holds the foreign pointer to the texture structure")
   (data
    :reader data
    :initarg :data
    :documentation "Holds the file that will be loaded when (bind) is called, or hold a texture that this texture acts as a proxy for.")
   (render-spec-fp
    :reader render-spec
    :documentation "Holds the renderspec foreign pointer.")
   (bind-fn
    :accessor bind-fn
    :initarg :bind-fn
    :initform #'standard-bind
    :documentation "The binding function called to bind the texture to the surface. See #'standard-bind documentation")
    ))

(defclass texture-clone (texture)
  ((clone-op
    :reader clone-op
    :initarg :clone-op)))

(defclass texture-proxy (texture)
  ())


(defclass texture-list ()
  ((sub-textures
    :reader sub-textures
    :initarg :sub-textures
    :documentation "A foreign array of textures filenames")
   (fp
    :reader fp
    :documentation "An array of sub-textures for the animation structure")
   (length
    :reader len
    :initarg :length
    :documentation "Length of the foreign array.")))

(defclass back-and-forth (texture-list)
  ())

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
  (when (fp self)
    (backend:free-render-spec (fp self))
    (setf (slot-value self 'fp) nil)))
    
  

(defgeneric loaded? (texture))

(defmethod loaded? ((self texture))
   (declare (inline loaded?))
   (and (fp self) (backend:loadedp (fp self))))

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

(defmethod initialize-instance ((self back-and-forth) &rest initargs &key sub-textures &allow-other-keys)

  (setf (getf initargs :sub-textures) (append sub-textures (reverse (rest (butlast sub-textures)))))
  (setf (getf initargs :length) (length (getf initargs :sub-textures)))
  (call-next-method))
  


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
  (assert (probe-file (data self)))
  (sdl:with-surface (surf (sdl-image:load-image (data self)))
    (gl:enable :texture-2d)
    (let* ((tex (first (gl:gen-textures 1)))
	   (result (backend:make-texture tex (backend:make-render-spec (sdl:width
									surf)
								       (sdl:height
									surf)))))
      (funcall (bind-fn self) tex surf)
      (gl:disable :texture-2d)
      (setf (slot-value self 'fp) result)
      (setf (slot-value self 'render-spec-fp) (cffi:foreign-slot-value result 'backend::texture 'backend::spec))))
  self)

(defmethod bind ((self texture-proxy))
  (with-slots (data) self
    (let ((tex (apply #'reference-texture data)))
      (unless (loaded? tex)
	(bind tex))
      (setf (slot-value self 'fp) (fp tex))
      (setf (slot-value self 'render-spec-fp) (render-spec tex))))
  self)

(defmethod bind ((self texture-clone))
  (with-slots (data) self
    (let ((tex (apply #'reference-texture data)))
      (unless (loaded? tex)
	(bind tex))
      (let ((result (backend:clone-texture (fp tex) (clone-op self))))
	(setf (slot-value self 'fp) result)
	(setf (slot-value self 'render-spec-fp) (cffi:foreign-slot-value result 'backend::texture 'backend::spec)))))
  self)
    

(defmethod bind ((self texture-list))
  (setf (slot-value self 'fp)
	(backend:make-texture-array (loop for i in (sub-textures self)
				       collect (fp (bind i))))))


		       

(defmethod bind ((self texture-dict))
  (loop for i being the hash-values in (sub-textures self)
     do (bind i)))


       
    
      

(defgeneric free (texture))

(defmethod free ((self texture))
  (when (fp self)
       (backend:free-texture (fp self)))
  (setf (slot-value self 'fp) nil)
  (setf (slot-value self 'render-spec-fp) nil))

(defmethod free ((self texture-clone))
  (when (fp self)
    (backend:free-texture-clone (fp self)))
  (setf (slot-value self 'fp) nil))

(defmethod free ((self texture-proxy))
  nil)

(defmethod free ((self texture-list))
  (loop for i in (sub-textures self)
       do (free i))
  (cffi-sys:foreign-free (fp self))
  (setf (slot-value self 'fp) nil))

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
(defmacro def-texture (name class path &rest keys)
  (typecase path
    ((or pathname string list)
     `(setf (gethash ',name *texture-database*) (make-instance ',class :data ,path ,@keys)))))
					    

(defmacro def-texture-list (name class path &key (start 0) end texture)

  (let ((start-g (gensym "start-"))
	(end-g (gensym "end-"))
	
	(path-g (gensym "path-")))
    `(let ((,start-g ,start)
	   (,end-g ,end)   
	   (,path-g ,path)
	   )
       (setf (gethash ',name *texture-database*)
	     
	     (make-instance ',class :sub-textures (loop for i from ,start-g to ,end-g
							   
							   collect (make-instance ',(if (and (first texture) (symbolp (first texture)))
											(first texture)
											'texture)
										    :data ,(typecase path
												     ((or string pathname)
												      `(format nil ,path-g i))
												     (list
												      `(append ,path (list i))))
												      
										  ,@texture))
			    :length (1+ (- ,end-g ,start-g))))
       )))


	      

(defun make-textures (class data &rest args)
  (if (subtypep class 'texture-list)
      (let* ((start (or (getf args :start) 1))
	     (end (getf args :end))
	     (len (1+ (- end start))))
	(funcall #'make-instance class :sub-textures (if (or (stringp data) (pathnamep data))
						      
						   (loop for i from start to end
							collect (apply #'make-instance 
								       (or (first (getf args :texture)) 'texture)
								       :data (format nil data i)  (rest (getf args :texture))))
						   (loop for i from 0 below len
							collect (apply #'make-instance 
								       (or (first (getf args :texture)) 'texture)
								       (append (list :data (append data (list i))) (rest (getf args :texture))))))
		 :length len))
      (apply #'make-instance class :data data args)))


(defmacro def-texture-dict (name &rest spec)
  (labels ((make-dict-instance (spec)
	      (let ((hash-g (gensym "hash-")))
		`(make-instance 'texture-dict :sub-textures 
				(let ((,hash-g (make-hash-table :test 'eq)))
				  ,@(loop for (subname . args) in spec
				       collect (cond 
						 ((and (listp (first args)) (keywordp (caar args)))
						  `(setf (gethash ',subname ,hash-g)
							 ,(make-dict-instance args)))
						 ((listp (second args))
						  `(setf (gethash ',subname ,hash-g)
							  (make-textures ',(first args) ',(second args) ,@(cddr args))))
						 (t 
						   `(setf (gethash ',subname ,hash-g)
							  (make-textures ',(first args) ,@(rest args))))))
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


(def-reader-methods render-spec texture-rect quad-points center-ratio w h)
(def-reader-methods texture name)
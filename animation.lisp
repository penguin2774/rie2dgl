(in-package rie2dgl)


(defclass animation (image)
  ((frame-rate
    :initarg :frame-rate
    :type ratio
    :accessor frame-rate)
   (ticks

    
    :reader ticks)
   (frames
    :initarg :frames
    :accessor frames)
   (frames-left
    :reader frames-left)
   (flags
    :initform 0
    :reader flags)))

(def-flag-fn animation-flags :only-once)

(defmethod initialize-instance  ((self animation) &rest initargs &key texture (frame-rate 1) flags &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (frames (texture-slot texture) frames-left width height ticks (frame-rate-slot frame-rate) (flag-slot flags)) self
    (if (not (loaded? texture))
	(error "Frames texture ~a not loaded." texture))
    (when (not (slot-boundp self 'texture))
      (setf texture-slot (car (sub-textures texture)))
      (setf frames-left (cdr (sub-textures texture)))
      (setf frames (sub-textures texture)))
    (if (and (> frame-rate 0) (<= frame-rate 1))
	(setf frame-rate-slot frame-rate )
	(error "Ticks must be in the range (0 1]."))
   
    (setf width (width texture-slot))
    (setf height (height texture-slot))
    (setf ticks :stop)
    (setf flag-slot (if flags
		   (apply #'animation-flags flags)
		   0)))
  
  (call-next-method))

(defgeneric stopped? (object))
(defmethod stopped? ((self animation))
  (eq (slot-value self 'ticks) :stop))


(defgeneric toggle (object))
(defmethod toggle ((self animation))
  (with-slots (ticks) self
    (if (stopped? self)
	(start self)
	(stop self))))
	       

(defgeneric start (object))
(defmethod start ((self animation))
  (with-slots (ticks) self
    (when (slot-boundp self 'frames)
      (if (eq ticks :done)
	  (first-frame self))
      (setf ticks 0))))

      
(defgeneric stop (object))
(defmethod stop ((self animation))
  (with-slots (ticks) self
    (setf ticks :stop)))

(defgeneric reset-ticks (object))
(defmethod reset-ticks ((self animation))
  (if (not (slot-boundp self 'ticks))
      (setf (slot-value self 'ticks) :stop)
      (with-slots (ticks) self
	(if (typep ticks 'number)
	    (setf ticks 0)))))
	

(defgeneric change-frames (object texture-list &optional new-frame-rate))
(defmethod change-frames ((self animation) texture-list &optional new-frame-rate)
  (with-slots (frames-left frames texture  frame-rate ticks) self
    (setf frames (sub-textures texture-list))
    (setf frames-left (cdr frames))
    (change-texture self (car frames))
    (reset-ticks self)
    (when new-frame-rate
      (setf frame-rate  new-frame-rate))))
	  


(defgeneric next-frame (object))
(defmethod next-frame ((self animation))
  (with-slots (frames-left frames texture ticks flags) self
    (declare (optimize (speed 3) (safety 1))
	     
	     (fixnum flags))
    (reset-ticks self)
    
    (let ((next-frame (car frames-left)))
      
      (if next-frame
	  (progn
	    (change-texture self next-frame)
	    (setf frames-left (cdr frames-left)))
	  (if (test-flags #'animation-flags flags :only-once)
	      (when (not (eq ticks :done))
		(setf ticks :done))
	      (progn
		(change-texture self (car frames))
		(setf frames-left (cdr frames))))))))
  
(defgeneric prev-frame (object))
(defmethod prev-frame ((self animation))
  (with-slots (frames-left frames texture ticks) self
    (declare (optimize (speed 3) (safety 1)))
    (reset-ticks self)
    
    (if (eq texture (first frames))
	(progn 
	  (change-texture self (car (last frames)))
	  (setf frames-left nil))
	(progn 
	  (loop for i on frames
	     until (eq (cadr i) texture)
	     finally
	       (change-texture self (car i))
	       (setf frames-left (cdr i)))))))

(defgeneric set-frame (object num))

(defmethod set-frame ((self animation) num)
  (with-slots (frames-left frames texture ticks) self
    (reset-ticks self)
    (when (>= num (length frames))
      (error "Frame number ~a is out of range for texture list ~a." num frames))
    (change-texture self (nth num frames))
    (setf frames-left (nthcdr num frames))))

(defgeneric first-frame (object))
(defmethod first-frame ((self animation))
  (reset-ticks self)
  (with-slots (frames-left frames texture ticks) self
    (change-texture self (car frames))
    (setf frames-left (cdr frames))))

(defgeneric last-frame (object))
(defmethod last-frame ((self animation))
  (reset-ticks self)
  (with-slots (frames-left frames texture ticks) self
    (change-texture self (car (last frames)))
    (setf frames-left nil)))


(defmethod render ((self animation))
  
  (when (typep  (slot-value self 'ticks) 'number)
    (with-slots (frame-rate ticks) self
      (declare (optimize (safety 1) (speed 3))
	       ((or ratio integer) ticks frame-rate))
      (incf ticks frame-rate)
      (when (>= ticks 1)
	(next-frame self))))
  (call-next-method))


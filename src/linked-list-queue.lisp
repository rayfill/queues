(in-package :cl-user)
(defpackage :linked-list-queue
  (:use :cl :blocking-queue)
  (:import-from :sb-ext :atomic-update :compare-and-swap)
  (:import-from :blocking-queue :offer :poll)
  (:import-from :kmrcl :aif :it)
  (:export :linked-list-queue)
  (:nicknames :llq))
(in-package :linked-list-queue)

(defclass linked-list-queue ()
  ((allocator :accessor allocator)
   (head :initform nil :initarg :head :accessor head :type list)
   (tail :initform nil :initarg :tail :accessor tail :type list)))

(defgeneric allocate (allocator))
(defgeneric deallocate (allocator cons))

(defclass unlimited-allocator ()
  ())

(defmethod allocate ((allocator unlimited-allocator))
  (cons nil nil))

(defmethod deallocate ((allocator unlimited-allocator) cell)
  (declare (ignore cell)))

(defclass limited-allocator ()
  ((current :accessor current :initform 0 :type integer)
   (limit :accessor limit :initarg :limit :type integer)))

(defun type-check (type &rest values)
  (dolist (value values)
    (unless (typep value type)
      (error 'simple-type-error
	     :format-control "value ~A is not compatible type of ~A"
	     :format-arguments (list value type)
	     :expected-type type
	     :datum value))))

(defmethod initialize-instance :after 
    ((instance limited-allocator) &key capacity)
  (type-check 'integer (current instance) capacity)
  (setf (limit instance) capacity)
  (assert (< (current instance) (limit instance))))
  
(defmethod allocate ((allocator limited-allocator))
  (tagbody
     :retry
     (let ((old (current allocator))
	   (limit (limit allocator)))
       (when (< old limit)
	 (let ((new (1+ old)))
	   (unless (eq old (sb-ext:cas (slot-value allocator 'current) old new))
	     (go :retry))
	   (when (<= new limit)
	       (cons nil nil)))))))

(defmethod deallocate ((allocator limited-allocator) cell)
  (declare (ignore cell))
  (tagbody
   :retry
     (let ((old (current allocator)))
       (if (> old 0)
	   (let ((new (1- old)))
	     (unless (eq old (sb-ext:cas (slot-value allocator 'current) old new))
	       (go :retry)))
	   (error 'simple-error :format-control "over release memory cell.")))))


  

(defmethod initialize-instance :after ((instance linked-list-queue)
				       &key capacity)
  (with-slots (allocator head tail)
      instance
    (let ((dummy-cell (cons nil nil)))
      (setf allocator 
	    (if capacity
		(make-instance 'limited-allocator :capacity capacity)
		(make-instance 'unlimited-allocator))
	    head dummy-cell
	    tail dummy-cell))))

(defmethod print-object ((queue linked-list-queue) stream)
  (with-slots (head tail)
      queue
  (format stream "#<linked-list-queue head: ~A, tail: ~A>" head tail)))

(defun tail-advance (instance)
  (null (cdr (atomic-update (slot-value instance 'tail)
			    (lambda (tail)
			      (aif (cdr tail)
				   it
				   tail))))))

(let ((undef (gensym)))
  (defun head-advance (instance)
    (let ((result undef)
	  cell)
      (atomic-update (slot-value instance 'head)
		     (lambda (head)
		       (setf result undef)
		       (aif (cdr head)
			    (progn
			      (setf result (car it)
				    cell it)
			      it)
			    head)))
      (if (eq result undef)
	  (values nil nil)
	  (progn
	    (when cell
	      (deallocate (allocator instance) cell))
	    (values result t))))))

(defmethod offer ((queue linked-list-queue) value)
  (let ((cell (allocate (allocator queue))))
    (unless cell
      (return-from offer (values value nil)))
    (setf (car cell) value
	  (cdr cell) nil)
    (tagbody
     :retry
       (loop until (tail-advance queue))
       (unless (eq (atomic-update (cdr (slot-value queue 'tail))
				  (lambda (cdr-slot)
				    (if cdr-slot
					cdr-slot
					cell))) cell)
	 (go :retry)))
    (loop until (tail-advance queue)))
  (values value t))

(defmethod poll ((queue linked-list-queue))
  (loop until (tail-advance queue))
  (head-advance queue))

(defmethod peek ((queue linked-list-queue))
  (loop until (tail-advance queue))
  (let ((head-cell (cdr (head queue))))
    (if head-cell
	(values (car head-cell) t)
	(values nil nil))))


    

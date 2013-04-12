(in-package :cl-user)
(defpackage :allocator-impl
  (:use :cl :allocator)
  (:export :unlimited-allocator :limited-allocator :allocation-failed-error))
(in-package :allocator-impl)

(define-condition allocation-failed-error (storage-condition)
  ())

(defclass unlimited-allocator (allocator)
  ())

(defmethod allocate ((allocator unlimited-allocator))
  (cons nil nil))

(defmethod deallocate ((allocator unlimited-allocator) cell)
  (declare (ignore cell)))

(defclass limited-allocator (allocator)
  ((current :accessor current :initform 0 :type integer)
   (limit :accessor limit :initarg :limit :type integer)))

(defmethod print-object ((allocator limited-allocator) stream)
  (with-slots (current limit)
      allocator
    (format stream "#<limited-allocator current: ~A, limit: ~A>" current limit)))

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
	     (go :retry))))
       (if (<= (1+ old) limit)
	   (return-from allocate (cons nil nil))
	   (error 'allocation-failed-error :format-control "allocate limit reached.")))))

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

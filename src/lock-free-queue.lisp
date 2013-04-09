(in-package :cl-user)
(defpackage :lock-free-queue
  (:use :cl :queues :allocator)
  (:import-from :sb-ext :atomic-update :compare-and-swap)
  (:import-from :queues :offer :poll :peek)
  (:import-from :kmrcl :aif :it :awhen)
  (:import-from :allocator-impl :unlimited-allocator :limited-allocator)
  (:export :lock-free-queue)
  (:nicknames :lfq))
(in-package :lock-free-queue)

(defclass lock-free-queue ()
  ((allocator :accessor allocator)
   (head :initform nil :initarg :head :accessor head :type list)
   (tail :initform nil :initarg :tail :accessor tail :type list)))

(defmethod initialize-instance :after ((instance lock-free-queue)
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

(defmethod print-object ((queue lock-free-queue) stream)
  (with-slots (head tail)
      queue
    (format stream "#<lock-free-queue head: ~A, tail: ~A>" head tail)))

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

(defmethod offer ((queue lock-free-queue) value)
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

(defmethod poll ((queue lock-free-queue))
  (loop until (tail-advance queue))
  (head-advance queue))

(defmethod peek ((queue lock-free-queue))
  (loop until (tail-advance queue))
  (let ((head-cell (cdr (head queue))))
    (if head-cell
	(values (car head-cell) t)
	(values nil nil))))

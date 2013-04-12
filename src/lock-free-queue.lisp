(in-package :cl-user)
(defpackage :lock-free-queue
  (:use :cl)
  (:import-from :sb-ext :atomic-update :compare-and-swap)
  (:import-from :queues :offer :poll :peek :put :take
		:abstract-queue :head :tail)
  (:import-from :kmrcl :aif :it :awhen)
  (:import-from :allocator-impl :unlimited-allocator :limited-allocator
		:allocation-failed-error)
  (:import-from :allocator :allocate :deallocate)
  (:export :lock-free-queue)
  (:nicknames :lfq))
(in-package :lock-free-queue)

(defclass lock-free-queue (abstract-queue)
  ((allocator :accessor allocator)))
   
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
  (handler-case
      (let ((cell (allocate (allocator queue))))
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
	(loop until (tail-advance queue))
	(values value t))
    (allocation-failed-error () (values nil nil))))

(defmethod put ((queue lock-free-queue) value)
  (tagbody
   :retry
     (multiple-value-bind (value is-succeed)
	 (offer queue value)
       (declare (ignorable value))
       (unless is-succeed
	 (go :retry)))))

(defmethod poll ((queue lock-free-queue))
  (loop until (tail-advance queue))
  (head-advance queue))

(defmethod take ((queue lock-free-queue))
  (tagbody
   :retry
     (multiple-value-bind (value is-succeed)
	 (poll queue)
    (unless is-succeed
      (go :retry))
    value)))

(defmethod peek ((queue lock-free-queue))
  (loop until (tail-advance queue))
  (let ((head-cell (cdr (head queue))))
    (if head-cell
	(values (car head-cell) t)
	(values nil nil))))

(in-package :queues)
(import 'lock-free-queue:lock-free-queue)
(export 'lock-free-queue)

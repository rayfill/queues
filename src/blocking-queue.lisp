(in-package :cl-user)
(defpackage :blocking-queue
  (:use :cl)
  (:import-from :queues :peek :poll :offer :take :put
		:abstract-queue :head :tail :create-queue)
  (:import-from :allocator :allocator :allocate :deallocate)
  (:import-from :allocator-impl :unlimited-allocator :limited-allocator
		:allocation-failed-error)
  (:import-from :sb-thread :make-mutex :with-mutex
		:make-waitqueue :condition-wait
		:condition-notify :condition-broadcast)
  (:export :blocking-queue))
(in-package :blocking-queue)

(defclass blocking-queue (abstract-queue)
  ((allocator :accessor allocator)
   (tail-mutex :initform (make-mutex :name "read mutex") :accessor tail-mutex)
   (tail-condv :initform (make-waitqueue :name "read condv"))
   (head-mutex :initform (make-mutex :name "write mutex") :accessor head-mutex)
   (head-condv :initform (make-waitqueue :name "write condv"))))

(defmethod initialize-instance :after ((queue blocking-queue) &key capacity)
  (let ((alloc (if capacity
		   (make-instance 'limited-allocator :capacity capacity)
		   (make-instance 'unlimited-allocator)))
	(dummy (cons :dummy nil)))
    (with-slots (allocator head tail)
	queue
      (setf head dummy
	    tail dummy
	    allocator alloc))))

(defmethod print-object ((queue blocking-queue) stream)
  (with-mutex ((head-mutex queue) :wait-p nil) (tail queue))
  (format stream "#<blocking-queue: head:~A, tail:~A>"
	  (multiple-value-bind (value is-success-p)
	      (with-mutex ((tail-mutex queue) :wait-p nil)
		(values (head queue) t))
	    (if is-success-p value :unknown))
	  (multiple-value-bind (value is-success-p)
	      (with-mutex ((head-mutex queue) :wait-p nil)
		(values (tail queue) t))
	    (if is-success-p value :unknown))))

(defun update-tail (queue new-tail)
  (with-slots (tail)
      queue
    (setf (cdr tail) new-tail
	  tail new-tail)))

(defun update-head (queue new-head)
  (with-slots (head)
      queue
    (unless (eq (cdr head) new-head)
      (error 'simple-error :format-control "current head's cdr slot is not match new-head.head: ~%~A~%new-head: ~A" :format-arguments (list head new-head)))
    (setf head new-head)))

(defmethod put ((queue blocking-queue) value)
  
  (with-slots (allocator tail
			 tail-mutex tail-condv
			 head-condv)
      queue
    (with-mutex (tail-mutex)
      (loop
	 for cell = (ignore-errors (allocate allocator))
	 if cell
	 return (progn
		    (setf (car cell) value
			  (cdr cell) nil)
		    (update-tail queue cell))
	 else
	 do (condition-wait tail-condv tail-mutex)
	 end))
    (condition-broadcast head-condv)))
	
(defmethod take ((queue blocking-queue))
  (with-slots (allocator head
			 head-mutex head-condv
			 tail-condv)
      queue
    (values
     (prog1
	 (with-mutex (head-mutex)
	   (loop
	      for cell = (cdr head)
	      if cell
	      return (prog1
			 (car cell)
		       (update-head queue cell)
		       (deallocate allocator cell))
	      else
	      do (condition-wait head-condv head-mutex)
	      end))
       (condition-notify tail-condv)) t)))

(defmethod peek ((queue blocking-queue))
  (with-slots (write-mutex head)
      queue
    (with-mutex (write-mutex)
      (let ((front (cdr head)))
	(if front
	    (values (car front) t)
	    (values nil nil))))))

(defmethod offer ((queue blocking-queue) value)
  (handler-case
      (with-slots (allocator head-condv tail-mutex tail-condv tail)
	  queue
	(let ((cell (allocate allocator)))
	  (with-mutex (tail-mutex)
	    (setf (car cell) value
		  (cdr cell) nil)
	    (update-tail queue cell)
	    (condition-notify head-condv)
	    (return-from offer t))))
    (allocation-failed-error () (values nil nil))))

(defmethod poll ((queue blocking-queue))
  (with-slots (allocator head-mutex head-condv head tail-condv)
    queue
    (with-mutex (head-mutex)
      (let ((cell (cdr head)))
	(when cell
	  (return-from poll (values
			     (prog1
				 (car cell)
			       (update-head queue cell)
			       (deallocate allocator cell)
			       (condition-notify tail-condv)) t)))
	(return-from poll (values nil nil))))))

(in-package :queues)
(import 'blocking-queue:blocking-queue)
(export 'blocking-queue)
  
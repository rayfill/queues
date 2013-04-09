(in-package :cl-user)
(defpackage :blocking-queue
  (:use :cl :queues)
  (:import-from :allocator :allocator :allocate :deallocate)
  (:import-from :allocator-impl :unlimited-allocator :limited-allocator)
  (:import-from :sb-thread :make-mutex :with-mutex
		:make-waitqueue :condition-wait :condition-notify :condition-broadcast
		:with-mutex)
  (:export :blocking-queue))
(in-package :blocking-queue)

(defclass blocking-queue ()
  ((allocator :accessor allocator)
   (read-mutex :initform (make-mutex :name "read mutex") :accessor read-mutex)
   (read-condv :initform (make-waitqueue :name "read condv"))
   (write-mutex :initform (make-mutex :name "write mutex") :accessor write-mutex)
   (write-condv :initform (make-waitqueue :name "write condv"))
   (head :accessor head)
   (tail :accessor tail)))

(defmethod initialize-instance :after ((queue blocking-queue) &key capacity)
  (let ((alloc (if capacity
		   (make-instance 'limited-allocator :capacity capacity)
		   (make-instance 'unlimited-allocator)))
	(dummy (cons nil nil)))
    (with-slots (allocator head tail)
	queue
      (setf head dummy
	    tail dummy
	    allocator alloc))))

(defmethod print-object ((queue blocking-queue) stream)
  (with-mutex ((write-mutex queue) :wait-p nil) (tail queue))
  (format stream "#<blocking-queue: head:~A, tail:~A>"
	  (multiple-value-bind (value is-success-p)
	      (with-mutex ((read-mutex queue) :wait-p nil)
		(values (head queue) t))
	    (if is-success-p value :unknown))
	  (multiple-value-bind (value is-success-p)
	      (with-mutex ((write-mutex queue) :wait-p nil)
		(values (tail queue) t))
	    (if is-success-p value :unknown))))

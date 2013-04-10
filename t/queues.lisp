(in-package :cl-user)
(defpackage :queues-test
  (:use :cl
        :queues
	:lock-free-queue
	:blocking-queue
        :cl-test-more
	:workers))
(in-package :queues-test)

(plan nil)

(declaim (optimize (debug 3) (safety 3)))

(defun seq (base last &optional (step 1))
  (loop for i from base to last by step collect i))

(defun sum (max)
  (/ (* (+ 1 max) max) 2))

(defparameter +number-of-threads+ 8)
(defparameter +list-max+ 10000)
(defparameter +poison-pill+ (gensym))

(defparameter *debug* nil)
(let ((out *standard-output*)
      (lock (sb-thread:make-mutex :name "output")))
  (defun output (fmt &rest args)
    (when *debug*
      (sb-thread:with-mutex (lock)
	(apply #'format out fmt args)
	(funcall #'force-output out)))))

(defun enqueue-work (queue source &optional is-put)
  (let ((failed 0))
    (dolist (elem source)
      (multiple-value-bind (value succeed)
	  (if is-put
	      (queues:put queue elem)
	      (queues:offer queue elem))
	(declare (ignorable value))
	(unless succeed
	  (incf failed))))
					;      (output "enqueue finished at ~A~%" sb-thread:*current-thread*)
    failed))

(defun dequeue-work (queue poison-pill &optional is-take)
  (let ((result 0)
	(failed 0)
	taken)
    (loop
       (multiple-value-bind (value succeed)
	   (if is-take
	       (queues:take queue)
	       (queues:poll queue))
	 (if (or is-take succeed)
	     (progn
	       (output "value: ~A, thread: ~A~%" value sb-thread:*current-thread*)
	       (if (eq value poison-pill)
		   (progn
		     (output "reuslt:~A,  failed:~A~%" result failed)
		     (return-from dequeue-work
		       (list result failed (nreverse taken))))
		   (progn
		     (push value taken)
		     (sleep 0)
		     (incf result value))))
	     (progn
	       (incf failed)))))))

(defun enqueue-test (queue-class is-put)
  (let ((queue (make-instance queue-class))
	(source (seq 1 +list-max+)))
    (let ((enqueue-workers (create-workers +number-of-threads+
					   #'enqueue-work queue source is-put)))
      (unwind-protect
	   (progn
	     (start-workers enqueue-workers)
	     (let ((failed (wait-workers enqueue-workers)))
	       (declare (ignorable failed))
	       (let ((actual-source (sort (cdr (queues::head queue)) #'<)))
		 (let ((expect-source (sort
				       (apply #'append
					      (loop for i below +number-of-threads+
						 collect source)) #'<)))
		   (ok (equal actual-source expect-source)
		       (or (equal actual-source expect-source)
			   (format nil "not equal to queuing data.~%~A~%" 
				   (set-difference actual-source expect-source))))))))
	(workers:kill-workers enqueue-workers)))))

(defun dequeue-test (queue-class is-take)
  (let ((queue (make-instance queue-class))
	(source (seq 1 +list-max+)))
    (let ((dequeue-workers
	   (create-workers +number-of-threads+
			   #'dequeue-work queue +poison-pill+ is-take))
	  (expect-source
	   (sort (apply #'append
			(loop for i below +number-of-threads+
			   collect source)) #'<)))
      (unwind-protect
	   (progn
	     (dolist (elem expect-source)
	       (loop until (queues:offer queue elem)))
	     (ok (sort (cdr (queues::head queue)) #'<) expect-source)
	     (dotimes (i +number-of-threads+)
	       (loop until (queues:offer queue +poison-pill+)))
	     (start-workers dequeue-workers)
	     (destructuring-bind (sum failed taken)
		 (reduce (lambda (acc elem)
			   (let ((sum (first elem))
				 (failed (second elem))
				 (taken (third elem)))
			     (list (+ (first acc) sum)
				   (+ (second acc) failed)
				   (append (third acc) taken))))
			 (wait-workers dequeue-workers) :initial-value '(0 0 nil))
	       (declare (ignore failed) (ignorable sum))
	       (let ((actual-source (sort taken #'<)))
		 (ok (equal actual-source expect-source)
		     (or (equal actual-source expect-source)
			 (let ((actual-hash (make-hash-table :test 'eql)))
			   (loop for val in actual-source
			      do (incf (gethash val actual-hash 0)))
			   (format nil "~A~%"
				   (loop for i from 1 below +list-max+
				      unless (= (gethash i actual-hash) +number-of-threads+)
				      collect (list i (gethash i actual-hash))))))))))
	(workers:kill-workers dequeue-workers)))))


(defun queue-test (queue-class is-put is-take)
  (let ((queue (make-instance queue-class))
	(source (seq 1 +list-max+)))
    (let ((enqueue-workers
	   (create-workers +number-of-threads+ 
			   #'enqueue-work queue source is-put))
	  (dequeue-workers
	   (create-workers +number-of-threads+
			   #'dequeue-work queue +poison-pill+ is-take)))
      (unwind-protect
	   (progn
	     (start-workers enqueue-workers)
					;      (sleep 1)
	     (start-workers dequeue-workers)
	     (let ((enqueue-results (wait-workers enqueue-workers)))
	       (output "enqueue finished~%")
	       (dotimes (count +number-of-threads+)
		 (loop until (queues:offer queue +poison-pill+)))
	       (let ((dequeue-results (wait-workers dequeue-workers)))
		 (let ((enque-failed (reduce #'+ enqueue-results))
		       (deque-values (reduce (lambda (acc elem)
					       (list (+ (first acc) (first elem))
						     (+ (second acc) (second elem))
						     (append (third acc) (third elem))))
					     dequeue-results :initial-value '(0 0 ()))))
		   (declare (ignorable enque-failed))
		   ;;	    (format t "enqueue failed: ~A~%" enque-failed)
		   ;;	    (format t "dequeue failed: ~A~%" (second deque-values))
		   (let ((actual-source (sort (third deque-values) #'<))
			 (expect-source
			  (sort (apply #'append 
				       (loop for i below +number-of-threads+
					  collect source)) #'<))
			 (actual (first deque-values))
			 (expect (* (sum +list-max+) +number-of-threads+)))

		     (ok (equal actual-source expect-source)
			 (or (equal actual-source expect-source)
			     (let ((actual-hash (make-hash-table :test 'eql)))
			       (loop for val in actual-source
				  do (incf (gethash val actual-hash 0)))
			       (format nil "~A~%"
				       (loop for i from 1 below +list-max+
					  unless (= (gethash i actual-hash) +number-of-threads+)
					  collect (list i (gethash i actual-hash)))))))

		     (is actual expect))))))
	(workers:kill-workers enqueue-workers)
	(workers:kill-workers dequeue-workers)))))


(defmethod queues:offer :before ((queue blocking-queue) value)
  (output "enter offer ~A~%" sb-thread:*current-thread*))

(defmethod queues:offer :after ((queue blocking-queue) value)
  (output "leave offer ~A~%" sb-thread:*current-thread*))

(defmethod queues:poll :before ((queue blocking-queue))
  (output "enter poll ~A~%" sb-thread:*CURRENT-THREAD*))

(defmethod queues:poll :after ((queue blocking-queue))
  (output "leave poll ~A~%" sb-thread:*CURRENT-THREAD*))

(terpri)
(defmacro with-message ((message) &body body)
  (let ((msg (gensym)))
  `(let ((,msg ,message))
     (output "~A started~%" ,msg)
     (force-output)
     ,@body
     (output "~A finished~%" ,msg)
     (force-output))))

(with-message ("lock-free enqueue-test")
  (enqueue-test 'lock-free-queue nil))
(with-message ("lock-free dequeue-test")
  (dequeue-test 'lock-free-queue nil))
(with-message ("lock-free queue-test")
  (queue-test 'lock-free-queue nil nil))

(with-message ("nonblocking enqueue-test")
  (enqueue-test 'blocking-queue nil))

(with-message ("blocking enqueue-test")
  (enqueue-test 'blocking-queue t))

(with-message ("nonblocking dequeue-test")
  (dequeue-test 'blocking-queue nil))


(with-message ("blocking dequeue-test")
  (dequeue-test 'blocking-queue t))

(with-message ("nonblocking queue-test")
  (queue-test 'blocking-queue nil nil))

(with-message ("nonblocking enqueue and blocking dequeue queue-test")
  (queue-test 'blocking-queue nil t))

(with-message ("blocking enqueue and nonblocking dequeue queue-test")
  (queue-test 'blocking-queue t nil))

(with-message ("blocking queue-test")
  (queue-test 'blocking-queue t t))


(finalize)

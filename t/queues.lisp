(in-package :cl-user)
(defpackage :queues-test
  (:use :cl
        :queues
	:lock-free-queue
        :cl-test-more
	:workers))
(in-package :queues-test)

(plan nil)

(declaim (optimize (debug 3) (safety 3)))

(defun seq (base last &optional (step 1))
  (loop for i from base to last by step collect i))

(defun sum (max)
  (/ (* (+ 1 max) max) 2))

(defparameter +number-of-threads+ 4)
(defparameter +list-max+ 10000)
(defparameter +poison-pill+ (gensym))

(let ((out *standard-output*))
  (declare (ignorable out))
  (defun enqueue-work (queue source)
    (let ((failed 0))
      (dolist (elem source)
	(multiple-value-bind (value succeed)
	    (queues:offer queue elem)
	  (declare (ignorable value))
	  (unless succeed
	    (incf failed))))
;      (format out "enqueue finished at ~A~%" sb-thread:*current-thread*)
      failed)))

(let ((out *standard-output*))
  (declare (ignorable out))
  (defun dequeue-work (queue poison-pill)
    (let ((result 0)
	  (failed 0)
	  taken)
      (loop
	 (multiple-value-bind (value succeed)
	     (queues:poll queue)
	   (if succeed
	       (if (eq value poison-pill)
		   (return-from dequeue-work
		     (list result failed (nreverse taken)))
		   (progn
		     (push value taken)
		     (incf result value)))
	       (progn
		 (incf failed))))))))

(defun enqueue-test ()
  (let ((queue (make-instance 'lock-free-queue:lock-free-queue))
	(source (seq 1 +list-max+)))
    (let ((enqueue-workers (create-workers +number-of-threads+
					   #'enqueue-work queue source)))
      (start-workers enqueue-workers)
      (let ((failed (wait-workers enqueue-workers)))
	(declare (ignorable failed))
	(let ((actual-source (sort (cdr (lock-free-queue::head queue)) #'<)))
	  (let ((expect-source (sort
				(apply #'append
				       (loop for i below +number-of-threads+
					  collect source)) #'<)))
	    (ok (equal actual-source expect-source)
		(or (equal actual-source expect-source)
		    (format nil "not equal to queuing data.~%~A~%" 
			    (set-difference actual-source expect-source))))))))))

(defun dequeue-test ()
  (let ((queue (make-instance 'lock-free-queue:lock-free-queue))
	(source (seq 1 +list-max+)))
    (let ((dequeue-workers (create-workers +number-of-threads+
					   #'dequeue-work queue +poison-pill+))
	  (expect-source (sort (apply #'append (loop for i below +number-of-threads+ collect source)) #'<)))
      (dolist (elem expect-source)
	(loop until (queues:offer queue elem)))
      (ok (sort (cdr (lock-free-queue::head queue)) #'<) expect-source)
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
			       collect (list i (gethash i actual-hash))))))))))))

(defun queue-test ()
  (let ((queue (make-instance 'lock-free-queue:lock-free-queue))
	(source (seq 1 +list-max+)))
    (let ((enqueue-workers
	   (create-workers +number-of-threads+ 
			   #'enqueue-work queue source))
	  (dequeue-workers
	   (create-workers +number-of-threads+
			   #'dequeue-work queue +poison-pill+)))
      (start-workers enqueue-workers)
					;      (sleep 1)
      (start-workers dequeue-workers)
      (let ((enqueue-results (wait-workers enqueue-workers)))
	(dotimes (count +number-of-threads+)
	  (loop until (queues:offer queue +poison-pill+)))
	(let ((dequeue-results (wait-workers dequeue-workers)))
	  (let ((enque-failed (reduce #'+ enqueue-results))
		(deque-values (reduce (lambda (acc elem)
					(list (+ (first acc) (first elem))
					      (+ (second acc) (second elem))
					      (append (third acc) (third elem))))
				      dequeue-results :initial-value '(0 0 ()))))
;;	    (format t "enqueue failed: ~A~%" enque-failed)
;;	    (format t "dequeue failed: ~A~%" (second deque-values))
	    (let ((actual-source (sort (third deque-values) #'<))
		  (expect-source (sort (apply #'append (loop for i below +number-of-threads+ collect source)) #'<))
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

	      (is actual expect))))))))

(terpri)
(enqueue-test)
(dequeue-test)
(queue-test)

(finalize)

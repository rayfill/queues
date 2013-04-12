(in-package :cl-user)
(defpackage allocator-test
  (:use :cl
	:allocator
	:allocator-impl
	:cl-test-more))
(in-package :allocator-test)

(plan nil)
(is (ignore-errors
      (let ((alloc (make-instance 'unlimited-allocator)))
	(loop for i below most-positive-fixnum do (allocator:allocate alloc)))) nil)

(defun limited-test-stub (num cap)
  (multiple-value-bind (val err)
      (handler-case
	  (let ((alloc (make-instance 'limited-allocator :capacity cap)))
	    (loop for i below num do (allocator:allocate alloc)))
	(allocator-impl:allocation-failed-error (e) (values nil e)))
    (list val err)))

(defmacro limited-test (get limit err)
  `(destructuring-bind (val err)
       (limited-test-stub ,get ,limit)
     (is val nil)
     (ok (typep err ,err)) (or (typep err ,err) (format t "actual type of err is ~A" (type-of err)))))

(limited-test 100 100 'null)
(limited-test 101 100 'allocation-failed-error)

(ok (not (let ((alloc (make-instance 'unlimited-allocator)))
	   (allocator:deallocate alloc nil))))

(let ((results (multiple-value-list (ignore-errors
				      (let ((alloc (make-instance 'limited-allocator :capacity 10)))
					(allocator:deallocate alloc nil))))))
  (is (first results) nil)
  (ok (typep (second results) 'simple-error)))

(finalize)
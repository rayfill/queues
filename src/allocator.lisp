(in-package :cl-user)
(defpackage :allocator
  (:use :cl)
  (:export :allocator :allocate :deallocate))
(in-package :allocator)

(defgeneric allocate (allocator))
(defgeneric deallocate (allocator cons))
(defclass allocator () ())

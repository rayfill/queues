(in-package :cl-user)
(defpackage :queues
  (:use :cl)
  (:export :offer :poll :peek :put :take :create-queue
	   :lock-free-queue :blocking-queue))
(in-package :queues)

;; non-blocking methods.
(defgeneric offer (queue value))
(defgeneric poll (queue))
(defgeneric peek (queue))

;; blocking methods.
(defgeneric put (queue value))
(defgeneric take (queue))

(defclass abstract-queue ()
  ((head :accessor head :type list)
   (tail :accessor tail :type list)))

(defgeneric create-queue (class &key capacity))
(defmethod create-queue (class &key (capacity 0 supplied-p))
  (apply #'make-instance class (when supplied-p `(:capacity ,capacity))))
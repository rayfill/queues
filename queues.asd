#|
  This file is a part of queues project.
|#

(in-package :cl-user)
(defpackage queues-asd
  (:use :cl :asdf))
(in-package :queues-asd)

(defsystem queues
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cons-pool :kmrcl :bordeaux-threads)
  :components ((:module "src"
			:components
			(
			 (:file "queues")
			 (:file "allocator")
			 (:file "allocator-impl")
			 (:file "lock-free-queue")
			 (:file "blocking-queue"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op queues-test))))

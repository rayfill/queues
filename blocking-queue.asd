#|
  This file is a part of blocking-queue project.
|#

(in-package :cl-user)
(defpackage blocking-queue-asd
  (:use :cl :asdf))
(in-package :blocking-queue-asd)

(defsystem blocking-queue
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cons-pool :kmrcl)
  :components ((:module "src"
                :components
                ((:file "blocking-queue")
		 (:file "linked-list-queue"))))
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
  :in-order-to ((test-op (load-op blocking-queue-test))))

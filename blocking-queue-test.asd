#|
  This file is a part of blocking-queue project.
|#

(in-package :cl-user)
(defpackage blocking-queue-test-asd
  (:use :cl :asdf))
(in-package :blocking-queue-test-asd)

(defsystem blocking-queue-test
  :author ""
  :license ""
  :depends-on (:blocking-queue
               :cl-test-more
	       :workers)
  :components ((:module "t"
                :components
                ((:file "blocking-queue"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))

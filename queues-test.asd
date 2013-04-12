#|
  This file is a part of queues project.
|#

(in-package :cl-user)
(defpackage queues-test-asd
  (:use :cl :asdf))
(in-package :queues-test-asd)

(defsystem queues-test
  :author ""
  :license ""
  :depends-on (:queues
               :cl-test-more
	       :workers)
  :components ((:module "t"
                :components
                ((:file "allocator")
		 (:file "queues"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))

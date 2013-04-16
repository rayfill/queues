(in-package :queues)
(import '(blocking-queue:blocking-queue lock-free-queue:lock-free-queue))
(export (list (find-symbol "BLOCKING-QUEUE" :blocking-queue)
	      (find-symbol "LOCK-FREE-QUEUE" :lock-free-queue)))


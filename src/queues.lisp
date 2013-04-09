(in-package :cl-user)
(defpackage :queues
  (:use :cl)
  (:nicknames :q)
  (:export :offer :poll :peek :put :take))
(in-package :queues)

;; non-blocking methods.
(defgeneric offer (queue value))
(defgeneric poll (queue))
(defgeneric peek (queue))

;; blocking methods.
(defgeneric put (queue value))
(defgeneric take (queue))

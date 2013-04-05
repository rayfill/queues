(in-package :cl-user)
(defpackage :blocking-queue
  (:use :cl)
  (:nicknames :bq)
  (:export :abstract-blocking-queue :offer :poll :peek))
(in-package :blocking-queue)

;; non-blocking methods.
(defgeneric offer (queue value))
(defgeneric poll (queue))
(defgeneric peek (queue))

;; blocking methods.
(defgeneric put (queue value))
(defgeneric take (queue))

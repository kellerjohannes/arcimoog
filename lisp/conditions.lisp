(in-package :arcimoog)


(defun instantiate-empty-parameter (condition)
  (declare (ignore condition))
  (invoke-restart 'instantiate-empty-parameter))

(define-condition no-parameter-found (error)
  ((parameter-id :initarg :id :reader parameter-id)))

(defun get-parameter-value-from-user (id)
  (format *query-io* "Enter a value for the new parameter with it ~a: " id)
  (force-output *query-io*)
  (list (read)))

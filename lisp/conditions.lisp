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



(define-condition incudine-is-not-in-rt (error) ())

(define-condition pm-error (error)
  ((pm-error-flag :initarg :pm-error-flag :reader pm-error-flag)))

(defun get-faderfox-name-from-user ()
  (format *query-io* "Enter a name for the Faderfox device: ")
  (force-output *query-io*)
  (list (read)))

(define-condition faderfox-id-not-found (error)
  ((faderfox-name :initarg :faderfox-name :reader faderfox-name)))



(define-condition buffer-file-not-found (error)
  ((root-path :initarg :root-path :reader root-path)
   (sample-path :initarg :sample-path :reader sample-path)))

(define-condition buffer-file-not-loaded (error)
  ((root-path :initarg :root-path :reader root-path)
   (sample-path :initarg :sample-path :reader sample-path)))

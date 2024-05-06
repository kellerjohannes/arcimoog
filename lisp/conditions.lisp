(in-package :arcimoog.conditions)

(define-condition compile-error (error) ())


(define-condition incudine-is-not-in-rt (error) ())

(define-condition pm-error (error)
  ((pm-error-flag :initarg :pm-error-flag
                  :reader pm-error-flag)))

(defun get-faderfox-name-from-user ()
  (format *query-io* "Enter a name for the Faderfox device: ")
  (force-output *query-io*)
  (list (read)))

(define-condition faderfox-id-not-found (error)
  ((faderfox-name :initarg :faderfox-name
                  :reader faderfox-name)))



(define-condition buffer-file-not-found (error)
  ((root-path :initarg :root-path
              :reader root-path)
   (sample-path :initarg :sample-path
                :reader sample-path)))

(define-condition buffer-file-not-loaded (error)
  ((root-path :initarg :root-path
              :reader root-path)
   (sample-path :initarg :sample-path
                :reader sample-path)))


;;; probably to be removed after parameter revision

(defun instantiate-empty-parameter (condition)
  (declare (ignore condition))
  (invoke-restart 'instantiate-empty-parameter))

(define-condition no-parameter-found (error)
  ((parameter-id :initarg :id :reader parameter-id)))

(defun get-parameter-value-from-user (id)
  (format *query-io* "Enter a value for the new parameter with it ~a: " id)
  (force-output *query-io*)
  (list (read)))




;;; new parameter implementation

;; (define-condition parameter-data-is-not-number (error)
;;   ((parameter-data-instance :initarg :parameter-data-instance
;;                             :reader parameter-data-instance)))

;; (define-condition parameter-data-out-of-range (error)
;;   ((parameter-data-instance :initarg :parameter-data-instance
;;                             :reader parameter-data-instance)))

;; (define-condition parameter-data-is-not-string (error)
;;   ((parameter-data-instance :initarg :parameter-data-instance
;;                             :reader parameter-data-instance)))

(define-condition parameter-data-invalid (error)
  ((parameter-data-instance :initarg :parameter-data-instance
                            :reader parameter-data-instance)))

(define-condition parameter-data-type-unsupported (error)
  ((data-expression :initarg :data-expression
                    :reader data-expression
                    :documentation "Needs to accept alists describing a PARAMETER-DATA instance as well as single data objects (numbers,strings, vectors).")))

(define-condition key-not-supported (error)
  ((key :initarg :key
        :reader key
        :documentation "Keyword or string allowed.")
   (parameter-bank-instance :initarg :parameter-bank-instance
                            :reader parameter-bank-instance))
  (:report (lambda (condition stream)
             (format stream "The key ~a is not found in bank ~a."
                     (key condition)
                     (parameter-bank-instance condition)))))

(define-condition parameter-not-found (error)
  ((parameter-bank-instance :initarg parameter-bank-instance
                            :reader parameter-bank-instance)
   (key :initarg :key
        :reader key)))

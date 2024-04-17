(in-package :arcimoog)

(defparameter *faderfox-setup-range* (cons 176 189))
(defparameter *faderfox-controller-range* (cons 0 127))

(defun valid-faderfox-setup-p (setup)
  (cond ((and (>= setup (car *faderfox-setup-range*)) (<= setup (cdr *faderfox-setup-range*))) t)
        (t (log:debug "Invalid Faderfox setup: ~a" setup)
           nil)))

(defun valid-faderfox-controller-p (controller)
  (cond ((and (>= controller (car *faderfox-controller-range*))
              (<= controller (cdr *faderfox-controller-range*)))
         t)
        (t (log:debug "Invalid Faderfox controller: ~a" controller)
           nil)))


(defclass parameter-class ()
  ((value :initform 0 :initarg :value :accessor value)
   (callback-fun :initform (lambda (v d dd)
                             (declare (ignore v d dd)))
                 :initarg :callback-fun :accessor callback-fun)
   (interpreter-fun :initform (lambda (value) value) :initarg :interpreter-fun :accessor interpreter-fun)
   (activep :initform t :initarg :activep :accessor activep)
   (short-description :initform "[empty]" :initarg :short-description :accessor short-description)
   (long-description :initform "[empty parameter slot]" :initarg :long-description :accessor long-description)))

(defmethod get-parameter-value ((parameter parameter-class))
  (value parameter))

(defmethod update-parameter ((parameter parameter-class) &rest arguments)
  (when (activep parameter)
    (setf (value parameter) (apply (interpreter-fun parameter) (cons (value parameter) arguments)))
    (funcall (callback-fun parameter) (value parameter) (short-description parameter) (long-description parameter)))
  (value parameter))


(defclass parameter-slot-class ()
  ((parameters :initform (make-hash-table :test 'equal) :accessor parameters)
   (id-dictionary :initform nil :accessor id-dictionary)))

(defun make-parameter-key (setup controller)
  (format nil "P~a-~a" setup controller))

(defmethod initialize-instance :after ((bank parameter-slot-class) &key)
  (loop for setup from (car *faderfox-setup-range*) to (cdr *faderfox-setup-range*) do
    (loop for controller from (car *faderfox-controller-range*) to (cdr *faderfox-controller-range*) do
      (setf (gethash (make-parameter-key setup controller)
                     (parameters bank))
            (make-instance 'parameter-class)))))

(defmethod update ((bank parameter-slot-class) setup controller value)
  (when (and (valid-faderfox-setup-p setup)
             (valid-faderfox-controller-p controller))
    (let ((parameter (gethash (make-parameter-key setup controller) (parameters bank))))
      (if parameter
          (update-parameter parameter value)
          (log:error "No parameter for setup ~a and controller ~a found." setup controller)))))

(defmethod add-id ((bank parameter-slot-class) setup controller id)
  (push (cons id (make-parameter-key setup controller)) (id-dictionary bank)))

(defmethod get-key-from-id ((bank parameter-slot-class) id)
  (cdr (assoc id (id-dictionary bank))))

(defmethod get-parameter ((bank parameter-slot-class) id)
  (gethash (get-key-from-id bank id) (parameters bank)))

(defmethod access ((bank parameter-slot-class) id)
  (get-parameter-value (get-parameter bank id)))

(defmethod get-short-description ((bank parameter-slot-class) id)
  (short-description (get-parameter bank id)))

(defmethod activate-parameter ((bank parameter-slot-class) id)
  (setf (activep (get-parameter bank id)) t))

(defmethod deactivate-parameter ((bank parameter-slot-class) id)
  (setf (activep (get-parameter bank id)) nil))

(defmethod configure-parameter ((bank parameter-slot-class) setup controller id
                                short-name long-name value interpreter-fun callback-fun)
  ;; TODO: remove id in case it changed, implement checking function to make sure that dictionary
  ;; contains unique ids only.
  (add-id bank setup controller id)
  (let ((parameter (get-parameter bank id)))
    (if parameter
        (setf (short-description parameter) short-name
              (long-description parameter) long-name
              (callback-fun parameter) callback-fun
              (interpreter-fun parameter) interpreter-fun
              (value parameter) value)
        (log:error "No parameter found for setup ~a and controller ~a." setup controller)))
  (activate-parameter bank id))



(defparameter *parameter-bank* (make-instance 'parameter-slot-class))

(in-package :arcimoog.parameters)


(defclass parameter ()
  ((name :initarg :name :reader name)
   (value :initarg :value :accessor value)
   (hooks :initarg :hooks :initform nil :accessor hooks)))

(defgeneric valid-value-p (parameter value))

(defgeneric set-value (parameter value))

(defgeneric inc-value (parameter value))

(defgeneric get-value (parameter))

(defgeneric push-hook (parameter hook-fun))

(defgeneric get-hook-docstrings (parameter))

;; TODO Add functions to remove and manage the hook list.


(defmethod push-hook ((param parameter) hook-fun)
  (if (functionp hook-fun)
      (push hook-fun (hooks param))
      (log:warn "Hook ~a is not of type FUNCTION." hook-fun)))

(defmethod call-hooks ((param parameter))
  (dolist (hook (hooks param))
    (funcall hook (name param) (value param))))

(defmethod get-hook-docstrings ((param parameter))
  (mapcar (lambda (hook)
            (documentation hook 'function))
          (hooks param)))


(defclass parameter-scalar (parameter)
  ((range-min :initarg :range-min :accessor range-min)
   (range-max :initarg :range-max :accessor range-max)))

(defmethod valid-value-p ((param parameter-scalar) val)
  (and (>= val (range-min param))
       (<= val (range-max param))))

(defmethod set-value ((param parameter-scalar) val)
  (cond ((valid-value-p param val)
         (setf (value param) val)
         (call-hooks param))
        ;; TODO This should be reimplemented with proper condition handling, offering either to
        ;; ignore the faulty value, or to throw an error.
        (t (log:warn "Value ~a out of range." val))))

(defmethod inc-value ((param parameter-scalar) val)
  (let ((new-value (+ (value param) val)))
    (cond ((valid-value-p param new-value)
           (set-value param new-value))
          ;; TODO This could be reimplemented using the condition system, by trying and ignoring
          ;; out-of-range results.
          (t (log:warn "Increment ~a for value ~a out of range." val (value param))))))

(defmethod get-value ((param parameter-scalar))
  (value param))



(defparameter *parameter-bank* (make-hash-table))

(defun find-parameter (name)
  ;; TODO condition handling, check for existence
  (gethash name *parameter-bank*))

(defun clear-parameter-bank ()
  "Deletes the current *PARAMETER-BANK* and initialises a new, empty hash-table."
  (setf *parameter-bank* (make-hash-table)))

(defun print-parameter-list ()
  (maphash (lambda (hash-key parameter-instance)
             (format t "~&~a: ~a~{~&⤷~a~}~%~%"
                     hash-key
                     (get-value parameter-instance)
                     (get-hook-docstrings parameter-instance)))
           *parameter-bank*))

(defun register-hook (parameter-name fun)
  (push-hook (find-parameter parameter-name) fun))

(defun register-scalar (name val min max hooks)
  (setf (gethash name *parameter-bank*) ; not using 'find-parameter because a new entry in the
                                        ; hash-table should be created if there is no entry under
                                        ; NAME.
        (make-instance 'parameter-scalar
                       :name name
                       :value val
                       :range-min min
                       :range-max max
                       :hooks hooks)))

(defun get-scalar (parameter-name)
  (get-value (find-parameter parameter-name)))

(defun set-scalar (parameter-name val)
  (set-value (find-parameter parameter-name) val))

(defun inc-scalar (parameter-name val)
  (inc-value (find-parameter parameter-name) val))

(in-package :arcimoog.parameters)


(defclass parameter-data ()
  ((data :initform nil :initarg :data :accessor data)))

(defgeneric valid-parameter-data-p (parameter-data))
(defgeneric print-string (parameter-data))
(defgeneric export-parameter-data (parameter-data))
(defgeneric get-parameter-data (parameter-data))

(defclass parameter-data-scalar (parameter-data)
  ((lower-border :initform nil :initarg :lower-border :accessor lower-border)
   (upper-border :initform nil :initarg :upper-border :accessor upper-border)))

(defmethod valid-parameter-data-p ((parameter-data parameter-data-scalar))
  (with-accessors ((data data)
                   (lower lower-border)
                   (upper upper-border))
      parameter-data
    (let ((result t))
      (unless (numberp data)
        (setf result nil)
        (error "Parameter data ~a is not a number." data))
      (when (< data lower)
        (setf result nil)
        (error "Parameter data ~a is smaller than its lower border ~a." data lower))
      (when (> data upper)
        (setf result nil)
        (error "Parameter data ~a is larger than its upper border ~a." data upper))
      result)))

(defmethod print-string ((scalar parameter-data-scalar))
  (format nil "~a [~@[~a~]-~@[~a~]]" (data scalar) (lower-border scalar) (upper-border scalar)))

(defmethod export-parameter-data ((scalar parameter-data-scalar))
  (list :type :scalar
        :data (data scalar)
        :lower-border (lower-border scalar)
        :upper-border (upper-border scalar)))

(defmethod get-parameter-data ((scalar parameter-data-scalar))
  (data scalar))



(defclass parameter-data-rgb (parameter-data)
  ())

(defmethod valid-parameter-data-p ((parameter-data parameter-data-rgb))
  (with-accessors ((data data))
      parameter-data
    (let ((result t))
      (unless (typep data '(array float (3)))
        (setf result nil)
        (error "Parameter data ~a is not a vector of 3 floats." data))
      (loop for component across data
            for index from 1 do
              (when (< component 0)
                (setf result nil)
                (error "Element ~a of RGB vector ~a is too small." index data))
              (when (> component 1)
                (setf result nil)
                (error "Element ~a of RGB vector ~a is too large." index data)))
      result)))

(defmethod inc-rgb ((parameter-data parameter-data-rgb) channel amount)
  (let ((index (cdr (assoc channel (list (cons :r 0) (cons :g 1) (cons :b 2))))))
    (incf (aref (data parameter-data) index) amount)))

(defmethod print-string ((rgb-data parameter-data-rgb))
  (with-accessors ((data data))
      rgb-data
    (format nil "R: ~a, G: ~a, B: ~a" (aref data 0) (aref data 1) (aref data 2))))

(defmethod export-parameter-data ((rgb parameter-data-rgb))
  (list :type :rgb :data (data rgb)))

(defmethod get-parameter-data ((rgb parameter-data-rgb))
  (data rgb))


(defun import-data (data-expression)
  (unless (eq data-expression :uninitialized)
    (case (getf data-expression :type)
      (:scalar (make-instance 'parameter-data-scalar
                              :data (getf data-expression :data)
                              :lower-border (getf data-expression :lower-border)
                              :upper-border (getf data-expression :upper-border)))
      (:rgb (make-instance 'parameter-data-rgb
                           :data (getf data-expression :data)))
      (otherwise (error "Data type ~a of data expression ~a is not supported by import function."
                        (getf data-expression :type)
                        data-expression)))))



(defclass parameter-class ()
  ((data :initarg :data :initform (cons nil nil) :accessor data)
   (short-doc :initarg :short-doc :initform "" :accessor short-doc)
   (long-doc :initarg :long-doc :initform "" :accessor long-doc)))

(defmethod write-parameter-to-file ((parameter parameter-class) file-stream))

(defmacro set-data-cell (parameter data access-fun)
  `(typecase ,data
     (number (setf (,access-fun (data ,parameter))
                   (make-instance 'parameter-data-scalar :data ,data)))
     ((array float (3)) (setf (,access-fun (data ,parameter))
                              (make-instance 'parameter-data-rgb :data ,data)))
     (null nil)
     (parameter-data-scalar (setf (,access-fun (data ,parameter)) ,data))
     (parameter-data-rgb (setf (,access-fun (data ,parameter)) ,data))
     (otherwise (error "Parameter data ~a is of unsupported type." ,data))))

(defmethod get-data-cell ((parameter parameter-class) access-fun)
  (funcall access-fun (data parameter)))

(defmethod get-data-cell-string ((parameter parameter-class) access-fun)
  (let ((data-cell (get-data-cell parameter access-fun)))
    (if data-cell
        (print-string data-cell)
        "[uninitialized]")))

(defmethod export-data-cell ((parameter parameter-class) access-fun)
  (let ((data-cell (get-data-cell parameter access-fun)))
    (if data-cell
        (export-parameter-data data-cell)
        :uninitialized)))



(defmethod set-data ((parameter parameter-class) data)
  (set-data-cell parameter data car))

(defmethod get-data ((parameter parameter-class))
  (get-data-cell parameter #'car))

(defmethod get-data-string ((parameter parameter-class))
  (get-data-cell-string parameter #'car))

(defmethod export-data ((parameter parameter-class))
  (export-data-cell parameter #'car))


(defmethod set-default-data ((parameter parameter-class) default-data)
  (set-data-cell parameter default-data cdr))

(defmethod get-default-data ((parameter parameter-class))
  (get-data-cell parameter #'cdr))

(defmethod get-default-data-string ((parameter parameter-class))
  (get-data-cell-string parameter #'cdr))

(defmethod export-default-data ((parameter parameter-class))
  (export-data-cell parameter #'cdr))



(defmethod set-short-doc ((parameter parameter-class) short-doc)
  (setf (short-doc parameter) short-doc))

(defmethod get-short-doc ((parameter parameter-class))
  (short-doc parameter))

(defmethod set-long-doc ((parameter parameter-class) long-doc)
  (setf (long-doc parameter) long-doc))

(defmethod get-long-doc ((parameter parameter-class))
  (long-doc parameter))

(defun make-parameter (data default-data short-doc long-doc)
  (let ((result (make-instance 'parameter-class
                               :short-doc short-doc
                               :long-doc long-doc)))
    (set-data result data)
    (set-default-data result default-data)
    result))

(defmethod print-parameter ((parameter parameter-class) &optional (stream *standard-output*))
  (format stream "~&  SHORT='~a'" (get-short-doc parameter))
  (format stream "~&  LONG='~a'" (get-long-doc parameter))
  (format stream "~&  DATA='~a'" (get-data-string parameter))
  (format stream "~&  DEFAULT-DATA='~a'" (get-default-data-string parameter)))

(defmethod export-parameter ((parameter parameter-class))
  (list :short-doc (get-short-doc parameter)
        :long-doc (get-long-doc parameter)
        :data (export-data parameter)
        :default-data (export-default-data parameter)))


(defun parse-key (string-or-keyword)
  (typecase string-or-keyword
    (keyword string-or-keyword)
    (string (alexandria:make-keyword (string-upcase string-or-keyword)))
    (otherwise (error "Type of key ~a not supported." string-or-keyword))))

(defclass parameter-bank-class ()
  ((parameters :initform (make-hash-table) :accessor parameters)))

(defmethod get-parameter ((bank parameter-bank-class) key)
  (let* ((keyword-key (parse-key key))
         (result (gethash keyword-key (parameters bank))))
    (if result result (error "No parameter with key ~a found in parameter bank." keyword-key))))

(defmethod update-parameter ((bank parameter-bank-class) key data
                             &key (default-data nil default-data-supplied-p)
                               (short-doc nil short-doc-supplied-p)
                               (long-doc nil long-doc-supplied-p))
  (let ((parameter (get-parameter bank (parse-key key))))
    (set-data parameter data)
    (when default-data-supplied-p (set-default-data parameter default-data))
    (when short-doc-supplied-p (set-short-doc parameter short-doc))
    (when long-doc-supplied-p (set-long-doc parameter long-doc))))

(defmethod add-parameter ((bank parameter-bank-class) key data default short-doc long-doc)
  (let ((keyword-key (parse-key key)))
    (if (gethash keyword-key (parameters bank))
        (update-parameter bank keyword-key data
                          :default-data default
                          :short-doc short-doc
                          :long-doc long-doc)
        (setf (gethash keyword-key (parameters bank))
              (make-parameter data default short-doc long-doc)))))

(defmethod import-parameter ((bank parameter-bank-class) key parameter-expression)
  (add-parameter bank
                 key
                 (import-data (getf parameter-expression :data))
                 (import-data (getf parameter-expression :default-data))
                 (getf parameter-expression :short-doc)
                 (getf parameter-expression :long-doc)))

(defmethod print-bank ((bank parameter-bank-class) &optional (stream *standard-output*))
  (loop for key being the hash-keys of (parameters bank) do
    (format stream "~&PARAMETER (key='~a'):~%" key)
    (print-parameter (get-parameter bank key))))

(defmethod export-bank ((bank parameter-bank-class))
  (loop for key being the hash-keys of (parameters bank)
        collect (cons key (export-parameter (get-parameter bank key)))))

(defmethod delete-all-parameters ((bank parameter-bank-class))
  (setf (parameters bank) (make-hash-table)))

(defmethod import-bank ((bank parameter-bank-class) data-expression &key (mode :reinit))
  (when (eq mode :reinit) (delete-all-parameters bank))
  (loop for entry in data-expression do
        (import-parameter bank (car entry) (cdr entry))))

(defmethod write-bank-to-file ((bank parameter-bank-class) filepath)
  (with-open-file (stream filepath
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write (export-bank bank) :stream stream :pretty t)))

(defmethod read-bank-from-file ((bank parameter-bank-class) filepath &key (mode :reinit))
  (with-open-file (stream filepath)
    (import-bank bank (read stream) :mode mode)))

(defmethod get-parameter-value ((bank parameter-bank-class) key)
  (get-parameter-data (get-data (get-parameter bank key))))


(defparameter *bank* (make-instance 'parameter-bank-class))





(defun read-parameters (&optional (filepath "/parameter-bank/parameters.lisp"))
  (read-bank-from-file *bank* filepath))

(defun write-parameters (&optional (filepath "/parameter-bank/parameters.lisp"))
  (write-bank-to-file *bank* filepath))

(defun defp (key data default short-doc long-doc)
  (add-parameter *bank* key data default short-doc long-doc))

(defun setp (key data)
  (update-parameter *bank* key data))

(defun getp (key)
  (get-parameter-value *bank* key))




(defp :p1 0.0 0.0 "nix" "nixnix")
(defp :p2 1.0 1.0 "nix1" "nixnix1")

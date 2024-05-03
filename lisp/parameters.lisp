(in-package :arcimoog.parameters)

(defclass parameter-data ()
  ((data :initform nil
         :initarg :data
         :accessor data
         :documentation "Actual data."))
  (:documentation "Instances represent the value of a parameter. Methods define how they are manipulated, checked for validity and exported to S-expressions."))

(defgeneric valid-parameter-data-p (parameter-data)
  (:documentation "Checks for validity."))

(defgeneric print-string (parameter-data)
  (:documentation "Returns a string describing the content of the instance."))

(defgeneric export-parameter-data (parameter-data)
  (:documentation "Returns an S-expression for dump export."))

(defgeneric get-parameter-data (parameter-data)
  (:documentation "Returns the actual data to be used in CL code."))


(defclass parameter-data-scalar (parameter-data)
  ((lower-border :initform nil
                 :initarg :lower-border
                 :accessor lower-border
                 :documentation "Number or NIL if unlimited.")
   (upper-border :initform nil
                 :initarg :upper-border
                 :accessor upper-border
                 :documentation "Number or NIL if unlimited."))
  (:documentation "Subclass for simple numbers. Could be integers, ratios or floats."))

(defmethod valid-parameter-data-p ((parameter-data parameter-data-scalar))
  "Checks for type (NUMBER) and range if defined."
  (with-accessors ((data data) (lower lower-border) (upper upper-border))
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
  "String includes the range."
  (format nil "~a [~@[~a~]-~@[~a~]]" (data scalar) (lower-border scalar) (upper-border scalar)))

(defmethod export-parameter-data ((scalar parameter-data-scalar))
  (list :type :scalar
        :data (data scalar)
        :lower-border (lower-border scalar)
        :upper-border (upper-border scalar)))

(defmethod get-parameter-data ((scalar parameter-data-scalar))
  (data scalar))



(defclass parameter-data-rgb (parameter-data)
  ()
  (:documentation "Subclass for RGB values, which are represented as a SIMPLE-VECTOR of length 3, consisting of floats ranging from 0.0 to 1.0."))

(defmethod valid-parameter-data-p ((parameter-data parameter-data-rgb))
  "Tests for data type and ranges."
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
  "Applies INCF to one of the channels (:R, :G or :B) without checking for the correct range."
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


(defun import-parameter-data (data-expression)
  "Expects an S-expression describing the data, as they are created by `EXPORT-PARAMETER-DATA'. Returns an instance of an appropriate PARAMETER-DATA subclass."
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

;; TODO add modifying methods
;; TODO add validity checks at certain points (when importing / exporting? Certainly when modifying)


(defclass parameter-class ()
  ((data :initarg :data
         :initform (cons nil nil)
         :accessor data
         :documentation "A pair, of which each cell holds either NIL or an instance of PARAMETER-DATA. The CAR is the actual data, the CDR is the default data of the parameter.")
   (short-doc :initarg :short-doc
              :initform ""
              :accessor short-doc
              :documentation "String with a short description of the parameter.")
   (long-doc :initarg :long-doc
             :initform ""
             :accessor long-doc
             :documentation "String with a detailed description of the parameter."))
  (:documentation "Holds info and data for one parameter."))

(defmacro set-data-cell (parameter data access-fun)
  "SETFs the DATA slot of a PARAMETER. ACCESS-FUN can be either CAR (for the actual data) or CDR (for the default data). Expects any Common Lisp object and returns an appropriate PARAMETER-DATA subclass representing the object."
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
  "Returns the PARAMETER-DATA instance according to ACCESS-FUN: CAR for the actual data, CDR for the default data."
  (funcall access-fun (data parameter)))

(defmethod get-data-cell-string ((parameter parameter-class) access-fun)
  "Returns a descriptive string of the data. ACCESS-FUN can be CAR for the actual parameter data or CDR for the default data."
  (let ((data-cell (get-data-cell parameter access-fun)))
    (if data-cell
        (print-string data-cell)
        "[uninitialized]")))

(defmethod export-data-cell ((parameter parameter-class) access-fun)
  "Returns an S-expression (plist) for data export. ACCESS-FUN can be CAR for the actual data or CDR for the default data."
  (let ((data-cell (get-data-cell parameter access-fun)))
    (if data-cell
        (export-parameter-data data-cell)
        :uninitialized)))



(defmethod set-data ((parameter parameter-class) data)
  "Wrapper around SET-DATA-CELL."
  (set-data-cell parameter data car))

(defmethod get-data ((parameter parameter-class))
  "Wrapper around GET-DATA-CELL."
  (get-data-cell parameter #'car))

(defmethod get-data-string ((parameter parameter-class))
  "Wrapper around GET-DATA-CELL-STRING."
  (get-data-cell-string parameter #'car))

(defmethod export-data ((parameter parameter-class))
  "Wrapper around EXPORT-DATA-CELL."
  (export-data-cell parameter #'car))


(defmethod set-default-data ((parameter parameter-class) default-data)
  "Wrapper around SET-DATA-CELL."
  (set-data-cell parameter default-data cdr))

(defmethod get-default-data ((parameter parameter-class))
  "Wrapper around GET-DATA-CELL."
  (get-data-cell parameter #'cdr))

(defmethod get-default-data-string ((parameter parameter-class))
  "Wrapper around GET-DATA-CELL-STRING."
  (get-data-cell-string parameter #'cdr))

(defmethod export-default-data ((parameter parameter-class))
  "Wrapper around EXPORT-DATA-CELL."
  (export-data-cell parameter #'cdr))

(defmethod set-short-doc ((parameter parameter-class) short-doc)
  "Expects a string."
  (setf (short-doc parameter) short-doc))

(defmethod get-short-doc ((parameter parameter-class))
  "Returns a string."
  (short-doc parameter))

(defmethod set-long-doc ((parameter parameter-class) long-doc)
  "Expects a string."
  (setf (long-doc parameter) long-doc))

(defmethod get-long-doc ((parameter parameter-class))
  "Returns a string."
  (long-doc parameter))

(defun make-parameter (data default-data short-doc long-doc)
  "Returns an instance of PARAMETER-CLASS, properly initialized, which primarily concerns the pair (CAR is the actual data, CDR is the default data) in the DATA slot. This function should be used in place of MAKE-INSTANCE because the latter doesn't set the DATA slot correctly."
  (let ((result (make-instance 'parameter-class
                               :short-doc short-doc
                               :long-doc long-doc)))
    (set-data result data)
    (set-default-data result default-data)
    result))

(defmethod print-parameter ((parameter parameter-class) &optional (stream *standard-output*))
  "Prints human readable contents to a stream."
  (format stream "~&  SHORT='~a'" (get-short-doc parameter))
  (format stream "~&  LONG='~a'" (get-long-doc parameter))
  (format stream "~&  DATA='~a'" (get-data-string parameter))
  (format stream "~&  DEFAULT-DATA='~a'" (get-default-data-string parameter)))

(defmethod export-parameter ((parameter parameter-class))
  "Returns an S-expression (plist) containing everything about this PARAMETER for exporting purposes."
  (list :short-doc (get-short-doc parameter)
        :long-doc (get-long-doc parameter)
        :data (export-data parameter)
        :default-data (export-default-data parameter)))


(defun parse-key (string-or-keyword)
  "Expects a keyword or a string. If string, it will be converted to a case-insensitive keyword. To be used as keys for a hash-table."
  (typecase string-or-keyword
    (keyword string-or-keyword)
    (string (alexandria:make-keyword (string-upcase string-or-keyword)))
    (otherwise (error "Type of key ~a not supported." string-or-keyword))))



(defclass parameter-bank-class ()
  ((parameters :initform (make-hash-table)
               :accessor parameters
               :documentation "Hash-table that exclusively contains instances of PARAMETER-CLASS."))
  (:documentation "Collection of parameters that can be set up, manipulated, printed and exported as a bulk."))

(defmethod get-parameter ((bank parameter-bank-class) key)
  "Expects a keyword or string to find a parameter in a parameter bank, returns the corresponding instance of PARAMETER-CLASS."
  (let* ((keyword-key (parse-key key))
         (result (gethash keyword-key (parameters bank))))
    (if result result (error "No parameter with key ~a found in parameter bank." keyword-key))))

(defmethod update-parameter ((bank parameter-bank-class) key data
                             &key (default-data nil default-data-supplied-p)
                               (short-doc nil short-doc-supplied-p)
                               (long-doc nil long-doc-supplied-p))
  "Expects a keyword or string to be used as key for the parameter bank hash-table. DATA can be any Common Lisp object that is supported by the PARAMETER-DATA class."
  (let ((parameter (get-parameter bank (parse-key key))))
    (set-data parameter data)
    (when default-data-supplied-p (set-default-data parameter default-data))
    (when short-doc-supplied-p (set-short-doc parameter short-doc))
    (when long-doc-supplied-p (set-long-doc parameter long-doc))))

(defmethod add-parameter ((bank parameter-bank-class) key data default short-doc long-doc)
  "KEY can be a keyword or a string. DATA and DEFAULT-DATA can be any Common Lisp object that is supported by the PARAMETER-DATA class. SHORT-DOC and LONG-DOC are strings for documentation purposes. If the parameter already exists, it will be updated, otherwise created."
  (let ((keyword-key (parse-key key)))
    (if (gethash keyword-key (parameters bank))
        (update-parameter bank keyword-key data
                          :default-data default
                          :short-doc short-doc
                          :long-doc long-doc)
        (setf (gethash keyword-key (parameters bank))
              (make-parameter data default short-doc long-doc)))))

;; TODO delete parameter

(defmethod import-parameter ((bank parameter-bank-class) key parameter-expression)
  "KEY can be a keyword or a string. PARAMETER-EXPRESSION is an S-expression describing the content of a PARAMETER-CLASS instance, as they are created by `EXPORT-PARAMETER'. Parameters that are already existing in BANK are updated, non-existent ones are created."
  (add-parameter bank
                 key
                 (import-parameter-data (getf parameter-expression :data))
                 (import-parameter-data (getf parameter-expression :default-data))
                 (getf parameter-expression :short-doc)
                 (getf parameter-expression :long-doc)))

(defmethod print-bank ((bank parameter-bank-class) &optional (stream *standard-output*))
  "Prints a human readable description of the contents of BANK to a STREAM."
  (loop for key being the hash-keys of (parameters bank) do
    (format stream "~&PARAMETER (key='~a'):~%" key)
    (print-parameter (get-parameter bank key))))

(defmethod export-bank ((bank parameter-bank-class))
  "Creates an S-expression that represents the complete contents of BANK. It is a nested plist. The value of :parameters is the content of the hash-table containing all parameters is an alist."
  (list :parameters
        (loop for key being the hash-keys of (parameters bank)
              collect (cons key (export-parameter (get-parameter bank key))))))

(defmethod delete-all-parameters ((bank parameter-bank-class))
  "Creates an empty hash-table that replaces the former hash-table containing all parameters."
  (setf (parameters bank) (make-hash-table)))

(defmethod import-bank ((bank parameter-bank-class) data-expression &key (mode :reinit))
  "DATA-EXPRESSION is a nested plist that represents the contents of BANK, as it is created by `EXPORT-BANK'. When MODE has the value :REINIT, existing parameters will be deleted. Any other value will cause updating existing parameters."
  (when (eq mode :reinit) (delete-all-parameters bank))
  (loop for entry in (getf data-expression :parameters) do
        (import-parameter bank (car entry) (cdr entry))))

(defmethod write-bank-to-file ((bank parameter-bank-class) filepath)
  "Writes the S-expression created by `EXPORT-BANK' to a file."
  (with-open-file (stream filepath
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write (export-bank bank) :stream stream :pretty t)))

(defmethod read-bank-from-file ((bank parameter-bank-class) filepath &key (mode :reinit))
  "Reads the S-expression describing BANK from a file and populates BANK accordingly."
  (with-open-file (stream filepath)
    (import-bank bank (read stream) :mode mode)))

(defmethod get-parameter-value ((bank parameter-bank-class) key)
  "KEY can be a keyword or a string. Returns the Common Lisp object with the actual data of the parameter."
  ;; validity check?
  (get-parameter-data (get-data (get-parameter bank key))))


(defparameter *bank* (make-instance 'parameter-bank-class)
  "Globally used parameter bank, accessible through the exported functions of this package.")





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

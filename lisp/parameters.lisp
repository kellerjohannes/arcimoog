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
           (set-value param new-value)
           (call-hooks param))
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

(defun inc-scalar (parameter-name val)
  (inc-value (find-parameter parameter-name) val))


;; From here: obsolete, old implementation.

;; To add a parameter data type, follow these steps:
;; - add a subclass to PARAMETER-DATA or any of its subclasses
;; - implement methods for all generic functions that are defined for the PARAMETER-DATA super-class
;; - implement methods that are unique for the new data type (example: `INC-RGB')
;; - add the creation of an instance of the new sub-class to `IMPORT-DATA'
;; - add the creation of an instance of the new sub-class to `SET-DATA-CELL'
;; - add the new data type to the docstring of `DEFP'


;; (defclass parameter-data ()
;;   ((data :initform nil
;;          :initarg :data
;;          :accessor data
;;          :documentation "Actual data."))
;;   (:documentation "Instances represent the value of a parameter. Methods define how they are manipulated, checked for validity and exported to S-expressions."))

;; (defgeneric valid-parameter-data-p (parameter-data)
;;   (:documentation "Checks for validity."))

;; (defgeneric print-string (parameter-data)
;;   (:documentation "Returns a string describing the content of the instance."))

;; (defgeneric export-parameter-data (parameter-data)
;;   (:documentation "Returns an S-expression for dump export."))

;; (defgeneric get-parameter-data (parameter-data)
;;   (:documentation "Returns the actual data to be used in CL code."))


;; (defmethod get-parameter-data :before ((parameter parameter-data))
;;   "Testing for validity before retrieving the parameter data. The result of the test will be discarded, but the testing function would raise a condition if necessary."
;;   (valid-parameter-data-p parameter))

;; (defmethod initialize-instance :after ((parameter parameter-data) &key)
;;   "Testing for validity after the creation of a new instance of a PARAMETER-DATA subclass. The result of the test will be discarded, but the testing function would raise a condition if necessary."
;;   (valid-parameter-data-p parameter))


;; (defclass parameter-data-scalar (parameter-data)
;;   ((lower-border :initform nil
;;                  :initarg :lower-border
;;                  :accessor lower-border
;;                  :documentation "Number or NIL if unlimited.")
;;    (upper-border :initform nil
;;                  :initarg :upper-border
;;                  :accessor upper-border
;;                  :documentation "Number or NIL if unlimited."))
;;   (:documentation "Subclass for simple numbers. Could be integers, ratios or floats."))

;; (defmethod valid-parameter-data-p ((parameter-data parameter-data-scalar))
;;   "Checks for type (NUMBER) and range if defined."
;;   (with-accessors ((data data) (lower lower-border) (upper upper-border))
;;       parameter-data
;;     (let ((result t))
;;       (unless (or (numberp data))
;;         (setf result nil)
;;         (error 'acond:parameter-data-invalid :parameter-data-instance parameter-data))
;;       (when (and lower (numberp data) (or (> data upper) (< data lower)))
;;         (setf result nil)
;;         (error 'acond:parameter-data-invalid :parameter-data-instance parameter-data))
;;       result)))

;; (defmethod print-string ((scalar parameter-data-scalar))
;;   "String includes the range."
;;   (format nil "~a [~@[~a~]-~@[~a~]]" (data scalar) (lower-border scalar) (upper-border scalar)))

;; (defmethod export-parameter-data ((scalar parameter-data-scalar))
;;   (list :type :scalar
;;         :data (data scalar)
;;         :lower-border (lower-border scalar)
;;         :upper-border (upper-border scalar)))

;; (defmethod get-parameter-data ((scalar parameter-data-scalar))
;;   (data scalar))


;; (defclass parameter-data-string (parameter-data)
;;   ()
;;   (:documentation "Subclass for strings."))

;; (defmethod valid-parameter-data-p ((parameter-data parameter-data-string))
;;   "Tests for data being of type STRING."
;;   (with-accessors ((data data))
;;       parameter-data
;;     (let ((result t))
;;       (unless (typep data 'string)
;;         (setf result nil)
;;         (error 'acond:parameter-data-invalid :parameter-data-instance parameter-data))
;;       result)))

;; (defmethod print-string ((parameter-data parameter-data-string))
;;   "Returns a string containing the data string."
;;   (data parameter-data))

;; (defmethod export-parameter-data ((parameter-data parameter-data-string))
;;   "Returns a plist describing the parameter data."
;;   (list :type :string :data (data parameter-data)))

;; (defmethod get-parameter-data ((parameter-data parameter-data-string))
;;   "Returns the raw data (string)."
;;   (data parameter-data))



;; (defun import-parameter-data (data-expression)
;;   "Expects an S-expression describing the data, as they are created by `EXPORT-PARAMETER-DATA'. Returns an instance of an appropriate PARAMETER-DATA subclass."
;;   (unless (eq data-expression :uninitialized)
;;     (case (getf data-expression :type)
;;       (:scalar (make-instance 'parameter-data-scalar
;;                               :data (getf data-expression :data)
;;                               :lower-border (getf data-expression :lower-border)
;;                               :upper-border (getf data-expression :upper-border)))
;;       (:string (make-instance 'parameter-data-string
;;                               :data (getf data-expression :data)))
;;       (otherwise (error 'acond:parameter-data-type-unsupported :data-expression data-expression)))))

;; ;; TODO add modifying methods

;; (defclass parameter-class ()
;;   ((data :initarg :data
;;          :initform (cons 0 0)
;;          :accessor data
;;          :documentation "A pair, of which each cell holds either NIL or an instance of PARAMETER-DATA. The CAR is the actual data, the CDR is the default data of the parameter.")
;;    (short-doc :initarg :short-doc
;;               :initform ""
;;               :accessor short-doc
;;               :documentation "String with a short description of the parameter.")
;;    (long-doc :initarg :long-doc
;;              :initform ""
;;              :accessor long-doc
;;              :documentation "String with a detailed description of the parameter."))
;;   (:documentation "Holds info and data for one parameter."))


;; ;; Warning: when changing this macro, consider recompiling all functions containing it.

;; (defmacro set-data-cell (parameter data access-fun)
;;   "SETFs the DATA slot of a PARAMETER. ACCESS-FUN can be either CAR (for the actual data) or CDR (for the default data). Expects any Common Lisp object and returns an appropriate PARAMETER-DATA subclass representing the object."
;;   `(typecase ,data
;;      ;;(null nil)
;;      (number (setf (,access-fun (data ,parameter))
;;                    (make-instance 'parameter-data-scalar :data ,data)))
;;      (string (setf (,access-fun (data ,parameter))
;;                    (make-instance 'parameter-data-string :data ,data)))
;;      ((or parameter-data-scalar parameter-data-string)
;;       (setf (,access-fun (data ,parameter)) ,data))
;;      (otherwise (error 'acond:parameter-data-type-unsupported :data-expression data))))

;; (defmethod get-data-cell ((parameter parameter-class) access-fun)
;;   "Returns the PARAMETER-DATA instance according to ACCESS-FUN: CAR for the actual data, CDR for the default data."
;;   (funcall access-fun (data parameter)))

;; (defmethod get-data-cell-string ((parameter parameter-class) access-fun)
;;   "Returns a descriptive string of the data. ACCESS-FUN can be CAR for the actual parameter data or CDR for the default data."
;;   (let ((data-cell (get-data-cell parameter access-fun)))
;;     (if data-cell
;;         (print-string data-cell)
;;         "[uninitialized]")))

;; (defmethod export-data-cell ((parameter parameter-class) access-fun)
;;   "Returns an S-expression (plist) for data export. ACCESS-FUN can be CAR for the actual data or CDR for the default data."
;;   (let ((data-cell (get-data-cell parameter access-fun)))
;;     (if data-cell
;;         (export-parameter-data data-cell)
;;         :uninitialized)))



;; (defmethod set-data ((parameter parameter-class) data)
;;   "Wrapper around SET-DATA-CELL."
;;   (set-data-cell parameter data car))

;; (defmethod get-data ((parameter parameter-class))
;;   "Wrapper around GET-DATA-CELL."
;;   (get-data-cell parameter #'car))

;; (defmethod get-data-string ((parameter parameter-class))
;;   "Wrapper around GET-DATA-CELL-STRING."
;;   (get-data-cell-string parameter #'car))

;; (defmethod export-data ((parameter parameter-class))
;;   "Wrapper around EXPORT-DATA-CELL."
;;   (export-data-cell parameter #'car))

;; (defmethod set-default-data ((parameter parameter-class) default-data)
;;   "Wrapper around SET-DATA-CELL."
;;   (set-data-cell parameter default-data cdr))

;; (defmethod get-default-data ((parameter parameter-class))
;;   "Wrapper around GET-DATA-CELL."
;;   (get-data-cell parameter #'cdr))

;; (defmethod get-default-data-string ((parameter parameter-class))
;;   "Wrapper around GET-DATA-CELL-STRING."
;;   (get-data-cell-string parameter #'cdr))

;; (defmethod export-default-data ((parameter parameter-class))
;;   "Wrapper around EXPORT-DATA-CELL."
;;   (export-data-cell parameter #'cdr))

;; (defmethod reset-data ((parameter parameter-class))
;;   (set-data parameter (get-default-data parameter)))


;; (defmethod set-short-doc ((parameter parameter-class) short-doc)
;;   "Expects a string."
;;   (setf (short-doc parameter) short-doc))

;; (defmethod get-short-doc ((parameter parameter-class))
;;   "Returns a string."
;;   (short-doc parameter))

;; (defmethod set-long-doc ((parameter parameter-class) long-doc)
;;   "Expects a string."
;;   (setf (long-doc parameter) long-doc))

;; (defmethod get-long-doc ((parameter parameter-class))
;;   "Returns a string."
;;   (long-doc parameter))

;; (defun make-parameter (data default-data short-doc long-doc)
;;   "Returns an instance of PARAMETER-CLASS, properly initialized, which primarily concerns the pair (CAR is the actual data, CDR is the default data) in the DATA slot. This function should be used in place of MAKE-INSTANCE because the latter doesn't set the DATA slot correctly."
;;   (let ((result (make-instance 'parameter-class
;;                                :short-doc short-doc
;;                                :long-doc long-doc)))
;;     (set-data result data)
;;     (set-default-data result default-data)
;;     result))

;; (defmethod print-parameter ((parameter parameter-class) &optional (stream *standard-output*))
;;   "Prints human readable contents to a stream."
;;   (format stream "~&  SHORT='~a'" (get-short-doc parameter))
;;   (format stream "~&  LONG='~a'" (get-long-doc parameter))
;;   (format stream "~&  DATA='~a'" (get-data-string parameter))
;;   (format stream "~&  DEFAULT-DATA='~a'" (get-default-data-string parameter)))

;; (defmethod export-parameter ((parameter parameter-class))
;;   "Returns an S-expression (plist) containing everything about this PARAMETER for exporting purposes."
;;   (list :short-doc (get-short-doc parameter)
;;         :long-doc (get-long-doc parameter)
;;         :data (export-data parameter)
;;         :default-data (export-default-data parameter)))





;; (defclass parameter-bank-class ()
;;   ((parameters :initform (make-hash-table)
;;                :accessor parameters
;;                :documentation "Hash-table that exclusively contains instances of PARAMETER-CLASS."))
;;   (:documentation "Collection of parameters that can be set up, manipulated, printed and exported as a bulk."))

;; (defmethod parse-key ((bank parameter-bank-class) string-or-keyword)
;;   "Expects a keyword or a string. If string, it will be converted to a case-insensitive keyword. To be used as keys for a hash-table."
;;   (typecase string-or-keyword
;;     (keyword string-or-keyword)
;;     (string (alexandria:make-keyword (string-upcase string-or-keyword)))
;;     (otherwise (error 'acond:key-not-supported :key string-or-keyword))))

;; (defmethod get-parameter ((bank parameter-bank-class) key)
;;   "Expects a keyword or string to find a parameter in a parameter bank, returns the corresponding instance of PARAMETER-CLASS."
;;   ;; implement handler-case
;;   (let* ((keyword-key (parse-key bank key))
;;          (result (gethash keyword-key (parameters bank))))
;;     (if result result (error 'acond:parameter-not-found :parameter-bank-instance bank :key key))))

;; (defmethod update-parameter ((bank parameter-bank-class) key data
;;                              &key (default-data nil default-data-supplied-p)
;;                                (short-doc nil short-doc-supplied-p)
;;                                (long-doc nil long-doc-supplied-p))
;;   "Expects a keyword or string to be used as key for the parameter bank hash-table. DATA can be any Common Lisp object that is supported by the PARAMETER-DATA class."
;;   ;; implement handler-case
;;   (let ((parameter (get-parameter bank (parse-key bank key))))
;;     (set-data parameter data)
;;     (when default-data-supplied-p (set-default-data parameter default-data))
;;     (when short-doc-supplied-p (set-short-doc parameter short-doc))
;;     (when long-doc-supplied-p (set-long-doc parameter long-doc))))

;; (defmethod generate-unique-key ((bank parameter-bank-class))
;;   ;; to be implemented!
;;   :xxxupup)

;; (defmethod add-parameter ((bank parameter-bank-class) key data default short-doc long-doc)
;;   "KEY can be a keyword or a string. DATA and DEFAULT-DATA can be any Common Lisp object that is supported by the PARAMETER-DATA class. SHORT-DOC and LONG-DOC are strings for documentation purposes. If the parameter already exists, it will be updated, otherwise created."
;;   ;; maybe change to restart-case?
;;   (let ((keyword-key (handler-case (parse-key bank key)
;;                        (acond:key-not-supported () (generate-unique-key bank)))))
;;     (if (gethash keyword-key (parameters bank))
;;         (update-parameter bank keyword-key data
;;                           :default-data default
;;                           :short-doc short-doc
;;                           :long-doc long-doc)
;;         (setf (gethash keyword-key (parameters bank))
;;               (make-parameter data default short-doc long-doc)))))

;; ;; TODO delete parameter

;; (defmethod import-parameter ((bank parameter-bank-class) key parameter-expression)
;;   "KEY can be a keyword or a string. PARAMETER-EXPRESSION is an S-expression describing the content of a PARAMETER-CLASS instance, as they are created by `EXPORT-PARAMETER'. Parameters that are already existing in BANK are updated, non-existent ones are created."
;;   (add-parameter bank
;;                  key
;;                  (import-parameter-data (getf parameter-expression :data))
;;                  (import-parameter-data (getf parameter-expression :default-data))
;;                  (getf parameter-expression :short-doc)
;;                  (getf parameter-expression :long-doc)))

;; (defmethod print-bank ((bank parameter-bank-class) &optional (stream *standard-output*))
;;   "Prints a human readable description of the contents of BANK to a STREAM."
;;   (loop for key being the hash-keys of (parameters bank) do
;;     (format stream "~&PARAMETER (key='~a'):~%" key)
;;     (print-parameter (get-parameter bank key))))

;; (defmethod export-bank ((bank parameter-bank-class))
;;   "Creates an S-expression that represents the complete contents of BANK. It is a nested plist. The value of :parameters is the content of the hash-table containing all parameters is an alist."
;;   (list :parameters
;;         (loop for key being the hash-keys of (parameters bank)
;;               collect (cons key (export-parameter (get-parameter bank key))))))

;; (defmethod delete-all-parameters ((bank parameter-bank-class))
;;   "Creates an empty hash-table that replaces the former hash-table containing all parameters."
;;   (setf (parameters bank) (make-hash-table)))

;; (defmethod import-bank ((bank parameter-bank-class) data-expression &key (mode :reinit))
;;   "DATA-EXPRESSION is a nested plist that represents the contents of BANK, as it is created by `EXPORT-BANK'. When MODE has the value :REINIT, existing parameters will be deleted. Any other value will cause updating existing parameters."
;;   (when (eq mode :reinit) (delete-all-parameters bank))
;;   (loop for entry in (getf data-expression :parameters) do
;;         (import-parameter bank (car entry) (cdr entry))))

;; (defmethod write-bank-to-file ((bank parameter-bank-class) filepath)
;;   "Writes the S-expression created by `EXPORT-BANK' to a file."
;;   (with-open-file (stream filepath
;;                           :direction :output
;;                           :if-exists :supersede
;;                           :if-does-not-exist :create)
;;     (write (export-bank bank) :stream stream :pretty t)))

;; (defmethod read-bank-from-file ((bank parameter-bank-class) filepath &key (mode :reinit))
;;   "Reads the S-expression describing BANK from a file and populates BANK accordingly."
;;   (with-open-file (stream filepath)
;;     (import-bank bank (read stream) :mode mode)))

;; (defmethod get-parameter-value ((bank parameter-bank-class) key)
;;   "KEY can be a keyword or a string. Returns the Common Lisp object with the actual data of the parameter."
;;   (get-parameter-data (get-data (get-parameter bank key))))

;; (defmethod reset-parameter-data-to-default ((bank parameter-bank-class) key)
;;   "Sets the default data of a parameter as its actual data. KEY can be of type KEYWORD or STRING."
;;   (reset-data (get-parameter bank key)))


;; (defparameter *bank* (make-instance 'parameter-bank-class)
;;   "Globally used parameter bank, accessible through the exported functions of this package.")





;; (defun read-parameters (&optional (filepath "parameter-bank/parameters.lisp"))
;;   "Reads global parameter bank from file, overwriting existing parameters, deleting parameters that are not in the file."
;;   (read-bank-from-file *bank* filepath))

;; (defun write-parameters (&optional (filepath "parameter-bank/parameters.lisp"))
;;   "Writes global parameter bank to file. Existing file will be superseded."
;;   (write-bank-to-file *bank* filepath))

;; (defun defp (key data default short-doc long-doc)
;;   "Creates a new parameter in the global parameter bank. If the parameter already exists, it will be updated. KEY can be of type KEYWORD or STRING. DATA and DEFAULT can be any Common Lisp object that is supported by the PARAMETER-DATA class (NUMBER, STRING, (SIMPLE-VECTOR 3)). SHORT-DOC and LONG-DOC are strings for documentation purposes."
;;   (add-parameter *bank* key data default short-doc long-doc))

;; (defun setp (key data)
;;   "Updates an existing parameter in the global parameter bank. KEY can be of type KEYWORD or STRING. DATA can be of any type that is supported by the PARAMETER-DATA class (see docstring of `DEFP' for a list of supported types)."
;;   (update-parameter *bank* key data))

;; (defun getp (key)
;;   "Returns the Common Lisp object that represents the actual data of a parameter."
;;   (get-parameter-value *bank* key))

;; (defun resp (key)
;;   "Resets the data of a parameter in the global bank to its default."
;;   (reset-parameter-data-to-default *bank* key))


;; TODO condition handling:
;;
;; - error binding within the public functions, maybe with a key parameter to select the kind of
;;   handling:
;;   - wrong type: coerce, if not possible, set to default value, if not possible, invent something, ask user?
;;   - out of limits: set to closest limit, ignore restriction, ask user



;; TODO proper testing

;; testing

;; (defp :p1 0.0 0.0 "nix" "nixnix")
;; (defp :px 0.0 0.0 "nix" "nixnix")

;; (defp :p2 1.0 1.0 "nix1" "nixnix1")

;; (setp :p1 15.0)

;; (resp :p1)

;; (defp :ts "hi" "[default]" "test-string" "testing a string")

;; (defp :wrong 'd 'd "Symbol" "unsupported symbol")

;; (defp 27 30 25 "Number" "unsupported key")

;; (defp :26 25 25 "Number" "unsupported key")

;; (write-parameters)

;; (read-parameters)

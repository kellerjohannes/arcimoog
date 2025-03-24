(in-package :arcimoog.utilities)

;;; Macros as shorthands for registering parameters and MIDI callbacks.

(defmacro register-constant (name value)
  `(am-par:register-scalar ,name ,value ,value ,value nil))

(defmacro register-cv (name audio-output-channel)
  `(progn
     (am-par:register-scalar ,name -1.0 -1.0 1.0
                             (list (lambda (name value)
                                     ,(format nil "Send OSC message to audio output channel ~a."
                                              audio-output-channel)
                                     (declare (ignore name))
                                     (am-osc:send ,audio-output-channel (coerce value 'float)))))
     (am-par:register-hook ,name (lambda (name value)
                                   "Print the updated value to the REPL."
                                   (format t "~&~a updated to ~a.~%" name value)))))

(defmacro register-toggle-dial (controller channel doc target-parameter)
  `(am-midi:register-callback ,controller ,channel
                              (lambda (value)
                                ,doc
                                (cond ((< value 64)
                                       (am-par:set-scalar ,target-parameter
                                                          (am-par:get-scalar :toggle-off-value)))
                                      ((> value 64)
                                       (am-par:set-scalar ,target-parameter
                                                          (am-par:get-scalar :toggle-on-value)))))))

(defmacro register-precision-dial (controller
                                   channel-a
                                   channel-b
                                   channel-c
                                   channel-d
                                   doc
                                   target-parameter)
  "CONTROLLER represents the Faderfox Setup (0-based). The four CHANNEL arguments are Faderfox controller channels (0-based). They can be left NIL, in which case that precision level will be omitted. A stands for the lowest precision, D for the highest. DOC is a string that is used to generate a docstring for the callback function. TARGET-PARAMETER is a keyword for an existing Arcimoog parameter (see ARCIMOOG.PARAMETER package)."
  `(progn
     ,@(mapcan (lambda (channel precision)
                 (when channel
                   `((am-midi:register-callback
                      ,controller ,channel
                      (lambda (value)
                        ,(format nil "~a Precision level: ~a."
                                 doc
                                 precision)
                        (am-par:inc-scalar ,target-parameter
                                           (* (am-par:get-scalar
                                               ,(alexandria:make-keyword
                                                 (format nil "PRECISION-FACTOR-~a" precision)))
                                              (- value 64))))))))
               (list channel-a channel-b channel-c channel-d)
               (list 'low 'medium 'high 'extreme))))


;; Probably obsolete

;; (defun reduce-equal-keyword-list (keyword-list)
;;   "Expects a list of identical keywords, returns this keyword."
;;   (if (and (listp keyword-list)
;;            (plusp (length keyword-list)))
;;       (let ((simple-list (remove-duplicates keyword-list)))
;;         (cond ((= 1 (length simple-list))
;;                (cond ((keywordp (car simple-list))
;;                       (car simple-list))
;;                      (t (log:warn "The simplified KEYWORD-LIST" keyword-list "doesn't consist of a keyword and is therefore ignored. This might produce unwanted results.")
;;                         nil)))
;;               (t (log:warn "The KEYWORD-LIST" keyword-list "doesn't consist of exclusively identical keywords and will therefore be ignored. This might produce unwanted results.")
;;                  nil)))
;;       (progn
;;         (log:warn "The KEYWORD-LIST" keyword-list "doesn't contain any elements. NIL will be returned.")
;;         nil)))


;; (defun shrink (number)
;;   "Reduces positive integers / increases negative integers by 1."
;;   (cond ((zerop number) number)
;;         ((plusp number) (1- number))
;;         (t (1+ number))))

;; (defun coerce-vector (vec type)
;;   (declare (type simple-vector vec))
;;   (let ((result (make-array (length vec) :element-type type)))
;;     (loop for element across vec
;;           for i from 0 do
;;           (setf (aref result i) (coerce element type)))
;;     result))

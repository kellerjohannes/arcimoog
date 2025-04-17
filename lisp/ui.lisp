(in-package :arcimoog.ui)

(defparameter *parameter-hooks* (make-hash-table))
(defparameter *parameter-values* (make-hash-table))

(defun register-value-hook (name hook-function)
  (let ((hooks (gethash name *parameter-hooks*)))
    (if (listp hooks)
        (push hook-function (gethash name *parameter-hooks*))
        (setf (gethash name *parameter-hooks*) (list hook-function))))
  (funcall hook-function (gethash name *parameter-values*)))

(defun update-value (name new-value)
  (setf (gethash name *parameter-values*) new-value)
  (dolist (callback-function (gethash name *parameter-hooks*))
    (when new-value
      (funcall callback-function new-value))))

(defmacro intern-string (format-string &rest data)
  `(intern (string-upcase (format nil ,format-string ,@data))))

(defun build-cv-meters (parent)
  (macrolet ((meter-row (parent name)
               (let ((row-container (gensym))
                     (visual-container (gensym))
                     (meter-body (gensym))
                     (meter-label (gensym)))
                 `(let* ((,row-container (clog:create-div ,parent :class "meter-row"))
                         (,visual-container (clog:create-div ,row-container
                                                             :class "meter-visual-container"))
                         (,meter-body (clog:create-div ,visual-container
                                                       :class "meter-visual-filler"))
                         (,meter-label (clog:create-div ,row-container :class "meter-label")))
                    (register-value-hook
                     ,name (lambda (data) (setf (clog:text ,meter-label)
                                                (format nil "~a ~a" ,(symbol-name name) data))))
                    (register-value-hook
                     ,name (lambda (data) (setf (clog:width ,meter-body) (* (+ data 1) 100)))))))
             (generate-rows (prefixes index)
               `(progn ,@(mapcar (lambda (prefix)
                                   `(meter-row tile-container
                                               ,(alexandria:make-keyword (format nil "~a~a"
                                                                                 prefix
                                                                                 index))))
                                 prefixes))))
    (let ((tile-container (clog:create-div parent :class "tile-title" :content "CV levels")))
      (generate-rows (vco vcf res vca gate) 1)
      (generate-rows (vco vcf res vca gate) 2)
      (generate-rows (vco vcf res vca gate) 3))))

(defun build-ui (parent)
  (clog:with-clog-create parent
      (div (:content "Arcimoog Internals" :class "main-title")
           (div (:bind cv-tile :class "tile")))
    (build-cv-meters cv-tile)))

(defun on-main (body)
  (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "Arcimoog Main Parameters")
  (let ((main-container (create-div body :class "main-container")))
    (build-ui main-container)))

(defun init ()
  (initialize #'on-main
              :host "127.0.0.1"
              :port 8080
              :static-root (merge-pathnames "clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser))

(defun reset ()
  (setf *parameter-hooks* (make-hash-table))
  (shutdown)
  (init))

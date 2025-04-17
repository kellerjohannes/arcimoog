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


(defun build-cv-meters (parent)
  (macrolet ((meter-row (name)
               `(div (:class "meter-row")
                     (div (:class "meter-visual-container")
                          (div (:bind ,(format nil "meter-~a" name) :class "meter-visual-filler")))
                     (div (:bind ,(format nil "label-~a" name) :class "meter-label")))))
    (clog:with-clog-create parent
        (div (:content "CV levels" :class "tile-title")
             (meter-row vco1)
             ;; (div (:class "meter-row")
             ;;      (div (:class "meter-visual-container")
             ;;           (div (:bind meter-vco1 :class "meter-visual-filler")))
             ;;      (div (:bind label-vco1 :class "meter-label")))

             )
      (register-value-hook :vco1 (lambda (data) (setf (clog:text label-vco1) (write-to-string data))))
      (register-value-hook :vco1 (lambda (data) (setf (clog:width meter-vco1) (* (+ data 1) 100)))))))


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

(in-package :arcimoog.ui)


;;; Literal concept, obsolete

;; (defparameter *meter-levels* (make-array 16 :initial-element 0))
;; (defparameter *meter-instances* (make-array 16 :initial-element nil))

;; (defun create-output-meters (element)
;;   (let ((container (create-div element :class "meter-container")))
;;     (dotimes (meter-id 16)
;;       (setf (aref *meter-instances* meter-id)
;;             (create-div container :class "meter-core"))))
;;   (update-meters))

;; (defun update-meter (meter-id)
;;   (setf (style (aref *meter-instances* meter-id) "width")
;;         (format nil "~a" (* 200 (+ 1 (aref *meter-levels* meter-id))))))

;; (defun update-meters ()
;;   (dotimes (meter-id 16)
;;     (update-meter meter-id)))

;; (defun change-meter-level (meter-id new-value)
;;   (setf (aref *meter-levels* meter-id) new-value)
;;   (update-meter meter-id))



;;; Object oriented concept, obsolete


;; (defclass tile ()
;;   ((title :initform "" :initarg :title :accessor title)))

;; (defclass tile-cvs (tile)
;;   ((meters :initform nil :accessor meters)))



;; (defgeneric build-clog-elements (tile clog-parent)
;;   (:method ((tile tile) clog-parent)
;;     (format t "~&build basic tile")
;;     (setf (clog-element tile) (clog:create-div clog-parent :class "tile"))
;;     (format t "~&tile-div created")
;;     (clog:create-div (clog-element tile) :class "tile-title" :content (title tile))
;;     (format t "~&div for tile title created"))
;;   (:method :after ((tile tile-cvs) clog-parent)
;;     (format t "~&build cvs-tile")
;;     (setf (clog-meter-container tile) (clog:create-div clog-parent :class "meter-container"))
;;     (dolist (meter (meters tile))
;;       (setf (cv-meter-clog-element meter) (create-div (clog-meter-container tile) :class "meter"))
;;       (setf (style (cv-meter-clog-element meter) "width")
;;             (format nil "~a" (cv-meter-value meter))))))

;; (defmethod initialize-instance :after ((tile tile) &key clog-parent)
;;   (build-clog-elements tile clog-parent))

;; (defmethod initialize-instance :after ((tile tile-cvs) &key clog-parent)
;;   (build-clog-elements tile clog-parent))




;; (defparameter *tiles* (make-hash-table))

;; (defun register-cvs-tile (name)
;;   (setf (gethash name *tiles*) (make-instance 'tile-cvs :title "Control Voltages")))

;; (defun build-ui (parent)
;;   (maphash (lambda (key value)
;;              (declare (ignore key))
;;              (build-clog-elements value parent))
;;            *tiles*))



;;; Node concept, probably also obsolete

;; (defparameter *nodes* (make-hash-table))

;; (defun add-node (name parent &rest alist-attributes)
;;   "NAME and PARENT must be keywords."
;;   (setf (gethash name *nodes*) (append (list :parent parent) alist-attributes)))

;; (add-node :cv-meter-tile :origin :type :tile-container :title "CV meters")

;; (add-node :cv-meter-row-1 :cv-meter-tile :type :container)

;; (add-node :cv-meter-row-1 :cv-meter-table )



;;; Clog stuff


;; (defparameter *test* nil)

;; (defparameter *test-test* nil)

;; (defun change-all-instances ()
;;   (dolist (body *test*)
;;     (if (clog:validp body)
;;         (setf (clog:color body) :green)
;;         (format t "~&Connection lost."))))

;; (defun cleanup ()
;;   (setf *test* (mapcan (lambda (body) (when (clog:validp body) (list body))) *test*)))

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
    (funcall callback-function new-value)))


(update-value :test-value "one")
(update-value :vco1 0.0)

(defun build-cv-meters (parent)
  (clog:with-clog-create parent
      (div (:content "CV levels" :class "tile-title")
           (div (:class "meter-row")
                (div (:class "meter-visual-container")
                     (div (:bind meter-vco1 :class "meter-visual-filler")))
                (div (:bind label-vco1 :class "meter-label"))))
    (register-value-hook :vco1 (lambda (data) (setf (clog:text label-vco1) (write-to-string data))))
    (register-value-hook :vco1 (lambda (data) (setf (clog:width meter-vco1) (* (+ data 1) 100))))))


(defun build-ui (parent)
  (clog:with-clog-create parent
      (div (:content "Arcimoog Internals" :class "main-title")
           (div (:bind cv-tile :class "tile"))
           (div (:content "[After the tiles.]")))
    (build-cv-meters cv-tile)
    ))

(defun build-ui (parent)
  (let* ((container (create-div parent))
         (row-1 (create-div container :content "Div test 2."))
         (row-2 (create-div container :content "2nd row. [2]"))
         (test-div (create-div container :content "[]")))
    (register-value-hook :test-value (lambda (data)
                                       (setf (clog:text test-div) data)))))

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

(in-package :arcimoog.ui)

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

(defclass tile ()
  ((clog-element :accessor clog-element)
   (title :initform "" :initarg :title :accessor title)))

(defstruct cv-meter name value clog-element)

(defclass tile-cvs (tile)
  ((clog-meter-container :accessor clog-meter-container)
   (meters :initform nil :accessor meters)))



(defgeneric build-clog-elements (tile clog-parent)
  (:method ((tile tile) clog-parent)
    (format t "~&build basic tile")
    (setf (clog-element tile) (clog:create-div clog-parent :class "tile"))
    (format t "~&tile-div created")
    (clog:create-div (clog-element tile) :class "tile-title" :content (title tile))
    (format t "~&div for tile title created"))
  (:method :after ((tile tile-cvs) clog-parent)
    (format t "~&build cvs-tile")
    (setf (clog-meter-container tile) (clog:create-div clog-parent :class "meter-container"))
    (dolist (meter (meters tile))
      (setf (cv-meter-clog-element meter) (create-div (clog-meter-container tile) :class "meter"))
      (setf (style (cv-meter-clog-element meter) "width")
            (format nil "~a" (cv-meter-value meter))))))


(defmethod initialize-instance :after ((tile tile) &key clog-parent)
  (build-clog-elements tile clog-parent))

(defmethod initialize-instance :after ((tile tile-cvs) &key clog-parent)
  (build-clog-elements tile clog-parent))




(defparameter *tiles* (make-hash-table))

(defun register-cvs-tile (name)
  (setf (gethash name *tiles*) (make-instance 'tile-cvs :title "Control Voltages")))

(defun build-ui (parent)
  (maphash (lambda (key value)
             (declare (ignore key))
             (build-clog-elements value parent))
           *tiles*))



;;; Clog stuff

(defun on-main (body)
  (format t "~&Rebuilding page")
  (create-div body :content "Test")
  (load-css (html-document body) "styles.css")
  (format t "~&css loaded")
  (setf (title (html-document body)) "Arcimoog Main Parameters")
  (let ((main-container (create-div body :class "main-container")))
    (create-div main-container :content "Hi There")
    (format t "~&main-container created: ~a" main-container)
    (build-ui main-container)))

(defun init ()
  (initialize 'on-main
              :host "127.0.0.1"
              :port 8080
              :static-root (merge-pathnames "clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser))

(defun reset ()
  (shutdown)
  (init))

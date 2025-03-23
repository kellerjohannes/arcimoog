(in-package :arcimoog.ui)

(defparameter *meter-levels* (make-array 16 :initial-element 0))
(defparameter *meter-instances* (make-array 16 :initial-element nil))

(defun create-output-meters (element)
  (let ((container (create-div element :class "meter-container")))
    (dotimes (meter-id 16)
      (setf (aref *meter-instances* meter-id)
            (create-div container :class "meter-core"))))
  (update-meters))

(defun update-meter (meter-id)
  (setf (style (aref *meter-instances* meter-id) "width")
        (format nil "~a" (* 200 (+ 1 (aref *meter-levels* meter-id))))))

(defun update-meters ()
  (dotimes (meter-id 16)
    (update-meter meter-id)))

(defun change-meter-level (meter-id new-value)
  (setf (aref *meter-levels* meter-id) new-value)
  (update-meter meter-id))

(defun on-main (body)
  ;;   (create-child (head-element body) "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
  ;; <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
  ;; <link href=\"https://fonts.googleapis.com/css2?family=Red+Hat+Mono:ital,wght@0,300..700;1,300..700&display=swap\" rel=\"stylesheet\">")
  (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "Arcimoog Main Parameters")
  (let ((container (create-div body :class "main-container")))
    (create-section container :h1 :content "Arcimoog")
    (create-output-meters container)))

(defun init ()
  (initialize 'on-main
              :static-root (merge-pathnames "clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser))

(defun reset ()
  (shutdown)
  (start-ui))

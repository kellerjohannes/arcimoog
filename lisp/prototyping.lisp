(in-package :arcimoog)

(ql:quickload :clog)

(defun hi ()
  (format t "~&hi."))


(defun on-new-window (obj)
  (clog:load-css (clog:html-document obj) "styles.css")
  (clog:create-section obj :h1 :content "Hi.")
  (setf (clog:background-color (clog:body-element (clog:html-document obj))) "black")
  (setf (clog:color (clog:body-element (clog:html-document obj))) "white")
  (let ((slider-frame (clog:create-div obj :class "slider-frame")))
    (let ((slider (clog:create-div slider-frame :class "slider-body")))
      ;;(setf (clog:draggablep slider) t)
      ;;(clog:set-margin-side slider :top "25px")
      (clog:set-on-mouse-down slider (lambda (obj data) (format t "~&~a~%"
                                                                (parse-integer (clog:margin obj)
                                                                               :junk-allowed t))
                                       (format t "~a~%" data)))
      ;;(clog:set-on-drag-)
      ;;(clog:set-on-drag)
      )))



(defun start-ui ()
  (clog:initialize 'on-new-window
                   :static-root (merge-pathnames "clog/static-files/"
                                                 (asdf/system:system-source-directory :arcimoog))
                   )
  (clog:open-browser))

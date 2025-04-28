(in-package :arcimoog.history-tracker)

(defparameter *trackers* (make-hash-table))

(defun get-tracker (name)
  (gethash name *trackers*))

(defun register-tracker (name)
  "Creates new tracker. If it already exists, it clears the tracker. This function is probably
unnecessary, since it is possible to just use ADD-DATA-POINT without registering a tracker
first. ADD-DATA-POINT will create non-existing trackers."
  (setf (gethash name *trackers*) nil))

(defun add-data-point (name timecode value)
  (push (cons timecode value) (gethash name *trackers*)))

(defun loop-over-history (name fun)
  (dolist (data-point (reverse (gethash name *trackers*)))
    (funcall fun (car data-point) (cdr data-point))))

(defun dump-list (name &optional (time-transformer (lambda (x) x)) (vc-transformer (lambda (x) x)))
  (mapcan (lambda (data-point)
            (list (funcall time-transformer (car data-point))
                  (funcall vc-transformer (cdr data-point))))
          (reverse (gethash name *trackers*))))

(defun add-points (dump)
  (loop for (time1 cv1 time2 cv2) on dump by #'cddr
        nconc (list time1 cv1 time2 cv1)))

(defun print-all (name &optional (output-stream t))
  (loop-over-history name (lambda (time val)
                            (format output-stream "~&~a, ~a" time val))))

(defun get-latest-data-point (name)
  (let ((data (first (gethash name *trackers*))))
    (values (car data) (cdr data))))

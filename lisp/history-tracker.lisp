(in-package :arcimoog.history-tracker)

(defparameter *trackers* (make-hash-table))

(defclass tracker ()
  ((raw-data :initform nil :accessor raw-data)
   (gl-data :initform nil :accessor gl-data)
   (updatedp :initform nil :accessor updatedp)
   (gl-time-factor :initform (/ 1 (coerce (incudine:rt-sample-rate) 'single-float))
                   :accessor gl-time-factor)
   (gl-value-factor :initform 1 :accessor gl-value-factor)))


(defun register-tracker (name)
  "Creates new tracker. If it already exists, it clears the tracker."
  (setf (gethash name *trackers*) (make-instance 'tracker)))

(defun get-tracker (name)
  "Returns an instance of the tracker class."
  (unless (gethash name *trackers*)
    (register-tracker name))
  (gethash name *trackers*))

(defun add-data-point (name timecode value)
  (let ((tracker (get-tracker name)))
    (push (cons timecode value) (raw-data tracker))
    ;; Push a pair of coordinates to finish the horizontal line up until this moment.
    (push (second (gl-data tracker)) (gl-data tracker))
    (push (coerce (* timecode (gl-time-factor tracker)) 'single-float) (gl-data tracker))
    ;; Push a pair of coordinates representing the current value.
    (push (coerce (* value (gl-value-factor tracker)) 'single-float) (gl-data tracker))
    (push (coerce (* timecode (gl-time-factor tracker)) 'single-float) (gl-data tracker))
    (setf (updatedp tracker) t)))

(defun loop-over-history (name fun)
  (dolist (data-point (reverse (raw-data (get-tracker name))))
    (funcall fun (car data-point) (cdr data-point))))

(defun dump-list (name &optional (time-transformer (lambda (x) x)) (vc-transformer (lambda (x) x)))
  "Returns the raw data list, with possible transformations."
  (mapcan (lambda (data-point)
            (list (funcall time-transformer (car data-point))
                  (funcall vc-transformer (cdr data-point))))
          (reverse (raw-data (get-tracker name)))))

(defun dump-gl-list (name)
  "Returns the data optimised for OpenGL rendering."
  (append (list (expt 2 31) (second (gl-data (get-tracker name))))
          (gl-data (get-tracker name))))

(defun length-gl-data (name)
  (length (gl-data (get-tracker name))))

(defun update-data-required-p (name)
  (updatedp (get-tracker name)))

(defun data-updated (name)
  (setf (updatedp (get-tracker name)) nil))

(defun add-points (dump)
  (loop for (time1 cv1 time2 cv2) on dump by #'cddr
        nconc (list time1 cv1 time2 cv1)))

(defun print-all (name &optional (output-stream t))
  (loop-over-history name (lambda (time val)
                            (format output-stream "~&~a, ~a" time val))))

(defun get-latest-data-point (name)
  "Latest data point of the raw data."
  (let ((data (first (raw-data (get-tracker name)))))
    (values (car data) (cdr data))))

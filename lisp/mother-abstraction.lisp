(in-package :arcimoog.mothers)

(defparameter *cv-1/1* -0.8)

(defclass mother ()
  ((pitch-ratio :initform 1/1 :accessor pitch)
   (natura :initform 4 :accessor natura)
   (cv-offset :initform 0 :accessor cv-offset)
   (cv-factor :initform 0.2 :accessor cv-factor)
   (vco-name :initarg :vco-name :reader vco-name)
   (vcf-name :initarg :vcf-name :reader vcf-name)
   (res-name :initarg :res-name :reader res-name)
   (vca-name :initarg :vca-name :reader vca-name)
   (gate-name :initarg :gate-name :reader gate-name)))

(defmethod ratio-to-cv-relative ((instance mother) ratio)
  (* (cv-factor instance) (/ (log ratio) (log 2/1))))

(defmethod ratio-to-cv-absolute ((instance mother) ratio)
  (+ *cv-1/1* (cv-offset instance) (ratio-to-cv-relative instance ratio)))

(defmethod update-cvs ((instance mother))
  "To be called whenever any of the slots of INSTANCE have been changed. This method updates CV relevant parameters. It does not change any slots of INSTANCE."
  (am-par:set-scalar (vco-name instance) (ratio-to-cv-absolute instance (pitch instance)))
  ;; TODO update scalars for vcf, res, vca. This is the secret sauce for the sound design aspects
  ;; beyond pitch definitions.
  )

(defmethod set-natura ((instance mother) new-natura)
  ;; TODO check for valid natura values
  (setf (natura instance) new-natura)
  (update-cvs instance))

(defmethod modify-natura ((instance mother) natura-delta)
  (set-natura instance (+ (natura instance) natura-delta)))

(defmethod set-cv-offset ((instance mother) new-offset)
  (setf (cv-offset instance) new-offset)
  (update-cvs instance))

(defmethod set-cv-factor ((instance mother) new-factor)
  (setf (cv-factor instance) new-factor)
  (update-cvs instance))

(defmethod set-pitch ((instance mother) new-ratio)
  (setf (pitch instance) new-ratio)
  (update-cvs instance))

(defmethod modify-pitch ((instance mother) interval-ratio)
  (set-pitch instance (* (pitch instance) interval-ratio)))

(defmethod modify ((instance mother) interval-ratio natura-delta)
  (modify-pitch instance interval-ratio)
  (modify-natura instance natura-delta))


;; Public functions

(defun modify-local-origin (name pitch-delta)
  (let ((mother (get-mother name)))
    (when mother (set-cv-offset mother (+ (cv-offset mother) pitch-delta)))))

(defun modify-local-stretch (name pitch-stretch)
  (let ((mother (get-mother name)))
    (when mother (set-cv-factor mother pitch-stretch))))

(defun modify-sound (name interval natura)
  (modify (get-mother name) interval natura))





(defparameter *mothers* (make-hash-table))
(defparameter *mother-tuning-path* "mother-tuning.lisp")

(defun register-mother (name vco vcf res vca gate)
  (setf (gethash name *mothers*) (make-instance 'mother
                                                :vco-name vco
                                                :vcf-name vcf
                                                :res-name res
                                                :vca-name vca
                                                :gate-name gate)))

(defun get-mother (name)
  (gethash name *mothers*))

(defun apply-to-all-mothers (fun)
  (maphash (lambda (mother-name mother) (funcall fun mother-name mother)) *mothers*))

(defun update-all-mothers ()
  (apply-to-all-mothers (lambda (name instance) (declare (ignore name)) (update-cvs instance))))



(defun set-cv-1/1 (new-cv-1/1)
  (setf *cv-1/1* new-cv-1/1)
  (update-all-mothers))

(defun modify-cv-1/1 (cv-delta)
  (set-cv-1/1 (+ *cv-1/1* cv-delta)))

(defparameter *selected-mother* nil)

(defun select-mother (name)
  (when (get-mother name)
    (setf *selected-mother* name)))





(defun create-tuning-data-expression ()
  (loop for mother-name being the hash-keys of *mothers*
          using (hash-value mother-instance)
        collect `(,mother-name (:offset ,(cv-offset mother-instance)
                                :factor ,(cv-factor mother-instance)))))

(defun write-mother-tunings ()
  (with-open-file (file-stream *mother-tuning-path*
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (write (create-tuning-data-expression) :stream file-stream)))

(defun parse-tuning-data-expression (data)
  (dolist (mother-data data)
    (let ((mother-name (first mother-data))
          (cv-offset (getf (second mother-data) :offset))
          (cv-factor (getf (second mother-data) :factor)))
      (set-cv-offset (get-mother mother-name) cv-offset)
      (set-cv-factor (get-mother mother-name) cv-factor))))

(defun read-mother-tunings ()
  (with-open-file (file-stream *mother-tuning-path*)
    (parse-tuning-data-expression (read file-stream))))

(defun tune-offset-selected (offset-delta)
  (let ((mother (get-mother *selected-mother*)))
    (set-cv-offset mother (+ (cv-offset mother) offset-delta))
    (write-mother-tunings)))

(defun tune-factor-selected (factor-delta)
  (let ((mother (get-mother *selected-mother*)))
    (set-cv-factor mother (+ (cv-factor mother) factor-delta))
    (write-mother-tunings)))

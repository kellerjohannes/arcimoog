(in-package :arcimoog.mothers)

(defparameter *cv-1/1* -0.8)

(defclass mother ()
  ((pitch-ratio :initform 1/1 :accessor pitch)
   (natura :initform 0 :accessor natura)
   (soundingp :initform nil :accessor soundingp)
   (cv-offset :initform 0 :accessor cv-offset)
   (cv-factor :initform 0.2 :accessor cv-factor)
   (vco-name :initarg :vco-name :reader vco-name)
   (vcf-name :initarg :vcf-name :reader vcf-name)
   (res-name :initarg :res-name :reader res-name)
   (vca-name :initarg :vca-name :reader vca-name)
   (gate-name :initarg :gate-name :reader gate-name)))

(defmethod ratio-to-cv-relative ((instance mother) ratio)
  (* 0.1 (cv-factor instance) (/ (log ratio) (log 2/1))))

(defmethod ratio-to-cv-absolute ((instance mother) ratio)
  (+ *cv-1/1* (cv-offset instance) (ratio-to-cv-relative instance ratio)))

(defmethod update-cvs ((instance mother))
  "To be called whenever any of the slots of INSTANCE have been changed. This method updates CV relevant parameters. It does not change any slots of INSTANCE."
  (am-par:set-scalar (vco-name instance) (ratio-to-cv-absolute instance (pitch instance)))
  ;; TODO update scalars for vcf, res, vca. This is the secret sauce for the sound design aspects
  ;; beyond pitch definitions.
  ;;
  ;; These are dummy values to test synths.
  (am-par:set-scalar (vcf-name instance) (+ 0.1 (* (natura instance) 0.1)))
  (am-par:set-scalar (res-name instance) -0.8)
  (am-par:set-scalar (vca-name instance) (if (soundingp instance) 0.7 -1.0))
  (am-par:set-scalar (gate-name instance) (if (soundingp instance) 1.0 -1.0))
  ;; Because high VCA values might cause bleeding when gate closed, VCA needs to be set to zero as
  ;; well.
  )

(defmethod set-cv-offset ((instance mother) new-offset)
  (setf (cv-offset instance) new-offset)
  (update-cvs instance))

(defmethod modify-cv-offset ((instance mother) offset-delta)
  (incf (cv-offset instance) offset-delta)
  (update-cvs instance))

(defmethod set-cv-factor ((instance mother) new-factor)
  (setf (cv-factor instance) new-factor)
  (update-cvs instance))

(defmethod modify-cv-factor ((instance mother) factor-delta)
  (incf (cv-factor instance) factor-delta)
  (update-cvs instance))

(defmethod set-gate ((instance mother) gate-state)
  (setf (soundingp instance) gate-state)
  (update-cvs instance))

(defmethod set-pitch ((instance mother) new-ratio)
  (setf (pitch instance) new-ratio)
  (update-cvs instance))

(defmethod modify-pitch ((instance mother) interval-ratio)
  (set-pitch instance (* (pitch instance) interval-ratio)))

(defmethod set-natura ((instance mother) new-natura)
  ;; TODO check for valid natura values
  (setf (natura instance) new-natura)
  (update-cvs instance))

(defmethod modify-natura ((instance mother) natura-delta)
  (set-natura instance (+ (natura instance) natura-delta)))

(defmethod set-pitch-and-natura ((instance mother) ratio natura)
  (set-pitch instance ratio)
  (set-natura instance natura))

(defmethod modify-pitch-and-natura ((instance mother) interval-ratio natura-delta)
  (modify-pitch instance interval-ratio)
  (modify-natura instance natura-delta))






;;; Handling the Mother database. Each instance of `MOTHER' represents a hardware or software
;;; synthesizer.

(defparameter *mothers* (make-hash-table))

(defparameter *mother-tuning-path*
  (merge-pathnames "mother-tuning.lisp" (asdf/system:system-source-directory :arcimoog)))

(defun register-mother (name vco vcf res vca gate)
  (setf (gethash name *mothers*) (make-instance 'mother
                                                :vco-name vco
                                                :vcf-name vcf
                                                :res-name res
                                                :vca-name vca
                                                :gate-name gate)))

(defun get-mother (name)
  ;; TODO Handle invalid names.
  (gethash name *mothers*))

(defun apply-to-all-mothers (fun)
  (maphash (lambda (mother-name mother) (funcall fun mother-name mother)) *mothers*))

(defun update-all-mothers ()
  (apply-to-all-mothers (lambda (name instance) (declare (ignore name)) (update-cvs instance))))

(defparameter *selected-mother* nil)


;;; Manage and securing tuning of mothers

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
    (write (create-tuning-data-expression) :stream file-stream))
  (format t "~&Tuning of all Mothers written to disk."))

(defun parse-tuning-data-expression (data)
  (dolist (mother-data data)
    (let ((mother-name (first mother-data))
          (cv-offset (getf (second mother-data) :offset))
          (cv-factor (getf (second mother-data) :factor)))
      (set-cv-offset (get-mother mother-name) cv-offset)
      (set-cv-factor (get-mother mother-name) cv-factor))))

(defun read-mother-tunings ()
  (with-open-file (file-stream *mother-tuning-path*)
    (parse-tuning-data-expression (read file-stream)))
  (format t "~&Tuning of all Mothers read from file."))




;;; Public functions

(defun select-mother (name)
  (setf *selected-mother* name))

(defun is-valid-mother-name (name)
  (gethash name *mothers*))


;; PITCH

(defun set-mother-pitch (name ratio)
  (set-pitch (get-mother name) ratio))

(defun modify-mother-pitch (name interval)
  (modify-pitch (get-mother name) interval))

(defun set-selected-pitch (ratio)
  (set-pitch (get-mother *selected-mother*) ratio))

(defun modify-selected-pitch (interval)
  (modify-pitch (get-mother *selected-mother*) interval))


;; NATURA

(defun set-mother-natura (name natura)
  (set-natura (get-mother name) natura))

(defun modify-mother-natura (name natura-delta)
  (modify-natura (get-mother name) natura-delta))

(defun set-selected-natura (natura)
  (set-natura (get-mother *selected-mother*) natura))

(defun modify-selected-natura (natura-delta)
  (set-natura (get-mother *selected-mother*) natura-delta))


;; PITCH and NATURA

(defun set-mother-pitch-and-natura (name ratio natura)
  (set-pitch-and-natura (get-mother name) ratio natura))

(defun modify-mother-pitch-and-natura (name interval natura-delta)
  (modify-pitch-and-natura (get-mother name) interval natura-delta))

(defun set-selected-pitch-and-natura (ratio natura)
  (set-mother-pitch-and-natura (get-mother *selected-mother*) ratio natura))

(defun modify-selected-pitch-and-natura (interval natura-delta)
  (modify-pitch-and-natura (get-mother *selected-mother*) interval natura-delta))


;; GATE

(defun mother-on (name)
  (set-gate (get-mother name) t))

(defun mother-off (name)
  (set-gate (get-mother name) nil))

(defun selected-on ()
  (let ((mother (get-mother *selected-mother*)))
    (set-gate mother 1)))

(defun selected-off ()
  (let ((mother (get-mother *selected-mother*)))
    (set-gate mother nil)))

(defun set-all-gates (gate-state)
  (apply-to-all-mothers (lambda (name instance)
                          (declare (ignore name))
                          (set-gate instance gate-state))))


;; Tuning (CV-OFFSET and CV-FACTOR)

(defun set-mother-cv-offset (name cv-offset)
  (set-cv-offset (get-mother name) cv-offset)
  (write-mother-tunings))

(defun modify-mother-cv-offset (name cv-offset-delta)
  (modify-cv-offset (get-mother name) cv-offset-delta)
  (write-mother-tunings))

(defun set-selected-cv-offset (cv-offset)
  (set-cv-offset (get-mother *selected-mother*) cv-offset)
  (write-mother-tunings))

(defun modify-selected-cv-offset (cv-offset-delta)
  (modify-cv-offset (get-mother *selected-mother*) cv-offset-delta)
  (write-mother-tunings))

(defun set-mother-cv-factor (name cv-factor)
  (set-cv-factor (get-mother name) cv-factor)
  (write-mother-tunings))

(defun modify-mother-cv-factor (name cv-factor-delta)
  (modify-cv-factor (get-mother name) cv-factor-delta)
  (write-mother-tunings))

(defun set-selected-cv-factor (cv-factor)
  (set-cv-factor (get-mother *selected-mother*) cv-factor)
  (write-mother-tunings))

(defun modify-selected-cv-factor (cv-factor-delta)
  (modify-cv-factor (get-mother *selected-mother*) cv-factor-delta)
  (write-mother-tunings))


;; Global reference pitch (CV value for pitch 1/1)

(defun set-cv-1/1 (new-cv-1/1)
  (setf *cv-1/1* new-cv-1/1)
  (update-all-mothers))

(defun get-cv-1/1 ()
  *cv-1/1*)

(defun modify-cv-1/1 (cv-delta)
  (set-cv-1/1 (+ *cv-1/1* cv-delta)))




;;; Mother state handling

(defparameter *snapshots* (make-hash-table))

(defun take-snapshot (snapshot-name)
  (let ((new-snapshot))
    (apply-to-all-mothers (lambda (mother-name mother-instance)
                            (push (list mother-name
                                        (pitch mother-instance)
                                        (natura mother-instance))
                                  new-snapshot)))
    (setf (gethash snapshot-name *snapshots*) new-snapshot)))


(defun read-snapshot (snapshot-name)
  (let ((snapshot (gethash snapshot-name *snapshots*)))
    (when snapshot
      (dolist (mother-state snapshot)
        (set-mother-pitch (first mother-state) (second mother-state))
        (set-mother-natura (first mother-state) (third mother-state))))))

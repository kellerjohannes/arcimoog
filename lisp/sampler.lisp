(in-package :arcimoog)



;;;; Testing sample playback, obsolete


;; (incudine:set-control 1 :rate 1)

;; (incudine:set-control 1 :loopp nil)


(vug:define-vug cv-generator (value)
  value)

(vug:dsp! cvs (v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
  (vug:out v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16))

(cvs 0.1 0.15 0.2 0.25 0.3 0.35 0.38 0.4 0.42 0.44 0.48 0.5 0.55 0.6 0.65 0.7)


(incudine:stop 1)
(setf (incudine:control-value 1 :v1) 0.1)

(defun make-waver (init-value constant-delta)
  (let ((direction 1)
        (delta constant-delta)
        (value init-value))
    (lambda ()
      (let ((preview-value (+ value (* direction delta))))
        (when (or (> preview-value 1) (< preview-value 0))
          (setf direction (- direction)))
        (incf value (* direction delta)))
      value)))

(defmacro start-wavers (num)
  `(cvs ,@(loop for i from 0 below num collect
                `(funcall (aref *wavers* ,i)))))

(defparameter *wavers* (make-array 16))

(defun start-wave ()
  (loop for waver across *wavers*
        for i from 0
        for init-value from 0.1 by (/ 0.9 16) do
          (setf (aref *wavers* i) (make-waver init-value 0.01))))

(progn
  (incudine:stop 1)
  (start-wave)
  (start-wavers 16))

(defun make-v-id (num)
  (alexandria:make-keyword (format nil "V~a" num)))

(defun update-wavers ()
  (loop for i from 0 below 16 do
    (setf (incudine:control-value 1 (make-v-id (1+ i))) (funcall (aref *wavers* i)))))

(defun waver-update-loop ()
  (update-wavers)
  (incudine:at (+ (incudine:now) 2000) #'waver-update-loop))

(defun waver-update-loop ())

(defun access-cv (num)
  (incudine:control-value 1 (make-v-id num)))

(defun print-all-values ()
  (loop for i from 1 to 16 do
        (format t "~&V~a: ~a"
                i
                (access-cv i))))

(defmacro set-cv (index value)
  `(setf (incudine:control-value 1 (make-v-id ,index)) ,value))

(defun set-all-cvs (value)
  (loop for i from 1 to 16 do
        (set-cv i value)))


(vug:dsp! sample-player ((buffer incudine:buffer) rate start-position (loopp boolean))
  (vug:foreach-channel
    (vug:cout (vug:buffer-play buffer rate start-position loopp #'incudine:stop))))

(defclass sampler-class ()
  ((sample-bank :initform (make-hash-table) :accessor sample-bank)))

(defun load-sample-from-file (root-path sample-path)
  (let ((full-path (concatenate 'string root-path sample-path)))
    (unless (uiop:file-exists-p full-path)
      (error 'buffer-file-not-found :root-path root-path :sample-path sample-path))
    (let ((buf (incudine:buffer-load full-path)))
      (if buf buf (error 'buffer-file-not-loaded :root-path root-path :sample-path sample-path)))))

(defmethod load-samples ((sampler sampler-class) key-list root-path path-list)
  (loop for key in key-list
        for sample-path in path-list
        for slot-number from 1 do
          (setf (gethash key (sample-bank sampler))
                (list :slot slot-number
                      :buffer (handler-case (load-sample-from-file root-path sample-path)
                                (buffer-file-not-found () :undefined))))))

(defmethod get-buffer ((sampler sampler-class) key)
  (getf (gethash key (sample-bank sampler)) :buffer))

(defmethod get-slot-number ((sampler sampler-class) key)
  (getf (gethash key (sample-bank sampler)) :slot))

(defmethod start-sample ((sampler sampler-class) key &optional (rate 1))
  (let ((buf (get-buffer sampler key)))
    (when (incudine:buffer-p buf)
      (sample-player buf rate 0 nil :id (get-slot-number sampler key)))))

(defmethod stop-sample ((sampler sampler-class) key)
  (incudine:stop (get-slot-number sampler key)))


(defun make-sample-path-list (format-string counter-start counter-end)
  (loop for counter from counter-start to counter-end
        collect (format nil format-string counter)))

(defun make-sampler-key-list (format-string counter-start counter-end)
  (loop for counter from counter-start to counter-end
        collect (alexandria:make-keyword (string-upcase (format nil format-string counter)))))

(defparameter *sampler-arciorgano-mode1* (make-instance 'sampler-class))

(load-samples *sampler-arciorgano-mode1*
              (make-sampler-key-list "key-~a" 1 146)
              "/home/johannes/common-lisp/arciorgano-player/samples/mode1/mode1-export-wav-neumann/"
              (make-sample-path-list "mode1-~a.wav" 1 146))

(loop for key in (make-sampler-key-list "key-~a" 1 54) do
       (start-sample *sampler-arciorgano-mode1* key))

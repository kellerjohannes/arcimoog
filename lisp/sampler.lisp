(in-package :arcimoog)



;;;; Testing sample playback, obsolete


;; (incudine:set-control 1 :rate 1)

;; (incudine:set-control 1 :loopp nil)





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

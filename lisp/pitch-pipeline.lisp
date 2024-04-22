(in-package :arcimoog)

(defun keyon (oct letter &optional acc dot)
  (with-note-name-convention 'note-name-vicentino
    (start-sample *sampler-arciorgano-mode1*
                  (transform (nn letter acc oct dot) 'arciorgano-key-number))))

(defun keyoff (oct letter &optional acc dot)
  (with-note-name-convention 'note-name-vicentino
    (stop-sample *sampler-arciorgano-mode1*
                 (transform (nn letter acc oct dot) 'arciorgano-key-number))))

(defun plk (duration oct letter &optional acc dot)
  (keyon oct letter acc dot)
  (incudine:at (+ (incudine:now) duration) #'keyoff oct letter acc dot))

(defun play-mode (pitch-list duration)
  (when pitch-list
    (apply #'plk (* 1.15 duration) (first pitch-list))
    (incudine:at (+ (incudine:now) duration) #'play-mode (rest pitch-list) duration)))


(defparameter *modes*
  `(:diatonico-semplice (:primo ((1 :d)
                                 (1 :e)
                                 (1 :f)
                                 (1 :g)
                                 (1 :a)))))

(defun get-mode-pitch-list (genus number)
  (getf (getf *modes* genus) number))

(defparameter *current-mode-number* :primo)

(defparameter *current-genus* :diatonico-semplice)

(defun plm ()
  (play-mode (get-mode-pitch-list *current-genus* *current-mode-number*) 44100))


(defparameter *global-mode-player* nil)

(defun plmi ()
  (setf *global-mode-player* (cons nil (get-mode-pitch-list *current-genus* *current-mode-number*))))

(defun progress-global-mode-player ()
  (when (first *global-mode-player*) (apply #'keyoff (first *global-mode-player*)))
  (setf *global-mode-player* (rest *global-mode-player*))
  (unless (zerop (length *global-mode-player*))
    (apply #'keyon (first *global-mode-player*))))

(in-package :arcimoog)

(defparameter *midi-in* nil)
(defparameter *midi-responder* nil)




(defun midi-scale (value lower upper)
  (+ (* (/ value 127.0) (- upper lower)) lower))

(defun set-slot (slot-number value)
  (setf (aref *parameter-slots* slot-number) value))

(defun slot (slot-number)
  (aref *parameter-slots* slot-number))

(defun midi-responder (channel-raw controller-raw value-raw)
  (update *parameter-bank* channel-raw controller-raw value-raw)
  ;; (case (- channel-raw 176)
  ;;   (2 (case controller-raw
  ;;        (0 (incf (aref *global-projection-translation* 0) (* -0.005 (- 64.0 value-raw))))
  ;;        (1 (incf (aref *global-projection-translation* 1) (* -0.005 (- 64.0 value-raw))))
  ;;        (2 (incf *global-projection-scaling* (* -0.01 (- 64.0 value-raw))))
  ;;        (3 (reset-global-projection-parameters))
  ;;        (4 (setf (aref *global-background-color* 0) (midi-scale value-raw 0.0 1.0)))
  ;;        (5 (setf (aref *global-background-color* 1) (midi-scale value-raw 0.0 1.0)))
  ;;        (6 (setf (aref *global-background-color* 2) (midi-scale value-raw 0.0 1.0)))
  ;;        (50 (set-slot 0 (midi-scale value-raw 0.0 1.0)))
  ;;        (51 (set-slot 1 (midi-scale value-raw 0.0 1.0)))
  ;;        (52 (set-slot 2 (midi-scale value-raw 0.0 1.0)))
  ;;        (53 (set-slot 3 (midi-scale value-raw 0.0 1.0)))
  ;;        (54 (set-slot 4 (midi-scale value-raw 0.0 1.0)))
  ;;        (55 (set-slot 5 (midi-scale value-raw 0.0 1.0)))
  ;;        (56 (set-slot 6 (midi-scale value-raw 0.0 1.0)))
  ;;        (57 (set-slot 7 (midi-scale value-raw 0.0 1.0)))
  ;;        (58 (set-slot 8 (midi-scale value-raw 0.0 1.0)))
  ;;        (59 (set-slot 9 (midi-scale value-raw 0.0 1.0)))
  ;;        (60 (set-slot 10 (midi-scale value-raw 0.0 1.0)))
  ;;        (61 (set-slot 11 (midi-scale value-raw 0.0 1.0)))
  ;;        (62 (set-slot 12 (midi-scale value-raw 0.0 1.0)))
  ;;        (63 (set-slot 13 (midi-scale value-raw 0.0 1.0)))
  ;;        (64 (set-slot 14 (midi-scale value-raw 0.0 1.0)))
  ;;        (65 (set-slot 15 (midi-scale value-raw 0.0 1.0)))
  ;;        (otherwise (format t "~&Unknown Faderfox controller ~a in setup page 2." controller-raw))))
  ;;   (otherwise (format t "~&Unknown Faderfox setup page."))
  )



(defun init-faderfox-communication ()
  (when (eq (incudine:rt-status) :stopped)
    (incudine:rt-start))

  (if (eq (incudine:rt-status) :started)
      (log:info "Incudine is running in real time mode.")
      (log:warn "Incudine couldn't be started in real time mode."))

  (unless (eq (pm:initialize) :pm-no-error)
    (log:warn "There has been an error initializing PortMIDI."))

  (let ((faderfox-id (pm:get-device-id-by-name "Faderfox EC4 MIDI 1" :input)))
    (if (and (not *midi-in*) faderfox-id)
      (setf *midi-in* (pm:open faderfox-id))
      (log:warn "Faderfox interface not found. No MIDI input set up.")))

  (when *midi-in*
    (incudine:recv-start *midi-in*)
    (unless *midi-responder*
      (setf *midi-responder* (incudine:make-responder *midi-in* (lambda (a b c)
                                                                  (midi-responder a b c)))))))

(in-package :arcimoog)

(defparameter *midi-in* nil)
(defparameter *midi-responder* nil)



;;; Obsolete, this is all covered by the parameter bank implementation

;; (defun midi-scale (value lower upper)
;;   (+ (* (/ value 127.0) (- upper lower)) lower))

;; (defun set-slot (slot-number value)
;;   (setf (aref *parameter-slots* slot-number) value))

;; (defun slot (slot-number)
;;   (aref *parameter-slots* slot-number))


(defun midi-responder (channel-raw controller-raw value-raw)
  (update *parameter-bank* channel-raw controller-raw value-raw))

(defun incudine-real-time-p ()
  (let ((status (incudine:rt-status)))
    (cond ((eq status :started) t)
          ((eq status :stopped) nil)
          (t (log:warn "Incudine RT-STATUS '~a' is unknown and will be treated as :STOPPED."
                       status)))))

(defun start-incudine-rt ()
  (unless (incudine-real-time-p)
    (incudine:rt-start))
  (unless (incudine-real-time-p)
    (error 'incudine-is-not-in-rt)))

(defun init-port-midi ()
  (let ((status (pm:initialize)))
    (unless (eq status :pm-no-error)
      (error 'pm-error :pm-error-flag status))))

(defun find-faderfox-id (faderfox-name-string)
  (pm:print-devices-info :input)
  (let ((id (pm:get-device-id-by-name faderfox-name-string :input)))
    (if id id (error 'faderfox-id-not-found :faderfox-name faderfox-name-string))))

(defun init-faderfox-communication ()
  (restart-case (start-incudine-rt)
    (continue-without-rt () :report "Don't attempt to start real time." nil))
  (restart-case (init-port-midi)
    (continue-without-pm () :report "Don't use PortMidi from this point on." nil))
  (let ((id (restart-case (find-faderfox-id "Faderfox EC4 MIDI 2")
              (input-different-name (name)
                :report "Input custom Faderfox name string (in \"\")."
                :interactive (lambda () (get-faderfox-name-from-user))
                (find-faderfox-id name)))))
    (log:info "The Faderfox device ID is ~a." id))

  ;; * TODO actually open the port, once the ID is done.

  ;; (let ((faderfox-id (pm:get-device-id-by-name "Faderfox EC4 MIDI 1" :input)))
  ;;   (if (and (not *midi-in*) faderfox-id)
  ;;     (setf *midi-in* (pm:open faderfox-id))
  ;;     (log:warn "Faderfox interface not found. No MIDI input set up.")))

  (when *midi-in*
    (incudine:recv-start *midi-in*)
    (unless *midi-responder*
      (setf *midi-responder* (incudine:make-responder *midi-in* (lambda (a b c)
                                                                  (midi-responder a b c)))))))

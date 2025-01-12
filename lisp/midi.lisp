(in-package :arcimoog.midi-communication)

;; Storing callback functions for MIDI input, connecting MIDI controls to any place in the whole
;; system.

(defconstant +number-of-controllers+ 16
  "Number of used controller IDs. On the Faderfox EC4 this refers to the number of 'Setups', which is 16.")

(defconstant +controller-offset+ 176
  "Value of the first controller ID. On the Faderfox EC4 this is 176.")

(defconstant +number-of-channels+ 127
  "Number of channels in use. On the Faderfox EC4 this is 127 channels per 'Setup'.")

(defparameter *midi-callbacks* (make-array (list *number-of-controllers* *number-of-channels*)
                                           :initial-element nil)
  "Stores the callback function for each MIDI channel. Can be populated from anywhere.")

(defun valid-subscripts-p (controller channel)
  "Checks ranges."
  (and (>= controller 0)
       (< controller +number-of-controllers+)
       (>= channel 0)
       (< channel +number-of-channels+)))

(defun midi-responder (controller-raw channel value-raw)
  "Callback function for the MIDI listener. This function is called whenever there is incoming MIDI
data. The first and the second argument are used as array subscripts, the third one represents the
actual value."
  ;; (format t "~&~a, ~a, ~a" channel-raw controller-raw value-raw)
  (let ((controller (- controller-raw +controller-offset+)))
    (if (valid-subscripts-p controller channel)
        (let ((callback-fun (aref *midi-callbacks* controller channel)))
          (if callback-fun
              (funcall callback-fun value-raw)
              (log:warn "No callback function defined for setup ~a, channel ~a."
                        controller channel)))
        (log:warn "No MIDI slot defined for setup ~a, channel ~a." controller channel))))

(defun register-callback (controller channel callback-fun)
  "Exported function to be used to store a callback function for a controller channel."
  (cond ((valid-subscripts-p controller channel)
         (when (functionp (aref *midi-callbacks* controller channel))
           (log:info "Redefining callback function for controller ~a, channel ~a."
                     controller channel))
         (setf (aref *midi-callbacks* controller channel) callback-fun))
        (t (error 'acond:midi-subscripts-out-of-range))))



;; Handling the MIDI connection.
;; TODO: from here, add doc strings.

(defparameter *midi-in* nil)
(defparameter *midi-responder* nil)

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
    (error 'acond:incudine-is-not-in-rt)))

(defun init-port-midi ()
  (let ((status (pm:initialize)))
    (unless (eq status :pm-no-error)
      (error 'acond:pm-error :pm-error-flag status))))

(defun find-faderfox-id (faderfox-name-string)
  (pm:print-devices-info :input)
  (let ((id (pm:get-device-id-by-name faderfox-name-string :input)))
    (if id id (error 'acond:faderfox-id-not-found
                     :faderfox-name faderfox-name-string))))

(defun init-faderfox-communication ()
  (restart-case (init-port-midi)
    (continue-without-pm () :report "Don't use PortMidi from this point on." nil))
  (let ((id (restart-case (find-faderfox-id "Faderfox EC4 MIDI 1")
              (ignore () (log:warn "No Faderfox interface found."))
              (input-different-name (name)
                :report "Input custom Faderfox name string (in \"\")."
                :interactive (lambda () (get-faderfox-name-from-user))
                (find-faderfox-id name)))))
    (log:info "The Faderfox device ID is ~a." id))

  (let ((faderfox-id (pm:get-device-id-by-name "Faderfox EC4 MIDI 1" :input)))
    (if *midi-in*
        (log:warn "MIDI-IN is already in use.")
        (if faderfox-id
            (setf *midi-in* (pm:open faderfox-id))
            (log:warn "Faderfox interface not found. No MIDI input set up."))))

  (when *midi-in*
    (incudine:recv-stop *midi-in*)
    (incudine:remove-all-responders *midi-in*)
    (setf *midi-responder*
          (incudine:make-responder *midi-in* (lambda (a b c) (midi-responder a b c))))
    (incudine:recv-start *midi-in*)
    (sleep 1)
    (if (eq (incudine:recv-status *midi-in*) :running)
      (log:info "MIDI listener is running.")
      (log:warn "MIDI listener is not running.")))

  (restart-case (start-incudine-rt)
    (continue-without-rt () :report "Don't attempt to start real time." nil)))

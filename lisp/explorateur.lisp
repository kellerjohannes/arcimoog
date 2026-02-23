(in-package :arcimoog.explorateur)

(defconstant +number-of-explorateur-channels+ 16)
(defconstant +number-of-explorateur-notes+ 128)

(defparameter *channel-headspaces*
  (make-array +number-of-explorateur-channels+ :initial-element 1.0))

(defparameter *channel-pressures*
  (make-array +number-of-explorateur-channels+ :initial-element 0.0))

(defparameter *explorateur-state*
  (make-array (list +number-of-explorateur-channels+ +number-of-explorateur-notes+)
              :initial-element 0.5))


(defun valid-channel-subscript-p (channel)
  (or (< -1 channel +number-of-explorateur-channels+)
      (error "Channel ~a invalid." channel)))

(defun valid-note-subscript-p (note)
  (or (< -1 note +number-of-explorateur-notes+)
      (error "Note ~a invalid." note)))

(defun valid-subscripts-p (channel note)
  (and (valid-channel-subscript-p channel)
       (valid-note-subscript-p note)))

(defun get-pressure (channel note)
  (when (valid-subscripts-p channel note)
    (aref *explorateur-state* channel note)))

(defun apply-global-pressures (channel)
  (when (valid-channel-subscript-p channel)
    (+ (aref *channel-pressures* channel)
       (aref *channel-headspaces* channel))))

(defun note-on (channel note)
  (when (valid-subscripts-p channel note)
    (setf (aref *explorateur-state* channel note) (apply-global-pressures channel))))

(defun note-off (channel note)
  (when (valid-subscripts-p channel note)
    (setf (aref *explorateur-state* channel note) 0)))

(defun midi-aftertouch-to-unit (midi-value)
  (- (/ midi-value 128.0) 0.5))

(defun update-aftertouch (channel note midi-aftertouch-value)
  (when (and (valid-subscripts-p channel note)
             (not (zerop (get-pressure channel note))))
    (incf (aref *explorateur-state* channel note) (midi-aftertouch-to-unit midi-aftertouch-value))))





;;; Sketches done at the Explorateur in Basel

(defparameter *explo-in* nil)
(defparameter *launchkey-in* nil)
(defparameter *seaboard-in* nil)
(defparameter *explo-out* nil)

(defun init-explo-midi ()
  ;; (incudine:remove-all-responders *explo-in*)
  ;; (incudine:remove-all-responders *launchkey-in*)
  ;; (incudine:remove-all-responders *seaboard-in*)
  (setf (incudine.util:logger-level) :info)
  (incudine:rt-start)
  (unless *explo-out*
    (setf *explo-out* (jackmidi:open :direction :output
                                     :port-name "Explorateur out")))
  (unless *explo-in*
    (setf *explo-in* (jackmidi:open :direction :input
                                    :port-name "Explorateur in")))
  (unless *launchkey-in*
    (setf *launchkey-in* (jackmidi:open :direction :input
                                       :port-name "Launchkey in")))
  (unless *seaboard-in*
    (setf *seaboard-in* (jackmidi:open :direction :input
                                       :port-name "Seaboard in")))

  (incudine:recv-start *explo-in*)
  (incudine:recv-start *launchkey-in*)
  (incudine:recv-start *seaboard-in*))



(defun play-explo (type d1 d2)
  (format t "~&Explo out: ~d ~d ~d~%" type d1 d2)
  (jackmidi:write-short *explo-out* (jackmidi:message type d1 d2) 3)
  )


(defun note-on (note &optional (channel 1) (velocity 64))
  "Channel 1-16 (not 0-15)."
  (play-explo (+ 143 channel) note velocity))

(defun note-off (note &optional (channel 1))
  "Channel 1-16 (not 0-15)."
  (play-explo (+ 127 channel) note 0))

(defun play-note (note duration-in-s &optional (channel 1) (velocity 64))
  (note-on note channel velocity)
  (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
               #'note-off note channel))

(defun aftertouch (note value &optional (channel 1))
  (play-explo (+ 159 channel) note value))

(defparameter *running* t)

(defun stop () (setf *running* nil))

(defun start () (setf *running* t))

(defun pulse (&optional (duration-in-s 1) (note 60) (channel 1) (velocity 90))
  (when *running*
    (note-on note channel velocity)
    (incudine:at (+ (incudine:now) (* duration-in-s 0.5 (incudine:rt-sample-rate)))
                 #'note-off note channel)
    (incudine:at (+ (incudine:now) duration-in-s (incudine:rt-sample-rate))
                 #'pulse duration-in-s note channel velocity)))

(defun cluster (duration-in-s &optional (channel 1) (start-note 0) (end-note 127))
  ;; (loop for note from start-note to end-note do
  ;;       (play-note note duration-in-s channel))
  (let ((data-on (coerce (loop for note from start-note to end-note
                               append (list (+ channel 143) note 64))
                         'jackmidi:data))
        (data-off (coerce (loop for note from start-note to end-note
                                append (list (+ channel 127) note 64))
                          'jackmidi:data)))
    (jackmidi:write *explo-out* data-on)
    (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
                 #'jackmidi:write *explo-out* data-off)))

(defun stutter (duration-in-s &optional (channel 1) (start-note 0) (end-note 127))
  (when *running*
    (cluster (* duration-in-s 0.5) channel start-note end-note)
    ;; (swipe start-note 1 start-note end-note 0.001 duration-in-s)
    (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
                 #'stutter duration-in-s channel start-note end-note)))


(defun killall ()
  (stop)
  (incudine:at (+ (incudine:now) 44100) #'start))


(defparameter *pipe-ranges* '((1 36 102 1 "Bourdon") ; Bourdon, 144
                              (2 92 120 3 "Flute") ; Flute, 145
                              (3 48 91 2 "Principal") ; Principal, 146
                              (4 48 91 8 "Trompette") ; Trompette, 147
                              (5 0 0) ; empty
                              (6 0 0) ; empty
                              (7 48 102 7 "Traverse") ; Traverse, 150
                              (8 0 0) ; empty
                              (9 59 119 6 "Tierce") ; Tierce, 152
                              (10 0 0) ; empty
                              (11 48 103 4 "Salicional") ; Salicional, 154
                              (12 48 91 5 "Gambe") ; Gambe, 155
                              (13 0 0) ; empty
                              (14 0 0) ; empty
                              (15 0 0) ; empty
                              (16 0 0) ; empty
                              )
  "MIDI channel, lowest pipe, highest pipe, module id, module name.")

(defun get-channel-by-module (module-id)
  (car (find module-id ))
  )

(defun switch-module ())

(defun swipe (current-note &optional (channel 1) (start-note 0) (end-note 127) (duration-in-s 0.1) (atomic-duration-in-s 0.3))
  (when *running*
    (when (<= start-note current-note (1+ end-note))
      (play-note current-note atomic-duration-in-s channel)
      (incudine:at (+ (incudine:now) (* duration-in-s (incudine:rt-sample-rate)))
                   #'swipe (1+ current-note) channel start-note end-note duration-in-s))))





(defparameter *midi-dump-test* nil)

(defun dump-responder (st d1 d2)
  (cond ((jackmidi:sysex-message-p st)
         (format t "EXPLO sysex: ~s~%"
                 (jackmidi:input-stream-sysex-octets
                  *explo-in*)))
        (t (format t "EXPLO: ~d ~d ~d~%" st d1 d2))))

(defun make-dump-test ()
  (when *explo-in*
    (setf *midi-dump-test* (incudine:make-responder *explo-in* #'dump-responder)))
  (when *launchkey-in*
    (setf *midi-dump-test* (incudine:make-responder *explo-in* #'dump-responder)))
  (when *seaboard-in*
    (setf *midi-dump-test* (incudine:make-responder *explo-in* #'dump-responder)))
  )

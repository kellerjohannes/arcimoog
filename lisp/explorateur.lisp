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
  (incudine:remove-all-responders *explo-in*)
  (incudine:remove-all-responders *launchkey-in*)
  (incudine:remove-all-responders *seaboard-in*)
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
  (jackmidi:write-short *explo-out* (jackmidi:message type d1 d2) 3))


(defun note-on (note &optional (channel 1) (velocity 90))
  "Channel 1-16 (not 0-15)."
  (play-explo (+ 143 channel) note velocity))

(defun note-off (note &optional (channel 1) (velocity 90))
  "Channel 1-16 (not 0-15)."
  (play-explo (+ 127 channel) note velocity))

(defun aftertouch (note value &optional (channel 1))
  (play-explo (+ 159 channel) note value))





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

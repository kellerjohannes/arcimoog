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

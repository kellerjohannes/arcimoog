(in-package :arcimoog.osc-communication)

(defparameter *osc-out* nil)

;; localhost: "127.0.0.1"

(defun init (&optional (port 5800) (host "192.168.1.10"))
  (incudine:rt-start)
  (setf *osc-out* (osc:open :port port :direction :output :host host)))

(defun send (audio-output-channel value)
  (osc:message *osc-out* "/arcimoog" "if" audio-output-channel value))

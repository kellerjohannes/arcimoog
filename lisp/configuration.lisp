(in-package :arcimoog)

(defun scale-midi-to-0-1 (orig value)
  (declare (ignore orig))
  (midi-scale value 0.0 1.0))

(defun neutral-interpreter (value)
  value)

(defun setup-parameter-bank ()
  (configure-parameter *parameter-bank*
                       176
                       0
                       :bg-red
                       "BG R"
                       "Display background color, red component"
                       0.0
                       #'scale-midi-to-0-1
                       (lambda (v d dd)
                         (declare (ignore d dd))
                         (set-element-value *display* :main color (vector v 0.1 0.1)))))


(setup-parameter-bank)



;; TODO: to be tested

(defun param! (value-or-id)
  (if (symbolp value-or-id)
      (lambda () (access *parameter-bank* value-or-id))
      (lambda () value-or-id)))

(defun param (parameter-fun)
  (funcall parameter-fun))

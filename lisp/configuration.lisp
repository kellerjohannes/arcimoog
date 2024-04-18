(in-package :arcimoog)

(defun scale-midi-to-0-1 (orig value)
  (declare (ignore orig))
  (midi-scale value 0.0 1.0))

;; (defun adder (orig value)
;;   (+ orig value))

;; (defun multiplier (orig factor)
;;   (* orig factor))

(defun make-adder (increase-amount-factor)
  (lambda (orig value)
    (+ orig (* (- value 64) increase-amount-factor))))

(defun relative-mode (orig value)
  (+ orig (- value 64)))

(defun neutral-interpreter (value)
  value)

(defmacro display-element-setter (element-id slot-name)
  `(lambda (value short-description long-description)
     (declare (ignore short-description long-description))
     (set-element-value *display* ,element-id ,slot-name value)))

(defmacro current-value (element-id slot-name)
  `(get-element-value *display* ,element-id ,slot-name))


(defmacro display-control (setup controller id short long element-slot factor faderfox-doc)
  `(configure-parameter *parameter-bank*
                        ,setup
                        ,controller
                        ,id
                        ,short
                        ,long
                        (current-value id-of-selected-element ,element-slot)
                        (make-adder ,factor)
                        (display-element-setter id-of-selected-element ,element-slot)
                        ,faderfox-doc))

(defun setup-parameter-bank (id-of-selected-element)
  (setf (id-dictionary *parameter-bank*) nil)

  (display-control 176 0 :x "El. X" "X-Coordinate" x-position 1 "")
  (display-control 176 1 :y "El. Y" "y-Coordinate" y-position 1 "")
  (display-control 176 2 :scaling "Sc." "Scaling" scaling 0.001 "")
  ;; (configure-parameter *parameter-bank*
  ;;                      176
  ;;                      0
  ;;                      :x
  ;;                      "El X"
  ;;                      "X-coordinate of element"
  ;;                      (current-value :main x-position)
  ;;                      (make-adder 1)
  ;;                      (display-element-setter :main x-position))

  ;; (configure-parameter *parameter-bank*
  ;;                      176
  ;;                      1
  ;;                      :y
  ;;                      "El Y"
  ;;                      "Y-coordinate of element"
  ;;                      (current-value :main y-position)
  ;;                      (make-adder 1)
  ;;                      (display-element-setter :main y-position))

  ;; (configure-parameter *parameter-bank*
  ;;                      176
  ;;                      2
  ;;                      :scaling
  ;;                      "El sc"
  ;;                      "Scaling of the selected element"
  ;;                      (current-value :main scaling)
  ;;                      (make-adder 0.01)
  ;;                      (display-element-setter :main scaling))


  ;; (configure-parameter *parameter-bank*
  ;;                      176
  ;;                      12
  ;;                      :bg-red
  ;;                      "BG R"
  ;;                      "Display background color, red component"
  ;;                      0.0
  ;;                      #'scale-midi-to-0-1
  ;;                      (lambda (v d dd)
  ;;                        (declare (ignore d dd))
  ;;                        (set-element-value *display* :main color (vector v 0.1 0.1))))

  )


(setup-parameter-bank :table)

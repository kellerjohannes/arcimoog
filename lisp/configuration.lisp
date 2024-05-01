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

(defmacro cv-setter (index)
  `(lambda (value short-description long-description)
     (declare (ignore short-description long-description))
     (set-cv ,index value)))

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

(defmacro cv-control (setup controller parameter-id val-number factor faderfox-doc)
  `(configure-parameter *parameter-bank*
                        ,setup
                        ,controller
                        ,parameter-id
                        (format nil "CV~a" ,val-number)
                        (format nil "Control Voltage slot ~a" ,val-number)
                        0.0
                        (make-adder ,factor)
                        (cv-setter ,val-number)
                        ,faderfox-doc))

(defun setup-parameter-bank (id-of-selected-element)
  (setf (id-dictionary *parameter-bank*) nil)

  (display-control 176 0 :x "El. X" "X-Coordinate" x-position 1 "")
  (display-control 176 1 :y "El. Y" "y-Coordinate" y-position 1 "")
  (display-control 176 2 :scaling "Sc." "Scaling" scaling 0.001 "")

  (cv-control 177 0 :cv1 1 0.001 "")
  (cv-control 177 1 :cv2 2 0.001 "")
  (cv-control 177 2 :cv3 3 0.001 "")
  (cv-control 177 3 :cv4 4 0.001 "")
  (cv-control 177 4 :cv5 5 0.001 "")
  (cv-control 177 5 :cv6 6 0.001 "")
  (cv-control 177 6 :cv7 7 0.001 "")
  (cv-control 177 7 :cv8 8 0.001 "")
  (cv-control 177 8 :cv9 9 0.001 "")
  (cv-control 177 9 :cv10 10 0.001 "")
  (cv-control 177 10 :cv11 11 0.001 "")
  (cv-control 177 11 :cv12 12 0.001 "")
  (cv-control 177 12 :cv13 13 0.001 "")
  (cv-control 177 13 :cv14 14 0.001 "")
  (cv-control 177 14 :cv15 15 0.001 "")
  (cv-control 177 15 :cv16 16 0.001 "")

  (cv-control 177 16 :cv1 1 0.0001 "")
  (cv-control 177 17 :cv2 2 0.0001 "")
  (cv-control 177 18 :cv3 3 0.0001 "")
  (cv-control 177 19 :cv4 4 0.0001 "")
  (cv-control 177 20 :cv5 5 0.0001 "")
  (cv-control 177 21 :cv6 6 0.0001 "")
  (cv-control 177 22 :cv7 7 0.0001 "")
  (cv-control 177 23 :cv8 8 0.0001 "")
  (cv-control 177 24 :cv9 9 0.0001 "")
  (cv-control 177 25 :cv10 10 0.0001 "")
  (cv-control 177 26 :cv11 11 0.0001 "")
  (cv-control 177 27 :cv12 12 0.0001 "")
  (cv-control 177 28 :cv13 13 0.0001 "")
  (cv-control 177 29 :cv14 14 0.0001 "")
  (cv-control 177 30 :cv15 15 0.0001 "")
  (cv-control 177 31 :cv16 16 0.0001 "")

  (cv-control 177 32 :cv1 1 0.00001 "")
  (cv-control 177 33 :cv2 2 0.00001 "")
  (cv-control 177 34 :cv3 3 0.00001 "")
  (cv-control 177 35 :cv4 4 0.00001 "")
  (cv-control 177 36 :cv5 5 0.00001 "")
  (cv-control 177 37 :cv6 6 0.00001 "")
  (cv-control 177 38 :cv7 7 0.00001 "")
  (cv-control 177 39 :cv8 8 0.00001 "")
  (cv-control 177 40 :cv9 9 0.00001 "")
  (cv-control 177 41 :cv10 10 0.00001 "")
  (cv-control 177 42 :cv11 11 0.00001 "")
  (cv-control 177 43 :cv12 12 0.00001 "")
  (cv-control 177 44 :cv13 13 0.00001 "")
  (cv-control 177 45 :cv14 14 0.00001 "")
  (cv-control 177 46 :cv15 15 0.00001 "")
  (cv-control 177 47 :cv16 16 0.00001 "")

  (cv-control 177 48 :cv1 1 0.000001 "")
  (cv-control 177 49 :cv2 2 0.000001 "")
  (cv-control 177 50 :cv3 3 0.000001 "")
  (cv-control 177 51 :cv4 4 0.000001 "")
  (cv-control 177 52 :cv5 5 0.000001 "")
  (cv-control 177 53 :cv6 6 0.000001 "")
  (cv-control 177 54 :cv7 7 0.000001 "")
  (cv-control 177 55 :cv8 8 0.000001 "")
  (cv-control 177 56 :cv9 9 0.000001 "")
  (cv-control 177 57 :cv10 10 0.000001 "")
  (cv-control 177 58 :cv11 11 0.000001 "")
  (cv-control 177 59 :cv12 12 0.000001 "")
  (cv-control 177 60 :cv13 13 0.000001 "")
  (cv-control 177 61 :cv14 14 0.000001 "")
  (cv-control 177 62 :cv15 15 0.000001 "")
  (cv-control 177 63 :cv16 16 0.000001 "")
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

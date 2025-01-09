(in-package :osc-communication)

(incudine:rt-start)

(defun done ()
  (format t "~&done."))

(defparameter *osc-out* nil)

(setf *osc-out* (osc:open :port 5800 :direction :output :host "192.168.1.10"))


(defparameter *midi-in* nil)
(defparameter *midi-responder* nil)

(defparameter *mother-state* '(1 (:vco 0.0 :vcf 0.0 :res 0.0 :vca 0.0)
                               2 (:vco 0.0 :vcf 0.0 :res 0.0 :vca 0.0)))

(defparameter *test* '(:a 5 :b 6 :c (a b c) 1 15))

(defun get-mother-place (mother-id parameter-name)
  (getf (getf *mother-state* mother-id) parameter-name))

(defun inc-mother-place (mother-id parameter-name amount out-channel)
  (incf (getf (getf *mother-state* mother-id) parameter-name) amount)
  (osc:message *osc-out* "/arcimoog" "if" out-channel (get-mother-place mother-id parameter-name))
  (ui::change-meter-level (1- out-channel) (get-mother-place mother-id parameter-name)))

(defun set-mother-place (mother-id parameter-name value out-channel)
  (setf (getf (getf *mother-state* mother-id) parameter-name) value)
  (osc:message *osc-out* "/arcimoog" "if" out-channel (get-mother-place mother-id parameter-name))
  (ui::change-meter-level (1- out-channel) (get-mother-place mother-id parameter-name)))

(defun update-precision-dial (midi-raw mother-id parameter-name update-factor out-channel)
  (let ((old-value (get-mother-place mother-id parameter-name)))
    (inc-mother-place mother-id parameter-name (* (- midi-raw 64) update-factor) out-channel)
    (format t "~&Updating Mother ~a, ~a from ~a to ~a"
            mother-id
            parameter-name
            old-value
            (get-mother-place mother-id parameter-name))))

(defun update-std-dial (mother-id parameter-name value scale-fun out-channel)
  (let ((old-value (get-mother-place mother-id parameter-name)))
    (set-mother-place mother-id parameter-name (funcall scale-fun value) out-channel)
    (format t "~&Updating Mother ~a, ~a from ~a to ~a"
            mother-id
            parameter-name
            old-value
            (get-mother-place mother-id parameter-name))))

(defun scale-midi-to-cv (val)
  (- (* 2.0 (/ val 127.0)) 1.0))

(defun midi-responder (channel-raw controller-raw value-raw)
  ;; (format t "~&~a ~a ~a~%" channel-raw controller-raw value-raw)
  (case (- channel-raw 176)
    (0 (case controller-raw
         (0 (update-precision-dial value-raw 1 :vco 0.001 1))
         (4 (update-precision-dial value-raw 1 :vco 0.00001 1))
         (1 (update-precision-dial value-raw 1 :vcf 0.01 2))
         (5 (update-precision-dial value-raw 1 :vcf 0.001 2))
         (2 (update-std-dial 1 :res value-raw #'scale-midi-to-cv 3))
         (3 (update-std-dial 1 :vca value-raw #'scale-midi-to-cv 4))

         (8 (update-precision-dial value-raw 2 :vco 0.001 5))
         (12 (update-precision-dial value-raw 2 :vco 0.00001 5))
         (9 (update-precision-dial value-raw 2 :vcf 0.01 6))
         (13 (update-precision-dial value-raw 2 :vcf 0.001 6))
         (10 (update-std-dial 2 :res value-raw #'scale-midi-to-cv 7))
         (11 (update-std-dial 2 :vca value-raw #'scale-midi-to-cv 8))
         ))))

(pm:initialize)

(setf *midi-in* (pm:open 3))

(setf *midi-responder* (incudine:make-responder *midi-in* (lambda (a b c) (midi-responder a b c))))

(incudine:recv-start *midi-in*)

(in-package :arcimoog)

(am-ui:init)

(am-midi:init)

(am-osc:init 5800 "127.0.0.1")

;; TODO These kinds of calls could be condensed with simple macros, to reduce typing.

;;; Set up global parameters

(defmacro register-cv (name audio-output-channel)
  `(am-par:register-scalar ,name 0.0 -1.0 1.0
                           (list (lambda (name value)
                                   ,(format nil "Send OSC message to audio output channel ~a."
                                            audio-output-channel)
                                   (declare (ignore name))
                                   (am-osc:send ,audio-output-channel value)))))

(register-cv :vco1 0)
(am-par:register-hook :vco1 (lambda (name value)
                              "Printing the new value to the REPL."
                              (format t "~&~a updated to ~a." name value)))
(register-cv :vcf1 1)
(register-cv :res1 2)
(register-cv :vca1 3)
(register-cv :gate1 4)

;; (am-par:register-scalar :vco1 0.0 -1.0 1.0
;;                         (list (lambda (name value)
;;                                 (format t "~&Callback: param ~a updated to ~a." name value))
;;                               (lambda (name value)
;;                                 (declare (ignore name))
;;                                 (ui::change-meter-level 0 value))
;;                                         ; Add a function that sends OSC out.
;;                               ))



;;; Set up MIDI behaviour
;;; (How to process Faderfox knobs)

(am-midi:register-callback 0 0 (lambda (value)
                                 (format t "~&MIDI input ~a" value)
                                 (am-par:inc-scalar :vco1 (* 0.01 (- value 64)))))

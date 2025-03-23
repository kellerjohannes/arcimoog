(in-package :arcimoog)

(am-ui:init)

(am-midi:init)

(am-osc:init 5800 "127.0.0.1")

;; TODO These kinds of calls could be condensed with simple macros, to reduce typing.

;;; Set up global parameters

(defmacro register-constant (name value)
  `(am-par:register-scalar ,name ,value ,value ,value nil))

(defmacro register-cv (name audio-output-channel)
  `(progn
     (am-par:register-scalar ,name 0.0 -1.0 1.0
                             (list (lambda (name value)
                                     ,(format nil "Send OSC message to audio output channel ~a."
                                              audio-output-channel)
                                     (declare (ignore name))
                                     (am-osc:send ,audio-output-channel value))))
     (am-par:register-hook ,name (lambda (name value)
                                   "Print the updated value to the REPL."
                                   (format t "~&~a updated to ~a.~%" name value)))))

(register-constant :precision-factor-low 0.01)
(register-constant :precision-factor-medium 0.001)
(register-constant :precision-factor-high 0.0001)
(register-constant :precision-factor-extreme 0.00001)
(register-constant :toggle-off-value -1.0)
(register-constant :toggle-on-value 1.0)

(register-cv :vco1 0)
(register-cv :vcf1 1)
(register-cv :res1 2)
(register-cv :vca1 3)
(register-cv :gate1 4)

(register-cv :vco2 5)
(register-cv :vcf2 6)
(register-cv :res2 7)
(register-cv :vca2 8)
(register-cv :gate2 9)

(register-cv :vco3 10)
(register-cv :vcf3 11)
(register-cv :res3 12)
(register-cv :vca3 13)
(register-cv :gate3 14)


;;; Set up MIDI behaviour
;;; (How to process Faderfox knobs)

(defmacro register-toggle-dial (controller channel doc target-parameter)
  `(am-midi:register-callback ,controller ,channel
                              (lambda (value)
                                ,doc
                                (cond ((< value 64)
                                       (am-par:set-scalar ,target-parameter
                                                          ,(am-par:get-scalar :toggle-off-value)))
                                      ((> value 64)
                                       (am-par:set-scalar ,target-parameter
                                                          ,(am-par:get-scalar :toggle-on-value)))))))

(defmacro register-precision-dial (controller
                                   channel-a
                                   channel-b
                                   channel-c
                                   channel-d
                                   doc
                                   target-parameter)
  "CONTROLLER represents the Faderfox Setup (0-based). The four CHANNEL arguments are Faderfox controller channels (0-based). They can be left NIL, in which case that precision level will be omitted. A stands for the lowest precision, D for the highest. DOC is a string that is used to generate a docstring for the callback function. TARGET-PARAMETER is a keyword for an existing Arcimoog parameter (see ARCIMOOG.PARAMETER package)."
  `(progn
     ,@(mapcan (lambda (channel precision)
                 (when channel
                   `((am-midi:register-callback
                      ,controller ,channel
                      (lambda (value)
                        ,(format nil "~a Precision level: ~a."
                                 doc
                                 precision)
                        (am-par:inc-scalar ,target-parameter
                                           (* (am-par:get-scalar
                                               ,(alexandria:make-keyword
                                                 (format nil "PRECISION-FACTOR-~a" precision)))
                                              (- value 64))))))))
               (list channel-a channel-b channel-c channel-d)
               (list 'low 'medium 'high 'extreme))))

(register-precision-dial 1 0 1 2 3 "VCO1 quad precision." :vco1)
(register-precision-dial 1 4 5 6 7 "VCF1 quad precision." :vcf1)
(register-precision-dial 1 8 9 10 11 "Res1 quad precision." :res1)
(register-precision-dial 1 12 13 nil nil "VCA1 dual precision." :vca1)
(register-toggle-dial 1 14 "Gate1 toggle dial." :gate1)

(register-precision-dial 1 16 17 18 19 "VCO2 quad precision." :vco2)
(register-precision-dial 1 20 21 22 23 "VCF2 quad precision." :vcf2)
(register-precision-dial 1 24 25 26 27 "Res2 quad precision." :res2)
(register-precision-dial 1 28 29 nil nil "VCA2 dual precision." :vca2)
(register-toggle-dial 1 30 "Gate2 toggle dial." :gate2)

(register-precision-dial 1 32 33 34 35 "VCO3 quad precision." :vco3)
(register-precision-dial 1 36 37 38 39 "VCF3 quad precision." :vcf3)
(register-precision-dial 1 40 41 42 43 "Res3 quad precision." :res3)
(register-precision-dial 1 44 45 nil nil "VCA3 dual precision." :vca3)
(register-toggle-dial 1 46 "Gate3 toggle dial." :gate3)

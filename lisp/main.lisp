(in-package :arcimoog)

(ui:start)

(am-midi:init-faderfox-communication)


;; TODO These kinds of calls could be condensed with simple macros, to reduce typing.

(am-par:register-scalar :vco1 0.0 -1.0 1.0
                        (list (lambda (name value)
                                (format t "~&Callback: param ~a updated to ~a." name value))
                              (lambda (name value)
                                (declare (ignore name))
                                (ui::change-meter-level 0 value))))

(am-midi:register-callback 0 0 (lambda (value)
                                 (format t "~&MIDI input ~a" value)
                                 (am-par:inc-scalar :vco1 (* 0.01 (- value 64)))))

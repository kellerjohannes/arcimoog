(in-package :arcimoog)


(defun init ()
  "Prepare a live session. To be called after loading the system, before starting to make music."

  ;; Start up all the modules for a live session.
  (am-midi:init)
  ;; (am-osc:init 5800 "127.0.0.1")
  (am-osc:init)

  ;; Define global constants used by callback functions.
  (register-constant :precision-factor-low 0.01)
  (register-constant :precision-factor-medium 0.001)

  (register-constant :precision-factor-high 0.0001)
  (register-constant :precision-factor-extreme 0.000001)
  (register-constant :toggle-off-value -1.0)
  (register-constant :toggle-on-value 1.0)

  ;; Global values that control the UI
  (register-display-parameter :cv-history-x-offset 0.0)
  (register-precision-dial 2 0 1 2 3 "CV history X offset." :cv-history-x-offset)
  (register-display-parameter :cv-history-x-scale 1.0 0.0)
  (register-precision-dial 2 4 5 6 7 "CV history X scale." :cv-history-x-scale)

  ;; Set up CV parameters.
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

  (register-cv :vco4 15)
  (register-cv :vcf4 16)
  (register-cv :res4 17)
  (register-cv :vca4 18)
  (register-cv :gate4 19)

  (register-cv :vco5 20)
  (register-cv :vcf5 21)
  (register-cv :res5 22)
  (register-cv :vca5 23)
  (register-cv :gate5 24)

  ;; Set up Faderfox input for direct CV manipulation.
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

  (register-precision-dial 1 48 49 50 51 "VCO4 quad precision." :vco4)
  (register-precision-dial 1 52 53 54 55 "VCF4 quad precision." :vcf4)
  (register-precision-dial 1 56 57 58 59 "Res4 quad precision." :res4)
  (register-precision-dial 1 60 61 nil nil "VCA4 dual precision." :vca4)
  (register-toggle-dial 1 62 "Gate4 toggle dial." :gate4)

  (register-precision-dial 1 64 65 66 67 "VCO3 quad precision." :vco5)
  (register-precision-dial 1 68 69 70 71 "VCF3 quad precision." :vcf5)
  (register-precision-dial 1 72 73 74 75 "Res3 quad precision." :res5)
  (register-precision-dial 1 76 77 nil nil "VCA3 dual precision." :vca5)
  (register-toggle-dial 1 78 "Gate3 toggle dial." :gate5)

  ;; Setup Mother abstractions
  (am-mo:register-mother :soprano :vco1 :vcf1 :res1 :vca1 :gate1)
  (am-mo:register-mother :alto :vco2 :vcf2 :res2 :vca2 :gate2)
  (am-mo:register-mother :tenore :vco3 :vcf3 :res3 :vca3 :gate3)
  (am-mo:register-mother :basso :vco4 :vcf4 :res4 :vca4 :gate4)
  (am-mo:register-mother :quinto :vco5 :vcf5 :res5 :vca5 :gate5)

  (am-mo:select-mother :soprano)

  (am-mo:read-mother-tunings)

  (register-precision-dial-callback 3 0 1 2 3
                                    "Tuning Mother offset, quad precision."
                                    #'am-mo:modify-selected-cv-offset)

  (register-precision-dial-callback 3 4 5 6 7
                                    "Tuning Mother F-factor, quad precision."
                                    #'am-mo:modify-selected-cv-factor)

  ;; Global pitch control
  (register-precision-dial-callback 3 16 17 18 19
                                    "Setting master transposer, quad precision."
                                    #'am-mo:modify-master-transpose)

  (register-precision-dial-callback 3 20 21 22 23
                                    "Modifying the tempering of the fifth in linear systems. Quad precision."
                                    #'am-si:modify-fifth)

  (register-precision-dial-callback 3 24 25 26 27
                                    "Modifying the identity interval in EDx systems. Quad precision"
                                    #'am-si:modify-edx)

  (register-precision-dial-callback 3 28 29 30 31
                                    "Setting global VCO offset (origin pitch)."
                                    #'am-mo:modify-cv-1/1)

  ;; Startup webserver UI (CLOG)
  (am-ui:init))





;;; Define domain specific language for live sessions

;; Helper functions, not intended for live use

(defparameter *mother-dict* '((:s . :soprano)
                              (:a . :alto)
                              (:t . :tenore)
                              (:b . :basso)
                              (:q . :quinto)))

(defun lookup-mother-name (shorthand-or-full-name)
  ;; TODO Handle undefined input.
  (or (cdr (assoc shorthand-or-full-name *mother-dict*))
      (when (am-mo:is-valid-mother-name shorthand-or-full-name) shorthand-or-full-name)))



;; DSL commands

(defun s (mother-name)
  "Select a Mother for later manipulation. MOTHER-NAME can be its full name defined when AM-MO:REGISTER-MOTHER was called, or a shorthand alias defined in *MOTHER-DICT*."
  (am-mo:select-mother (lookup-mother-name mother-name)))

(defun offset (mother-name cv-offset)
  "Set a MOTHERs CV offset. The maximal range of CV offset is -1 to 1."
  (am-mo:set-mother-cv-offset (lookup-mother-name mother-name) cv-offset))

(defun soffset (cv-offset)
  "Set the selected MOTHERs CV offset. The maximal range of CV offset is -1 to 1."
  (am-mo:set-selected-cv-offset cv-offset))

(defun factor (mother-name cv-factor)
  "Set a MOTHERs CV stretching factor."
  (am-mo:set-mother-cv-factor (lookup-mother-name mother-name) cv-factor))

(defun sfactor (cv-factor)
  "Set the selected MOTHERs CV stretching factor."
  (am-mo:set-selected-cv-factor cv-factor))

(defun morel (mother-name interval &optional (natura-delta 0))
  "Change pitch and timbre of a MOTHER. The INTERVAL is used to modify the current pitch of the MOTHER. NATURE-DELTA is used to modify the current NATURE of the MOTHER. MOTHER-NAME can be its full name or a shorthand alias."
  (am-mo:modify-mother-pitch-and-natura (lookup-mother-name mother-name) interval natura-delta))

(defun smorel (interval &optional (natura-delta 0))
  "Behaves like the function MOREL, but affects the currently selected MOTHER."
  (am-mo:modify-selected-pitch-and-natura interval natura-delta))

(defun moabs (mother-name pitch &optional (natura 0))
  "Define pitch and timbre of a MOTHER. The pitch will be set to the ratio provided in PITCH, overwriting the current pitch of the MOTHER. NATURA will be used to overwrite the current NATURA of the MOTHER. If the argument NATURA is not provided, it will be set to 0. If it is set to NIL, the current NATURA of the MOTHER won't be affected."
  (am-mo:set-mother-pitch (lookup-mother-name mother-name) pitch)
  (when natura (am-mo:set-mother-natura (lookup-mother-name mother-name) natura)))

(defun smoabs (pitch &optional (natura 0))
  "Behaves like the function `MOABS', but affects the currently selected MOTHER."
  (am-mo:set-selected-pitch pitch)
  (when natura (am-mo:set-selected-natura natura)))

(defun on (mother-name)
  "Sets the GATE CV of a MOTHER to 1."
  (am-mo:mother-on (lookup-mother-name mother-name)))

(defun off (mother-name)
  "Sets the GATE CV of a MOTHER to 0."
  (am-mo:mother-off (lookup-mother-name mother-name)))

(defun son ()
  "Sets the GATE CV of the selected MOTHER to 1."
  (am-mo:selected-on))

(defun soff ()
  "Sets the GATE CV of the selected MOTHER to 0."
  (am-mo:selected-off))

(defun allon ()
  "Sets the GATE CV of all MOTHERs to 1"
  (am-mo:set-all-gates t))

(defun alloff ()
  "Sets the GATE CV of all MOTHERs to 0"
  (am-mo:set-all-gates nil))

(defun cvdon ()
  "Enable text output for all CV changes."
  (setf *output-cv-updates-p* t))

(defun cvdoff ()
  "Disable text output for all CV changes."
  (setf *output-cv-updates-p* nil))

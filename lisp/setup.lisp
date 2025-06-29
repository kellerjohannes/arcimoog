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

  ;; (register-cv :vco5 20)
  ;; (register-cv :vcf5 21)
  ;; (register-cv :res5 22)
  ;; (register-cv :vca5 23)
  ;; (register-cv :gate5 24)

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

  ;; (register-precision-dial 1 64 65 66 67 "VCO3 quad precision." :vco5)
  ;; (register-precision-dial 1 68 69 70 71 "VCF3 quad precision." :vcf5)
  ;; (register-precision-dial 1 72 73 74 75 "Res3 quad precision." :res5)
  ;; (register-precision-dial 1 76 77 nil nil "VCA3 dual precision." :vca5)
  ;; (register-toggle-dial 1 78 "Gate3 toggle dial." :gate5)

  ;; Setup Mother abstractions
  (am-mo:register-mother :soprano :vco1 :vcf1 :res1 :vca1 :gate1)
  (am-mo:register-mother :alto :vco2 :vcf2 :res2 :vca2 :gate2)
  (am-mo:register-mother :tenore :vco3 :vcf3 :res3 :vca3 :gate3)
  (am-mo:register-mother :basso :vco4 :vcf4 :res4 :vca4 :gate4)
  ;; (am-mo:register-mother :quinto :vco5 :vcf5 :res5 :vca5 :gate5)

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




(defun origin-simple-swipe (time value direction target-value &optional
                                                         (time-interval 1410)
                                                         (cv-delta 0.001))
  (cond ((and (> direction 0) (>= value target-value)) (am-mo:set-cv-1/1 target-value))
        ((and (< direction 0) (<= value target-value)) (am-mo:set-cv-1/1 target-value))
        (t (am-mo:set-cv-1/1 value)
           (incudine:at (+ time time-interval)
                        #'origin-simple-swipe
                        (+ time time-interval)
                        (+ value cv-delta)
                        direction
                        target-value
                        time-interval
                        cv-delta))))

(defun calculate-cv-delta (target-value origin-value time-delta time-step)
  (coerce (/ (- target-value origin-value)
             (/ (* time-delta (incudine:rt-sample-rate)) time-step))
          'single-float))

(defun origin-swipe (target-value time-delta)
  (origin-simple-swipe (incudine:now)
                (am-mo:get-cv-1/1)
                (if (> target-value (am-mo:get-cv-1/1)) 1 -1)
                target-value
                2000
                (calculate-cv-delta target-value (am-mo:get-cv-1/1) time-delta 2000)))





(defun simple-swipe (time value direction target-value name &optional
                                                              (time-interval 1410)
                                                              (interval-delta (expt 2 1/1200)))
  (cond ((and (> direction 0) (>= value target-value)) (moabs name target-value))
        ((and (< direction 0) (<= value target-value)) (moabs name target-value))
        (t (moabs name value)
           (incudine:at (+ time time-interval)
                        #'simple-swipe
                        (+ time time-interval)
                        (* value interval-delta)
                        direction
                        target-value
                        name
                        time-interval
                        interval-delta))))

(defun calculate-interval-delta (target-value origin-value time-delta time-step)
  (coerce (expt (/ target-value origin-value)
                (/ 1 (/ (* time-delta (incudine:rt-sample-rate)) time-step)))
          'single-float))

(defun time-swipe (mother-name origin-value target-value time-delta)
  (simple-swipe (incudine:now)
                origin-value
                (if (> target-value origin-value) 1 -1)
                target-value
                mother-name
                2000
                (calculate-interval-delta target-value origin-value time-delta 2000)))



;; (progn
;;   (on :s)
;;   (on :a)
;;   (on :t)
;;   (moabs :s 1/128)
;;   (moabs :a (* 3/2 1/128))
;;   (moabs :t (* 2/1 1/128)))

;; (progn
;;   (simple-swipe (incudine:now) 1/128 1 1/2 :s)
;;   (simple-swipe (incudine:now) (* 3/2 1 1/128) (* 3/2 1/2) :a)
;;   (simple-swipe (incudine:now) (* 2/1 1 1/128) (* 2/1 1/2) :t))


(defun flux-1-o ()
  (alloff)
  (on :s)
  (on :a)
  (moabs :s 5/4)
  (moabs :a 1/1))

(defun flux-1-a (&optional (time-delta 5))
  (time-swipe :s 5/4 (* 128/125 6/5) time-delta)
  (time-swipe :a 1/1 128/125 time-delta))

(defun flux-1-b (&optional (time-delta 5))
  (time-swipe :s (* 128/125 6/5) 5/4 time-delta)
  (time-swipe :a 128/125 1/1 time-delta))



(defun flux-2-o ()
  (alloff)
  (on :s)
  (on :a)
  (on :t)
  (moabs :s 3/2)
  (moabs :a 5/4)
  (moabs :t 1/1))

(defun flux-2-a (&optional (time-delta 5))
  (time-swipe :s 3/2 (* 3/2 128/125) time-delta)
  (time-swipe :a 5/4 (* 128/125 6/5) time-delta)
  (time-swipe :t 1/1 128/125 time-delta))

(defun flux-2-b (&optional (time-delta 5))
  (time-swipe :s (* 3/2 128/125) 3/2 time-delta)
  (time-swipe :a (* 128/125 6/5) 5/4 time-delta)
  (time-swipe :t 128/125 1/1 time-delta))


(defun flux-3-o ()
  (alloff)
  (on :s)
  (on :a)
  (on :t)
  (on :b)
  (moabs :s 3/2)
  (moabs :a 5/4)
  (moabs :t 1/1)
  (moabs :b 7/4))

(defun flux-3-a (&optional (time-delta 5))
  (time-swipe :s 3/2 (* 3/2 128/125) time-delta)
  (time-swipe :a 5/4 (* 128/125 6/5) time-delta)
  (time-swipe :t 1/1 128/125 time-delta)
  (time-swipe :b 7/4 (* 7/4 128/125) time-delta))

(defun flux-3-b (&optional (time-delta 5))
  (time-swipe :s (* 3/2 128/125) 3/2 time-delta)
  (time-swipe :a (* 128/125 6/5) 5/4 time-delta)
  (time-swipe :t 128/125 1/1 time-delta)
  (time-swipe :b (* 7/4 128/125) 7/4 time-delta))




(defmacro make-flux (name p-1-s p-1-a p-1-t p-1-b p-2-s p-2-a p-2-t p-2-b)
  `(progn
     (defun ,(intern (format nil "~a-O" (symbol-name name))) ()
       (alloff)
       (on :s)
       (on :a)
       (on :t)
       (on :b)
       (moabs :s ,p-1-s)
       (moabs :a ,p-1-a)
       (moabs :t ,p-1-t)
       (moabs :b ,p-1-b))
     (defun ,(intern (format nil "~a-A" (symbol-name name))) (&optional (time-delta 5))
       (time-swipe :s ,p-1-s ,p-2-s time-delta)
       (time-swipe :a ,p-1-a ,p-2-a time-delta)
       (time-swipe :t ,p-1-t ,p-2-t time-delta)
       (time-swipe :b ,p-1-b ,p-2-b time-delta))
     (defun ,(intern (format nil "~a-B" (symbol-name name))) (&optional (time-delta 5))
       (time-swipe :s ,p-2-s ,p-1-s time-delta)
       (time-swipe :a ,p-2-a ,p-1-a time-delta)
       (time-swipe :t ,p-2-t ,p-1-t time-delta)
       (time-swipe :b ,p-2-b ,p-1-b time-delta))))

(make-flux flux-4 3/2 5/4 1/1 7/4 (* 3/2 128/125) (* 6/5 128/125) 128/125 (* 2/1 128/125))

(make-flux flux-5 7/4 3/2 5/4 1/1 3/2 (* 3/2 5/4) (* 3/2 7/4) (* 3/2 3/2))

(make-flux flux-6 1/1 3/1 7/2 5/1 (* 128/125 1/1) (* 128/125 3/1) (* 128/125 4/1) (* 128/125 24/5))

(make-flux flux-7 1/2 1/2 1/2 1/2 2 3 4 5)

(make-flux oct-1 1/1 1/1 1/1 1/1
           (* 1/1 2)
           (* 1/1 2)
           (* 1/1 1)
           (* 1/1 8)
           )

(make-flux oct-2 1/1 81/80 128/125 80/81
           (* 128/125 2)
           (* 81/80 2)
           (* 1/1 1)
           (* 80/81 8)
           )

;; (progn
;;   (allon)
;;   (moabs :b 1/1)
;;   (moabs :t 2/1)
;;   (moabs :a 3/1)
;;   (moabs :s 5/1))


(defun takeoff-o ()
  (allon)
  (moabs :s 5/4)
  (moabs :a 9/8)
  (moabs :t 3/4)
  (moabs :b 1/2)
  (am-mo:set-cv-1/1 -2))

(defun takeoff-go ()
  (origin-swipe -0.82 313))


(defun uni ()
  (moabs :s 1)
  (moabs :a 1)
  (moabs :t 1)
  (moabs :b 1))

(defun s1 ()
  (moabs :s 2/1)
  (moabs :a (* 2/1 128/125))
  (moabs :t (* 2/1 81/80))
  (moabs :b (* 2/1 80/81)))

;; 1 octave
(make-flux c1 1/1 1/1 1/1 1/1 2 2 2 2)

;; septimal
(make-flux c2 1/1 1/1 1/1 1/1 2 3 5/2 7/2)

;; 2 octaves
(make-flux c3 1 1 1 1 4 4 4 4)

;; swarm
(make-flux c4 1 1 1 1 4 81/20 128/32 25/6)

;; wide
(make-flux c5 1 1 1 1 1 6 8 10)

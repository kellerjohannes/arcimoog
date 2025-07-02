(in-package :arcimoog)

(format t "~&Hi. System loaded. Preparing live session now.")

(init)

(format t "~&Ready. Make music now!")




;;; Sketches for Apollo Kreuzlingen, not in use for live set

;;; STARTING HERE

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



(defun cvdon ()
  (setf *output-cv-updates-p* t))

(defun cvdoff ()
  (setf *output-cv-updates-p* nil))


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


;;; ENDING HERE


;;; DONE find out where Complex Numbers come from!
;;; TODO debug natura modification in repeated notes

;;; Apollo Kreuzlingen live set


(defun all-1 (&optional duration)
  (am-mo:set-mother-pitch :soprano 1/1 duration)
  (am-mo:set-mother-pitch :alto 1/1 duration)
  (am-mo:set-mother-pitch :tenore 1/1 duration)
  (am-mo:set-mother-pitch :basso 1/1 duration))

(defun pitchpanic ()
  "Good to tune everything."
  (all-1)
  (am-mo:set-master-transpose 1/1)
  (am-mo:set-cv-1/1 -0.802) ; resulting in ca. 0V output current
  )

(pitchpanic)

(defparameter *transposer-slots* (make-array 16 :initial-element 1/1))

(defun init-transposer-values ()
  (setf (aref *transposer-slots* 1) 1/1) ; target takeoff
  (setf (aref *transposer-slots* 2) 0.0052932655) ; origin takeoff
  (setf (aref *transposer-slots* 3) 1/1) ; neutral (best to tune?)
  (setf (aref *transposer-slots* 4) 0.35219964) ; F# for ruedi (touchdown)
  (setf (aref *transposer-slots* 5) 0.9133662) ; origin for Willaert, bassa cappella
  (setf (aref *transposer-slots* 6) 1.8538494) ; origin Vicentino
 )

(defun save-transposer (slot-id)
  (setf (aref *transposer-slots* slot-id) (am-mo:get-master-transpose)))

(defun recall-transposer (slot-id)
  (am-mo:set-master-transpose (aref *transposer-slots* slot-id)))

(defun reset-natura (&optional (value-list (list 0 0 0 0)))
  (loop for value in value-list
        for mother in (list :soprano :alto :tenore :basso)
        do (progn (format t "~&Resetting natura of ~a." mother)
                  (am-mo:set-mother-natura mother value))))

(defun set-chord (soprano-pitch alto-pitch tenore-pitch basso-pitch &optional duration)
  (am-mo:set-mother-pitch :soprano soprano-pitch duration)
  (am-mo:set-mother-pitch :alto alto-pitch duration)
  (am-mo:set-mother-pitch :tenore tenore-pitch duration)
  (am-mo:set-mother-pitch :basso basso-pitch duration))

;;;;;;;;;;;;;
;; Takeoff ;;
;;;;;;;;;;;;;

(defun takeoff-reset ()
  (cvdoff)
  (reset-natura)
  (init-transposer-values)
  (all-1)
  (recall-transposer 2))

(defun takeoff-chord (&optional duration)
  (set-chord 4 5/2 3/2 1 duration))

;; TAKEOFF-RESET to prepare (called when loading ARCIMOOG)
;; ALLON in advance, Ruedi is mute
;; TAKEOFF-CHORD, timing ca. 30'
;; Manually raise
;; Ruedi cuts everything, then:
;; ALLOFF (Ruedi needs to open the channels for Willaert)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Willaert / Vicentino ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun willaert-reset ()
  (am-mo:set-natura-muted-p nil)
  (recall-transposer 5)
  (model 2)
  (reset-meantone)
  (am-si:set-edx 1/1))

(defun vicentino-reset ()
  (am-mo:set-natura-muted-p nil)
  (reset-natura)
;;  (am-mo:set-natura-muted-p nil)
  (recall-transposer 6)
  (model 2)
  (reset-meantone)
  (reset-ed2))

(defun soave ()
  (vicentino-reset)
  (go-vicentino))


;; Light on

;; WILLAERT-RESET to prepare (no sound)
;; M32 MIX 100%, Res 75%

;; GO-WILLAERT, Ruedi fadein

;; SOAVE
;; Ruedi fadeout


;; Check synth settings
;; Light off


;; TODO loop Willaert 2x
;; TODO solve natura problem in Vicentino




;;;;;;;;;;;
;; YanaY ;;
;;;;;;;;;;;

;; tacet



;;;;;;;;;;;;;;;
;; Touchdown ;;
;;;;;;;;;;;;;;;

(defparameter *chord-slots* (make-array 16 :initial-element (list 1 1 1 1)))

(defun set-chord-slot (slot-id soprano alto tenore basso)
  (setf (aref *chord-slots* slot-id) (list basso tenore alto soprano)))

(defun get-chord-slot (slot-id)
  (aref *chord-slots* slot-id))

(defun populate-chord-slots ()
  (set-chord-slot 1 1/1 1/1 1/1 1/1) ;; origin, unsplit
  (set-chord-slot 2 1/1 81/80 80/81 25/24) ;; origin, detuned
  (set-chord-slot 3 8 4 2 1) ;; origin, octave split
  (set-chord-slot 4 (* 8 1/1) (* 4 81/80) (* 2 80/81) (* 1 128/125)) ;; origin, octaves, detuned
  (set-chord-slot 5 (* 4 6/5) (* 4 9/4) (* 4 3/2) (* 2 1/1)) ;; special 9 chord, octave

  (set-chord-slot 6 3/2 3/2 3/2 3/2) ;; quinta, unsplit
  (set-chord-slot 7 (* 8 3/2) (* 4 3/2) (* 2 3/2) (* 1 3/2)) ;; quinta, split
  (set-chord-slot 8 (* 1 3/2)  (* 2 3/2)  (* 4 3/2) (* 8 3/2)) ;; quinta, reordered
  (set-chord-slot 9 (* 3/2 4 6/5) (* 3/2 4 9/4) (* 3/2 4 3/2) (* 3/2 2 1/1)) ;; quinta special 9 chord
  (set-chord-slot 0 3/2 3/2 3/2 3/2) ;; quinta, unsplit
  )

(defun chord (slot-id &optional (duration 1))
  (apply #'set-chord (append (get-chord-slot slot-id) (list duration))))

(defun touchdown-reset ()
  (all-1)
  (chord 4 nil)
  (recall-transposer 4)
  (populate-chord-slots))


(defun go-ruedi ()
  (chord 1 nil)
  (allon)
  (chord 4))

(defun reset-all ()
  (touchdown-reset)
  (takeoff-reset))

;; Open filters for everybody, especially I (soprano, sounding as bass)
;; TOUCHDOWN-RESET
;; To start sound: GO-RUEDI
;; Intro: play with chords 4, 2
;; Song: play with all chords RH/LH
;; Outro: chord 2, manually down
;; Close filters manually, then close VCAs



;; This is for startup, before the performance
(reset-all)

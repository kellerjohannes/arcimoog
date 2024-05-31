(in-package :arcimoog)

;;;; Experiments with Benedetti

(sc:def 'ji.ottava 'size (sc:val 2/1))
(sc:def 'ji.quarta 'size (sc:val 4/3))
(sc:def 'ji.quinta 'size (sc:val 3/2))
(sc:def 'ji.terza-maggiore 'size (sc:val 5/4))
(sc:def 'ji.terza-minore 'size (sc:val 6/5))
(sc:def 'ji.tono-maggiore 'size (sc:calc (sc:diff (sc:of 'ji.quinta 'size) (sc:of 'ji.quarta 'size))))
(sc:def 'ji.tono-minore 'size (sc:calc (sc:diff (sc:of 'ji.terza-maggiore 'size)
                                             (sc:of 'ji.tono-maggiore 'size))))
(sc:def 'ji.semitono-maggiore 'size (sc:calc (sc:diff (sc:of 'ji.quarta 'size)
                                                   (sc:of 'ji.terza-maggiore 'size))))


(sc:def 'mt.ottava 'size (sc:val 2/1))
(sc:def 'mt.quinta 'size (sc:calc (sc:temper (sc:of 'ji.quinta 'size) -1/4)))
(sc:def 'mt.quarta 'size (sc:calc (sc:diff (sc:of 'mt.ottava 'size)
                                        (sc:of 'mt.quinta 'size))))
(sc:def 'mt.terza-maggiore 'size (sc:val 5/4))
(sc:def 'mt.tono 'size (sc:calc (sc:edx 2 (sc:of 'mt.terza-maggiore 'size))))


(sc:def '12ed2.ottava 'size (sc:val 2/1))
(sc:def '12ed2.quinta 'size (sc:calc (sc:series 7 (sc:edx 12))))
(sc:def '12ed2.quarta 'size (sc:calc (sc:series 5 (sc:edx 12))))
(sc:def '12ed2.tono 'size (sc:calc (sc:series 2 (sc:edx 12))))


(sc:def 'reference-pitch.1 'pitch (sc:val 1/1))





(defvar tmp.tono-maggiore 'ji.tono-maggiore)
(defvar tmp.tono-minore 'ji.tono-minore)
(defvar tmp.ottava 'ji.ottava)
(defvar tmp.quinta 'ji.quinta)
(defvar tmp.quarta 'ji.quarta)
(defvar tmp.reference-pitch 'reference-pitch.1)

;; JI setup
(progn
  (setf tmp.reference-pitch 'reference-pitch.1
        tmp.ottava 'ji.ottava
        tmp.quinta 'ji.quinta
        tmp.quarta 'ji.quarta
        tmp.tono-maggiore 'ji.tono-maggiore
        tmp.tono-minore 'ji.tono-minore))


;; MT setup
(progn
  (setf tmp.reference-pitch 'reference-pitch.1
        tmp.ottava 'mt.ottava
        tmp.quinta 'mt.quinta
        tmp.quarta 'mt.quarta
        tmp.tono-maggiore 'mt.tono
        tmp.tono-minore 'mt.tono))

;; 12ed2 setup
(progn
  (setf tmp.reference-pitch 'reference-pitch.1
        tmp.ottava '12ed2.ottava
        tmp.quinta '12ed2.quinta
        tmp.quarta '12ed2.quarta
        tmp.tono-maggiore '12ed2.tono
        tmp.tono-minore '12ed2.tono))


(sc:def 'time.longa
  'duration (sc:val 1))

(sc:def 'time.brevis
  'duration (sc:calc (* 1/2 (sc:of 'time.longa 'duration))))

(sc:def 'time.semibrevis
  'duration (sc:calc (* 1/2 (sc:of 'time.brevis 'duration))))

(sc:def 'time.minima
  'duration (sc:calc (* 1/2 (sc:of 'time.semibrevis 'duration))))


(sc:def 'benedetti.origin
  'pitch (sc:val (sc:of tmp.reference-pitch 'pitch))
  'time (sc:val 0)
  'duration (sc:val 0)
  'child '(benedetti.v1.origin
           benedetti.v2.origin
           benedetti.v3.origin))

(sc:def 'benedetti.v1.origin
  'pitch (sc:rel (sc:asc local-state (sc:of tmp.ottava 'size)))
  'time (sc:val 0)
  'duration (sc:val 0)
  'child 'benedetti.v1.n1)

(sc:def 'benedetti.v2.origin
  'pitch (sc:rel (sc:asc local-state (sc:of tmp.quinta 'size)))
  'time (sc:val 0)
  'duration (sc:val 0)
  'child 'benedetti.v2.n1)

(sc:def 'benedetti.v3.origin
  'pitch (sc:rel local-state)
  'time (sc:val 0)
  'duration (sc:val 0)
  'child 'benedetti.v3.n1)


(sc:def 'benedetti.v1.n1
  'pitch (sc:rel local-state)
  'time (sc:rel (- local-state (sc:of 'time.minima 'duration)))
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v1.n2)

(sc:def 'benedetti.v1.n2
  'pitch (sc:rel (sc:asc local-state (sc:of tmp.tono-maggiore 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.semibrevis 'duration)))
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v1.n3)

(sc:def 'benedetti.v1.n3
  'pitch (sc:rel (sc:desc local-state (sc:of tmp.tono-minore 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.semibrevis 'duration)))
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v1.n2)



(sc:def 'benedetti.v2.n1
  'pitch (sc:rel local-state)
  'time (sc:rel local-state)
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v2.n2)

(sc:def 'benedetti.v2.n2
  'pitch (sc:rel (sc:asc local-state (sc:of tmp.tono-maggiore 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.semibrevis 'duration)))
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v2.n3)

(sc:def 'benedetti.v2.n3
  'pitch (sc:rel (sc:desc local-state (sc:of tmp.tono-minore 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.semibrevis 'duration)))
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v2.n2)



(sc:def 'benedetti.v3.n1
  'pitch (sc:rel local-state)
  'time (sc:rel local-state)
  'duration (sc:val (sc:of 'time.minima 'duration))
  'child 'benedetti.v3.n2)

(sc:def 'benedetti.v3.n2
  'pitch (sc:rel (sc:asc local-state (sc:of tmp.quinta 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.minima 'duration)))
  'duration (sc:val (sc:of 'time.minima 'duration))
  'child 'benedetti.v3.n3)

(sc:def 'benedetti.v3.n3
  'pitch (sc:rel (sc:desc local-state (sc:of tmp.tono-minore 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.minima 'duration)))
  'duration (sc:val (sc:of 'time.semibrevis 'duration))
  'child 'benedetti.v3.n4)

(sc:def 'benedetti.v3.n4
  'pitch (sc:rel (sc:desc local-state (sc:of tmp.quarta 'size)))
  'time (sc:rel (+ local-state (sc:of 'time.semibrevis 'duration)))
  'duration (sc:val (sc:of 'time.minima 'duration))
  'child 'benedetti.v3.n2)


;;;; requires DRAWER system, needs to be properly loaded and included

;; (defparameter *score* nil)

;; (defun start ()
;;   (let ((tikz-backend (make-backend-tikz :filename "score.tex")))
;;     (setf *performers* '())
;;     (setf *score* nil)
;;     (defperformer 0 'time 0 'pitch 1/1 'duration 0)
;;     (process 'benedetti.origin 0 *tikz-drawer* nil 50)
;;     ;;(process 'score.o 0 *printer* nil)
;;     (draw-with-multiple-backends (list tikz-backend) *score*)
;;     (compile-tikz tikz-backend)))

;;;; Experiments with Rossi


(defparameter *rossi-alto* '(b 1 b 2 d 2 c 2 c 4 c 4 es 1 des 1 c 1 ;; r 2
                             e 2 g 2 fis 1 eis 2 fis 1))

(defparameter *rossi-tenore* '(fis 1 g 2 g 2 as 1 g 2 g 2 bes 1 a brevis ;; r 2
                               dis 2 b 1 ais 1))

(defparameter *rossi-quinto* '(d 1 d 2 d 2 f 1 es 1 ;; r 2
                               des 2 f 1 e 2 e 2 b 2p a 4 gis 1 fis 1))

(defparameter *rossi-basso* '(b 1 g 2 bes 2 f 1 c 1 ;; r 1p
                              a 2 c 1 b 1))

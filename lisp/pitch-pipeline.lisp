(in-package :arcimoog)

(defun keyon (oct letter &optional acc dot)
  (with-note-name-convention 'note-name-vicentino
    (start-sample *sampler-arciorgano-mode1*
                  (transform (nn letter acc oct dot) 'arciorgano-key-number))))

(defun keyoff (oct letter &optional acc dot)
  (with-note-name-convention 'note-name-vicentino
    (stop-sample *sampler-arciorgano-mode1*
                 (transform (nn letter acc oct dot) 'arciorgano-key-number))))

(defun plk (duration oct letter &optional acc dot)
  (keyon oct letter acc dot)
  (incudine:at (+ (incudine:now) duration) #'keyoff oct letter acc dot))

(defun play-mode (pitch-list duration)
  (when pitch-list
    (apply #'plk (* 1.15 duration) (first pitch-list))
    (incudine:at (+ (incudine:now) duration) #'play-mode (rest pitch-list) duration)))


(defparameter *modes*
  `(:mani
    (:primo ((1 :c) (1 :d) (1 :e) (1 :f) (1 :g) (1 :a))
     :secondo ((1 :c) (1 :c :sharp) (1 :d) (1 :d :sharp) (1 :e) (1 :e :sharp) (1 :f) (1 :f :sharp)
               (1 :g) (1 :g :sharp) (1 :a))
     :terzo ((1 :c) (1 :d :flat) (1 :d) (1 :e :flat) (1 :e) (1 :f) (1 :g :flat) (1 :g) (1 :a :flat)
             (1 :a))
     :quarto ((1 :c) (1 :c nil :dot) (1 :c :sharp) (1 :d :flat :dot) (1 :d) (1 :d nil :dot)
              (1 :d :sharp) (1 :e :flat :dot) (1 :e) (1 :e nil :dot) (1 :e :sharp) (1 :f)
              (1 :f nil :dot) (1 :f :sharp) (1 :g :flat :dot) (1 :g) (1 :g nil :dot) (1 :g :sharp)
              (1 :a :flat :dot) (1 :a)))

    :diatonico-semplice
    (:primo ((1 :d) (1 :e) (1 :f) (1 :g) (1 :a) (1 :b) (2 :c) (2 :d) (1 :a) (1 :b) (2 :c) (2 :d)
              (1 :a) (1 :g) (1 :a) (1 :d) (1 :e) (1 :f) (1 :g) (1 :a) (1 :d) (1 :e) (1 :f) (1 :g)
              (1 :d) (1 :a) (1 :g) (1 :f) (1 :e) (1 :d))
     :secondo ((1 :d) (1 :e) (1 :f) (1 :g) (1 :a) (1 :d) (1 :c) (0 :b) (0 :a) (1 :d) (1 :c) (1 :d)
                (1 :a) (1 :g) (1 :a) (1 :d) (1 :e) (1 :f) (1 :g) (1 :a) (0 :a) (0 :b) (1 :c) (1 :d)
                (0 :a) (1 :d) (1 :c) (0 :b) (0 :a))
     :terzo ((1 :e) (1 :f) (1 :g) (1 :a) (1 :b) (2 :c) (2 :d) (2 :e) (1 :b) (2 :c) (1 :b) (2 :e)
              (2 :d) (2 :c) (1 :b) (1 :e) (1 :f) (1 :g) (1 :a) (1 :b) (1 :e) (1 :b) (2 :c) (1 :b)
              (1 :a) (1 :g) (1 :f) (1 :e))
     :quarto ((1 :e) (1 :f) (1 :g) (1 :a) (1 :b) (1 :e) (1 :f) (1 :e) (1 :d) (1 :c) (0 :b) (1 :c)
               (0 :b) (1 :e) (1 :f) (1 :e) (1 :f) (1 :g) (1 :a) (1 :b) (0 :b) (1 :e) (1 :d) (1 :c)
               (0 :b))
     :quinto ((1 :f) (1 :g) (1 :a) (1 :b) (2 :c) (2 :d) (2 :e) (2 :f) (2 :c) (2 :d) (2 :e) (2 :f)
               (1 :f) (1 :g) (1 :a) (1 :b) (2 :c) (1 :b) (2 :c) (2 :f) (2 :e) (2 :d) (2 :c) (2 :f)
               (2 :e) (2 :f) (1 :f) (1 :g) (1 :a) (1 :g) (1 :f))
     :sesto ((1 :f) (1 :g) (1 :a) (1 :b) (2 :c) (1 :f) (1 :e) (1 :f) (1 :g) (1 :a) (1 :g) (1 :f)
              (1 :c) (1 :d) (1 :e) (1 :f) (1 :e) (1 :f) (1 :c) (2 :c) (1 :b) (2 :c) (1 :f) (1 :g)
              (1 :a) (1 :g) (1 :f))
     :settimo ((1 :g) (1 :a) (1 :b) (1 :c) (2 :d) (2 :e) (2 :f) (2 :g) (2 :d) (2 :g) (2 :f) (2 :e)
                (2 :d) (1 :g) (1 :a) (1 :b) (2 :c) (1 :g) (2 :d) (2 :e) (2 :f) (2 :g) (1 :g) (1 :a)
                (1 :b) (2 :c) (1 :b) (1 :a) (1 :g))
     :ottavo ((1 :g) (1 :a) (1 :b) (2 :c) (2 :d) (1 :g) (1 :f) (1 :e) (1 :d) (1 :g) (1 :a) (1 :b)
               (2 :c) (2 :d) (2 :e) (1 :a) (2 :d) (2 :c) (1 :b) (1 :a) (1 :g) (1 :d) (1 :e) (1 :f)
              (1 :g) (1 :d) (1 :g) (1 :f) (1 :e) (1 :d) (1 :g)))

    :cromatico-semplice
    (:primo ((1 :d) (1 :e :flat) (1 :g :flat) (1 :g) (1 :a :flat) (1 :a) (1 :b :flat) (2 :d :flat)
             (2 :d) (2 :e :flat) (2 :d) (1 :d) (2 :d) (2 :c :sharp) (2 :d) (1 :g) (1 :b :flat)
             (1 :a) (1 :d) (1 :f) (1 :e) (1 :a))
     :secondo ((0 :a) (0 :b :flat) (1 :d :flat) (1 :d) (1 :e :flat) (1 :g :flat) (1 :g) (1 :a :flat)
               (1 :a) (1 :b :flat) (1 :a) (1 :f :sharp) (1 :g) (1 :e) (1 :f) (1 :d) (0 :a)
               (0 :b :flat) (0 :b) (1 :d) (0 :a) (1 :d)))))

(defun get-mode-pitch-list (genus number)
  (getf (getf *modes* genus) number))

(defparameter *modus-list* (list :primo :secondo :terzo :quarto :quinto :sesto :settimo :ottavo))
(defparameter *current-mode-number* :primo)

(defparameter *genus-list* (list :mani :diatonico-semplice :cromatico-semplice :enarmonico-semplice))
(defparameter *current-genus* :diatonico-semplice)

(defmacro step-through-list (candidate reference-list)
  `(let ((place (member ,candidate ,reference-list)))
    (if place
        (if (= 1 (length place))
            (setf ,candidate (first ,reference-list))
            (setf ,candidate (second place)))
        (setf ,candidate (first ,reference-list)))))

(defun step-genus ()
  (step-through-list *current-genus* *genus-list*)
  (format t "~&~a~%" *current-genus*))

(defun step-modus ()
  (step-through-list *current-mode-number* *modus-list*)
  (format t "~&~a~%" *current-mode-number*))

(defun plm ()
  (play-mode (get-mode-pitch-list *current-genus* *current-mode-number*) 44100))

(defparameter *global-mode-player* nil)

;; very dirty
(defparameter *global-mode-transposer* 36)

(defun plmi ()
  (format t "~&Ready for ~a / ~a.~%" *current-genus* *current-mode-number*)
  (setf *global-mode-player* (cons nil (get-mode-pitch-list *current-genus* *current-mode-number*))))

(defun progress-global-mode-player ()
  (when (first *global-mode-player*) (apply #'keyoff (first *global-mode-player*)))
  (setf *global-mode-player* (rest *global-mode-player*))
  (unless (zerop (length *global-mode-player*))
    (apply #'keyon (first *global-mode-player*))))






;;;; Calculations of beating frequencies based on cent tables

(defparameter *test-cents*
  '(:c 8.798 :c♯ -9.775 :d 2.933 :e♭ 15.640 :e -2.932 :f 11.731 :f♯ -7.819 :g 5.865 :g♯ -10.752 :a 0.000 :b♭ 13.686 :b♮ -5.864))

(defparameter *meantone*
  '(:c 10.265 :c♯ -13.686 :d 3.422 :e♭ 20.529 :e -3.421 :f 13.686 :f♯ -10.265 :g 6.843 :g♯ -17.108 :a 0.0 :b♭ 17.108 :b♮ -6.843))


(defparameter *12ed2-references*
  '(:c -9
    :c♯ -8
    :d -7
    :e♭ -6
    :e -5
    :f -4
    :f♯ -3
    :g -2
    :g♯ -1
    :a 0
    :b♭ 1
    :b♮ 2))

(defun calculate-interval-size (cent-table 12ed2-size-cents lower-note upper-note)
  (+ 12ed2-size-cents (- (getf cent-table lower-note)) (getf cent-table upper-note)))

(defun get-12ed2-frequency (reference-pitch-a2 note-name &optional (octave-shift 0))
  (* reference-pitch-a2 (expt 2 (/ (+ (getf *12ed2-references* note-name) (* octave-shift 12)) 12))))

(defun get-tuned-frequency (reference-pitch-a2 cent-table note-name &optional (octave-shift 0))
  (* (get-12ed2-frequency reference-pitch-a2 note-name octave-shift)
     (expt 2 (/ (getf cent-table note-name) 1200))))


(defun get-beating-frequency (interval-ratio reference-pitch-a2 cent-table
                              lower-note lower-octave-shift upper-note upper-octave-shift)
  (- (* (numerator interval-ratio)
        (get-tuned-frequency reference-pitch-a2 cent-table lower-note lower-octave-shift))
     (* (denominator interval-ratio)
        (get-tuned-frequency reference-pitch-a2 cent-table upper-note upper-octave-shift))))


(defun make-beating-table (reference-pitch-a2 cent-table)
  (loop for entry in '((3/2 :d 0 :a 0 "d'--a'" )
                          (3/2 :g -1 :d 0 "g--d'")
                          (3/2 :c 0 :g 0 "c'--g'")
                          (3/2 :f -1 :c 0 "f--c'")
                          (3/2 :b♭ -1 :f 0 "b♭--f'")
                          (3/2 :e♭ -1 :b♭ -1 "e♭--b♭")
                          (3/2 :a -1 :e 0 "a--e'")
                          (3/2 :e -1 :b♮ -1 "e--b♮")
                          (3/2 :b♮ -1 :f♯ 0 "b♮--f♯'")
                          (3/2 :f♯ -1 :c♯ 0 "f♯--c♯'")
                          (3/2 :c♯ 0 :g♯ 0 "c♯'--g♯'")
                          (5/4 :f 0 :a 0 "f'--a'")
                          (5/4 :f -1 :a -1 "f--a")
                          (5/4 :b♭ -1 :d 0 "b♭--d'")
                          (5/4 :e♭ -1 :g -1 "e♭--g")
                          (5/4 :e♭ 0 :g 0 "e♭'--g'")
                          (5/4 :c 0 :e 0 "c'--e'")
                          (5/4 :g -1 :b♮ -1 "g--b♮")
                          (5/4 :d 0 :f♯ 0 "d'--f♯'")
                          (5/4 :a -1 :c♯ 0 "a--c♯'")
                          (5/4 :e 0 :g♯ 0 "e'--g♯'")
                          (5/4 :e -1 :g♯ -1 "e--g♯"))
           collect (format nil "~a & ~,2f per second \\\\"
                           (sixth entry)
                           (abs (get-beating-frequency (first entry)
                                                       reference-pitch-a2
                                                       cent-table
                                                       (second entry)
                                                       (third entry)
                                                       (fourth entry)
                                                       (fifth entry))))))

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
               (0 :b :flat) (0 :b) (1 :d) (0 :a) (1 :d))
     :terzo ((1 :e) (1 :g) (1 :a :flat) (1 :a) (1 :b :flat) (1 :b) (2 :d) (2 :e :flat) (2 :e)
             (2 :f) (2 :e) (1 :b) (2 :e) (1 :a) (2 :c) (1 :b) (1 :e) (1 :f) (1 :e) (1 :b)
             (2 :c) (1 :b) (1 :e) (1 :a) (1 :e) (1 :f) (1 :e)))
    :enarmonico-semplice
    (:primo ((1 :d) (1 :d nil :dot) (1 :g :flat) (1 :g :flat :dot) (1 :g) (1 :g nil :dot)
             (1 :a :flat) (1 :a :flat :dot) (1 :a) (1 :a nil :dot) (2 :d :flat) (2 :d :flat :dot)
             (2 :d) (2 :d :flat :dot) (2 :c :sharp) (2 :d :flat :dot) (2 :d) (1 :a) (1 :d))
    :secondo ((0 :a) (0 :a nil :dot) (1 :d :flat) (1 :d :flat :dot) (1 :d) (1 :d nil :dot)
               (1 :g :flat) (1 :g :flat :dot) (1 :g) (1 :g nil :dot) (1 :a :flat) (1 :a :flat :dot)
               (1 :a) (1 :d) (1 :d nil :dot) (1 :e :flat) (1 :d nil :dot) (1 :d) (0 :a) (1 :d))
    :terzo ((1 :e) (1 :g :sharp) (1 :a :flat :dot) (1 :a) (1 :a nil :dot) (1 :b :flat)
             (1 :b :flat :dot) (1 :b) (2 :d :sharp) (2 :e :flat :dot) (2 :e) (2 :e nil :dot)
             (2 :f) (2 :e nil :dot) (2 :e) (1 :b) (1 :b nil :dot) (2 :c) (1 :b nil :dot)
             (1 :b) (1 :e) (1 :e nil :dot) (1 :f) (1 :e nil :dot) (1 :e))
    :quarto ((1 :e) (1 :e :flat :dot) (1 :d :sharp) (0 :b) (0 :b nil :dot) (1 :c) (0 :b nil :dot)
             (0 :b) (1 :e) (1 :g :sharp) (1 :a :flat :dot) (1 :a) (1 :a nil :dot) (1 :b :flat)
             (1 :b :flat :dot) (1 :b) (1 :e) (1 :e nil :dot) (1 :f) (1 :e nil :dot) (1 :e)
             (1 :e :flat) (1 :e) (0 :b)))))

(progn
  (keyon 0 :d)
  (keyon 0 :a)
  (keyon 1 :d))

(progn
  (keyoff 0 :d)
  (keyoff 0 :a)
  (keyoff 1 :d))


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


  fifth & d\\str--a\\str & 2.58 per second\\\\

(defparameter *dict-ratio-interval-name* '((3/2 . "fifth")
                                           (5/4 . "major third")
                                           (6/5 . "minor third")))

(defun lookup-interval-name-from-ratio (ratio)
  (cdr (assoc ratio *dict-ratio-interval-name*)))

(defun make-beating-table (reference-pitch-a2 cent-table)
  (loop for entry in '((3/2 :d 0 :a 0 "d\\str--a\\str" )
                          (3/2 :g -1 :d 0 "g--d\\str")
                          (3/2 :c 0 :g 0 "c\\str--g\\str")
                          (3/2 :f -1 :c 0 "f--c\\str")
                          (3/2 :b♭ -1 :f 0 "b♭--f\\str")
                          (3/2 :e♭ -1 :b♭ -1 "e♭--b♭")
                          (3/2 :a -1 :e 0 "a--e\\str")
                          (3/2 :e -1 :b♮ -1 "e--b♮")
                          (3/2 :b♮ -1 :f♯ 0 "b♮--f♯\\str")
                          (3/2 :f♯ -1 :c♯ 0 "f♯--c♯\\str")
                          (3/2 :c♯ 0 :g♯ 0 "c♯\\str--g♯\\str")
                          (5/4 :f 0 :a 0 "f\\str--a\\str")
                          (5/4 :f -1 :a -1 "f--a")
                          (5/4 :b♭ -1 :d 0 "b♭--d\\str")
                          (5/4 :e♭ -1 :g -1 "e♭--g")
                          (5/4 :e♭ 0 :g 0 "e♭\\str--g\\str")
                          (5/4 :c 0 :e 0 "c\\str--e\\str")
                          (5/4 :g -1 :b♮ -1 "g--b♮")
                          (5/4 :d 0 :f♯ 0 "d\\str--f♯\\str")
                          (5/4 :a -1 :c♯ 0 "a--c♯\\str")
                          (5/4 :e 0 :g♯ 0 "e\\str--g♯\\str")
                          (5/4 :e -1 :g♯ -1 "e--g♯"))
           collect (format nil "~a & ~a & ~,2f per second \\\\"
                           (lookup-interval-name-from-ratio (first entry))
                           (sixth entry)
                           (abs (get-beating-frequency (first entry)
                                                       reference-pitch-a2
                                                       cent-table
                                                       (second entry)
                                                       (third entry)
                                                       (fourth entry)
                                                       (fifth entry))))))


(defun concat-string (string-a string-b)
  (concatenate 'string string-a string-b))

(defun concat-strings (list-of-strings)
  (reduce #'concat-string list-of-strings))

(defun generate-tex-code (reference-pitch cent-table tuning-description)
  (format nil "
\\documentclass{standalone}
\\usepackage[utf8]{inputenc}
\\usepackage{longtable}
\\usepackage{booktabs}
\\usepackage{siunitx}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{soul}
\\usepackage{csquotes}
\\usepackage{newunicodechar}
\\newunicodechar{♮}{$\\natural$}
\\newunicodechar{♭}{$\\flat$}
\\newunicodechar{♯}{$\\sharp$}
\\newunicodechar{«}{\\flqq{}}
\\newunicodechar{»}{\\frqq{}}

\\def\\str{\\textquotesingle}
\\def\\cn{\\textcent}

\\begin{document}

\\begin{minipage}{8cm}
\\begin{center}
  {\\Large Beating frequencies}\\\\[2ex]
  Reference pitch for a\\str: ~a\\,Hz\\\\[1ex]
  ~a\\\\[1ex]
  Cent values, relative to 12\\caps{ed}2: ~{~a~~~~~a\\,\\cn~^, ~}
\\end{center}
\\begin{longtable}{p{2cm}p{1cm}p{3cm}}
  \\toprule
  ~{~a~}
  \\bottomrule
\\end{longtable}
\\end{minipage}
\\end{document}"
               reference-pitch
               tuning-description
               cent-table
               (make-beating-table reference-pitch cent-table)))

(defun write-tex-file (file-path reference-pitch cent-table tuning-description)
  (with-open-file (file file-path :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file (generate-tex-code reference-pitch cent-table tuning-description))))


(defun generate-all-tables ()
  (let ((1/4-meantone '(:c 10.265 :c♯ -13.686 :d 3.422 :e♭ 20.529 :e -3.421 :f 13.686 :f♯ -10.265 :g 6.843 :g♯ -17.108 :a 0.0 :b♭ 17.108 :b♮ -6.843))
        (1/6-meantone '(:c 4.89 :c♯ -6.52 :d 1.63 :e♭ 9.78 :e -1.63 :f 6.52 :f♯ -4.89 :g 3.26 :g♯ -8.15 :a 0 :b♭ 8.15 :b♮ -3.26))
        (kirnberger-iii '(:c 10.264 :c♯ 0.489 :d 3.421 :e♭ 4.399 :e -3.422 :f 8.309 :f♯ 0.488 :g 6.842 :g♯ 2.444 :a 0.000 :b♭ 6.354 :b♮ -1.467))
        (neidhardt-grosse-stadt '(:c 5.865 :c♯ 1.955 :d 1.955 :e♭ 3.910 :e 0.000 :f 3.910 :f♯ 1.955 :g 1.955 :g♯ 1.955 :a 0.000 :b♭ 3.910 :b♮ 1.955))
        (vallotti '(:c 5.865 :c♯ 0.000 :d 1.955 :e♭ 3.910 :e -1.955 :f 7.820 :f♯ -1.955 :g 3.910 :g♯ 1.955 :a 0.000 :b♭ 5.865 :b♮ -3.910))
        (werckmeister-iv '(:c 7.539 :c♯ -2.236 :d -5.307 :e♭ 5.026 :e 1.955 :f 5.584 :f♯ 2.513 :g 6.090 :g♯ -0.281 :a 0.000 :b♭ 6.981 :b♮ 3.910))
        (werckmeister-iii '(:c 0.000 :c♯ -3.910 :d 3.910 :e♭ 0.000 :e -3.910 :f 3.910 :f♯ 0.000 :g 1.955 :g♯ -7.820 :a 0.000 :b♭ 1.955 :b♮ -1.955)))
    (write-tex-file "meantone-1-4-comma-415.tex" 415 1/4-meantone "Regular 1/4-comma meantone, with B♭, E♭, F♯, C♯ and G♯.")
    (write-tex-file "meantone-1-4-comma-440.tex" 440 1/4-meantone "Regular 1/4-comma meantone, with B♭, E♭, F♯, C♯ and G♯.")
    (write-tex-file "meantone-1-6-comma-415.tex" 415 1/6-meantone "Regular 1/6-comma meantone, with B♭, E♭, F♯, C♯ and G♯.")
    (write-tex-file "meantone-1-6-comma-440.tex" 440 1/6-meantone "Regular 1/6-comma meantone, with B♭, E♭, F♯, C♯ and G♯.")
    (write-tex-file "kirnberger-iii-415.tex" 415 kirnberger-iii "Kirnberger III, 1779.")
    (write-tex-file "kirnberger-iii-440.tex" 440 kirnberger-iii "Kirnberger III, 1779")
    (write-tex-file "neidhardt-grosse-stadt-415.tex" 415 neidhardt-grosse-stadt "Neidhardt, \\enquote{grosse Stadt} (1724)")
    (write-tex-file "neidhardt-grosse-stadt-440.tex" 440 neidhardt-grosse-stadt "Neidhardt, \\enquote{grosse Stadt} (1724)")
    (write-tex-file "vallotti-415.tex" 415 vallotti "Vallotti")
    (write-tex-file "vallotti-440.tex" 440 vallotti "Vallotti")
    (write-tex-file "werckmeister-iv-415.tex" 415 werckmeister-iv "Werckmeister IV")
    (write-tex-file "werckmeister-iv-440.tex" 440 werckmeister-iv "Werckmeister IV")
    (write-tex-file "werckmeister-iii-415.tex" 415 werckmeister-iii "Werckmeister III")
    (write-tex-file "werckmeister-iii-440.tex" 440 werckmeister-iii "Werckmeister III")
    ))

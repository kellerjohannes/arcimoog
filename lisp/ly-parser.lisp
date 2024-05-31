(in-package :ly-parser)



(defparameter *dict-ly-pitch* '((b . b♮)
                                (bes . b♭)
                                (es . e♭)
                                (as . a♭)
                                (des . d♭)
                                (ges . g♭)
                                (ces . c♭)
                                (fes . f♭)
                                (fis . f♯)
                                (cis . c♯)
                                (gis . g♯)
                                (dis . d♯)
                                (ais . a♯)
                                (eis . e♯)
                                (bis . b♯)))

(defun translate-ly-pitch (ly-pitch)
  (let ((result (cdr (assoc ly-pitch *dict-ly-pitch*))))
    (if result result ly-pitch)))

(defparameter *dict-ly-values* '((brevis . brevis)
                                 (1 . semibrevis)
                                 (2p . minima-dot)
                                 (2 . minima)
                                 (4 . semiminima)))

(defun translate-ly-value (ly-value)
  (cdr (assoc ly-value *dict-ly-values*)))

(defun translate-ly-pitch-data (ly-data)
  (mapcar (lambda (element)
            (if (numberp element)
                element
                (translate-ly-pitch element)))
          ly-data))








(defparameter *dict-intervals* '((quinta . ((e . b♮)
                                            (f . c)))
                                 (tritono . ((a . d♯)))
                                 (quarta . ((f . b♭)))
                                 (terza-maggiore . ((c . e)
                                                    (g . b♮)
                                                    (b♮ . d♯)
                                                    (d♭ . f)))
                                 (terza-minore . ((b♮ . d)
                                                  (c . e♭)
                                                  (d . f)
                                                  (e . g)
                                                  (g . b♭)
                                                  (a . c)))
                                 (tono . ((c . d)
                                          (a . b♮)
                                          (f♯ . g♯)
                                          (e♭ . f)
                                          (d♭ . e♭)))
                                 (semitono-maggiore . ((e . f)
                                                       (b♮ . c)
                                                       (c . d♭)
                                                       (g . a♭)
                                                       (a . b♭)
                                                       (f♯ . g)
                                                       (g♯ . a)
                                                       (a♯ . b♮)
                                                       (e♯ . f♯)))))

(defun search-interval (note-name-pair)
  (car (rassoc-if (lambda (x) (member note-name-pair x :test #'equal)) *dict-intervals*)))

(defun identify-interval (note-name-a note-name-b)
  ;; TODO implement rests
  ;; (format t "~&~a : ~a" note-name-a note-name-b)
  (if (or (eq note-name-a note-name-b))
      (list 'unisono)
      (let ((interval-original (search-interval (cons note-name-a note-name-b))))
        (if interval-original
            (list interval-original 'ascendente)
            (let ((interval-inverted (search-interval (cons note-name-b note-name-a))))
              (if interval-inverted
                  (list interval-inverted 'discendente)
                  (list 'unidentified)))))))

(defun parse-ly-notation (ly-data)
  (do* ((result nil)
        (rest-data (translate-ly-pitch-data ly-data) (rest (rest rest-data)))
        (previous-pitch (first rest-data)))
       ((null rest-data) (reverse result))
    ;; (format t "~&~a" (first rest-data))
    (push (append (identify-interval previous-pitch (first rest-data))
                  (list (translate-ly-value (second rest-data))))
          result)
    (setf previous-pitch (first rest-data))))

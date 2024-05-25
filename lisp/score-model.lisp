(defun def (node-symbol &rest property-list)
  ;; (setf (symbol-plist node-symbol) property-list)
  (loop for (key value) on property-list by #'cddr do
        (setf (get node-symbol key) value)))

(defun update-property (node-symbol property content)
  (setf (get node-symbol property) content))

(defun remove-property (node-symbol property)
  (remprop node-symbol property))

(defun print-node (node-symbol)
  (describe node-symbol))

(defun get-property (node-symbol property)
  (let ((result (get node-symbol property)))
    (if result result (lambda (state) (declare (ignore state))))))

(defun val (self-evaluating-expression)
  (lambda (state)
    (declare (ignore state))
    self-evaluating-expression))

(defun of (node-symbol property &optional local-state)
  (funcall (get-property node-symbol property) local-state))

(defmacro calc (&rest body)
  `(lambda (state)
     (declare (ignore state))
     ,@body))

(defmacro rel (&rest body)
  `(lambda (local-state)
     ,@body))



(defun chain (&rest intervals)
  (apply #'* intervals))

(defun series (number-of-elements interval)
  (expt interval number-of-elements))

(defun diff (interval-a interval-b)
  (cond ((> interval-a interval-b) (/ interval-a interval-b))
        ((< interval-a interval-b) (/ interval-b interval-a))
        (t 1/1)))

(defun asc (pitch interval)
  (* pitch interval))

(defun desc (pitch interval)
  (/ pitch interval))

(defun temper (interval fraction-of-comma &optional (size-of-comma 81/80))
  (* interval (expt size-of-comma fraction-of-comma)))

(defun edx (number-of-divisions &optional (divided-interval 2/1))
  (expt divided-interval (/ 1 number-of-divisions)))

(def 'ji.ottava 'size (val 2/1))
(def 'ji.quarta 'size (val 4/3))
(def 'ji.quinta 'size (val 3/2))
(def 'ji.terza-maggiore 'size (val 5/4))
(def 'ji.terza-minore 'size (val 6/5))
(def 'ji.tono-maggiore 'size (calc (diff (of 'ji.quinta 'size) (of 'ji.quarta 'size))))
(def 'ji.tono-minore 'size (calc (diff (of 'ji.terza-maggiore 'size)
                                          (of 'ji.tono-maggiore 'size))))
(def 'ji.semitono-maggiore 'size (calc (diff (of 'ji.quarta 'size)
                                                 (of 'ji.terza-maggiore 'size))))


(def 'mt.ottava 'size (val 2/1))
(def 'mt.quinta 'size (calc (temper (of 'ji.quinta 'size) -1/4)))
(def 'mt.quarta 'size (calc (diff (of 'mt.ottava 'size)
                                      (of 'mt.quinta 'size))))
(def 'mt.terza-maggiore 'size (val 5/4))
(def 'mt.tono 'size (calc (edx 2 (of 'mt.terza-maggiore 'size))))


(def '12ed2.ottava 'size (val 2/1))
(def '12ed2.quinta 'size (calc (series 7 (edx 12))))
(def '12ed2.quarta 'size (calc (series 5 (edx 12))))
(def '12ed2.tono 'size (calc (series 2 (edx 12))))


(def 'reference-pitch.1 'pitch (val 1/1))





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


(def 'time.longa
  'duration (val 1))

(def 'time.brevis
  'duration (calc (* 1/2 (of 'time.longa 'duration))))

(def 'time.semibrevis
  'duration (calc (* 1/2 (of 'time.brevis 'duration))))

(def 'time.minima
  'duration (calc (* 1/2 (of 'time.semibrevis 'duration))))


(def 'benedetti.origin
  'pitch (val (of tmp.reference-pitch 'pitch))
  'time (val 0)
  'duration (val 0)
  'child '(benedetti.v1.origin
           benedetti.v2.origin
           benedetti.v3.origin
           ))

(def 'benedetti.v1.origin
  'pitch (rel (asc local-state (of tmp.ottava 'size)))
  'time (val 0)
  'duration (val 0)
  'child 'benedetti.v1.n1)

(def 'benedetti.v2.origin
  'pitch (rel (asc local-state (of tmp.quinta 'size)))
  'time (val 0)
  'duration (val 0)
  'child 'benedetti.v2.n1)

(def 'benedetti.v3.origin
  'pitch (rel local-state)
  'time (val 0)
  'duration (val 0)
  'child 'benedetti.v3.n1)


(def 'benedetti.v1.n1
  'pitch (rel local-state)
  'time (rel (- local-state (of 'time.minima 'duration)))
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v1.n2)

(def 'benedetti.v1.n2
  'pitch (rel (asc local-state (of tmp.tono-maggiore 'size)))
  'time (rel (+ local-state (of 'time.semibrevis 'duration)))
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v1.n3)

(def 'benedetti.v1.n3
  'pitch (rel (desc local-state (of tmp.tono-minore 'size)))
  'time (rel (+ local-state (of 'time.semibrevis 'duration)))
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v1.n2)



(def 'benedetti.v2.n1
  'pitch (rel local-state)
  'time (rel local-state)
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v2.n2)

(def 'benedetti.v2.n2
  'pitch (rel (asc local-state (of tmp.tono-maggiore 'size)))
  'time (rel (+ local-state (of 'time.semibrevis 'duration)))
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v2.n3)

(def 'benedetti.v2.n3
  'pitch (rel (desc local-state (of tmp.tono-minore 'size)))
  'time (rel (+ local-state (of 'time.semibrevis 'duration)))
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v2.n2)



(def 'benedetti.v3.n1
  'pitch (rel local-state)
  'time (rel local-state)
  'duration (val (of 'time.minima 'duration))
  'child 'benedetti.v3.n2)

(def 'benedetti.v3.n2
  'pitch (rel (asc local-state (of tmp.quinta 'size)))
  'time (rel (+ local-state (of 'time.minima 'duration)))
  'duration (val (of 'time.minima 'duration))
  'child 'benedetti.v3.n3)

(def 'benedetti.v3.n3
  'pitch (rel (desc local-state (of tmp.tono-minore 'size)))
  'time (rel (+ local-state (of 'time.minima 'duration)))
  'duration (val (of 'time.semibrevis 'duration))
  'child 'benedetti.v3.n4)

(def 'benedetti.v3.n4
  'pitch (rel (desc local-state (of tmp.quarta 'size)))
  'time (rel (+ local-state (of 'time.semibrevis 'duration)))
  'duration (val (of 'time.minima 'duration))
  'child 'benedetti.v3.n2)




(defparameter *dict-ly-pitch* '((es . e♭)
                                (des . d♭)
                                (fis . f♯)
                                (eis . e♯)))

(defun translate-ly-pitch (ly-pitch)
  (let ((result (cdr (assoc ly-pitch *dict-ly-pitch*))))
    (if result result ly-pitch)))

(defun translate-ly-pitch-data (ly-data)
  (mapcar (lambda (element)
            (if (numberp element)
                element
                (translate-ly-pitch element)))
          ly-data))

(defparameter *dict-intervals* '((tono . ((c . d)
                                          (d♭ . e♭)))
                                 (terza-maggiore ((c . e)))
                                 (terza-minore . ((b . d)
                                                  (c . e♭)
                                                  (e . g)))
                                 (semitono-maggiore . ((c . d♭)
                                                       (f♯ . g)
                                                       (e♯ . f♯)))))

(defun search-interval (note-name-pair)
  (car (rassoc-if (lambda (x) (member note-name-pair x :test #'equal)) *dict-intervals*)))

(defun identify-interval (note-name-a note-name-b)
  ;; TODO implement rests
  (if (or (eq note-name-a note-name-b))
      (list 'unisono)
      (let ((interval-original (search-interval (cons note-name-a note-name-b))))
        (if interval-original
            (list interval-original 'ascendente)
            (let ((interval-inverted (search-interval (cons note-name-b note-name-a))))
              (if interval-inverted
                  (list interval-inverted 'discendente)
                  (list 'unidentified)))))))

(defparameter *dict-ly-values* '((1 . semibrevis)
                                 (2 . minima)
                                 (4 . semiminima)))

(defun translate-ly-value (ly-value)
  (cdr (assoc ly-value *dict-ly-values*)))


(defparameter *rossi-alto* '(b 1 b 2 d 2 c 2 c 4 c 4 es 1 des 1 c 1 ;; r 2
                             e 2 g 2 fis 1 eis 2 fis 1
                             ))

;; TODO implement octave-indication
;; TODO implement origin pitch

(defun parse-ly-notation (ly-data)
  (do ((result nil)
       (rest-data (translate-ly-pitch-data ly-data) (rest (rest rest-data)))
       (previous-pitch (first ly-data)))
      ((null rest-data) (reverse result))
    (push (append (identify-interval previous-pitch (first rest-data))
                  (list (translate-ly-value (second rest-data))))
          result)
    (setf previous-pitch (first rest-data))))

(defparameter *interval-combinations*
  '((ottava . ((quinta . quarta)
               (quinta-imperfetta . tritono)
               (sesta-minore . terza-maggiore)
               (sesta-maggiore . terza-minore)
               (settima-minore . tono)
               (settima-maggiore . semitono-maggiore)))
    (settima-maggiore . ((quinta . terza-maggiore)
                         (quarta . tritono)
                         (sesta-maggiore . tono)
                         (settima-minore . semitono-minore)))
    (settima-minore . ((quinta . terza-minore)
                       (quarta . quarta)
                       (terza-maggiore . quinta-imperfetta)))
    (sesta-maggiore . ((quarta . terza-maggiore)
                       (quinta . tono)))
    (sesta-minore . ((quarta . terza-minore)
                     (quinta . semitono-maggiore)))
    (quinta . ((terza-maggiore . terza-minore)
               (quarta . tono)
               (tritono . semitono-maggiore)))
    (quinta-imperfetta . ((terza-minore . terza-minore)
                          (quarta . semitono-maggiore)))
    (tritono . ((terza-maggiore . tono)
                (quarta . semitono-minore)))
    (quarta . ((terza-maggiore . semitono-maggiore)
               (terza-minore . tono)))
    (terza-piu-di-maggiore . ((terza-minore . semitono-maggiore)
                              (tono-maggiore . tono)))
    (terza-maggiore . ((tono . tono)
                       (terza-minore . semitono-minore)))
    (terza-minore . ((tono . semitono-maggiore)))
    (tono-maggiore . ((semitono-maggiore . semitono-maggiore)
                      (tono . diesis)))
    (tono . ((semitono-maggiore . semitono-minore)))
    (semitono-maggiore . ((semitono-minore . diesis)))
    (semitono-minore . ((diesis . diesis)))))

(defun find-interval-divisions (interval)
  (cdr (assoc interval *interval-combinations*)))

(defun member-in-pair (item pair)
  (or (eq item (car pair)) (eq item (cdr pair))))

(defun combine-intervals (interval-name-a interval-name-b)
  (cond ((eq interval-name-a 'unisono) interval-name-b)
        ((eq interval-name-b 'unisono) interval-name-a)
        (t (let ((combination-1 (cons interval-name-a interval-name-b))
                 (combination-2 (cons interval-name-b interval-name-a)))
             (car (rassoc-if (lambda (x)
                               (or (member combination-1 x :test #'equal)
                                   (member combination-2 x :test #'equal)))
                             *interval-combinations*))))))


(defun interval-division (interval-name-a interval-name-b)
  (let ((division (find interval-name-b
                        (find-interval-divisions interval-name-a)
                        :test #'member-in-pair)))
    (if (eq (car division) interval-name-b)
        (cdr division)
        (car division))))


(defun chain-intervals (interval-a interval-b)
  (format t "~&~a / ~a" interval-a interval-b)
  (cond ((eq (first interval-a) 'unisono) interval-b)
        ((eq (first interval-b) 'unisono) interval-a)
        ((and (eq (first interval-a) (first interval-b))
              (not (eq (second interval-a) (second interval-b))))
         (list 'unisono))
        ((eq (second interval-a) (second interval-b))
         (list (combine-intervals (first interval-a) (first interval-b))
               (second interval-a)))
        (t (let ((first-attempt (interval-division (first interval-a) (first interval-b))))
             (if first-attempt
                 (list first-attempt (second interval-a))
                 (let ((second-attempt (interval-division (first interval-b) (first interval-a))))
                   (if second-attempt
                       (list second-attempt (second interval-b))
                       nil)))))))

(defun interval-path (interval-list)
  (reduce #'chain-intervals interval-list))






(defparameter *performers* '())


(defun construct-performer (id property-list)
  (let ((performer (list 'id id)))
    (loop for (key value) on property-list by #'cddr do
          (setf (getf performer key) value))
    performer))

(defun find-performer-position (id)
  (position id *performers* :key (lambda (performer) (getf performer 'id))))

(defun defperformer (id &rest property-list)
  (let ((pos (find-performer-position id)))
    (if pos
        (setf (nth pos *performers*)
              (construct-performer id property-list))
        (push (construct-performer id property-list) *performers*))))

(defun get-performer (id)
  (find id *performers* :key (lambda (performer) (getf performer 'id))))

(defun get-performer-properties (id)
  (let ((performer (copy-list (get-performer id))))
    (remf performer 'id)
    performer))

(defun extract-all-ids ()
  (mapcar (lambda (performer) (getf performer 'id)) *performers*))

(defun find-next-available-id ()
  (let ((existing-ids (extract-all-ids)))
    (do ((id 0 (1+ id)))
        ((not (member id existing-ids)) id))))

(defun duplicate-performer (id)
  (let ((new-id (find-next-available-id)))
    (apply #'defperformer new-id (get-performer-properties id))
    new-id))

(defun of-performer (id property)
  (getf (get-performer id) property))

(defun set-of-performer (id property value)
  (setf (getf (nth (find-performer-position id) *performers*) property) value))

(defun remove-performer (id)
  (setf *performers*
        (remove-if (lambda (performer) (equal (getf performer 'id) id)) *performers*)))



(ql:quickload :incudine)

(incudine:rt-start)



(defun update-performer (current-node performer-id)
  (loop for (key value) on (symbol-plist current-node) by #'cddr do
    (when (of-performer performer-id key)
      (set-of-performer performer-id key
                        (of current-node key (of-performer performer-id key))))))

(defun terminate-performer (id)
  (remove-performer id)
  (format t "~&THE END~%"))

(defun process (origin performer-id callback-fun real-time-p &optional (count-down 20))
  (cond ((zerop (decf count-down)) (format t "~&Countdown reached zero.~%"))
        (t (let ((old-performer (get-performer-properties performer-id)))
             (update-performer origin performer-id)
             (funcall callback-fun origin performer-id)
             (let ((successors (get origin 'child)))
               (macrolet ((rec (active-successor active-id)
                            `(if real-time-p
                                 (incudine:at (+ (incudine:now) (of-performer performer-id 'time))
                                              #'process
                                              ,active-successor
                                              ,active-id
                                              callback-fun
                                              real-time-p
                                              count-down)
                                 (process ,active-successor
                                          ,active-id
                                          callback-fun
                                          real-time-p
                                          count-down))))
                 (cond ((null successors) (terminate-performer performer-id))
                       ((or (atom successors) (and (listp successors) (null (rest successors))))
                        (rec (if (listp successors) (car successors) successors) performer-id))
                       ((listp successors)
                        (rec (first successors) performer-id)
                        (dolist (successor (rest successors))
                          (let ((new-id (find-next-available-id)))
                            (apply #'defperformer new-id old-performer)
                            (rec successor new-id)))))))))))



(defparameter *printer* (lambda (origin performer-id)
                          (format t "~&P~a; ~a: ~a (~a)"
                                  (of-performer performer-id 'id)
                                  (symbol-name origin)
                                  (of-performer performer-id 'pitch)
                                  (of-performer performer-id 'time))))


(defparameter *tikz-drawer* (lambda (origin id)
                              (let ((time-factor 50)
                                    (pitch-factor 1/50)
                                    (dot-size 0.5))
                                (let ((time (* time-factor (of-performer id 'time)))
                                      (duration (* time-factor (of-performer id 'duration)))
                                      (pitch (* pitch-factor
                                                (vicentino-tunings:ratio->length
                                                 (of-performer id 'pitch)))))
                                  (format t "~&drawing ~a" (symbol-name origin))
                                  ;; (push (make-text (symbol-name origin)
                                  ;;                  (pt time (- pitch (* 3.5 dot-size))))
                                  ;;       *score*)
                                  (push (circ time pitch dot-size) *score*)
                                  (push (ln (pt time pitch)
                                            (pt (+ time duration) pitch)
                                            :style-update '(:line-type :thick))
                                        *score*)))))

(defparameter *score* nil)

(defun start ()
  (let ((tikz-backend (make-backend-tikz :filename "score.tex")))
    (setf *performers* '())
    (setf *score* nil)
    (defperformer 0 'time 0 'pitch 1/1 'duration 0)
    (process 'benedetti.origin 0 *tikz-drawer* nil 50)
    ;;(process 'score.o 0 *printer* nil)
    (draw-with-multiple-backends (list tikz-backend) *score*)
    (compile-tikz tikz-backend)))

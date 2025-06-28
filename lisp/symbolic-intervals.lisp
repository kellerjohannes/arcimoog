(in-package :arcimoog.symbolic-intervals)



;;; Navigating a tree

(defun find-interval-divisions (interval interval-tree)
  (cdr (assoc interval interval-tree)))

(defun inventory (interval-divisions)
  (remove-duplicates (alexandria:flatten interval-divisions)))

(defun process-subdivision (division-pair interval-tree)
  (let ((child-division-a (find-interval-divisions (car division-pair) interval-tree))
        (child-division-b (find-interval-divisions (cdr division-pair) interval-tree)))
    (append (if child-division-a
                (alexandria:flatten (process-subdivision (first child-division-a) interval-tree))
                (list (car division-pair)))
            (if child-division-b
                (alexandria:flatten (process-subdivision (first child-division-b) interval-tree))
                (list (cdr division-pair))))))

(defun collect-subdivisions (division-list interval-tree)
  (cond ((null division-list) nil)
        (t (cons (process-subdivision (first division-list) interval-tree)
                 (collect-subdivisions (rest division-list) interval-tree)))))

(defun same-size-p (interval-list-a interval-list-b)
  (and (subsetp interval-list-a interval-list-b) (subsetp interval-list-b interval-list-a)))

(defun smallest-divisions (interval-name interval-tree)
  (let ((subdivisions (find-interval-divisions interval-name interval-tree)))
    (when subdivisions
      (remove-duplicates (collect-subdivisions subdivisions interval-tree) :test #'same-size-p))))

(defun pair-plist (plist)
  (loop for (p v) on plist by #'cddr collect (list p v)))

(defun summarize-interval-list (interval-list)
  (let ((result))
    (dolist (interval interval-list (pair-plist result))
      (unless (getf result interval) (setf (getf result interval) 0))
      (incf (getf result interval)))))

(defun compare-interval-list (predicate interval-components-a interval-components-b)
  (loop for item in (mapcar (lambda (a b) (funcall predicate (second a) (second b)))
                            interval-components-a
                            interval-components-b)
        always item))

(defun all-smallest-divisions (interval-tree)
  (sort (mapcar (lambda (interval-name)
                  (cons interval-name (mapcar #'summarize-interval-list
                                              (smallest-divisions interval-name interval-tree))))
                (mapcar #'car interval-tree))
        (lambda (a b) (compare-interval-list #'>= a b))
        :key #'second))





(defun member-in-pair (item pair)
  (or (eq item (car pair)) (eq item (cdr pair))))

(defun combine-interval-names (interval-name-a interval-name-b interval-tree)
  (cond ((eq interval-name-a 'unisono) interval-name-b)
        ((eq interval-name-b 'unisono) interval-name-a)
        (t (let ((combination-1 (cons interval-name-a interval-name-b))
                 (combination-2 (cons interval-name-b interval-name-a)))
             (car (rassoc-if (lambda (x)
                               (or (member combination-1 x :test #'equal)
                                   (member combination-2 x :test #'equal)))
                             interval-tree))))))

(defun subdivide-interval-names (interval-name-a interval-name-b interval-tree)
  (let ((division (find interval-name-b
                        (find-interval-divisions interval-name-a interval-tree)
                        :test #'member-in-pair)))
    (if (eq (car division) interval-name-b)
        (cdr division)
        (car division))))

(defun delta-interval-names (interval-name-a interval-name-b interval-tree)
  (let ((first-attempt (subdivide-interval-names interval-name-a interval-name-b interval-tree)))
    (if first-attempt
        (values first-attempt 'first-is-container)
        (let ((second-attempt
                (subdivide-interval-names interval-name-b interval-name-a interval-tree)))
          (when second-attempt (values second-attempt 'second-is-container))))))

(defun name (interval)
  (first interval))

(defun direction (interval)
  (second interval))

(defun multiplier (interval)
  (third interval))

(defun make-interval (name direction multiplier)
  (list name direction multiplier))

(defun mirror (interval)
  (list (first interval)
        (case (second interval)
          (ascendente 'discendente)
          (discendente 'ascendente)
          (nil nil))
        (third interval)))

(defun combine-interval-names-mod (name-a name-b interval-tree identity-interval-name)
  (let ((tmp (subdivide-interval-names identity-interval-name name-a interval-tree)))
    (subdivide-interval-names name-b tmp interval-tree)))


;; Main function for interval operations

(defun chain-intervals (a b interval-tree identity-interval-name)
  "A and B are in the form of '(quinta ascendente 0), where the first element is an interval
name (symbol defined in INTERVAL-TREE), the second one is NIL (only for UNISONO) or 'ASCENDENTE or
'DISCENDENTE."
  ;; (format t "~&DEBUG: ~a:~a" a b)
  (cond ((and (null (direction b)) (null (direction a)) a))
        ((null (direction a)) b)
        ((null (direction b)) a)
        ((and (= (multiplier a) (multiplier b))
              (eq (name a) (name b))
              (not (eq (direction a) (direction b))))
         (make-interval 'unisono nil 0))
        ;; This clause is new and untested, seems to be fine though
        ((and (eq (name a) (name b))
              (> (multiplier a) (multiplier b))
              (not (eq (direction a) (direction b))))
         (make-interval identity-interval-name
                        (direction a)
                        (- (multiplier a) 1 (multiplier b))))
        ;; This clause has been changed, since it was never used in Willaert and seems to have been
        ;; wrong, seems to be fine though
        ((and (> (multiplier a) (multiplier b))
              (eq (name b) identity-interval-name))
         (make-interval (name a)
                        (direction a)
                        (funcall (if (eq (direction a) (direction b)) #'+ #'-)
                                 (multiplier a)
                                 (1+ (multiplier b)))))
        ((eq (direction a) (direction b))
         (if (eq (name a) identity-interval-name)
             (make-interval (name b)
                            (direction a)
                            (funcall (if (eq (direction a) (direction b)) #'+ #'-)
                                     (1+ (multiplier a))
                                     (multiplier b)))
             (let ((simple-combination (combine-interval-names (name a) (name b) interval-tree)))
               (if simple-combination
                   (make-interval simple-combination
                                  (direction a)
                                  (+ (multiplier a) (multiplier b)))
                   (make-interval (combine-interval-names-mod (name a)
                                                              (name b)
                                                              interval-tree
                                                              identity-interval-name)
                                  (direction a)
                                  (+ 1 (multiplier a) (multiplier b)))))))
        ;; This clause has been modified and not tested, seems to be fine though
        ((and ;; (= (multiplier a) (multiplier b))  ; this one was original
              (zerop (multiplier b)) ; this one is new
              (not (eq (direction a) (direction b))))
         (multiple-value-bind (attempt container)
             (delta-interval-names (name a) (name b) interval-tree)
           (when attempt
             (if (eq container 'first-is-container)
                 (make-interval attempt (direction a) (multiplier a))
                 (cond ((and (zerop (multiplier a)) (zerop (multiplier b)))
                        (make-interval attempt (direction b) (multiplier b)))
                       ((zerop (multiplier a))
                        (make-interval attempt (direction b) (1- (multiplier b))))
                       (t (make-interval attempt (direction b) (- (multiplier a)
                                                                  (multiplier b)))))))))
        ((not (eq (direction a) (direction b)))
         (if (> (multiplier a) (multiplier b))
             (let ((attempt (subdivide-interval-names identity-interval-name
                                                      (delta-interval-names (name a)
                                                                            (name b)
                                                                            interval-tree)
                                                      interval-tree)))
               (when attempt
                 (make-interval attempt (direction a) (- (multiplier a) (multiplier b) 1))))
             (let ((attempt (delta-interval-names (name a) (name b) interval-tree)))
               (when attempt
                 (make-interval attempt (direction b) (- (multiplier b) (multiplier a)))))))
        (t (format t "~&Unknown situation. This means that the algorithm in CHAIN-INTERVALS does not cover all possible cases."))))



(defun interval-path (interval-list interval-tree identity-interval)
  (reduce (lambda (a b) (chain-intervals a b interval-tree identity-interval)) interval-list))

(defun condense (interval-list-shorthand interval-tree identity-interval)
  (interval-path (mapcar (lambda (interval-shorthand)
                           (if (listp interval-shorthand)
                               (make-interval
                                (first interval-shorthand)
                                (if-exists (second interval-shorthand) 'ascendente)
                                (if-exists (third interval-shorthand) 0))
                               (make-interval interval-shorthand 'ascendente 0)))
                         interval-list-shorthand)
                 interval-tree
                 identity-interval))



;;; Handling note values (durations)

(defparameter *note-values* (make-hash-table))

(defun add-note-value (value-name number-or-relation)
  (setf (gethash value-name *note-values*) number-or-relation))

(defun init-note-values ()
  (add-note-value 'maxima 1)
  (add-note-value 'longa '(maxima 1/2))
  (add-note-value 'brevis '(longa 1/2))
  (add-note-value 'semibrevis '(brevis 1/2))
  (add-note-value 'minima '(semibrevis 1/2))
  (add-note-value 'semiminima '(minima 1/2))
  (add-note-value 'fusa '(semiminima 1/2))
  (add-note-value 'semifusa '(fusa 1/2)))

(init-note-values)

(defun get-note-value (value-name &optional (collector 4))
  (let ((result (gethash value-name *note-values*)))
    (cond ((numberp result) collector)
          ((null result) (format t "~&Note value ~a not valid." value-name))
          (t (get-note-value (first result) (* (second result) collector))))))





;;; Utilities for public access

(defparameter *interval-trees* (list (cons :guido (cons 'diapason *guido*))
                                     (cons :musica-mista (cons 'ottava *musica-mista*))
                                     (cons :vicentino-diatonico
                                           (cons 'ottava *vicentino-diatonico*))
                                     (cons :vicentino-cromatico
                                           (cons 'ottava *vicentino-cromatico*))
                                     (cons :vicentino-enarmonico
                                           (cons 'ottava *vicentino-enarmonico*))))

(defun get-interval-tree (tree-name)
  (cddr (assoc tree-name *interval-trees*)))

(defun get-identity-interval (tree-name)
  (second (assoc tree-name *interval-trees*)))


;;; Handling score data

(defun parse-interval (interval-data)
  (make-interval (lookup-terminus (first interval-data))
                 (if (second interval-data)
                     (case (second interval-data)
                       (:➚ 'ascendente)
                       (:➘ 'discendente)
                       (error "Interval direction keyword ~a is not valid." (second interval-data)))
                     'ascendente)
                 (if-exists (third interval-data) 0)))

(defun interpret-note-value (note-value &optional dotp)
  (let ((numeric-note-value (get-note-value note-value)))
    (if (numberp numeric-note-value)
        (* numeric-note-value (if dotp 3/2 1))
        (error "~a not a recognized note value." note-value))))

(defun parse-singing (singing-data)
  (values
   (interpret-note-value (lookup-terminus (first singing-data))
                         (eq (second singing-data) :dot))
   (let ((candidate (car (last singing-data))))
     (when (member candidate '(:word :sillable)) candidate))))

(defun parse-tacet (tacet-data)
  (interpret-note-value (lookup-terminus (first tacet-data))
                        (eq (second tacet-data) :dot)))

(defun extract-note-value (singing-data)
  (if (eq (second singing-data) :dot)
      (list (first singing-data) (second singing-data))
      (list (first singing-data))))


(defun parse-melody-data (melody-data)
  (let ((time-cursor 0))
    (mapcar (lambda (melody-item)
              (case (first melody-item)
                (:i (list :type :interval
                          :time time-cursor
                          :interval-object (parse-interval (rest melody-item))))
                (:s (multiple-value-bind (numeric-note-value attack)
                        (parse-singing (rest melody-item))
                      (list :type :sound
                            :note-value (extract-note-value (rest melody-item))
                            :duration numeric-note-value
                            :start-time time-cursor
                            :attack attack
                            :end-time (incf time-cursor numeric-note-value))))
                (:t (list :type :tacet
                          :note-value (rest melody-item)
                          :duration (parse-tacet (rest melody-item))
                          :start-time time-cursor
                          :end-time (incf time-cursor (parse-tacet (rest melody-item)))))
                (otherwise (error "Melody data ID ~a not valid." (first melody-item)))))
            melody-data)))

(defun parse-score (score-data)
  (mapcar (lambda (voice)
            (cons (first voice) (parse-melody-data (rest voice))))
          score-data))

(defun init-head (parsed-score &optional (root-pitch 1/1))
  (mapcar (lambda (voice)
            (cons (car voice)
                  `(:new-note-p t
                    :interval-to-origin ,(make-interval 'unisono nil 0)
                    :last-melodic-interval ,(make-interval 'unisono nil 0)
                    :last-melodic-interval-updated-p nil
                    :soundingp nil
                    :pitch ,root-pitch
                    :attack nil
                    :relative-intervals
                    ,(loop for reference-voice in parsed-score
                           unless (eq (car voice) (car reference-voice))
                             collect (cons (car reference-voice) nil)))))
          parsed-score))

(defmacro get-head-voice (head voice-name)
  `(cdr (assoc ,voice-name ,head)))

(defmacro get-head-voice-property (head voice-name property)
  `(getf (get-head-voice ,head ,voice-name) ,property))

(defmacro get-head-relative-interval (head origin-voice-name reference-voice-name)
  `(cdr (assoc ,reference-voice-name
               (get-head-voice-property ,head ,origin-voice-name :relative-intervals))))


(defun has-duration-p (voice-data-item)
  (member (getf voice-data-item :type) '(:sound :tacet)))

(defun find-closest-start-time (voice-data)
  (unless (null voice-data)
    (if (has-duration-p (first voice-data))
        (getf (first voice-data) :start-time)
        (find-closest-start-time (rest voice-data)))))

(defun identify-closest-event (parsed-score)
  (let ((candidate (labels ((find-first-candidate (score)
                              (when score
                                (if (cdr (first score))
                                    (cons (car (first score))
                                          (find-closest-start-time (cdr (first score))))
                                    (find-first-candidate (rest score))))))
                     (find-first-candidate parsed-score))))
    (dolist (voice (rest parsed-score) candidate)
      (when (rest voice)
        (let ((closest-local-end-time (find-closest-start-time (cdr voice))))
          (when (and closest-local-end-time (< closest-local-end-time (cdr candidate)))
            (setf candidate (cons (first voice) closest-local-end-time))))))))

(defun process-consonances (data)
  ;; Careful with consonanza-counting, since keyframes might multiply consonances.
  (values (let ((numbers nil))
            (labels ((counting-loop (rest-data)
                       (when rest-data
                         (if (assoc (first rest-data) numbers)
                             (incf (cdr (assoc (first rest-data) numbers)))
                             (push (cons (first rest-data) 1) numbers))
                         (counting-loop (rest rest-data)))))
              (counting-loop (mapcar #'first data))
              (sort (mapcar (lambda (counter-pair)
                              (cons (car counter-pair) (* 1/2 (cdr counter-pair))))
                            numbers)
                    #'>
                    :key #'cdr)))
          (sort (remove-if #'null (remove-duplicates (mapcar #'first data)))
                #'<
                :key (lambda (interval-name)
                       (calculate-interval-size (list interval-name 'ascendente 0))))))

(defparameter *stats-consonanze* nil)
(defparameter *stats-chords* nil)

(defun update-relative-intervals (tree-name parsed-score head)
  (let ((all-voice-names (mapcar #'car parsed-score)))
    (dolist (voice parsed-score)
      (dolist (reference-voice-name (remove (car voice) all-voice-names))
        (setf (get-head-relative-interval head (car voice) reference-voice-name)
              (when (and (get-head-voice-property head (car voice) :soundingp)
                         (get-head-voice-property head reference-voice-name :soundingp))
                (let ((consonanza (chain-intervals
                                   (get-head-voice-property head (car voice) :interval-to-origin)
                                   (mirror (get-head-voice-property head reference-voice-name
                                                                    :interval-to-origin))
                                   (get-interval-tree tree-name)
                                   (get-identity-interval tree-name))))
                  (push consonanza *stats-consonanze*)
                  (when (null consonanza)
                    (format t "~&WARNING: 'consonanza' between ~a and ~a unknown."
                            (get-head-voice-property head (car voice) :interval-to-origin)
                            (mirror (get-head-voice-property head reference-voice-name
                                                             :interval-to-origin))))
                  consonanza)
                )))))
  head)


;;; Tuning interlude, needs to be abstracted and put in a different file.
;;; STARTING HERE

;; JI preliminary solution

(defparameter *ji* '((tono . 9/8)
                     (semitono-maggiore . 16/15)
                     (quinta . 3/2)
                     (quarta . 4/3)
                     (terza-maggiore . 5/4)
                     (terza-minore . 6/5)
                     (sesta-minore . 8/5)
                     (sesta-maggiore . 5/3)
                     (settima-minore . 16/9)
                     (ottava . 2)))

(defun lookup-interval-size (interval-name)
  (let ((result (cdr (assoc interval-name *ji*))))
    (if (numberp result)
        result
        (error "Interval ~a does not have a defined size." interval-name))))

(defun calculate-interval-size (interval-object)
  (if (eq (first interval-object) 'unisono)
      1/1
      (funcall (if (eq (second interval-object) 'ascendente) #'* #'/)
               1/1
               (* (lookup-interval-size (first interval-object))
                  (expt 2 (third interval-object))))))

(defun modify-interval-size (current-pitch interval-object)
  (* current-pitch (calculate-interval-size interval-object)))

(defun update-pitch (head)
  (dolist (voice-name (mapcar #'first head) head)
    (when (get-head-voice-property head voice-name :last-melodic-interval-updated-p)
      (setf (get-head-voice-property head voice-name :pitch)
            (modify-interval-size (get-head-voice-property head voice-name :pitch)
                                  (get-head-voice-property head voice-name :last-melodic-interval)))
      (setf (get-head-voice-property head voice-name :last-melodic-interval-updated-p)
            nil))))


;; Preliminary xEDy

(defparameter *31ed2-indices* '((unisono . 0)
                                (diesis-minore . 1)
                                (diesis-maggiore . 2) (semitono-minore . 2)
                                (semitono-maggiore . 3)
                                (tono-minore . 4)
                                (tono . 5)
                                (tono-maggiore . 6)
                                (terza-minima . 7)
                                (terza-minore . 8)
                                (terza-più-di-minore . 9)
                                (terza-maggiore . 10)
                                (terza-più-di-maggiore . 11)
                                (quarta-minima . 12)
                                (quarta . 13)
                                (quarta-propinqua . 14)
                                (tritono . 15)
                                (quinta-imperfetta . 16)
                                (quinta-imperfetta-propinqua . 17)
                                (quinta . 18)
                                (quinta-propinqua . 19)
                                (sesta-minima . 20)
                                (sesta-minore . 21)
                                (sesta-più-di-minore . 22)
                                (sesta-maggiore . 23)
                                (sesta-più-di-maggiore . 24)
                                (settima-minima . 25)
                                (settima-minore . 26)
                                (settima-più-di-minore . 27)
                                (settima-maggiore . 28)
                                (settima-più-di-maggiore . 29)
                                (ottava-minore . 30)
                                (ottava . 31)))

(defun calculate-interval-size (interval-object)
  (funcall (case (second interval-object)
             (ascendente #'*)
             (discendente #'/)
             (otherwise #'*))
           1/1
           (* (expt 2 (/ (cdr (assoc (first interval-object) *31ed2-indices*)) 31))
              (expt 2 (third interval-object)))))

(defun update-pitch (head)
  (dolist (voice-name (mapcar #'first head) head)
    (when (get-head-voice-property head voice-name :last-melodic-interval-updated-p)
      (setf (get-head-voice-property head voice-name :pitch)
            (calculate-interval-size (get-head-voice-property head voice-name :interval-to-origin)))
      (setf (get-head-voice-property head voice-name :last-melodic-interval-updated-p)
            nil))))


;; Preliminary linear system

(defparameter *fifth-indices* '((settima-più-di-minore . 17)
                                (terza-più-di-minore . 16)
                                (sesta-più-di-minore . 15)
                                (tono-minore . 14)
                                (quinta-imperfetta-propinqua . 13)
                                (ottava-minore . 12)
                                (quarta-minima . 11)
                                (settima-minima . 10)
                                (terza-minima . 9)
                                (sesta-minima . 8)
                                (semitono-minore . 7) (diesis-maggiore . 7)
                                (tritono . 6)
                                (settima-maggiore . 5)
                                (terza-maggiore . 4)
                                (sesta-maggiore . 3)
                                (tono . 2)
                                (quinta . 1)
                                (unisono . 0)
                                (ottava . 0)
                                (quarta . -1)
                                (settima-minore . -2)
                                (terza-minore . -3)
                                (sesta-minore . -4)
                                (semitono-maggiore . -5)
                                (quinta-imperfetta . -6)
                                (settima-più-di-maggiore . -7)
                                (terza-più-di-maggiore . -8)
                                (sesta-più-di-maggiore . -9)
                                (tono-maggiore . -10)
                                (quinta-propinqua . -11)
                                (diesis-minore . -12)
                                (quarta-propinqua . -13)))

(defun lookup-fifth-index (interval-name)
  (cdr (assoc interval-name *fifth-indices*)))

(defun to-pitchclass (interval-ratio &optional (identity-interval 2/1))
  (cond ((< interval-ratio 1)
         (to-pitchclass (* interval-ratio identity-interval) identity-interval))
        ((>= interval-ratio identity-interval)
         (to-pitchclass (/ interval-ratio identity-interval) identity-interval))
        (t interval-ratio)))

(defun temper (interval-ratio amount &optional (comma 81/80))
  (* interval-ratio (expt comma amount)))

(defun linear-system (index generator-interval &optional (identity-interval 2/1))
  (to-pitchclass (expt generator-interval index) identity-interval))

(defun lookup-linear-system (interval-name)
  (linear-system (lookup-fifth-index interval-name)
                 (temper 3/2 -1/4) ; meantone
                 ;; 3/2 ; pythagorean
                 ))

(defun calculate-interval-size (interval-object)
  (case (first interval-object)
    (unisono 1/1)
    (ottava 2/1)
    (otherwise (funcall (if (eq (second interval-object) 'ascendente) #'* #'/) 1/1
                        (* (lookup-linear-system (first interval-object))
                           (expt 2 (third interval-object)))))))

(defun modify-interval-size (current-pitch interval-object)
  (* current-pitch (calculate-interval-size interval-object)))

(defun update-pitch (head)
  (dolist (voice-name (mapcar #'first head) head)
    (when (get-head-voice-property head voice-name :last-melodic-interval-updated-p)
      (setf (get-head-voice-property head voice-name :pitch)
            ;; (modify-interval-size (get-head-voice-property head voice-name :pitch)
            ;;                       (get-head-voice-property head voice-name :last-melodic-interval))
            (calculate-interval-size (get-head-voice-property head voice-name :interval-to-origin))
            )
      (setf (get-head-voice-property head voice-name :last-melodic-interval-updated-p)
            nil))))

;;; ENDING HERE

(defun print-keyframe-info (head keyframe)
  (format t "~%~%------------------------~%Keyframe ~a:" keyframe)
  (dolist (voice head)
    (cond ((getf (rest voice) :soundingp)
           (format t "~%  ~a to origin ~a by ~a, ~a (new note: ~a), at pitch ~a, relative:"
                   (first voice)
                   (getf (rest voice) :interval-to-origin)
                   (getf (rest voice) :last-melodic-interval)
                   (getf (rest voice) :soundingp)
                   (getf (rest voice) :new-note-p)
                   (getf (rest voice) :pitch))
           (dolist (relation (getf (rest voice) :relative-intervals))
             (when (cdr relation)
               (format t "~%    to ~a ~a" (car relation) (cdr relation)))))
          (t (format t "~% ~a tacet" (first voice))))
    (format t "~%")))

(defun print-melody-info (head keyframe voice-name)
  (format t "~&Keyframe ~a:" keyframe)
  (dolist (voice head)
    (when (eq (first voice) voice-name)
      (format t "~&  ~a moves to ~a (from origin) by ~a."
              (first voice)
              (getf (rest voice) :interval-to-origin)
              (getf (rest voice) :last-melodic-interval)))))

(defun process-score-keyframe (tree-name parsed-score head keyframe)
  (labels ((loop-over-voice (voice-name voice-data)
             (let ((melody-item (first voice-data)))
               (cond ((null melody-item)
                      (setf (get-head-voice-property head voice-name :soundingp) nil)
                      (format t "~&Voice ~a is done." voice-name)
                      voice-data)
                     ((and (has-duration-p melody-item)
                           (<= (getf melody-item :start-time)
                               keyframe))
                      (case (getf melody-item :type)
                        (:tacet (setf (get-head-voice-property head voice-name :soundingp) nil))
                        (:sound
                         (setf (get-head-voice-property head voice-name :new-note-p) t)
                         (setf (get-head-voice-property head voice-name :attack)
                               (getf melody-item :attack))
                         (setf (get-head-voice-property head voice-name :soundingp) t)))
                      (loop-over-voice voice-name (rest voice-data)))
                     ((and (eq (getf melody-item :type) :interval)
                           (<= (getf melody-item :time) keyframe))
                      (setf (get-head-voice-property head voice-name :last-melodic-interval)
                            (getf melody-item :interval-object))
                      (setf (get-head-voice-property head
                                                     voice-name
                                                     :last-melodic-interval-updated-p)
                            t)
                      (setf (get-head-voice-property head voice-name :interval-to-origin)
                            (chain-intervals (get-head-voice-property head
                                                                      voice-name
                                                                      :interval-to-origin)
                                             (getf melody-item :interval-object)
                                             (get-interval-tree tree-name)
                                             (get-identity-interval tree-name)))
                      (loop-over-voice voice-name (rest voice-data)))
                     (t voice-data)))))
    (dolist (voice head)
      (setf (getf (rest voice) :new-note-p) nil))
    (let ((updated-score (mapcar (lambda (voice)
                                   (cons (first voice)
                                         (loop-over-voice (car voice) (cdr voice))))
                                 parsed-score)))
      (values updated-score (update-relative-intervals tree-name updated-score head)))))



;;; Sequencer interlude, needs to be abstracted away and probably put somewhere else.
;;; STARTING HERE

(defparameter *mother-dict* '((:cantus . :soprano)
                              (:altus . :alto)
                              (:tenor . :tenore)
                              (:bassus . :basso)))

(defun lookup-mother-name (voice-name)
  (cdr (assoc voice-name *mother-dict*)))

(defun update-mothers (head)
  (dolist (voice head)
    (let ((mother-name (lookup-mother-name (first voice))))
      (am-mo:set-mother-pitch mother-name (* 2 (getf (rest voice) :pitch)))
      (when (getf (rest voice) :new-note-p)
        ;; (format t "~&DEBUG attack ~a" (getf (rest voice) :attack))
        (case (getf (rest voice) :attack)
          (:word (am-mo:trigger-natura-accent mother-name 1 0.4))
          (:sillable (am-mo:trigger-natura-accent mother-name 0.4 0.2))))
      (if (getf (rest voice) :soundingp)
          (unless (am-mo:mother-on-p mother-name) (am-mo:mother-on mother-name))
          (when (am-mo:mother-on-p mother-name) (am-mo:mother-off mother-name))))))

(defun compute-time (keyframe factor)
  (* keyframe factor (incudine:rt-sample-rate)))

;;; ENDING HERE


;;; Public functions

(defun score-reader-loop (tree-name score head start-time skip-pitch-update skip-timing-function)
  (let ((next-keyframe (cdr (identify-closest-event score))))
    (when next-keyframe
      (multiple-value-bind (new-score new-head)
          (process-score-keyframe tree-name score head next-keyframe)
        (setf new-head (update-pitch new-head))
        (print-keyframe-info new-head next-keyframe)
        ;; (print-melody-info new-head next-keyframe :bassus)
        (unless skip-pitch-update (update-mothers new-head))
        (when (cdr (identify-closest-event new-score))
          (if skip-timing-function
              (score-reader-loop tree-name
                                 new-score
                                 new-head
                                 start-time
                                 skip-pitch-update
                                 skip-timing-function)
              (incudine:at (+ start-time (compute-time (cdr (identify-closest-event new-score))
                                                       4))
                           #'score-reader-loop
                           tree-name
                           new-score
                           new-head
                           start-time
                           skip-pitch-update
                           skip-timing-function)))))))

(defun read-score (tree-name score-data &key (root-pitch 1/1)
                                          (skip-pitch-update nil)
                                          (skip-timing-function nil))
  (let ((parsed-score (parse-score score-data)))
    (score-reader-loop tree-name
                       parsed-score
                       (init-head parsed-score root-pitch)
                       (incudine:now)
                       skip-pitch-update
                       skip-timing-function)))



;; TODO Delete, when done with debugging.
(defparameter *pscore* (parse-score *mirabile*))

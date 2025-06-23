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
        (if (eq (second interval) 'ascendente)
            'discendente
            'ascendente)
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
        ((and (> (multiplier a) (multiplier b))
              (eq (name b) identity-interval-name))
         (make-interval (name a)
                        (direction b)
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
        ((and (= (multiplier a) (multiplier b))
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
    (if (numberp result)
        collector
        (get-note-value (first result) (* (second result) collector)))))




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
                       (:➘ 'discendente))
                     'ascendente)
                 (if-exists (third interval-data) 0)))

(defun interpret-note-value (note-value &optional dotp)
  (* (get-note-value note-value) (if dotp 3/2 1)))

(defun parse-singing (singing-data)
  (interpret-note-value (lookup-terminus (first singing-data))
                        (eq (second singing-data) :dot)))

(defun parse-tacet (tacet-data)
  (interpret-note-value (lookup-terminus (first tacet-data))
                        (eq (second tacet-data) :dot)))



(defun parse-melody-data (melody-data)
  (let ((time-cursor 0))
    (mapcar (lambda (melody-item)
              (case (first melody-item)
                (:i (list :type :interval
                          :interval-object (parse-interval (rest melody-item))))
                (:s (list :type :sound
                          :note-value (rest melody-item)
                          :duration (parse-singing (rest melody-item))
                          :start-time time-cursor
                          :end-time (incf time-cursor (parse-singing (rest melody-item)))))
                (:t (list :type :tacet
                          :note-value (rest melody-item)
                          :duration (parse-tacet (rest melody-item))
                          :start-time time-cursor
                          :end-time (incf time-cursor (parse-tacet (rest melody-item)))))))
            melody-data)))

(defun parse-score (score-data)
  (mapcar (lambda (voice)
            (list :voice-name (first voice)
                  :voice-data (parse-melody-data (rest voice))))
          score-data))

;; (defun get-voice-by-name (parsed-score voice-name)
;;   (dolist (voice parsed-score)
;;     (when (eq (getf voice :voice-name) voice-name)
;;       (return (cons voice-name (getf voice :voice-data))))))



;;; Public functions


(defun init-voice-data (parsed-score)
  (let ((head nil))
    (dolist (voice parsed-score head)
      (setf (getf head (getf voice :voice-name))
            `(:interval-to-origin nil
              :soundingp nil
              :pitch 1/1
              ,@(loop for reference-voice in parsed-score
                      unless (eq (first voice) (first reference-voice))
                        append (list :interval-to-voice (first reference-voice)
                                     :interval nil)))))))

;; (defun find-next-duration (voice-data)
;;   (unless (null voice-data)
;;     (format t "~&~a" voice-data)
;;     (if (member (first (first voice-data)) '(:s :t))
;;         (second (first voice-data))
;;         (find-next-duration (rest voice-data)))))



;; (defun identify-closest-event (parsed-score)
;;   (let ((candidate (cons (first (first parsed-score))
;;                          (find-next-duration (second (first parsed-score))))))
;;     (dolist (voice (rest parsed-score) candidate)
;;       (when (< (find-next-duration (second voice))
;;                (cdr candidate))
;;         (setf candidate (cons (first voice) (find-next-duration (second voice))))))))


;; TODO messy at this point. Maybe completely rethink.

;; (defun process-score-keyframe (parsed-score voice-cursor keyframe)
;;   (dolist (voice parsed-score (values parsed-score voice-cursor))
;;     (dolist (voice-item (second voice))
;;       (case (first voice-item)
;;         (:t (setf (getf :(getf voice-cursor (first voice)))))))))

(defun read-score (tree-name score-data)
  (do* ((parsed-score (parse-score score-data))
        (voice-cursor (init-voice-data parsed-score))
        (time-cursor 0)
        (end-flag nil))
       (end-flag nil)
       (let ((next-keyframe (identify-closest-event parsed-score)))
         (multiple-value-bind (parsed-score voice-cursor)
                              (process-score-keyframe parsed-score voice-cursor next-keyframe)
                              (setf time-cursor next-keyframe)))))



(defparameter *bassus* '((:s :brevis)
                         (:s :semibrevis :dot)
                         (:i :tono :➘)
                         (:s :minima)
                         (:s :brevis)
                         (:i :semitono-maggiore :➘)
                         (:s :brevis)
                         (:t :longa :dot)
                         (:t :semibrevis)))

(defparameter *tenor* '((:s :brevis)
                        (:i :terza-minore)
                        (:s :semibrevis :dot)
                        (:i :semitono-maggiore :➘)
                        (:s :minima)
                        (:s :brevis)
                        (:t :semibrevis)
                        (:i :tono :➘)
                        (:s :brevis)
                        (:i :terza-minore)
                        (:s :semibrevis :dot)
                        (:i :semitono-maggiore :➘)
                        (:s :minima)
                        (:s :brevis)
                        (:i :tono :➘)
                        (:s :brevis)))

(defparameter *altus* '((:t :semibrevis)
                        (:i :quinta)
                        (:s :brevis)
                        (:s :semibrevis :dot)
                        (:i :tono :➘)
                        (:s :minima)
                        (:s :brevis)
                        (:i :semitono-maggiore :➘)
                        (:s :brevis)
                        (:i :terza-minore)
                        (:s :semibrevis :dot)
                        (:i :tono :➘)
                        (:s :minima)
                        (:s :brevis)
                        (:i :semitono-maggiore :➘)
                        (:s :brevis)))

(defparameter *cantus* '((:t :longa :dot)
                         (:i :ottava)
                         (:s :brevis)
                         (:s :semibrevis :dot)
                         (:i :tono :➘)
                         (:s :minima)
                         (:s :brevis)
                         (:i :semitono-maggiore :➘)
                         (:s :brevis)
                         (:i :tono :➘)
                         (:s :brevis)))

(defparameter *score* (list (cons :cantus *cantus*)
                            (cons :altus *altus*)
                            (cons :tenor *tenor*)
                            (cons :bassus *bassus*)))

;; TODO Delete, when done with debugging.
(defparameter *pscore* (parse-score *score*))

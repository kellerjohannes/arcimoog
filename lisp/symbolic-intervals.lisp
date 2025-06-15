(in-package :arcimoog.symbolic-intervals)


;;; Definitions of interval trees

(defparameter *guido*
  '((tono . ((apotome . limma)))
    (semiditono . ((limma . tono)))
    (ditono . ((apotome . semiditono)
               (tono . tono)))
    (diatessaron . ((limma . ditono)
                    (tono . semiditono)))
    (tritono . ((apotome . diatessaron)
                (tono . ditono)))
    (semidiapente . ((limma . diatessaron)
                     (semiditono . semiditono)))
    (diapente . ((apotome . semidiapente)
                 (limma . tritono)
                 (tono . diatessaron)
                 (ditono . semiditono)))
    (sesta-minore . ((limma . diapente)
                     (tono . semidiapente)
                     (semiditono . diatessaron)))
    (sesta-maggiore . ((apotome . sesta-minore)
                       (tono . diapente)
                       (semiditono . tritono)
                       (ditono . diatessaron)))
    (settima-minore . ((limma . sesta-maggiore)
                       (tono . sesta-minore)
                       (semiditono . diapente)
                       (ditono . semidiapente)
                       (diatessaron . diatessaron)))
    (settima-maggiore . ((apotome . settima-minore)
                         (tono . sesta-maggiore)
                         (ditono . diapente)
                         (diatessaron . tritono)))
    (diapason . ((limma . settima-maggiore)
                 (tono . settima-minore)
                 (semiditono . sesta-maggiore)
                 (ditono . sesta-minore)
                 (diatessaron . diapente)
                 (tritono . semidiapente)))))

(defparameter *musica-mista*
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
                       (tritono . terza-minore)
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
    (terza-maggiore . ((tono . tono)
                       (terza-minore . semitono-minore)))
    (terza-minore . ((tono . semitono-maggiore)))
    (tono . ((semitono-maggiore . semitono-minore)))
    (semitono-maggiore . ((semitono-minore . diesis)))
    (semitono-minore . ((diesis . diesis)))))

(defparameter *vicentino*
  ;; TODO Create subtrees *vicentino-cromatico* and *vicentino-diatonico* by deleting stuff from
  ;; this tree, which needs to be renamed to *vicentino-enarmonico*.
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
                       (tritono . terza-minore)
                       (quinta . tono)))
    (sesta-minore . ((quarta . terza-minore)
                     (quinta . semitono-maggiore)))
    (quinta . ((quinta-imperfetta-propinqua . diesis-minore)
               (quinta-imperfetta . semitono-minore)
               (tritono . semitono-maggiore)
               (quarta-propinqua . tono-minore)
               (quarta . tono)
               (quarta-minima . tono-maggiore)
               (terza-più-di-maggiore . terza-minima)
               (terza-maggiore . terza-minore)
               (terza-più-di-minore . terza-più-di-minore)))
    (quinta-imperfetta-propinqua . ((quinta-imperfetta . diesis-minore)
                                    (tritono . semitono-minore)
                                    (quarta-propinqua . semitono-maggiore)
                                    (quarta . tono-minore)
                                    (quarta-minima . tono)
                                    (terza-più-di-maggiore . tono-maggiore)
                                    (terza-maggiore . terza-minima)
                                    (terza-più-di-minore . terza-minore)))
    (quinta-imperfetta . ((tritono . diesis-minore)
                          (quarta-propinqua . semitono-minore)
                          (quarta . semitono-maggiore)
                          (quarta-minima . tono-minore)
                          (terza-più-di-maggiore . tono)
                          (terza-maggiore . tono-maggiore)
                          (terza-più-di-minore . terza-minima)
                          (terza-minore . terza-minore)))
    (tritono . ((quarta-propinqua . diesis-minore)
                (quarta . semitono-minore)
                (quarta-minima . semitono-maggiore)
                (terza-più-di-maggiore . tono-minore)
                (terza-maggiore . tono)
                (terza-più-di-minore . tono-maggiore)
                (terza-minore . terza-minima)))
    (quarta-propinqua . ((quarta . diesis-minore)
                         (quarta-minima . semitono-minore)
                         (terza-più-di-maggiore . semitono-maggiore)
                         (terza-maggiore . tono-minore)
                         (terza-più-di-minore . tono)
                         (terza-minore . tono-maggiore)
                         (terza-minima . terza-minima)))
    (quarta . ((quarta-minima . diesis-minore)
               (terza-più-di-maggiore . semitono-minore)
               (terza-maggiore . semitono-maggiore)
               (terza-più-di-minore . tono-minore)
               (terza-minore . tono)
               (terza-minima . tono-maggiore)))
    (quarta-minima . ((terza-più-di-maggiore . diesis-minore)
                      (terza-maggiore . semitono-minore)
                      (terza-più-di-minore . semitono-maggiore)
                      (terza-minore . tono-minore)
                      (terza-minima . tono)
                      (tono-maggiore . tono-maggiore)))
    (terza-più-di-maggiore . ((terza-maggiore . diesis-minore)
                              (terza-più-di-minore . semitono-minore)
                              (terza-minore . semitono-maggiore)
                              (terza-minima . tono-minore)
                              (tono-maggiore . tono)))
    (terza-maggiore . ((terza-più-di-minore . diesis-minore)
                       (terza-minore . semitono-minore)
                       (terza-minima . semitono-maggiore)
                       (tono-maggiore . tono-minore)
                       (tono . tono)))
    (terza-più-di-minore . ((terza-minore . diesis-minore)
                            (terza-minima . semitono-minore)
                            (tono-maggiore . semitono-maggiore)
                            (tono . tono-minore)))
    (terza-minore . ((terza-minima . diesis-minore)
                     (tono-maggiore . semitono-minore)
                     (tono . semitono-maggiore)
                     (tono-minore . tono-minore)))
    (terza-minima . ((tono-maggiore . diesis-minore)
                     (tono . semitono-minore)
                     (tono-minore . semitono-maggiore)))
    (tono-maggiore . ((tono . diesis-minore)
                      (tono-minore . semitono-minore)
                      (semitono-maggiore . semitono-maggiore)))
    (tono . ((tono-minore . diesis-minore)
             (semitono-maggiore . semitono-minore)))
    (tono-minore . ((semitono-maggiore . diesis-minore)
                    (semitono-minore . semitono-minore)))
    (semitono-maggiore . ((diesis-maggiore . diesis-minore)))
    (semitono-minore . ((diesis-minore . diesis-minore)))
    ;;(diesis-minore . ((comma . comma)))
    )
  "All melodic intervals according to Vicentino's \"Libro primo della prattica musicale\". There is a
conceptual distinction between the \"diesis enarmonico maggiore\" and the \"semitono minore\". No
\"propinquissimo\"-intervals are included. The \"comma\" is defined as half the size of the \"diesis
minore\", but not included in the interval tree. For the \"terza manca di minore\", the alternative
name \"terza minima\" is used. \"quarta minima\" is defined, although never mentioned by Vicentino.")



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

;; Probably obsolete, replaced by the public function CONDENSE-MELODY.
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
  (add-note-value 'longa 1)
  (add-note-value 'brevis '(longa 1/2))
  (add-note-value 'semibrevis '(brevis 1/2))
  (add-note-value 'minima '(semibrevis 1/2))
  (add-note-value 'semiminima '(minima 1/2))
  (add-note-value 'fusa '(semiminima 1/2)))

(init-note-values)

(defun get-note-value (value-name &optional (collector 1))
  (let ((result (gethash value-name *note-values*)))
    (if (numberp result)
        collector
        (get-note-value (first result) (* (second result) collector)))))




;;; Utilities for public access

(defparameter *interval-trees* (list (cons :guido (cons 'diapason *guido*))
                                     (cons :musica-mista (cons 'ottava *musica-mista*))
                                     (cons :vicentino (cons 'ottava *vicentino*))))

(defun get-interval-tree (tree-name)
  (cddr (assoc tree-name *interval-trees*)))

(defun get-identity-interval (tree-name)
  (second (assoc tree-name *interval-trees*)))

(defun parse-interval (interval-data)
  (make-interval (first interval-data)
                 (if (second interval-data)
                     (case (second interval-data)
                       (:➚ 'ascendente)
                       (:➘ 'discendente))
                     'ascendente)
                 (if-exists (third interval-data) 0)))

(defun parse-singing (singing-data)
  ;; For now this does nothing.
  singing-data)

(defun parse-tacet (tacet-data)
  ;; For now this does nothing.
  tacet-data
  )


;;; Public functions

(defun parse-melody-data (melody-data)
  (mapcar (lambda (melody-item)
            (case (first melody-item)
              (:i (parse-interval (rest melody-item)))
              (:s (parse-singing (rest melody-item)))
              (:t (parse-tacet (rest melody-item)))))
          melody-data))

(defun filter-pitch-data (melody-data)
  (remove-if (lambda (item)
               (member (first item) '(:s :t)))
             melody-data))

(defun condense-melody (tree-name melody-data)
  (interval-path (parse-melody-data (filter-pitch-data melody-data))
                 (get-interval-tree tree-name)
                 (get-identity-interval tree-name)))




(defparameter *test-melody* '((:i quinta :➚) (:s semibrevis) (:i tono :➘) (:t brevis)))

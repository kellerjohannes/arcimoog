(in-package :symbolic-intervals)

(defparameter *ordine-naturale*
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
    (terza-piu-di-maggiore . ((terza-minore . semitono-maggiore)
                              (tono-maggiore . tono)))
    (terza-maggiore . ((tono . tono)
                       (terza-minore . semitono-minore)))
    (terza-minore . ((tono . semitono-maggiore)))
    (tono-maggiore . ((semitono-maggiore . semitono-maggiore)
                      (tono . diesis)))
    (tono . ((semitono-maggiore . semitono-minore)))
    (tono-minore . ((semitono-minore . semitono-minore)
                    (semitono-maggiore . diesis)))
    (semitono-maggiore . ((semitono-minore . diesis)))
    (semitono-minore . ((diesis . diesis)))))

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

;; probably never used
(defun get-larger-multiplier (interval-a interval-b)
  (if (> (multiplier interval-a) (multiplier interval-b))
      (multiplier interval-a)
      (multiplier interval-b)))


(defun combine-interval-names-mod (name-a name-b interval-tree identity-interval-name)
  (let ((tmp (subdivide-interval-names identity-interval-name name-a interval-tree)))
    (subdivide-interval-names name-b tmp interval-tree)))

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
  ;; (format t "~&DEBUG: NEW CHAIN.")
  (reduce (lambda (a b) (chain-intervals a b interval-tree identity-interval)) interval-list))

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


(defvar tmp.tono-maggiore 'ji.tono-maggiore)
(defvar tmp.tono-minore 'ji.tono-minore)

(progn
  (setf tmp.tono-maggiore 'ji.tono-maggiore
        tmp.tono-minore 'ji.tono-minore))

(progn
  (setf tmp.tono-maggiore 'mt.tono
        tmp.tono-minore 'mt.tono))


(def 'score.o
  'child 'score.a
  'pitch (val 1/1)
  'time (val 44100))

(def 'score.a
  'child 'score.b
  'pitch (rel (asc local-state (of tmp.tono-maggiore 'size)))
  'time (val 44100))

(def 'score.b
  'child '(score.c score.alt1)
  ;;'child 'score.c
  'pitch (rel (asc local-state (of tmp.tono-maggiore 'size)))
  'time (val 44100))

(def 'score.c
  'child '(score.d score.slow1)
  'pitch (rel (desc local-state (of tmp.tono-minore 'size)))
  'time (val 44100))

(def 'score.d
  'child 'score.a
  'pitch (rel (desc local-state (of tmp.tono-minore 'size)))
  'time (val 44100))

(def 'score.slow1
  'child 'score.slow2
  'pitch (val 2000)
  'time (val 1500000))

(def 'score.slow2
  'pitch (val 5000)
  'time (val 1500000))

(def 'score.alt1
  'child 'score.alt2
  'pitch (val 17)
  'time (val 8000))

(def 'score.alt2
  'child 'score.alt3
  'pitch (val 18)
  'time (val 8000))

(def 'score.alt3
  'child 'score.alt4
  'pitch (val 19)
  'time (val 8000))

(def 'score.alt4
  'pitch (val 20)
  'time (val 8000))


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


(defun start ()
  (setf *performers* '())
  (defperformer 0 'time 0 'pitch 1/1)
  (process 'score.o 0))


(ql:quickload :incudine)

(incudine:rt-start)

(defun process (origin performer-id &optional (count-down 20))
  (cond ((zerop (decf count-down))
         (format t "~&Countdown reached zero.~%"))
        (t (loop for (key value) on (symbol-plist origin) by #'cddr do
          (when (of-performer performer-id key)
            (set-of-performer performer-id key (of origin key (of-performer performer-id key)))))
           (format t "~&P~a; ~a: ~a (~a)"
                   (of-performer performer-id 'id)
                   (symbol-name origin)
                   (of-performer performer-id 'pitch)
                   (of-performer performer-id 'time))
           (let ((successors (get origin 'child)))
             (cond ((null successors)
                    (remove-performer performer-id)
                    (format t "~&THE END~%"))
                   ((or (atom successors) (and (listp successors) (null (rest successors))))
                    (incudine:at (+ (incudine:now) (of-performer performer-id 'time))
                                 #'process
                                 (if (listp successors) (car successors) successors)
                                 performer-id
                                 count-down))
                   ((listp successors)
                    (incudine:at (+ (incudine:now) (of-performer performer-id 'time))
                                 #'process
                                 (first successors)
                                 performer-id
                                 count-down)
                    (dolist (successor (rest successors))
                      (incudine:at (+ (incudine:now) (of-performer performer-id 'time))
                                   #'process
                                   successor
                                   (duplicate-performer performer-id)
                                   count-down)))
                   (t nil))))))

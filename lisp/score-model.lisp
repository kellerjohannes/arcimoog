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


;;; from here: very sketchy, needs to be reimplemented

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

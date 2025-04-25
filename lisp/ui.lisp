(in-package :arcimoog.ui)

(defparameter *parameter-hooks* (make-hash-table))
(defparameter *parameter-values* (make-hash-table))

(defun register-value-hook (name hook-function)
  (let ((hooks (gethash name *parameter-hooks*)))
    (if (listp hooks)
        (push hook-function (gethash name *parameter-hooks*))
        (setf (gethash name *parameter-hooks*) (list hook-function))))
  (funcall hook-function (gethash name *parameter-values*)))

(defun update-value (name new-value)
  (setf (gethash name *parameter-values*) new-value)
  (dolist (callback-function (gethash name *parameter-hooks*))
    (when new-value
      (funcall callback-function new-value))))

(defun build-cv-meters (parent)
  (macrolet ((meter-row (parent name)
               (let ((row-container (gensym))
                     (visual-container (gensym))
                     (meter-body (gensym))
                     (meter-label (gensym)))
                 `(let* ((,row-container (clog:create-div ,parent :class "meter-row"))
                         (,visual-container (clog:create-div ,row-container
                                                             :class "meter-visual-container"))
                         (,meter-body (clog:create-div ,visual-container
                                                       :class "meter-visual-filler"))
                         (,meter-label (clog:create-div ,row-container :class "meter-label")))
                    (register-value-hook
                     ,name (lambda (data) (setf (clog:text ,meter-label)
                                                (format nil "~a ~a" ,(symbol-name name) data))))
                    (register-value-hook
                     ,name (lambda (data) (setf (clog:width ,meter-body) (* (+ data 1) 100)))))))
             (generate-rows (prefixes index)
               `(progn ,@(mapcar (lambda (prefix)
                                   `(meter-row tile-container
                                               ,(alexandria:make-keyword (format nil "~a~a"
                                                                                 prefix
                                                                                 index))))
                                 prefixes))))
    (let ((tile-container (clog:create-div parent :class "tile-title" :content "CV levels")))
      (generate-rows (vco vcf res vca gate) 1)
      (generate-rows (vco vcf res vca gate) 2)
      (generate-rows (vco vcf res vca gate) 3))))

(defparameter *roll-height* 600)
(defparameter *roll-width* 1200)
(defparameter *axis-padding* 10)
(defparameter *axis-color* :white)
(defparameter *axis-thickness* 2)
(defparameter *data-thickness* 1)

(defun draw-axes (context origin-x origin-y width height)
  (setf (clog:stroke-style context) *axis-color*)
  (setf (clog:line-width context) *axis-thickness*)
  (clog:begin-path context)
  (let ((ax-origin-x (+ origin-x *axis-padding*))
        (ax-origin-y (- origin-y *axis-padding*))
        (ax-x-end (+ origin-x width (- *axis-padding*)))
        (ax-y-end (- origin-y height (- *axis-padding*))))
    (clog:move-to context ax-origin-x ax-origin-y)
    (clog:line-to context ax-x-end ax-origin-y)
    (clog:move-to context ax-origin-x ax-origin-y)
    (clog:line-to context ax-origin-x ax-y-end)
    (clog:path-stroke context)))

(defun draw-data (context tracker-name x-transform y-transform origin-x origin-y)
  (setf (clog:stroke-style context) :yellow)
  (setf (clog:line-width context) *data-thickness*)
  (clog:begin-path context)
  (let ((data-origin-x (+ origin-x *axis-padding*))
        (data-origin-y (- origin-y *axis-padding*)))
    (clog:move-to context data-origin-x data-origin-y)
    (am-ht:loop-over-history
     tracker-name
     (lambda (time cv)
       (clog:line-to context
                     (+ data-origin-x (coerce (funcall x-transform time) 'single-float))
                     (- data-origin-y (funcall y-transform cv))))))
  (clog:path-stroke context))


(defun cv-draw-loop (clog-obj timecode)
  (declare (ignore timecode))
  (let ((context (clog:connection-data-item clog-obj "context2d")))
    (clog:clear-rect context 0 0 *roll-width* 100)
    (draw-axes context 0 100 *roll-width* 100)
    (setf (clog:global-composite-operation context) "destination-over")
    (draw-data context
               :vco1
               (lambda (x) (- (- *roll-width* (* 2 *axis-padding*))
                              (/ (- (incudine:now) x)
                                 (* 0.5 (incudine:rt-sample-rate)))))
               (lambda (y) (* 40 (+ 1 y)))
               0
               100)
    (draw-axes context 0 200 *roll-width* 100)
    (draw-axes context 0 300 *roll-width* 100)
    (draw-axes context 0 400 *roll-width* 100)
    (draw-axes context 0 500 *roll-width* 100))
  )

(defun build-cv-roll (clog-parent)
  (let* ((container (clog:create-div clog-parent :class "tile-title" :content "CV history"))
         (canvas (clog:create-canvas container :width *roll-width* :height *roll-height*))
         (context (clog:create-context2d canvas)))
    (setf (clog:connection-data-item clog-parent "context2d") context)
    (clog:set-border canvas :medium :solid :green)
    (clog:set-on-animation-frame (clog:connection-data-item clog-parent "window") #'cv-draw-loop)))

(defun build-ui (parent)
  (create-div parent :class "main-title" :content "Arcimoog")
  (build-cv-meters (create-div parent :class "tile"))
  (build-cv-roll (create-div parent :class "tile")))

(defun on-main (body)
  (setf (clog:connection-data-item body "window") (clog:window body))
  (format t "~&window: ~a" (clog:connection-data-item body "window"))
  (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "Arcimoog Main Parameters")
  (let ((main-container (create-div body :class "main-container")))
    (build-ui main-container))
  ;; TODO animation doesn't work like that. To try: global animation control, updating all canvases
  ;; on all connections, similar to updating the cv meters. with a global clock, driven by incudine,
  ;; or some other timing feature
  ;; (bordeaux-threads:make-thread (lambda () (loop
  ;; (clog:request-animation-frame (clog:window body)))))
  )

(defun init ()
  (initialize #'on-main
              :host "127.0.0.1"
              :port 8080
              :static-root (merge-pathnames "clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser))

(defun reset ()
  (setf *parameter-hooks* (make-hash-table))
  (shutdown)
  (init))

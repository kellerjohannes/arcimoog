(in-package :arcimoog.ui)

(defparameter *parameter-hooks* (make-hash-table))
(defparameter *parameter-values* (make-hash-table))
(defparameter *animated-data* nil)

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

(defun register-animation-callback (name callback-function)
  (register-value-hook name callback-function)
  (push name *animated-data*))

(defun update-animated-data ()
  (dolist (name *animated-data*)
    (dolist (callback-function (gethash name *parameter-hooks*))
      (funcall callback-function nil))))

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




(defclass gl-object ()
  ((webgl :initarg :webgl :reader webgl)
   (vbo :reader vbo)
   (vao :reader vao)
   (program :initarg :program :accessor program)
   (xy :initarg :xy :accessor xy)))

(defmethod initialize-instance :after ((instance gl-object) &rest initargs &key &allow-other-keys)
  (with-slots (vbo vao) instance
    (let ((webgl (getf initargs :webgl)))
      (setf vao (clog-webgl:create-vertex-array webgl)
            vbo (clog-webgl:create-webgl-buffer webgl)))))

(defgeneric draw (gl-object)
  (:documentation "Draws a GL-OBJECT."))


(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program
          (clog-webgl:compile-webgl-program
           webgl
           (clog-webgl:compile-shader-source webgl :VERTEX_SHADER vertex-shader)
           (clog-webgl:compile-shader-source webgl :FRAGMENT_SHADER fragment-shader))))
    (clog-webgl:use-program program)
    program))

(defclass rolls (gl-object)
  ())

(defmethod draw ((obj rolls))
  (clog-webgl:use-program (program obj))
  (clog-webgl:bind-vertex-array (vao obj))
  (clog-webgl:clear-webgl (webgl obj) :COLOR_BUFFER_BIT)
  (clog-webgl:bind-buffer (vbo obj) :ARRAY_BUFFER)
  (clog-webgl:uniform-float (webgl obj) (clog-webgl:uniform-location (program obj) "color")
                            1.0 1.0 0.0)
  (clog-webgl:uniform-float (webgl obj) (clog-webgl:uniform-location (program obj) "xFactor")
                            0.000001)
  (clog-webgl:uniform-float (webgl obj) (clog-webgl:uniform-location (program obj) "yFactor")
                            0.7)

  (macrolet ((draw-tracker (name r g b)
               `(progn
                  (clog-webgl:uniform-float (webgl obj)
                                            (clog-webgl:uniform-location (program obj) "color")
                                            ,r ,g ,b)
                  (let ((data (am-ht:add-points
                               (am-ht:dump-list ,name
                                                (lambda (time)
                                                  (coerce (- (incudine:now) time) 'single-float))))))
                    (clog-webgl:buffer-data (vbo obj)
                                            (append data (list 0.0 (car (last data))))
                                            "Float32Array"
                                            :STATIC_DRAW)
                    (clog-webgl:draw-arrays (webgl obj)
                                            :LINE_STRIP
                                            0
                                            (floor (+ 2 (length data)) 2))))))

    (draw-tracker :vco1 1.0 1.0 0.0)
    (draw-tracker :vcf1 0.0 1.0 1.0)
    (draw-tracker :res1 1.0 0.0 1.0)
    (draw-tracker :vca1 0.0 1.0 0.0)))

(defparameter *rolls-v-shader* "#version 300 es
in vec2 position;
out vec3 Color;

uniform vec3 color;
uniform float xFactor;
uniform float yFactor;

void main() {
  Color = color;
  gl_Position = vec4(1.0 - xFactor * position.x, yFactor * position.y, 0.0, 1.0);
}")

(defparameter *rolls-f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  outColor = vec4(Color, 1.0);
}")


(defun make-rolls (webgl)
  (let* ((program (compile-program webgl *rolls-v-shader* *rolls-f-shader*))
         (r (make-instance 'rolls
                           :webgl webgl
                           :program program
                           :xy (clog-webgl:attribute-location program "position"))))
    (clog-webgl:bind-vertex-array (vao r))
    (clog-webgl:bind-buffer (vbo r) :ARRAY_BUFFER)
    (clog-webgl:enable-vertex-attribute-array webgl (xy r))
    (clog-webgl:vertex-attribute-pointer webgl (xy r) 2 :float nil 0 0)
    r))

(defun animation-handler (obj time)
  (declare (ignore time))
  (draw (clog:connection-data-item obj "rolls"))
  (clog:request-animation-frame (clog:connection-data-item obj "window")))

(defun build-cv-roll (clog-parent)
  (clog:create-div clog-parent :class "tile-title" :content "CV history")
  (let* ((rolls-container (clog:create-div clog-parent))
         (canvas (clog:create-canvas rolls-container :width 1200 :height 700))
         (gl (clog-webgl:create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                           "powerPreference" "low-power"
                                                           "antialias" t)))
         (rolls (make-rolls gl)))
    (setf (clog:connection-data-item clog-parent "rolls") rolls)
    (clog:set-on-animation-frame (clog:connection-data-item clog-parent "window")
                                 #'animation-handler)
    (clog:set-border canvas :medium :solid :green)
    (clog-webgl:enable-capability gl :blend)
    (clog-webgl:blend-function gl :one :ONE_MINUS_SRC_ALPHA)
    (clog-webgl:clear-color gl 0.0f0 0.0f0 0.0f0 1.0f0)
    (clog-webgl:clear-webgl gl :COLOR_BUFFER_BIT)
    (clog:request-animation-frame (clog:connection-data-item clog-parent "window"))
    ))









(defun build-ui (parent)
  (create-div parent :class "main-title" :content "Arcimoog")
  (build-cv-meters (create-div parent :class "tile"))
  (build-cv-roll (create-div parent :class "tile"))
  )

(defun on-main (body)
  (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "Arcimoog Main Parameters")
  (setf (clog:connection-data-item body "window") (clog:window body))
  (let ((main-container (create-div body :class "main-container")))
    (build-ui main-container)))



(defun init ()
  (initialize #'on-main
              :host "127.0.0.1"
              :port 8080
              :static-root (merge-pathnames "clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser)
  ;; (start-data-animation)
  )

(defun reset ()
  ;; (stop-data-animation)
  (setf *parameter-hooks* (make-hash-table))
  (shutdown)
  (init)
  ;; (start-data-animation)
  )

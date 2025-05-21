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
                 `(let* ((,row-container (create-div ,parent :class "meter-row"))
                         (,visual-container (create-div ,row-container
                                                             :class "meter-visual-container"))
                         (,meter-body (create-div ,visual-container
                                                       :class "meter-visual-filler"))
                         (,meter-label (create-div ,row-container :class "meter-label")))
                    (register-value-hook
                     ,name (lambda (data) (setf (text ,meter-label)
                                                (format nil "~a ~a" ,(symbol-name name) data))))
                    (register-value-hook
                     ,name (lambda (data) (setf (width ,meter-body) (* (+ data 1) 100)))))))
             (generate-rows (prefixes index)
               `(progn ,@(mapcar (lambda (prefix)
                                   `(meter-row tile-container
                                               ,(alexandria:make-keyword (format nil "~a~a"
                                                                                 prefix
                                                                                 index))))
                                 prefixes))))
    (let ((tile-container (create-div parent :class "tile-title" :content "CV levels")))
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
      (setf vao (create-vertex-array webgl)
            vbo (create-webgl-buffer webgl)))))

(defgeneric draw (gl-object)
  (:documentation "Draws a GL-OBJECT."))


(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program
          (compile-webgl-program
           webgl
           (compile-shader-source webgl :VERTEX_SHADER vertex-shader)
           (compile-shader-source webgl :FRAGMENT_SHADER fragment-shader))))
    (use-program program)
    program))


(defclass roll (gl-object)
  ((color :initform (list 1.0 1.0 1.0) :accessor color)
   (name :initarg :name :accessor name)))

(defun set-uniform-f (clog-obj name-string x &optional y z w)
  (uniform-float (webgl clog-obj)
                            (uniform-location (program clog-obj) name-string)
                            x y z w))

(defmethod draw ((obj roll))
  ;;(format t "~&Drawing roll ~a.~%" (name obj))
  (use-program (program obj))
  (bind-vertex-array (vao obj))
  (bind-buffer (vbo obj) :ARRAY_BUFFER)


  ;; (set-uniform-f obj "xOffset" (am-par:get-scalar :cv-history-x-offset))
  ;; (set-uniform-f obj "xFactor" (am-par:get-scalar :cv-history-x-scale))
  ;; (set-uniform-f obj "yFactor" (am-par:get-scalar :cv-history-y-scale))

  (clog-webgl:use-program (program obj))

  (clog-webgl:bind-vertex-array (vao obj))
  (clog-webgl:bind-buffer (vbo obj) :ARRAY_BUFFER)
  ;;(clog-webgl:enable-vertex-attribute-array (webgl obj) (xy obj))
  ;;(clog-webgl:vertex-attribute-pointer (webgl obj) (xy obj) 2 :float nil 0 0)


  (set-uniform-f obj "xOffset" 0.0)
  (set-uniform-f obj "xFactor" 1.0)
  (set-uniform-f obj "yFactor" 1.0)
  (set-uniform-f obj "color" (first (color obj)) (second (color obj)) (third (color obj)))
  (when t ;(am-ht:update-data-required-p (name obj))
    (buffer-data (vbo obj)
                            ;;(am-ht:dump-gl-list (name obj))
                            (list 0.0 0.0 1.0 1.0)
                            "Float32Array"
                            :STATIC_DRAW)
    (am-ht:data-updated (name obj)))
  (draw-arrays (webgl obj)
                          :LINE_STRIP
                          0
                          ;;(floor (am-ht:length-gl-data (name obj)) 2)
                          2
                          )

  ;; (format t "~&~a" (webgl-error (webgl obj)))
  )



(defparameter *rolls-v-shader* "#version 300 es
in vec2 position;
out vec3 Color;

uniform vec3 color;
uniform float xFactor;
uniform float yFactor;
uniform float xOffset;

void main() {
 Color = color;
 gl_Position = vec4(xOffset + xFactor * position.x, yFactor * position.y, 0.0, 1.0);
}")


(defparameter *rolls-f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  outColor = vec4(Color, 1.0);
}")


(defun make-roll (webgl name color)
  (let* ((program (compile-program webgl *rolls-v-shader* *rolls-f-shader*))
         (r (make-instance 'roll
                           :name name
                           :color color
                           :webgl webgl
                           :program program
                           :xy (attribute-location program "position"))))
    (bind-vertex-array (vao r))
    (bind-buffer (vbo r) :ARRAY_BUFFER)
    (enable-vertex-attribute-array webgl (xy r))
    (vertex-attribute-pointer webgl (xy r) 2 :float nil 0 0)
    (buffer-data (vbo r)
                            (list 0.0 0.0 1.0 1.0)
                            "Float32Array"
                            :STATIC_DRAW)
    r))

(defparameter *frame-rate* (/ 1000 2))

(defun draw-rolls (clog-obj rolls)
  ;; (format t "~&Drawing.~%")
  (clear-webgl (connection-data-item clog-obj "gl-object") :COLOR_BUFFER_BIT)
  (dolist (roll rolls)
    (draw roll)))

(defparameter *rolls* nil)

(defun build-cv-roll (clog-parent)
  (create-div clog-parent :class "tile-title" :content "CV history")
  (let* ((rolls-container (create-div clog-parent))
         (canvas (create-canvas rolls-container :width 1200 :height 700))
         (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                           "powerPreference" "low-power"
                                                           "antialias" t)))
         (rolls (list (make-roll gl :vco1 (list 1.0 0.0 0.0))
      ;;                (make-roll gl :vcf1 (list 0.0 1.0 0.0))
      ;;                (make-roll gl :res1 (list 1.0 1.0 0.0))
                      )))
    (setf (connection-data-item clog-parent "gl-object") gl)
    (setf (connection-data-item clog-parent "rolls") rolls)
    (setf (connection-data-item clog-parent "previous-time") 0)
    (set-border canvas :medium :solid :green)
    (enable-capability gl :BLEND)
    (disable-capability gl :DEPTH_TEST)
    (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
    (clear-color gl 0.0f0 0.0f0 0.0f0 1.0f0)
    (clear-depth gl 1)
    (viewport gl 0 0 1200 700)
    (bt:make-thread (lambda ()
                      (loop
                        (draw-rolls clog-parent rolls)
                        (sleep 1/5))))))









(defun build-ui (parent)
  (create-div parent :class "main-title" :content "Arcimoog")
  (build-cv-meters (create-div parent :class "tile"))
  (build-cv-roll (create-div parent :class "tile")))

(defun on-main (body)
  (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "Arcimoog Main Parameters")
  (setf (connection-data-item body "window") (window body))
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

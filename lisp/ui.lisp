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



(defclass roll ()
  ((webgl :initarg :webgl :reader webgl)
   (program :initarg :program :accessor program)
   (curves :initform nil :initarg :curves :accessor curves)
   (framebuffer :reader roll-framebuffer)
   (texture :reader roll-texture)))

(defmethod initialize-instance :after ((instance roll) &rest initargs &key &allow-other-keys)
  (with-slots (framebuffer texture) instance
    (let ((webgl (getf initargs :webgl)))
      (setf framebuffer (create-webgl-frame-buffer webgl))
      (setf texture (create-webgl-texture webgl)))))

(defclass curve ()
  ((webgl :initarg :webgl :reader webgl)
   (name :initarg :name :accessor name)
   (vbo :reader vbo)
   (vao :reader vao)
   (xy :initarg :xy :accessor xy)
   (color :initform (list 1.0 1.0 1.0) :initarg :color :accessor color)))

(defmethod initialize-instance :after ((instance curve) &rest initargs &key &allow-other-keys)
  (with-slots (vbo vao) instance
    (let ((webgl (getf initargs :webgl)))
      (setf vao (create-vertex-array webgl))
      (setf vbo (create-webgl-buffer webgl)))))

(defun make-curve (webgl name color shader-program)
  (let ((curve (make-instance 'curve
                              :webgl webgl
                              :name name
                              :xy (attribute-location shader-program "position")
                              :color color)))
    (bind-vertex-array (vao curve))
    (bind-buffer (vbo curve) :ARRAY_BUFFER)
    (enable-vertex-attribute-array (webgl curve) (xy curve))
    (vertex-attribute-pointer (webgl curve) (xy curve) 2 :FLOAT nil 0 0)
    (buffer-data (vbo curve)
                 (loop repeat 4 collect (- (random 2.0) 1.0))
                 "Float32Array"
                 :STATIC_DRAW)
    curve))

;; TODO implement data update method

(defclass quad ()
  ((webgl :initarg :webgl :reader webgl)
   (program :initarg :program :accessor program)
   (xy :initarg :xy :accessor xy)
   (vbo :reader vbo)
   (vao :reader vao)
   (ebo :reader ebo)
   (tex-coords :initarg :tex-coordinates :accessor tex-coords)))

(defmethod initialize-instance :after ((instance quad) &rest initargs &key &allow-other-keys)
  (with-slots (vbo vao ebo) instance
    (let ((webgl (getf initargs :webgl))
          (xy (getf initargs :xy))
          (tex-coords (getf initargs :tex-coords)))
      (setf vao (create-vertex-array webgl))
      (setf vbo (create-webgl-buffer webgl))
      (setf ebo (create-webgl-buffer webgl))
      (bind-vertex-array vao)
      (bind-buffer vbo :ARRAY_BUFFER)
      (bind-buffer ebo :ELEMENT_ARRAY_BUFFER)
      (buffer-data vbo (coerce *quad* 'list) "Float32Array" :STATIC_DRAW)
      (buffer-data ebo (coerce *quad-elems* 'list) "Uint16Array" :STATIC_DRAW)
      (vertex-attribute-pointer webgl xy 2 :FLOAT nil 16 0)
      (vertex-attribute-pointer webgl tex-coords 2 :FLOAT nil 16 8)
      (enable-vertex-attribute-array webgl xy)
      (enable-vertex-attribute-array webgl tex-coords))))

(defun make-quad (webgl)
  (let ((program (compile-program webgl *quad-v-shader* *quad-f-shader*)))
    (make-instance 'quad
                   :webgl webgl
                   :program program
                   :xy (attribute-location program "aPos")
                   :tex-coords (attribute-location program "aTexCoords"))))

(defmethod draw-quad ((instance quad) texture)
  (use-program (program instance))
  (bind-canvas-frame-buffer (webgl instance) :DRAW_FRAMEBUFFER)
  (bind-vertex-array (vao instance))
  (when texture
    (bind-texture texture :TEXTURE_2D))
  (draw-elements (webgl instance) :TRIANGLES 6 :UNSIGNED_SHORT 0))


(defparameter *curve-v-shader* "#version 300 es
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


(defparameter *curve-f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  outColor = vec4(Color, 1.0);
}")

(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program
          (compile-webgl-program
           webgl
           (compile-shader-source webgl :VERTEX_SHADER vertex-shader)
           (compile-shader-source webgl :FRAGMENT_SHADER fragment-shader))))
    (use-program program)
    program))

(defun make-roll (webgl curves width height)
  (let* ((program (compile-program webgl *curve-v-shader* *curve-f-shader*))
         (curves (mapcar (lambda (curve) (make-curve webgl (first curve) (second curve) program))
                         curves))
         (roll (make-instance 'roll
                              :webgl webgl
                              :program program
                              :curves curves)))
    (bind-frame-buffer (roll-framebuffer roll) :DRAW_FRAMEBUFFER)
    (bind-texture (roll-texture roll) :TEXTURE_2D)
    (texture-image-2d webgl :TEXTURE_2D 0 :RGBA width height 0 :RGBA :UNSIGNED_BYTE nil)
    (texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MIN_FILTER :LINEAR)
    (texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MAG_FILTER :LINEAR)
    (frame-buffer-texture-2d webgl :DRAW_FRAMEBUFFER :COLOR_ATTACHMENT0 :TEXTURE_2D
                             (roll-texture roll) 0)
    ; (format t "~&WebGL error: ~a.~%" (webgl-error webgl))
    roll))

(defmethod draw ((instance roll))
  (use-program (program instance))
  (uniform-float (webgl instance) (uniform-location (program instance) "xOffset") *offset*)
  (uniform-float (webgl instance) (uniform-location (program instance) "xFactor") 1.0)
  (uniform-float (webgl instance) (uniform-location (program instance) "yFactor") 1.0)
  (bind-frame-buffer (roll-framebuffer instance) :DRAW_FRAMEBUFFER)
  (clear-color (webgl instance) 0.12 0.12 0.1 1.0)
  (clear-webgl (webgl instance) :COLOR_BUFFER_BIT)
  (dolist (curve (curves instance))
    (draw-curve curve (program instance))))

(defmethod draw-curve ((instance curve) shader-program)
  (bind-vertex-array (vao instance))
  (uniform-float (webgl instance)
                 (uniform-location shader-program "color")
                 (first (color instance))
                 (second (color instance))
                 (third (color instance)))
  ;; TODO change to represent actual data
  (draw-arrays (webgl instance) :LINE_STRIP 0 2))




(defparameter *quad*
  (make-array 16 :element-type 'single-float
                 :initial-contents '(-1.0  1.0 0.0 1.0
                                     1.0  1.0 1.0 1.0
                                     1.0 -1.0 1.0 0.0
                                     -1.0 -1.0 0.0 0.0)))

(defparameter *quad-elems*
  (make-array 6 :element-type 'fixnum
                :initial-contents '(0 1 2
                                    2 3 0)))

(defparameter *quad-v-shader*
  "#version 300 es
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoords;

out vec2 TexCoords;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
    TexCoords = aTexCoords;
}")

(defparameter *quad-f-shader*
  "#version 300 es
precision highp float;
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D screenTexture;

void main()
{
    FragColor = texture(screenTexture, TexCoords);
}")


(defun animation-handler (clog-obj time-string)
  (declare (ignore time-string))
  ;;(format t "~&Animation frame at time: ~a.~%" time-string)
  (draw (connection-data-item clog-obj "roll"))
  (draw-quad (connection-data-item clog-obj "quad")
             (roll-texture (connection-data-item clog-obj "roll"))
             )
  (request-animation-frame (connection-data-item clog-obj "window")))


(defun build-cv-roll (clog-parent)
  (create-div clog-parent :class "tile-title" :content "CV history")
  (let* ((rolls-container (create-div clog-parent))
         (canvas (create-canvas rolls-container :width 1200 :height 700))
         (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                "powerPreference" "low-power"
                                                "antialias" t)))
         (quad (make-quad gl))

         (roll (make-roll gl
                          '((:vco1 (0.0 1.0 0.0))
                            (:vcf1 (1.0 1.0 0.0))
                            (:res1 (0.8 0.5 0.0))
                            (:vca (0.2 0.2 1.0))
                            (:gate1 (1.0 0.0 0.0)))
                          1200 700)))
    (setf (connection-data-item clog-parent "gl-object") gl)
    (setf (connection-data-item clog-parent "quad") quad)
    (setf (connection-data-item clog-parent "roll") roll)
    (setf (connection-data-item clog-parent "previous-time") 0)
    (set-border canvas :medium :solid :green)
    (enable-capability gl :BLEND)
    (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
    (clear-color gl 0.0f0 0.0f0 1.0f0 1.0f0)
    (clear-webgl gl :COLOR_BUFFER_BIT)
    (viewport gl 0 0 1200 700)
    (set-on-animation-frame (connection-data-item clog-parent "window") #'animation-handler)
    (request-animation-frame (connection-data-item clog-parent "window"))))





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
  (open-browser))

(defun reset ()
  (setf *parameter-hooks* (make-hash-table))
  (shutdown)
  (init))

(in-package :arcimoog.ui)

;;; The UI is generated with CLOG elements entirely. Styling is defined in a separate CSS file. All
;;; UI data is strictly kept within the open connection (CONNECTION-DATA-ITEMs). All other data is
;;; taken from the ARCIMOOG system. Therefore, multiple UIs can be opened simultaneously with
;;; exactly the same view. If a browser crashes, the UI IP address can just be reopened without
;;; causing any reset.


;;; Parameter bar visualisation

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
      (generate-rows (vco vcf res vca gate) 3)
      (generate-rows (vco vcf res vca gate) 4)
      (generate-rows (vco vcf res vca gate) 5))))


;;; Parameter rolls (CV and Pitch rolls), webGL-based.

;; Multiple CURVES are drawn into a ROLL. Each CURVE has its own VAO/VBO. Therefore, a curve can
;; have an individual color and its can be updated individually.

;; A ROLL provides the shader program for all curves and draws the curves into a framebuffer /
;; texture.

;; To draw ROLLs, a QUAD is used and bound to the corresponding texture of a ROLL.


(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program
          (compile-webgl-program
           webgl
           (compile-shader-source webgl :VERTEX_SHADER vertex-shader)
           (compile-shader-source webgl :FRAGMENT_SHADER fragment-shader))))
    (use-program program)
    program))


(defparameter *curve-v-shader* "#version 300 es
in vec2 position;
out vec3 Color;

uniform vec3 color;
uniform float xFactor;
uniform float yFactor;
uniform float xOffset;
uniform float yOffset;

void main() {
 Color = color;
 gl_Position = vec4(1.0 + xFactor * (position.x + (100.0 * xOffset)), yOffset + (yFactor * position.y), 0.0, 1.0);
}")


(defparameter *curve-f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  outColor = vec4(Color, 1.0);
}")


(defclass curve ()
  ((webgl :initarg :webgl :reader webgl)
   (name :initarg :name :accessor name)
   (vbo :reader vbo)
   (vao :reader vao)
   (xy :initarg :xy :accessor xy)
   (data-length :initform 0 :accessor data-length)
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
    (upload-data curve (am-ht:dump-gl-list (name curve)))
    curve))

(defmethod upload-data ((instance curve) data-list)
  (setf (data-length instance) (length data-list))
  (bind-buffer (vbo instance) :ARRAY_BUFFER)
  (buffer-data (vbo instance) data-list "Float32Array" :STATIC_DRAW))

(defmethod draw-curve ((instance curve) shader-program)
  (with-accessors ((color color) (name name) (webgl webgl)) instance
    (bind-vertex-array (vao instance))
    (uniform-float webgl (uniform-location shader-program "color")
                   (first color) (second color) (third color))
    (when (am-ht:update-data-required-p name)
      (upload-data instance (am-ht:dump-gl-list name))
      (am-ht:data-updated name))
    (draw-arrays webgl :LINE_STRIP 0 (floor (data-length instance) 2))))




(defclass roll ()
  ((webgl :initarg :webgl :reader webgl)
   (program :initarg :program :accessor program)
   (curves :initform nil :initarg :curves :accessor curves)
   (framebuffer :reader roll-framebuffer)
   (texture :reader roll-texture)
   (width :initarg :width :accessor width)
   (curve-height :initarg :curve-height :accessor curve-height)
   (roll-height :initarg :roll-height :accessor roll-height)
   (y-offset :initarg :y-offset :initform 0.0 :accessor y-offset)
   (y-scale :initarg :y-scale :initform 1.0 :accessor y-scale)))

(defmethod initialize-instance :after ((instance roll) &rest initargs &key &allow-other-keys)
  (with-slots (framebuffer texture) instance
    (let ((webgl (getf initargs :webgl)))
      (setf framebuffer (create-webgl-frame-buffer webgl))
      (setf texture (create-webgl-texture webgl)))))

(defun make-roll (webgl curves width roll-height curve-height y-scale y-offset)
  (let* ((program (compile-program webgl *curve-v-shader* *curve-f-shader*))
         (curves (mapcar (lambda (curve) (make-curve webgl (first curve) (second curve) program))
                         curves))
         (roll (make-instance 'roll
                              :webgl webgl
                              :program program
                              :y-scale y-scale
                              :y-offset y-offset
                              :width width
                              :roll-height roll-height
                              :curve-height curve-height
                              :curves curves)))
    (bind-frame-buffer (roll-framebuffer roll) :DRAW_FRAMEBUFFER)
    (viewport webgl 0 0 width curve-height)
    (bind-texture (roll-texture roll) :TEXTURE_2D)
    (texture-image-2d webgl :TEXTURE_2D 0 :RGBA width curve-height 0 :RGBA :UNSIGNED_BYTE nil)
    (texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MIN_FILTER :LINEAR)
    (texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MAG_FILTER :LINEAR)
    (frame-buffer-texture-2d webgl :DRAW_FRAMEBUFFER :COLOR_ATTACHMENT0 :TEXTURE_2D
                             (roll-texture roll) 0)
    ;; (format t "~&WebGL error: ~a.~%" (webgl-error webgl))
    roll))

(defun calculate-autoroll-x-offset (shader-factor)
  (coerce (- (* (incudine:now) (/ 1.0 (incudine:rt-sample-rate)) shader-factor)) 'single-float))

(defparameter *cv-autoroll-p* t)

(defun update-cv-x-offset ()
  (am-par:set-scalar :cv-history-x-offset (calculate-autoroll-x-offset 0.01)))

(defun toggle-cv-autoroll ()
  (cond (*cv-autoroll-p*
         (setf *cv-autoroll-p* nil)
         (update-cv-x-offset))
        (t (setf *cv-autoroll-p* t))))

(defmethod draw ((instance roll))
  (use-program (program instance))
  (uniform-float (webgl instance) (uniform-location (program instance) "xOffset")
                 (if *cv-autoroll-p*
                     (calculate-autoroll-x-offset 0.01)
                     (am-par:get-scalar :cv-history-x-offset)))
  (uniform-float (webgl instance) (uniform-location (program instance) "xFactor")
                 (am-par:get-scalar :cv-history-x-scale))
  (uniform-float (webgl instance) (uniform-location (program instance) "yOffset") -0.8)
  (uniform-float (webgl instance) (uniform-location (program instance) "yFactor") 0.18)
  (bind-frame-buffer (roll-framebuffer instance) :DRAW_FRAMEBUFFER)
  (viewport (webgl instance) 0 0 (width instance) (roll-height instance))
  (clear-color (webgl instance) 0.10 0.16 0.1 1.0)
  (clear-webgl (webgl instance) :COLOR_BUFFER_BIT)
  (dolist (curve (curves instance))
    (draw-curve curve (program instance))))






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

uniform float yOffset;
uniform float yScale;

void main()
{
    gl_Position = vec4(aPos.x, (yScale * aPos.y) + yOffset, 0.0, 1.0);
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

(defmethod draw-quad ((instance quad) texture y-scale y-offset)
  (use-program (program instance))
  (uniform-float (webgl instance)
                 (uniform-location (program instance) "yScale")
                 y-scale)
  (uniform-float (webgl instance)
                 (uniform-location (program instance) "yOffset")
                 y-offset)
  (bind-canvas-frame-buffer (webgl instance) :DRAW_FRAMEBUFFER)
  (bind-vertex-array (vao instance))
  (when texture
    (bind-texture texture :TEXTURE_2D))
  (draw-elements (webgl instance) :TRIANGLES 6 :UNSIGNED_SHORT 0))










(defun animation-handler (clog-obj time-string)
  (declare (ignore time-string))
  ;; (format t "~&Animation frame at time: ~a.~%" time-string)
  (dolist (roll (connection-data-item clog-obj "rolls"))
    (draw roll)
    (draw-quad (connection-data-item clog-obj "quad")
               ;;nil
               (roll-texture roll)
               (y-scale roll)
               (y-offset roll)
               ))
  (request-animation-frame (connection-data-item clog-obj "window")))

(defparameter *curve-height* 150)
(defparameter *roll-width* 1200)

(defparameter *vco-color* (list 0.0 1.0 0.0))
(defparameter *vcf-color* (list 1.0 1.0 0.0))
(defparameter *res-color* (list 0.8 0.5 0.0))
(defparameter *vca-color* (list 0.2 0.2 1.0))
(defparameter *gate-color* (list 1.0 0.0 0.0))


(defun build-cv-roll (clog-parent)
  (create-div clog-parent :class "tile-title" :content "CV history")
  (macrolet ((generate-roll (id y-scale y-offset)
               `(make-roll gl
                           `((,(alexandria:make-keyword (format nil "VCO~a" ,id)) ,*vco-color*)
                             (,(alexandria:make-keyword (format nil "VCF~a" ,id)) ,*vcf-color*)
                             (,(alexandria:make-keyword (format nil "RES~a" ,id)) ,*res-color*)
                             (,(alexandria:make-keyword (format nil "VCA~a" ,id)) ,*vca-color*)
                             (,(alexandria:make-keyword (format nil "GATE~a" ,id)) ,*gate-color*))
                           *roll-width* (* 5 *curve-height*) *curve-height*
                           ,y-scale ,y-offset)))
    (let* ((rolls-container (create-div clog-parent))
           (canvas (create-canvas rolls-container :width *roll-width* :height (* 5 *curve-height*)))
           (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                  "powerPreference" "low-power"
                                                  "antialias" t)))
           (quad (make-quad gl))

           (rolls (list (generate-roll 1 0.18 0.8)
                        (generate-roll 2 0.18 0.4)
                        (generate-roll 3 0.18 0.0)
                        (generate-roll 4 0.18 -0.4)
                        (generate-roll 5 0.18 -0.8))))
      (setf (connection-data-item clog-parent "quad") quad)
      (setf (connection-data-item clog-parent "rolls") rolls)
      (setf (connection-data-item clog-parent "previous-time") 0)
      ;; (set-border canvas :medium :solid :brown)
      (enable-capability gl :BLEND)
      (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
      (clear-color gl 1.0f0 1.0f0 1.0f0 1.0f0)
      (clear-webgl gl :COLOR_BUFFER_BIT)
      ;; Probably unnecessary
      ;; (viewport gl 0 0 *roll-width* (* 2 *roll-height*))
      (set-on-animation-frame (connection-data-item clog-parent "window") #'animation-handler)
      (request-animation-frame (connection-data-item clog-parent "window")))))






;;; Explorateur UI

(defun build-explorateur-pressures (clog-parent)
  (dotimes (row-index 16)
    (let ((row (create-div clog-parent :class "explorateur-row")))
      (dotimes (note-index 128)
        (create-div row :class "explorateur-cell" :content "O")))))


;;; Entry point for UI construction.

(defun build-ui (parent)
  (create-div parent :class "main-title" :content "Arcimoog")
  ;; TODO implement proper GUI switching mechanism
  ;; Uncomment for visual interface of VC values
  ;; (build-cv-meters (create-div parent :class "tile"))
  ;; (build-cv-roll (create-div parent :class "tile"))

  (build-explorateur-pressures (create-div parent :class "tile"))
  )

(defun on-main (body)
  ;; (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "Arcimoog UI")
  (setf (connection-data-item body "window") (window body))
  (let ((main-container (create-div body :class "main-container")))
    (build-ui main-container)))



(defun init ()
  (initialize #'on-main
              :host "127.0.0.1"
              :port 8080
              :static-root (merge-pathnames "./lisp/clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser))

(defun reset ()
  (setf *parameter-hooks* (make-hash-table))
  (shutdown)
  (init))

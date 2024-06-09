(in-package :arcimoog.display)

;; TODO
;;
;; * move opengl stuff to separate package - start implementing an abstraction layer for graphics on top of opengl package, in its own package
;;
;; * create another abstraction layer for 'panels', representing the configuration and real time values for each arcimoog module
;;
;; * think about a way to organise panels and the connection to the faderfox slots. some are hardwired (global projection), others need to be changed dynamically (parameters for multiple (polyphonic) pipelines)


(defparameter *shader-path* "/home/johannes/common-lisp/arcimoog/lisp/shaders/")
(defparameter *texture-path* "/home/johannes/common-lisp/arcimoog/lisp/textures/")
(defparameter *font-source-file* "/usr/share/fonts/TTF/FiraCode-Bold.ttf"
                                        ; "/usr/share/fonts/TTF/DejaVuSans.ttf"
                                        ; "/usr/share/fonts/OTF/BravuraText.otf"
                                        ; "/usr/share/fonts/OTF/Bravura.otf"
  )

(defparameter *global-character-set*
  " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZȧḃċḋėḟġȦḂĊḊĖḞĠ♯♭♮❜ʼ'\"«»[]#{}/\\,.!?:;➙➚➘12345674890-+*·=<>@$%^&_|~`")

;;;; helper stuff, not in the original c code

(defun array-to-gl-array (lisp-array data-type)
  (declare (type simple-array lisp-array))
  (let ((gl-array (gl:alloc-gl-array data-type (length lisp-array))))
    (loop for item across lisp-array
          for i from 0 do
            (setf (gl:glaref gl-array i) item))
    gl-array))

;; Necessary, because gl:draw-elements contains a problematic implementation of the offset
;; parameter.

(defun my-gl-draw-elements (mode count type &key (offset 0))
  (%gl:draw-elements mode count type offset))

(glfw:def-window-size-callback framebuffer-size-callback (window width height)
  (declare (ignore window))
  (gl:viewport 0 0 width height)
  (setf (screen-width *display*) width)
  (setf (screen-height *display*) height)
  (update-projection *display*))

(defparameter *key-safety* 0)

(defun trigger-key-safety ()
  (setf *key-safety* 3))

(defun reduce-key-safety ()
  (when (plusp *key-safety*) (decf *key-safety*)))

(defun key-safe-p ()
  (zerop *key-safety*))

(defmacro when-key-safe (key function)
  `(when (eq (glfw:get-key ,key) :press)
     (when (key-safe-p) (funcall ,function))
     (trigger-key-safety)))

(defun process-input ()
  (reduce-key-safety)
  (when (or (eq (glfw:get-key :escape) :press)
            (eq (glfw:get-key :q) :press))
    (glfw:set-window-should-close))
  (when-key-safe :space #'progress-global-mode-player)
  (when-key-safe :g #'step-genus)
  (when-key-safe :m #'step-modus)
  (when-key-safe :p #'plmi))



(defclass font-render-class ()
  ((character-set :initform "" :initarg :character-set :accessor character-set)
   (font-source :initform nil :initarg :font-source :accessor font-source)
   (pixel-size :initform 16 :initarg :pixel-size :accessor pixel-size)
   (ft-face :accessor ft-face)
   (textures :initform (make-hash-table) :accessor textures
             :documentation "Keys are CHARs, values are lists where the first element is the GL texture id, the second is the width of the texture and the third its height.")
   (glyph-vao :initform -1 :accessor glyph-vao)
   (glyph-vbo :initform -1 :accessor glyph-vbo)
   (shader-instance :initform nil :initarg :shader-instance :accessor shader-instance)))

(defmethod lookup-texture ((renderer font-render-class) character)
  "Return values: 1. texture ID, 2. width, 3. height."
  (let ((result (gethash character (textures renderer))))
    (unless result
      (setf result (gethash #\⸮ (textures renderer))))
    (unless result (format t "~&No texture with key ~a found." character))
    (values (first result)
            (second result)
            (third result))))

(defmethod generate-textures ((renderer font-render-class))
  (gl:pixel-store :unpack-alignment 1)
  (with-accessors ((face ft-face)
                   (character-set character-set))
      renderer
    (ft2:set-pixel-sizes face 0 (pixel-size renderer))
    (ft2:do-string-render (face character-set bitmap x y :with-char character)
      (declare (ignore x y))
      (let* ((texture-id (gl:gen-texture))
             (texture-data (ft2:bitmap-to-array bitmap))
             (dimensions (array-dimensions texture-data)))
        (gl:bind-texture :texture-2d texture-id)
        (gl:tex-image-2d :texture-2d
                         0
                         :red
                         (second dimensions)
                         (first dimensions)
                         0
                         :red
                         :unsigned-byte
                         (aops:flatten texture-data))
        (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
        (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
        (setf (gethash character (textures renderer))
              (list texture-id
                    (coerce (second dimensions) 'single-float)
                    (coerce (first dimensions) 'single-float)))))))

(defmethod initialize-instance :after ((renderer font-render-class) &key)
  (with-accessors ((source font-source)
                   (character-set character-set))
      renderer
    (unless source
      (setf source *font-source-file*))
    (setf (ft-face renderer) (ft2:new-face source))
    (when (zerop (length character-set))
      (setf character-set *global-character-set*))
    (unless (position #\⸮ character-set)
      (setf character-set (concatenate 'string character-set "⸮")))
    (unless (shader-instance renderer)
      (setf (shader-instance renderer)
            (make-instance 'shader-class
                           :vertex-source (concatenate 'string
                                                       *shader-path*
                                                       "font-shader.vert")
                           :fragment-source (concatenate 'string
                                                         *shader-path*
                                                         "font-shader.frag")))))
  (generate-textures renderer)
  (with-accessors ((vao glyph-vao)
                   (vbo glyph-vbo))
      renderer
    (setf vao (gl:gen-vertex-array)
          vbo (gl:gen-buffer))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer
                    :dynamic-draw
                    (array-to-gl-array (make-array (* 6 4) :initial-element 0.0) :float))
    (gl:vertex-attrib-pointer 0
                              4
                              :float
                              nil
                              (* 4 (cffi:foreign-type-size :float))
                              (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))





(defclass renderer-2d-class ()
  ((shape-shader :initform nil :accessor shape-shader)
   (texture-shader :initform nil :accessor texture-shader)
   (shape-vao :initform nil :accessor shape-vao)
   (shape-vbo :initform nil :accessor shape-vbo)
   (texture-vao :initform nil :accessor texture-vao)
   (texture-vbo :initform nil :accessor texture-vbo)
   (texture-array-size :initform 100 :accessor texture-array-size)
   (texture-array :initform nil :accessor texture-array)
   (texture-use-flags :initform nil :accessor texture-use-flags)
   (default-color :initform (vector 1.0 1.0 1.0) :initarg :color :accessor default-color)
   (view-matrix :initform nil :accessor view-matrix)
   (model-matrix :initform nil :accessor model-matrix)
   (vbo-allocation-size :initform 1000 :initarg :vbo-size :accessor vbo-allocation-size
                        :documentation "Max number of 2d-coordinates that will be allocated in the VBO.")))

(defmethod initialize-instance :after ((renderer renderer-2d-class) &key)
  (with-accessors ((shader shape-shader)
                   (texture-shader texture-shader)
                   (view view-matrix)
                   (model model-matrix)
                   (vao shape-vao)
                   (vbo shape-vbo)
                   (texture-vao texture-vao)
                   (texture-vbo texture-vbo))
      renderer
    (setf shader (make-instance 'shader-class
                                :vertex-source (concatenate 'string *shader-path*
                                                            "shader-2d.vert")
                                :fragment-source (concatenate 'string *shader-path*
                                                              "shader-2d.frag")))
    (setf texture-shader (make-instance 'shader-class
                                        :vertex-source (concatenate 'string *shader-path*
                                                                    "texture-shader.vert")
                                        :fragment-source (concatenate 'string *shader-path*
                                                                      "texture-shader.frag")))

    (setf (texture-array renderer) (make-array (texture-array-size renderer)
                                               :initial-element -1))
    (setf (texture-use-flags renderer) (make-array (texture-array-size renderer)
                                                   :initial-element nil))
    (loop for i from 0 below (texture-array-size renderer) do
          (setf (aref (texture-array renderer) i) (gl:gen-texture)))

    (setf model (glm:create-identity-matrix 4))
    ;; view matrix is constant for now. Later it could be used to represent different layers of a 2d
    ;; display
    (setf view (glm:create-identity-matrix 4))
    (glm:transform-matrix view glm:translate (vector 0.0 0.0 -0.5))
    (set-uniform-matrix shader "view" (glm:lisp-to-gl-matrix view))
    (set-uniform-matrix texture-shader "view" (glm:lisp-to-gl-matrix view))

    (setf vao (gl:gen-vertex-array))
    (setf vbo (gl:gen-buffer))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer
                    :static-draw
                    (array-to-gl-array (make-array (* 2 (vbo-allocation-size renderer))
                                                   :initial-element 0.0)
                                       :float))
    (gl:vertex-attrib-pointer 0
                              2
                              :float
                              nil
                              (* 2 (cffi:foreign-type-size :float))
                              (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)

    (setf texture-vao (gl:gen-vertex-array))
    (setf texture-vbo (gl:gen-buffer))
    (gl:bind-vertex-array texture-vao)
    (gl:bind-buffer :array-buffer texture-vbo)
    (gl:buffer-data :array-buffer :static-draw (array-to-gl-array
                                                (make-array (* 6 4) :initial-element 0.0) :float))
    (gl:vertex-attrib-pointer 0
                              4
                              :float
                              nil
                              (* 4 (cffi:foreign-type-size :float))
                              (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))

;; TODO: this is unacceptable and will only work for one and only one texture!
(defmethod book-first-available-texture-id ((renderer renderer-2d-class))
  ;; (loop for i from 0 below (texture-array-size renderer) do
  ;;   (unless (aref (texture-use-flags renderer) i)
  ;;     (setf (aref (texture-use-flags renderer)) t)
  ;;     (return (aref (texture-array renderer) i))))
  (aref (texture-array renderer) 0)
  )

(defmethod get-texture-id ((renderer renderer-2d-class) slot-number)
  (aref (texture-array renderer) slot-number))


(defclass display-element ()
  ((color :initform (vector 1.0 1.0 1.0) :initarg :color :accessor color)
   (scaling :initform 1.0 :initarg :scaling :accessor scaling)
   (x-position :initform 0 :initarg :x-position :accessor x-position)
   (y-position :initform 0 :initarg :y-position :accessor y-position)))

(defclass display-element-panel (display-element)
  ((width :initform 300 :initarg :width :accessor width)
   (height :initform 500 :initarg :height :accessor height)
   (title :initform "[no title]" :initarg :title :accessor title)
   (title-x-padding :initform 12 :initarg :title-x-padding :accessor title-x-padding)
   (title-y-padding :initform 3 :initarg :title-y-padding :accessor title-y-padding)
   (selectedp :initform nil :initarg :selectedp :accessor selectedp)
   (selected-color :initform (vector 0.3 1.0 1.0) :initarg :selected-color :accessor selected-color)
   (selected-margin :initform 5 :initarg :selected-margin :accessor selected-margin)))

(defclass display-element-table (display-element)
  ((width :initform 300 :initarg :width :accessor width)
   (column-widths :initform (vector 50 50) :initarg :column-widths :accessor column-widths)
   (row-height :initform 20 :initarg :row-height :accessor row-height)
   (cell-padding :initform 1 :initarg :cell-padding :accessor cell-padding)
   (text-x-padding :initform 1 :initarg :text-padding :accessor text-x-padding)
   (text-y-padding :initform 0 :initarg :text-padding :accessor text-y-padding)
   (contents :initform (make-array '(2 2) :initial-element 0 :adjustable t)
             :initarg :contents :accessor contents)))

(defmethod number-of-columns ((element display-element-table))
  (array-dimension (contents element) 1))

(defmethod number-of-rows ((element display-element-table))
  (array-dimension (contents element) 0))

(defmethod column-width ((element display-element-table) column-index)
  (aref (column-widths element) column-index))

(defmethod get-cell-content ((element display-element-table) row-index column-index)
  (aref (contents element) row-index column-index))

(defmethod set-cell-content ((element display-element-table) row-index column-index content)
  (setf (aref (contents element) row-index column-index) content))


(defclass display-element-image (display-element)
  ((image-file-path :initform nil :initarg :image-file-path :accessor image-file-path)
   (image-width :initform nil :accessor image-width)
   (image-height :initform nil :accessor image-height)
   (texture-id :initform nil :initarg :texture-id :accessor texture-id)))

(defmethod initialize-instance :after ((element display-element-image) &key)
  (format t "~&Initialising image ~a with texture id ~a." (image-file-path element) (texture-id element))
  (with-accessors ((path image-file-path)
                   (width image-width)
                   (height image-height)
                   (texture texture-id))
      element
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (multiple-value-bind (jpg-data jpg-height jpg-width)
        (cl-jpeg:decode-image (concatenate 'string *texture-path* path))
      (setf width jpg-width
            height jpg-height)
      (cond (jpg-data
             (gl:tex-image-2d :texture-2d
                              0
                              :rgb
                              jpg-width
                              jpg-height
                              0
                              :rgb
                              :unsigned-byte
                              jpg-data)
             (gl:generate-mipmap :texture-2d)
             (format t "~&Texture ~a loaded." path))
            (t (format t "~&Failed to load texture ~a." (image-file-path element)))))))

;; (defclass renderer-texture-class ()
;;   ((image-file-path :initform "" :initarg :image-file-path :accessor image-file-path)
;;    (image-width :initform nil :accessor image-width)
;;    (image-height :initform nil :accessor image-height)
;;    (texture-id :initform -1 :accessor texture-id)
;;    (quad-vao :initform -1 :accessor quad-vao)
;;    (quad-vbo :initform -1 :accessor quad-vbo)
;;    (shader-instance :initform nil :accessor shader-instance)))

;; (defmethod initialize-instance :after ((renderer renderer-texture-class) &key)
;;   (with-accessors ((image image-file-path)
;;                    (image-width image-width)
;;                    (image-height image-height)
;;                    (vao quad-vao)
;;                    (vbo quad-vbo)
;;                    (texture texture-id)
;;                    (shader shader-instance))
;;       renderer
;;     (unless shader
;;       (setf shader (make-instance 'shader-class
;;                                   :vertex-source (concatenate 'string
;;                                                               *shader-path*
;;                                                               "texture-shader.vert")
;;                                   :fragment-source (concatenate 'string
;;                                                                 *shader-path*
;;                                                                 "texture-shader.frag"))))

;;     (setf texture (gl:gen-texture))
;;     (gl:bind-texture :texture-2d texture)
;;     (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
;;     (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
;;     (gl:tex-parameter :texture-2d :texture-min-filter :linear)
;;     (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
;;     (multiple-value-bind (data height width)
;;         (cl-jpeg:decode-image (concatenate 'string *texture-path* image))
;;       (setf image-width width
;;             image-height height)
;;       (cond (data
;;              (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
;;              (gl:generate-mipmap :texture-2d))
;;             (t (format t "~&Failed to load texture2."))))

;;     (set-uniform shader "aTexture" 'int 0)


;;     (let ((w (coerce image-width 'single-float))
;;           (h (coerce image-height 'single-float)))
;;       (let ((vertices (vector 0.0 h 0.0                 0.0 0.0
;;                               0.0 0.0 0.0                          0.0 1.0
;;                               w 0.0 0.0                  1.0 1.0
;;                               0.0 h 0.0                 0.0 0.0
;;                               w 0.0 0.0                  1.0 1.0
;;                               w h 0.0         1.0 0.0)))
;;         (setf vao (gl:gen-vertex-array)
;;               vbo (gl:gen-buffer))
;;         (gl:bind-vertex-array vao)
;;         (gl:bind-buffer :array-buffer vbo)
;;         (gl:buffer-data :array-buffer
;;                         :static-draw
;;                         (array-to-gl-array vertices :float))
;;         (gl:vertex-attrib-pointer 0
;;                                   3
;;                                   :float
;;                                   nil
;;                                   (* 3 (cffi:foreign-type-size :float))
;;                                   (cffi:null-pointer))
;;         (gl:enable-vertex-attrib-array 0)
;;         (gl:vertex-attrib-pointer 1
;;                                   2
;;                                   :float
;;                                   nil
;;                                   (* 2 (cffi:foreign-type-size :float))
;;                                   (cffi:null-pointer))
;;         (gl:enable-vertex-attrib-array 0)
;;         (gl:bind-buffer :array-buffer 0)
;;         (gl:bind-vertex-array 0)))))

;; (defclass display-element-image (display-element)
;;   ((image-path :initform nil :initarg :image-path :accessor image-path)
;;    (texture-renderer :initform nil :accessor texture-renderer)))

;; (defmethod initialize-instance :after ((element display-element-image) &key)
;;   (let ((renderer (make-instance 'renderer-texture-class
;;                                  :image-file-path (image-path element))))
;;     (format t "~&Texture renderer class instantiated.")
;;     (if renderer
;;         (setf (texture-renderer element) renderer)
;;         (format t "~&Error: could not create RENDERER-TEXTURE-CLASS."))))



(defclass display-class ()
  ((renderer :initform nil :accessor renderer)
   (font-renderer :initform nil :accessor font-renderer)
   (display-elements :initform (make-hash-table) :accessor display-elements)
   (window-title :initform "Arcimoog" :accessor window-title)
   (screen-width :initform 800 :accessor screen-width)
   (screen-height :initform 600 :accessor screen-height)
   (global-translation :initform (vector 0.0 0.0 0.0) :accessor global-translation)
   (global-scaling :initform 1.0 :accessor global-scaling)
   (global-projection-matrix :initform nil :accessor global-projection-matrix)
   (background-color :initform (vector 0.0 0.0 0.0) :accessor background-color)))

(defmethod initialize-renderers ((display display-class))
  (setf (renderer display) (make-instance 'renderer-2d-class))
  (setf (font-renderer display) (make-instance 'font-render-class)))

(defmethod clear-display ((display display-class))
  (gl:clear-color (aref (background-color display) 0)
                  (aref (background-color display) 1)
                  (aref (background-color display) 2)
                  1.0)
  (gl:clear :color-buffer-bit))

(defmethod update-projection ((display display-class))
  (with-accessors ((projection global-projection-matrix))
      display
    (setf projection (glm:ortho 0.0 (screen-width display) 0.0 (screen-height display) 0.1 100.0))
    (glm:transform-matrix projection glm:translate (global-translation display))
    (glm:transform-matrix projection glm:scale (vector (global-scaling display)
                                                       (global-scaling display)
                                                       1.0))))

(defmethod reset-display-projection-parameters ((display display-class))
  (setf (global-scaling display) 1.0)
  (setf (global-translation display) (vector 0.0 0.0 0.0)))

(defmethod render-display-elements ((display display-class))
  (process-input)
  (clear-display display)
  (update-projection display)

  (loop for element being the hash-values of (display-elements display) do
    (draw display element (renderer display) (font-renderer display)))

  (glfw:swap-buffers)
  (glfw:poll-events))

(defmethod create-render-context ((display display-class))
  (glfw:with-init-window (:title (window-title display)
                          :width (screen-width display)
                          :height (screen-height display))
    (initialize-renderers display)
    (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)
    (gl:viewport 0 0 (screen-width display) (screen-height display))

    (update-projection display)

    (loop until (glfw:window-should-close-p) do
      (render-display-elements display)
      (sleep (/ 1 30)))

    ;; TODO
    ;; cleaning up needs to be implemented for all involved classes (destructors)
    ;; (gl:delete-vertex-arrays (list vao))
    ;; (gl:delete-buffers (list vbo ebo))
    ;; (destroy our-shader)
    ))


(defmethod boot ((display display-class))
  (bt:make-thread (lambda () (create-render-context display)) :name "Arcimoog OpenGL display"))




(defmethod render ((display display-class) (renderer renderer-2d-class) vertex-data
                   &key (mode :line-strip)
                     scaling
                     translation
                     rotation
                     (color (default-color renderer)))
  (with-accessors ((vao shape-vao)
                   (vbo shape-vbo)
                   (model model-matrix)
                   (shader shape-shader))
      renderer
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-sub-data :array-buffer
                        (array-to-gl-array (if (typep vertex-data '(vector single-float *))
                                               vertex-data
                                               (utility:coerce-vector vertex-data 'single-float))
                                           :float))
    (gl:bind-buffer :array-buffer 0)
    (set-uniform-matrix shader
                        "projection"
                        (glm:lisp-to-gl-matrix (global-projection-matrix display)))
    (setf model (glm:create-identity-matrix 4))
    (when scaling (glm:transform-matrix model glm:scale scaling))
    (when translation (glm:transform-matrix model glm:translate translation))
    (when rotation (glm:transform-matrix model glm:rotate (car rotation) (cdr rotation)))
    (set-uniform-matrix shader "model" (glm:lisp-to-gl-matrix model))
    (set-uniform shader "vertexColor" 'float (aref color 0) (aref color 1) (aref color 2))
    (gl:draw-arrays mode 0 (floor (/ (length vertex-data) 2)))
    (gl:bind-vertex-array 0)))

(defmethod render-texture ((display display-class) (renderer renderer-2d-class)
                           (element display-element-image)
                           x-origin y-origin &key scaling translation rotation)
  (with-accessors ((shader texture-shader)
                   (model model-matrix)
                   (vao texture-vao)
                   (vbo texture-vbo))
      renderer
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (set-uniform-matrix shader "projection"
                        (glm:lisp-to-gl-matrix (global-projection-matrix display)))
    (gl:active-texture :texture0)
    (gl:bind-vertex-array vao)
    (gl:bind-texture :texture-2d (texture-id element))

    ;; TODO: not sure what this does
    (set-uniform shader "text" 'int 1)

    (let ((w (coerce (image-width element) 'single-float))
          (h (coerce (image-height element) 'single-float)))
      (declare (ignore w h))
      (let (
            ;; (quad-vertices (vector 0.0 h   0.0 0.0
            ;;                        0.0 0.0 0.0 1.0
            ;;                        w 0.0   1.0 1.0
            ;;                        0.0 h   0.0 0.0
            ;;                        w 0.0   1.0 1.0
            ;;                        w h     1.0 0.0))
            ;; TODO only for debugging:
            (quad-vertices (vector 0.0 700.0   0.0 0.0
                                   0.0 0.0 0.0 1.0
                                   400.0 0.0   1.0 1.0
                                   0.0 300.0   0.0 0.0
                                   400.0 0.0   1.0 1.0
                                   400.0 300.0     1.0 0.0))
            )
        (gl:bind-buffer :array-buffer vbo)
        (gl:buffer-sub-data :array-buffer
                            (array-to-gl-array quad-vertices :float))
        (gl:bind-buffer :array-buffer 0)))
    (set-uniform-matrix shader
                        "projection"
                        (glm:lisp-to-gl-matrix (global-projection-matrix display)))
    (setf model (glm:create-identity-matrix 4))
    (when scaling (glm:transform-matrix model glm:scale scaling))
    (when translation (glm:transform-matrix model glm:translate translation))
    (when rotation (glm:transform-matrix model glm:rotate (car rotation) (cdr rotation)))
    (set-uniform-matrix shader "model" (glm:lisp-to-gl-matrix model))
    (gl:draw-arrays :triangles 0 6)
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)
    ))


(defmethod render-string ((display display-class)
                          (renderer font-render-class) text x-origin y-origin
                          &key (scale-factor 1.0) (rgb-vector (vector 1.0 1.0 1.0)))
  "TEXT, X-ORIGIN and Y-ORIGIN can be params."
  (gl:pixel-store :unpack-alignment 1)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (with-accessors ((shader shader-instance)
                   (vao glyph-vao)
                   (vbo glyph-vbo)
                   (face ft-face)
                   (character-set character-set))
      renderer
    (set-uniform shader "textColor" 'float
                 (aref rgb-vector 0) (aref rgb-vector 1) (aref rgb-vector 2))
    (set-uniform-matrix shader "projection"
                        (glm:lisp-to-gl-matrix (global-projection-matrix display)))
    (gl:active-texture :texture0)
    (gl:bind-vertex-array vao)
    (utility:with-params (text x-origin y-origin)
      (unless (stringp text) (setf text (format nil "~a" text)))
      (ft2:do-string-render (face text bitmap ft-x ft-y :with-char character)
        (multiple-value-bind (texture-id texture-width texture-height)
            (lookup-texture renderer character)
          (let* ((glyph-width (* scale-factor texture-width))
                 (glyph-height (* scale-factor texture-height))
                 (x-pos (* scale-factor (+ x-origin (coerce ft-x 'single-float))))
                 (y-pos (* scale-factor (- y-origin texture-height (coerce ft-y 'single-float))))
                 (glyph-vertices (vector x-pos (+ y-pos glyph-height) 0.0 0.0
                                         x-pos y-pos 0.0 1.0
                                         (+ x-pos glyph-width) y-pos 1.0 1.0
                                         x-pos (+ y-pos glyph-height) 0.0 0.0
                                         (+ x-pos glyph-width) y-pos 1.0 1.0
                                         (+ x-pos glyph-width) (+ y-pos glyph-height) 1.0 0.0)))

            (gl:bind-texture :texture-2d texture-id)
            (gl:bind-buffer :array-buffer vbo)
            (gl:buffer-sub-data :array-buffer (array-to-gl-array glyph-vertices :float))
            (gl:bind-buffer :array-buffer 0)
            (gl:draw-arrays :triangles 0 6)))))
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)))





(defmethod draw ((display display-class) (element display-element-panel)
                 (renderer renderer-2d-class) (font-renderer font-render-class))
  (with-accessors ((x x-position)
                   (y y-position)
                   (sc-f scaling)
                   (w width)
                   (h height)
                   (m selected-margin))
      element
    (utility:with-params (x y w h)
      (let ((sc (vector sc-f sc-f 1.0)))
        (when (selectedp element)
          (render display renderer (vector (- m) (- m)
                                           (+ w m) (- m)
                                           (+ w m) (+ h m)
                                           (- m) (- m)
                                           (+ w m) (+ h m)
                                           (- m) (+ h m))
                  :mode :triangles
                  :translation (vector x y 0.0)
                  :scaling sc
                  :color (selected-color element)))
        (render display renderer (vector
                                  0 0
                                  w 0
                                  w h
                                  0 0
                                  w h
                                  0 h)
                :mode :triangles
                :translation (vector x y 0.0)
                :scaling sc
                :color (color element))
        (render-string display
                       font-renderer
                       (title element)
                       (+ (/ x sc-f) (title-x-padding element))
                       (+ (/ y sc-f) h (- (title-y-padding element)))
                       :scale-factor (scaling element))))))


(defmethod draw ((display display-class) (element display-element-image)
                 (renderer renderer-2d-class) (font-renderer font-render-class))
  (render-texture display renderer element (x-position element) (y-position element)))


(defmethod draw-cell-text ((display display-class) (element display-element-table)
                      (font-renderer font-render-class))
  (dotimes (i (number-of-rows element))
    (let ((cumulative-width 0))
      (dotimes (j (number-of-columns element))
        (let ((x (+ (x-position element) cumulative-width (cell-padding element)
                    (text-x-padding element)))
              (y (- (y-position element) (* i (row-height element)) (cell-padding element) (text-y-padding element))))
          (render-string display
                         font-renderer
                         (format nil "~a" (get-cell-content element i j))
                         x
                         y))
        (incf cumulative-width (column-width element j))))))

(defmethod draw-cell-backgrounds ((display display-class) (element display-element-table)
                                  (renderer renderer-2d-class))
  (dotimes (i (number-of-rows element))
    (let ((cumulative-width 0))
      (dotimes (j (number-of-columns element))
        (let ((x (+ (x-position element) cumulative-width (cell-padding element)))
              (y (- (y-position element) (* i (row-height element)) (cell-padding element)))
              (w (- (column-width element j) (* 2 (cell-padding element))))
              (h (- (row-height element) (* 2 (cell-padding element)))))
          (render display renderer (vector
                                    0 0
                                    w 0
                                    w (- h)
                                    0 0
                                    w (- h)
                                    0 (- h))
                  :mode :triangles
                  :translation (vector x y 0.0)
                  :scaling (vector (scaling element) (scaling element) 1.0)
                  :color (color element)))
        (incf cumulative-width (column-width element j))))))

(defmethod draw ((display display-class) (element display-element-table)
                 (renderer renderer-2d-class) (font-renderer font-render-class))
  (draw-cell-backgrounds display element renderer)
  (draw-cell-text display element font-renderer))




(defmethod add-element ((display display-class) id display-element-type &rest make-arguments)
  (let ((element-instance
          (case display-element-type
            (display-element-table (apply #'make-instance
                                          (cons 'display-element-table make-arguments)))
            (display-element-panel (apply #'make-instance
                                          (cons 'display-element-panel make-arguments)))
            (display-element-image (apply #'make-instance
                                            (append (list 'display-element-image
                                                          :texture-id
                                                          (book-first-available-texture-id
                                                           (renderer display)))
                                                    make-arguments))))))
    (setf (gethash id (display-elements display)) element-instance)))

(defmacro set-element-value (display element-id slot-name value)
  `(setf (,slot-name (gethash ,element-id (display-elements ,display))) ,value))

(defmacro get-element-value (display element-id slot-name)
  `(,slot-name (gethash ,element-id (display-elements ,display))))


;;; this is how to use the display class, to be extended massively ...

(defparameter *display* (make-instance 'display-class))


(defun scene ()

  (add-element *display* :main 'display-element-panel :title "Table configuration")
  (set-element-value *display* :main color (vector 0.6 0.1 0.1))

  (add-element *display* :table 'display-element-table :x-position 10 :y-position 500)
  (set-element-value *display* :table y-position 470)
  (set-element-value *display* :table x-position 10)
  (set-element-value *display* :table text-x-padding 2)
  (set-element-value *display* :table row-height 19)
  (set-element-value *display* :table color (vector 0.0 0.0 0.0))

  (set-element-value *display* :table column-widths #(20 50 100))
  (set-element-value *display* :table
                     contents
                     (make-array '(7 3)
                                 :initial-contents `(("A" 2 1.5)
                                                     ("B" 4 3.4283)
                                                     ("C" 6 1/2)
                                                     ("D" 8 1/15)
                                                     ("E" 10 ,(expt 2 1/12))
                                                     ("?" 12 "undef")
                                                     ("?" 14 "undef"))))

  (add-element *display* :vicentino 'display-element-image :image-file-path "vicentino-test.jpg")
  (set-element-value *display* :vicentino y-position 600)
  (set-element-value *display* :vicentino x-position 300)
  (set-element-value *display* :vicentino scaling 0.1)


  (setf (background-color *display*) (vector 0.2 0.2 0.2)))

(defun start ()
  (boot *display*)
  (scene))

;; (add-element *display* (make-instance 'display-element-panel :title "hi" :x-position 500 :y-position 500) :test)







;; (init-faderfox-communication)



;;; obsolete, kept for reference because texture stuff hasn't been migrated to new class system
;; TODO
;; - Migrate texture handling to a class


;; (defun main ()
;;   (init-faderfox-communication)
;;   (setf *root-position* (cons 0.0 0.0))
;;   (setf *mix-amount* 0.5)
;;   (glfw:with-init-window (:title "Arcimoog Display" :width *screen-width* :height *screen-height*)
;;     (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)
;;     (gl:viewport 0 0 *screen-width* *screen-height*)

;;     (update-global-projection)

;;     (let* ((font-shader (make-instance 'shader-class
;;                                        :vertex-source (concatenate 'string *shader-path*
;;                                                                    "font-shader.vert")
;;                                        :fragment-source (concatenate 'string *shader-path*
;;                                                                      "font-shader.frag")))
;;            (font (make-instance 'font-render-class
;;                                 :character-set *global-character-set*
;;                                 :shader-instance font-shader))
;;            (shape-drawer (make-instance 'renderer-2d-class)))

;;       (let ((our-shader (make-instance 'shader-class
;;                                        :vertex-source (concatenate 'string *shader-path*
;;                                                                    "shader-texture.vs")
;;                                        :fragment-source (concatenate 'string *shader-path*
;;                                                                      "shader-texture.fs"))))

;;         (let ((vertices (vector 0.5 0.5 0.0    1.0 0.0 0.0    1.0 1.0
;;                                 0.5 -0.5 0.0   0.0 1.0 0.0    1.0 0.0
;;                                 -0.5 -0.5 0.0  0.0 0.0 1.0    0.0 0.0
;;                                 -0.5 0.5 0.0   1.0 1.0 0.0    0.0 1.0))
;;               (indices (vector 0 1 3
;;                                1 2 3))
;;               (vao (gl:gen-vertex-array))
;;               (vbo (gl:gen-buffer))
;;               (ebo (gl:gen-buffer)))
;;           (gl:bind-vertex-array vao)
;;           (gl:bind-buffer :array-buffer vbo)
;;           (gl:buffer-data :array-buffer :static-draw (array-to-gl-array vertices :float))
;;           (gl:bind-buffer :element-array-buffer ebo)
;;           (gl:buffer-data :element-array-buffer :static-draw (array-to-gl-array indices :unsigned-int))
;;           (gl:vertex-attrib-pointer 0 3 :float nil (* 8 (cffi:foreign-type-size :float))
;;                                     (cffi:null-pointer))
;;           (gl:enable-vertex-attrib-array 0)
;;           (gl:vertex-attrib-pointer 1 3 :float nil (* 8 (cffi:foreign-type-size :float))
;;                                     (cffi:inc-pointer (cffi:null-pointer)
;;                                                       (* 3 (cffi:foreign-type-size :float))))
;;           (gl:enable-vertex-attrib-array 1)
;;           (gl:vertex-attrib-pointer 2 2 :float nil (* 8 (cffi:foreign-type-size :float))
;;                                     (cffi:inc-pointer (cffi:null-pointer)
;;                                                       (* 6 (cffi:foreign-type-size :float))))
;;           (gl:enable-vertex-attrib-array 2)
;;           (gl:bind-buffer :array-buffer 0)
;;           (gl:bind-vertex-array 0)


;;           (let ((texture1 (gl:gen-texture))
;;                 (texture2 (gl:gen-texture))
;;                 (image-size))
;;             (gl:bind-texture :texture-2d texture1)
;;             (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
;;             (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
;;             (gl:tex-parameter :texture-2d :texture-min-filter :linear)
;;             (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
;;             (multiple-value-bind (data height width)
;;                 (cl-jpeg:decode-image (concatenate 'string *texture-path* "vicentino-test.jpg"))
;;               (cond (data
;;                      (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
;;                      (gl:generate-mipmap :texture-2d))
;;                     (t (format t "~&Failed to load texture."))))

;;             (gl:bind-texture :texture-2d texture2)
;;             (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
;;             (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
;;             (gl:tex-parameter :texture-2d :texture-min-filter :linear)
;;             (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
;;             (multiple-value-bind (data height width)
;;                 (cl-jpeg:decode-image (concatenate 'string *texture-path* "aron.jpg"))
;;               (setf image-size (cons width height))
;;               (cond (data
;;                      (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
;;                      (gl:generate-mipmap :texture-2d))
;;                     (t (format t "~&Failed to load texture2."))))
;;             (use our-shader)
;;             (set-uniform our-shader "texture1" 'int 0)
;;             (set-uniform our-shader "texture2" 'int 1)


;; ;;; Scheduling-based loop, in the INCUDINE real time thread:

;;             ;; (format t "~&Starting scheduling based render loop.")
;;             ;; (let ((frame-counter 0))
;;             ;;   (labels ((loop-trigger ()
;;             ;;              (cond ((glfw:window-should-close-p)
;;             ;;                     (gl:delete-vertex-arrays (list vao))
;;             ;;                     (gl:delete-buffers (list vbo ebo))
;;             ;;                     (destroy our-shader)
;;             ;;                     (format t "~&Rendering terminated."))
;;             ;;                    (t (render-loop font shape-drawer)
;;             ;;                       (format t "~&Frame counter: ~a." frame-counter)
;;             ;;                       (incudine:at (+ (incudine:now) #[0.5 s]) #'loop-trigger)))))
;;             ;;     (loop-trigger)))

;; ;;; Gentle loop, with SLEEP
;;             (loop until (glfw:window-should-close-p) do
;;               (render-loop font shape-drawer)
;;               (sleep (/ 1 30)))


;; ;;; Hardcore loop, max FPS
;;             ;; (loop until (glfw:window-should-close-p) do
;;             ;; (progn
;;             ;;   (process-input)
;;             ;;   (clear-global-background)
;;             ;;   (update-global-projection)
;;             ;;   (gl:active-texture :texture0)
;;             ;;   (gl:bind-texture :texture-2d texture1)
;;             ;;   (gl:active-texture :texture1)
;;             ;;   (gl:bind-texture :texture-2d texture2)
;;             ;;   (use our-shader)
;;             ;;   (gl:bind-vertex-array vao)
;;             ;;   (set-uniform our-shader "mixAmount" 'float *mix-amount*)
;;             ;;   (setf *view-matrix* (glm:create-identity-matrix 4))
;;             ;;   (glm:transform-matrix *view-matrix* glm:translate (vector 0.0 0.0 -0.5))
;;             ;;   (update-global-projection)
;;             ;;   (set-uniform-matrix our-shader "projection" (glm:lisp-to-gl-matrix *projection-matrix*))
;;             ;;   (set-uniform-matrix our-shader "view" (glm:lisp-to-gl-matrix *view-matrix*))
;;             ;;   (setf *model-matrix* (glm:create-identity-matrix 4))
;;             ;;   (glm:transform-matrix *model-matrix* glm:scale (vector (* 0.2 (car image-size))
;;             ;;                                                  (* 0.2 (cdr image-size))
;;             ;;                                                  1.0))
;;             ;;   (glm:transform-matrix *model-matrix* glm:translate (vector 400.0 300.0 0.0))
;;             ;;   (glm:transform-matrix *model-matrix* glm:translate (vector (car *root-position*)
;;             ;;                                                      (cdr *root-position*)
;;             ;;                                                      0.0))


;;             ;;   (render-vicentinos our-shader)

;;             ;;   (render-all-texts font)

;;             ;;   (render-shapes shape-drawer font)


;;             ;;   (glfw:swap-buffers)
;;             ;;   (glfw:poll-events))
;;             ;; )


;;             (gl:delete-vertex-arrays (list vao))
;;             (gl:delete-buffers (list vbo ebo))
;;             (destroy our-shader)

;;             ))))))

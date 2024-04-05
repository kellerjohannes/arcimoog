(in-package :arcimoog)

;; dummy
(defun midi-responder (a b c)
  (declare (ignore a b c)))

(defparameter *midi-in* nil)
(defparameter *midi-responder* nil)

(defun init-faderfox-communication ()
  (incudine:rt-start)

  (pm:initialize)

  (unless *midi-in*
    (setf *midi-in* (pm:open (pm:get-device-id-by-name "Faderfox EC4 MIDI 1" :input))))

  (incudine:recv-start *midi-in*)

  (unless *midi-responder*
    (setf *midi-responder* (incudine:make-responder *midi-in* (lambda (a b c)
                                                                (midi-responder a b c))))))

(defparameter *shader-path* "/home/johannes/common-lisp/arcimoog/lisp/shaders/")
(defparameter *texture-path* "/home/johannes/common-lisp/arcimoog/lisp/textures/")

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *projection-matrix* nil)
(defparameter *view-matrix* nil)
(defparameter *model-matrix* nil)
(defparameter *root-position* (cons 0.0 0.0))
(defparameter *font-matrix* nil)

;;;; helper stuff, not in the original c code

(defun array-to-gl-array (lisp-array data-type)
  (declare (type simple-vector lisp-array))
  (let ((gl-array (gl:alloc-gl-array data-type (length lisp-array))))
    (loop for item across lisp-array
          for i from 0 do
            (setf (gl:glaref gl-array i) item))
    gl-array))

;; Necessary, because gl:draw-elements contains a problematic implementation of the offset
;; parameter.

(defun my-gl-draw-elements (mode count type &key (offset 0))
  (%gl:draw-elements mode count type offset))

(defparameter *mix-amount* 0.5)

(glfw:def-window-size-callback framebuffer-size-callback (window width height)
  (declare (ignore window))
  (gl:viewport 0 0 width height)
  (setf *projection-matrix* (ortho 0.0 width 0.0 height 0.1 100.0))
  )

(defun process-input ()
  (when (or (eq (glfw:get-key :escape) :press)
            (eq (glfw:get-key :q) :press))
    (glfw:set-window-should-close))
  (when (and (< *mix-amount* 1.0)
             (eq (glfw:get-key :up) :press))
    (incf *mix-amount* 0.01))
  (when (and (> *mix-amount* 0.0)
             (eq (glfw:get-key :down) :press))
    (decf *mix-amount* 0.01))
  (when (eq (glfw:get-key :h) :press)
    (decf (car *root-position*) 1.0))
  (when (eq (glfw:get-key :l) :press)
    (incf (car *root-position*) 1.0))
  (when (eq (glfw:get-key :j) :press)
    (decf (cdr *root-position*) 1.0))
  (when (eq (glfw:get-key :k) :press)
    (incf (cdr *root-position*) 1.0)))


(defparameter *amount-of-copies* 1)

(defun midi-responder (channel-raw controller-raw value-raw)
  ;;(format t "~&raw: ~d ~d ~d~%" channel-raw controller-raw value-raw)
  (case (- channel-raw 176)
    (2 (case controller-raw
         (0 (decf (car *root-position*) (- 64.0 value-raw)))
         (1 (decf (cdr *root-position*) (- 64.0 value-raw)))
         (3 (setf *amount-of-copies* value-raw))
         (otherwise (format t "~&Unknown Faderfox controller in setup page 2."))))
    (otherwise (format t "~&Unknown Faderfox setup page."))))

(defclass font-render-class ()
  ((character-set :initform "⸮" :initarg :character-set :accessor character-set)
   (font-source :initform nil :initarg :font-source :accessor font-source)
   (pixel-size :initform 48 :initarg :pixel-size :accessor pixel-size)
   (ft-face :accessor ft-face)
   (textures :initform (make-hash-table) :accessor textures
             :documentation "Keys are CHARs, values are lists where the first element is the GL texture id, the second is the
width of the texture and the third its height.")
   (glyph-vao :initform -1 :accessor glyph-vao)
   (glyph-vbo :initform -1 :accessor glyph-vbo)
   (shader-instance :initarg :shader-instance :accessor shader-instance)))


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
      (setf source "/usr/share/fonts/TTF/DejaVuSans.ttf"))
    (setf (ft-face renderer) (ft2:new-face source))

    (unless (position #\⸮ character-set)
      (setf character-set (concatenate 'string character-set "⸮"))))

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

(defmethod render-string ((renderer font-render-class) text x-origin y-origin
                          &key (scale-factor 1.0) (rgb-vector #(1.0 1.0 1.0)))
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
    (set-uniform-matrix shader "projection" (lisp-to-gl-matrix *projection-matrix*))
    (gl:active-texture :texture0)
    (gl:bind-vertex-array vao)
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
          (gl:draw-arrays :triangles 0 6))))
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)))

(defun render-vicentinos (shader)
  (dotimes (i *amount-of-copies*)
    (transform-matrix *model-matrix* translate #(10.1 10.1 0.0))
    (set-uniform-matrix shader "model" (lisp-to-gl-matrix *model-matrix*))
    (my-gl-draw-elements :triangles 6 :unsigned-int)))

(defun main ()
  (init-faderfox-communication)
  (setf *root-position* (cons 0.0 0.0))
  (setf *mix-amount* 0.5)
  (glfw:with-init-window (:title "Arcimoog Display" :width *screen-width* :height *screen-height*)
    (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)
    (gl:viewport 0 0 *screen-width* *screen-height*)

    (setf *projection-matrix* (ortho 0.0 *screen-width* 0.0 *screen-height* 0.1 100.0))

    (let* ((font-shader (make-instance 'shader-class
                                        :vertex-source (concatenate 'string *shader-path*
                                                                    "font-shader.vert")
                                        :fragment-source (concatenate 'string *shader-path*
                                                                      "font-shader.frag")))
           (font (make-instance 'font-render-class
                                :character-set " abcdefghijklmnopqrstuvwxyz"
                                :shader-instance font-shader)))

      (let ((our-shader (make-instance 'shader-class
                                       :vertex-source (concatenate 'string *shader-path*
                                                                   "shader-texture.vs")
                                       :fragment-source (concatenate 'string *shader-path*
                                                                     "shader-texture.fs"))))

        (let ((vertices #(0.5 0.5 0.0    1.0 0.0 0.0    1.0 1.0
                          0.5 -0.5 0.0   0.0 1.0 0.0    1.0 0.0
                          -0.5 -0.5 0.0  0.0 0.0 1.0    0.0 0.0
                          -0.5 0.5 0.0   1.0 1.0 0.0    0.0 1.0))
              (indices #(0 1 3
                         1 2 3))
              (vao (gl:gen-vertex-array))
              (vbo (gl:gen-buffer))
              (ebo (gl:gen-buffer)))
          (gl:bind-vertex-array vao)
          (gl:bind-buffer :array-buffer vbo)
          (gl:buffer-data :array-buffer :static-draw (array-to-gl-array vertices :float))
          (gl:bind-buffer :element-array-buffer ebo)
          (gl:buffer-data :element-array-buffer :static-draw (array-to-gl-array indices :unsigned-int))
          (gl:vertex-attrib-pointer 0 3 :float nil (* 8 (cffi:foreign-type-size :float))
                                    (cffi:null-pointer))
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 1 3 :float nil (* 8 (cffi:foreign-type-size :float))
                                    (cffi:inc-pointer (cffi:null-pointer)
                                                      (* 3 (cffi:foreign-type-size :float))))
          (gl:enable-vertex-attrib-array 1)
          (gl:vertex-attrib-pointer 2 2 :float nil (* 8 (cffi:foreign-type-size :float))
                                    (cffi:inc-pointer (cffi:null-pointer)
                                                      (* 6 (cffi:foreign-type-size :float))))
          (gl:enable-vertex-attrib-array 2)
          (gl:bind-buffer :array-buffer 0)
          (gl:bind-vertex-array 0)


          (let ((texture1 (gl:gen-texture))
                (texture2 (gl:gen-texture))
                (image-size))
            (gl:bind-texture :texture-2d texture1)
            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
            (gl:tex-parameter :texture-2d :texture-min-filter :linear)
            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
            (multiple-value-bind (data height width)
                (cl-jpeg:decode-image (concatenate 'string *texture-path* "vicentino-test.jpg"))
              (cond (data
                     (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
                     (gl:generate-mipmap :texture-2d))
                    (t (format t "~&Failed to load texture."))))

            (gl:bind-texture :texture-2d texture2)
            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
            (gl:tex-parameter :texture-2d :texture-min-filter :linear)
            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
            (multiple-value-bind (data height width)
                (cl-jpeg:decode-image (concatenate 'string *texture-path* "aron.jpg"))
              (setf image-size (cons width height))
              (cond (data
                     (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
                     (gl:generate-mipmap :texture-2d))
                    (t (format t "~&Failed to load texture2."))))
            (use our-shader)
            (set-uniform our-shader "texture1" 'int 0)
            (set-uniform our-shader "texture2" 'int 1)


            (loop until (glfw:window-should-close-p) do
              (progn
                (process-input)
                (gl:clear-color 0.2 0.3 0.3 1.0)
                (gl:clear :color-buffer-bit)
                (gl:active-texture :texture0)
                (gl:bind-texture :texture-2d texture1)
                (gl:active-texture :texture1)
                (gl:bind-texture :texture-2d texture2)
                (use our-shader)
                (gl:bind-vertex-array vao)
                (set-uniform our-shader "mixAmount" 'float *mix-amount*)
                (setf *view-matrix* (create-identity-matrix 4))
                (transform-matrix *view-matrix* translate #(0.0 0.0 -0.5))
                (set-uniform-matrix our-shader "projection" (lisp-to-gl-matrix *projection-matrix*))
                (set-uniform-matrix our-shader "view" (lisp-to-gl-matrix *view-matrix*))
                (setf *model-matrix* (create-identity-matrix 4))
                (transform-matrix *model-matrix* scale (vector (* 0.2 (car image-size))
                                                               (* 0.2 (cdr image-size))
                                                               1.0))
                (transform-matrix *model-matrix* translate #(400.0 300.0 0.0))
                (transform-matrix *model-matrix* translate (vector (car *root-position*)
                                                                   (cdr *root-position*)
                                                                   0.0))


                (render-vicentinos our-shader)

                (render-string font "abcdefghijklmnopqrstuvwxyz"
                               (+ (car *root-position*) 50.0)
                               (+ (cdr *root-position*) 100.0)
                               :rgb-vector #(0.9 0.2 0.2)
                               :scale-factor 2.0)



                (glfw:swap-buffers)
                (glfw:poll-events)))
            (gl:delete-vertex-arrays (list vao))
            (gl:delete-buffers (list vbo ebo))
            (destroy our-shader)))))))

(defpackage :opengl-dev5
  (:use :cl))

(in-package :opengl-dev5)

(asdf:load-system "incudine")

(incudine:rt-start)

(pm:initialize)

(defparameter *midi-in* (pm:open (pm:get-device-id-by-name "Faderfox EC4 MIDI 1" :input)))

(incudine:recv-start *midi-in*)

(defun midi-responder (a b c)
  (declare (ignore a b c)))


(defvar resp5
  (incudine:make-responder *midi-in* (lambda (a b c) (midi-responder a b c))))





(defparameter *shader-path* "/home/johannes/common-lisp/arcimoog/lisp/shaders/")
(defparameter *texture-path* "/home/johannes/common-lisp/arcimoog/lisp/textures/")

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *projection-matrix* nil)
(defparameter *view-matrix* nil)
(defparameter *model-matrix* nil)
(defparameter *root-position* (cons 0.0 0.0))
(defparameter *font-matrix* nil)

(asdf:load-system "array-operations")
(asdf:load-system "cl-freetype2")
(asdf:load-system "cl-opengl")
(asdf:load-system "cl-glfw3")
(asdf:load-system "cl-jpeg")

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

(defun validp (shader)
  (> shader -1))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn
        (format t "~A~%" error-string)
        (error 'acond:compile-error :message error-string)))))

(defclass shader-class ()
  ((id :accessor id
       :initform -1
       :documentation "OpenGL-identifier for shader program.")
   (vertex-source-path :initarg :vertex-source
                       :reader vertex-source-path)
   (fragment-source-path :initarg :fragment-source
                         :reader fragment-source-path)))

(defgeneric use (shader))

(defgeneric destroy (shader))

(defgeneric set-uniform (shader name type &rest parameters))

(defmethod initialize-instance :after ((shader shader-class) &key)
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader)))
    (loop while (not (validp (id shader))) do
      (with-simple-restart
          (retry "Retry compiling shaders.")
        (gl:shader-source vertex-shader (uiop:read-file-string (vertex-source-path shader)))
        (gl:shader-source fragment-shader (uiop:read-file-string (fragment-source-path shader)))
        (gl:compile-shader vertex-shader)
        (gl:compile-shader fragment-shader)
        (check-shader-error vertex-shader)
        (check-shader-error fragment-shader)
        (setf (id shader) (gl:create-program))
        (gl:attach-shader (id shader) vertex-shader)
        (gl:attach-shader (id shader) fragment-shader)
        (gl:link-program (id shader))
        (gl:use-program (id shader))
        (gl:delete-shader vertex-shader)
        (gl:delete-shader fragment-shader)))))

(defmethod use ((shader shader-class))
  (gl:use-program (id shader)))

(defmethod destroy ((shader shader-class))
  (gl:delete-program (id shader)))

(defmethod set-uniform ((shader shader-class) name type &rest parameters)
  (use shader)
  (ecase type
    (float (apply #'gl:uniformf (append (list (gl:get-uniform-location (id shader) name))
                                        parameters)))
    (int (apply #'gl:uniformi (append (list (gl:get-uniform-location (id shader) name))
                                        parameters)))))

(defmethod set-uniform-matrix ((shader shader-class) name matrix-array)
  (use shader)
  (gl:program-uniform-matrix-4fv (id shader) (gl:get-uniform-location (id shader) name) matrix-array))

(defmacro with-shader (shader &body body)
  `(progn
     (use ,shader)
     ,@body
     (gl:use-program 0)))

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
  ;; (format t "~&raw: ~d ~d ~d~%" channel-raw controller-raw value-raw)
  (case (- channel-raw 176)
    (2 (case controller-raw
         (0 (decf (car *root-position*) (- 64.0 value-raw)))
         (1 (decf (cdr *root-position*) (- 64.0 value-raw)))
         (3 (setf *amount-of-copies* value-raw))
         (otherwise (format t "~&Unknown Faderfox controller in setup page 2."))))
    (otherwise (format t "~&Unknown Faderfox setup page.")))

  )


(defparameter *texture-ids* nil)
(defparameter *character-set* "ᴀʙᴄ»«❜'➚ſꝑ➙ȧȦ♯♭♮abcdefghijklmnopqrstuvwxyzäöüABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ")
(defparameter *face* (ft2:new-face "/usr/share/fonts/TTF/DejaVuSans.ttf"))

(defun lookup-texture-ids (character)
  (let ((result (cdr (assoc character *texture-ids*))))
    (values (first result) ; texture-id
            (second result) ; width
            (third result) ; height
            )))

(defun generate-characters ()
  (setf *texture-ids* nil)
  (ft2:set-pixel-sizes *face* 0 48)
  (ft2:do-string-render (*face* *character-set* bitmap x y :with-char character)
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

      (push (cons character (list texture-id
                                  (coerce (second dimensions) 'single-float)
                                  (coerce (first dimensions) 'single-float)))
            *texture-ids*))))

(defun render-vicentinos (shader)
  (dotimes (i *amount-of-copies*)
    (transform *model-matrix* translate #(10.1 10.1 0.0))
    (set-uniform-matrix shader "model" (lisp-to-gl-matrix *model-matrix*))
    (my-gl-draw-elements :triangles 6 :unsigned-int)))

(defun main ()
  (setf *root-position* (cons 0.0 0.0))
  (setf *mix-amount* 0.5)
  (glfw:with-init-window (:title "LearnOpenGL" :width *screen-width* :height *screen-height*)
    (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)
    (gl:viewport 0 0 *screen-width* *screen-height*)
    (setf *projection-matrix* (ortho 0.0 *screen-width* 0.0 *screen-height* 0.1 100.0))
    (let ((our-shader (make-instance 'shader-class
                                     :vertex-source (concatenate 'string *shader-path*
                                                                 "shader-texture.vs")
                                     :fragment-source (concatenate 'string *shader-path*
                                                                   "shader-texture.fs")))
          (font-shader (make-instance 'shader-class
                                      :vertex-source (concatenate 'string *shader-path*
                                                                  "font-shader.vert")
                                      :fragment-source (concatenate 'string *shader-path*
                                                                    "font-shader.frag"))))

      (let ((vertices #(0.5 0.5 0.0    1.0 0.0 0.0    1.0 1.0
                        0.5 -0.5 0.0   0.0 1.0 0.0    1.0 0.0
                        -0.5 -0.5 0.0  0.0 0.0 1.0    0.0 0.0
                        -0.5 0.5 0.0   1.0 1.0 0.0    0.0 1.0))
            (indices #(0 1 3
                       1 2 3))
            (vao (gl:gen-vertex-array))
            (vbo (gl:gen-buffer))
            (ebo (gl:gen-buffer))
            (font-vao (gl:gen-vertex-array))
            (font-vbo (gl:gen-buffer)))
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

        (gl:bind-vertex-array font-vao)
        (gl:bind-buffer :array-buffer font-vbo)
        (gl:buffer-data :array-buffer
                        :dynamic-draw
                        (array-to-gl-array (make-array (* 6 4) :initial-element 0.0) :float))
        (gl:vertex-attrib-pointer 0 4 :float nil (* 4 (cffi:foreign-type-size :float)) (cffi:null-pointer))
        (gl:enable-vertex-attrib-array 0)
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

          (gl:pixel-store :unpack-alignment 1)
          (generate-characters)

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
              (transform *view-matrix* translate #(0.0 0.0 -0.5))
              (set-uniform-matrix our-shader "projection" (lisp-to-gl-matrix *projection-matrix*))
              (set-uniform-matrix our-shader "view" (lisp-to-gl-matrix *view-matrix*))
              (setf *model-matrix* (create-identity-matrix 4))
              (transform *model-matrix* scale (vector (* 0.2 (car image-size))
                                                      (* 0.2 (cdr image-size))
                                                      1.0))
              (transform *model-matrix* translate #(400.0 300.0 0.0))
              (transform *model-matrix* translate (vector (car *root-position*)
                                                          (cdr *root-position*)
                                                          0.0))


              (render-vicentinos our-shader)

              (gl:enable :blend)
              (gl:blend-func :src-alpha :one-minus-src-alpha)

              (use font-shader)
              (set-uniform font-shader "textColor" 'float 1.0 0.5 0.2)
              (set-uniform-matrix font-shader "projection" (lisp-to-gl-matrix *projection-matrix*))
              (gl:active-texture :texture0)

              (gl:bind-vertex-array font-vao)

              (let ((font-scale 0.5))
                (ft2:do-string-render (*face* *character-set* bitmap x y :with-char character)
                  (multiple-value-bind (texture-id texture-width texture-height)
                      (lookup-texture-ids character)
                    (let* ((glyph-width (* font-scale texture-width))
                           (glyph-height (* font-scale texture-height))
                           (x-pos (* font-scale (+ 20.0 (coerce x 'single-float))))
                           (y-pos (* font-scale (- 100.0
                                                   texture-height
                                                   (coerce y 'single-float))))
                           (quad-vertices (vector
                                           x-pos (+ y-pos glyph-height)    0.0 0.0
                                           x-pos y-pos                     0.0 1.0
                                           (+ x-pos glyph-width) y-pos     1.0 1.0
                                           x-pos (+ y-pos glyph-height)    0.0 0.0
                                           (+ x-pos glyph-width) y-pos     1.0 1.0
                                           (+ x-pos glyph-width) (+ y-pos glyph-height) 1.0 0.0)))

                      (setf *font-matrix* (create-identity-matrix 4))
                      (transform *font-matrix* translate #(0.0 0.0 -0.5))
                      (set-uniform-matrix font-shader "fontMatrix" (lisp-to-gl-matrix *font-matrix*))
                      (set-uniform-matrix font-shader "projection" (lisp-to-gl-matrix *projection-matrix*))


                      (gl:bind-texture :texture-2d texture-id)
                      (gl:bind-buffer :array-buffer font-vbo)
                      (gl:buffer-data :array-buffer
                                      :dynamic-draw
                                      (array-to-gl-array quad-vertices :float))
                      (gl:bind-buffer :array-buffer 0)
                      (gl:draw-arrays :triangles 0 6)))))
              (gl:bind-vertex-array 0)
              (gl:bind-texture :texture-2d 0)

              (glfw:swap-buffers)
              (glfw:poll-events)))
          (gl:delete-vertex-arrays (list vao))
          (gl:delete-buffers (list vbo ebo))
          (destroy our-shader))))))

(in-package :arcimoog)

;; TODO
;;
;; * explore Bravura usage
;;
;; * move opengl stuff to separate package - start implementing an abstraction layer for graphics on top of opengl package, in its own package
;;
;; * create another abstraction layer for 'panels', representing the configuration and real time values for each arcimoog module
;;
;; * think about a way to organise panels and the connection to the faderfox slots. some are hardwired (global projection), others need to be changed dynamically (parameters for multiple (polyphonic) pipelines)


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
(defparameter *global-character-set*
  " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZȧḃċḋėḟġȦḂĊḊĖḞĠ♯♭♮❜ʼ'\"«»[]#{}/\\,.!?:;➙➚➘12345674890-+*·")
(defparameter *global-background-color* (vector 0.0 0.0 0.0))
(defparameter *global-projection-scaling* 1.0)
(defparameter *global-projection-translation* (vector 0.0 0.0 0.0))

(defun reset-global-projection-parameters ()
  (setf *global-projection-scaling* 1.0)
  (setf *global-projection-translation* (vector 0.0 0.0 0.0)))

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

(defun update-global-projection ()
  (setf *projection-matrix* (ortho 0.0 *screen-width* 0.0 *screen-height* 0.1 100.0))
  (transform-matrix *projection-matrix* translate *global-projection-translation*)
  (transform-matrix *projection-matrix* scale (vector *global-projection-scaling*
                                                      *global-projection-scaling*
                                                      1.0)))

(glfw:def-window-size-callback framebuffer-size-callback (window width height)
  (declare (ignore window))
  (setf *screen-width* width)
  (setf *screen-height* height)
  (gl:viewport 0 0 width height)
  (update-global-projection))

(defun process-input ()
  (when (or (eq (glfw:get-key :escape) :press)
            (eq (glfw:get-key :q) :press))
    (glfw:set-window-should-close)))


(defparameter *amount-of-copies* 1)

(defun midi-scale (value lower upper)
  (+ (* (/ value 127.0) (- upper lower)) lower))

(defparameter *parameter-slots* (make-array 256 :initial-element 0))

(defun set-slot (slot-number value)
  (setf (aref *parameter-slots* slot-number) value))

(defun slot (slot-number)
  (aref *parameter-slots* slot-number))

(defun midi-responder (channel-raw controller-raw value-raw)
  ;;(format t "~&raw: ~d ~d ~d~%" channel-raw controller-raw value-raw)
  (case (- channel-raw 176)
    (2 (case controller-raw
         (0 (incf (aref *global-projection-translation* 0) (* -0.005 (- 64.0 value-raw))))
         (1 (incf (aref *global-projection-translation* 1) (* -0.005 (- 64.0 value-raw))))
         (2 (incf *global-projection-scaling* (* -0.01 (- 64.0 value-raw))))
         (3 (reset-global-projection-parameters))
         (4 (setf (aref *global-background-color* 0) (midi-scale value-raw 0.0 1.0)))
         (5 (setf (aref *global-background-color* 1) (midi-scale value-raw 0.0 1.0)))
         (6 (setf (aref *global-background-color* 2) (midi-scale value-raw 0.0 1.0)))
         (50 (set-slot 0 (midi-scale value-raw 0.0 1.0)))
         (51 (set-slot 1 (midi-scale value-raw 0.0 1.0)))
         (52 (set-slot 2 (midi-scale value-raw 0.0 1.0)))
         (53 (set-slot 3 (midi-scale value-raw 0.0 1.0)))
         (54 (set-slot 4 (midi-scale value-raw 0.0 1.0)))
         (55 (set-slot 5 (midi-scale value-raw 0.0 1.0)))
         (56 (set-slot 6 (midi-scale value-raw 0.0 1.0)))
         (57 (set-slot 7 (midi-scale value-raw 0.0 1.0)))
         (58 (set-slot 8 (midi-scale value-raw 0.0 1.0)))
         (59 (set-slot 9 (midi-scale value-raw 0.0 1.0)))
         (60 (set-slot 10 (midi-scale value-raw 0.0 1.0)))
         (61 (set-slot 11 (midi-scale value-raw 0.0 1.0)))
         (62 (set-slot 12 (midi-scale value-raw 0.0 1.0)))
         (63 (set-slot 13 (midi-scale value-raw 0.0 1.0)))
         (64 (set-slot 14 (midi-scale value-raw 0.0 1.0)))
         (65 (set-slot 15 (midi-scale value-raw 0.0 1.0)))
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
      (setf source
            "/usr/share/fonts/TTF/DejaVuSans.ttf"
            ;; "/usr/share/fonts/OTF/BravuraText.otf"
            ;; "/usr/share/fonts/OTF/Bravura.otf"
            ))
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
                          &key (scale-factor 1.0) (rgb-vector (vector 1.0 1.0 1.0)))
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

(defclass graphics-renderer-2d ()
  ((shader :initform nil :accessor shader)
   (vao :initform nil :accessor vao)
   (vbo :initform nil :accessor vbo)
   (default-color :initform (vector 1.0 1.0 1.0) :initarg :color :accessor default-color)
   (view-matrix :initform nil :accessor view-matrix)
   (model-matrix :initform nil :accessor model-matrix)
   (vbo-allocation-size :initform 1000 :initarg :vbo-size :accessor vbo-allocation-size
                        :documentation "Max number of 2d-coordinates that will be allocated in the VBO.")))

(defmethod initialize-instance :after ((renderer graphics-renderer-2d) &key)
  (with-accessors ((shader shader)
                   (view view-matrix)
                   (model model-matrix)
                   (vao vao)
                   (vbo vbo))
      renderer
    (setf shader (make-instance 'shader-class
                                :vertex-source (concatenate 'string *shader-path*
                                                            "shader-2d.vert")
                                :fragment-source (concatenate 'string *shader-path*
                                                              "shader-2d.frag")))
    (setf model (create-identity-matrix 4))

    ;; view matrix is constant for now. Later it could be used to represent different layers of a 2d
    ;; display
    (setf view (create-identity-matrix 4))
    (transform-matrix view translate (vector 0.0 0.0 -0.5))
    (set-uniform-matrix shader "view" (lisp-to-gl-matrix view))

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
    (gl:bind-vertex-array 0)))

(defmethod render ((renderer graphics-renderer-2d) vertex-data
                   &key (mode :line-strip)
                     scaling
                     translation
                     rotation
                     (color (default-color renderer)))
  (with-accessors ((vao vao)
                   (vbo vbo)
                   (model model-matrix)
                   (shader shader))
      renderer
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-sub-data :array-buffer (array-to-gl-array vertex-data :float))
    (gl:bind-buffer :array-buffer 0)
    (set-uniform-matrix shader "projection" (lisp-to-gl-matrix *projection-matrix*))
    (setf model (create-identity-matrix 4))
    (when scaling (transform-matrix model scale scaling))
    (when translation (transform-matrix model translate translation))
    (when rotation (transform-matrix model rotate (car rotation) (cdr rotation)))
    (set-uniform-matrix shader "model" (lisp-to-gl-matrix model))
    (set-uniform shader "vertexColor" 'float (aref color 0) (aref color 1) (aref color 2))
    (gl:draw-arrays mode 0 (floor (/ (length vertex-data) 2)))
    (gl:bind-vertex-array 0)))


(defun render-vicentinos (shader)
  (dotimes (i *amount-of-copies*)
    (transform-matrix *model-matrix* translate (vector 10.1 10.1 0.0))
    (set-uniform-matrix shader "model" (lisp-to-gl-matrix *model-matrix*))
    (my-gl-draw-elements :triangles 6 :unsigned-int)))

(defun render-all-texts (font-instance)
  (loop for y from 0.0 by 55.0 repeat 16
        for i from 0 do
          (render-string font-instance (format nil "Slot ~a: ~,2f" i (slot i)) 10.0 (- 1000.0 y))))

(defun render-scale (renderer-instance font-renderer-instance offset tick-offsets helper-lines-p)
  (let ((main-line (vector 0.0 0.0 0.0 100.0))
        (tick-labels (vector "C" "D" "E" "F𝄡𝃅" "G♭♯" "A♯" "B♮" "C" "D" "E" "F"))
        (scale (vector 10.0 10.0 1.0))
        (origin (vector (+ 800.0 offset) 50.0 0.0))
        (tick-width 5.0)
        (label-padding 3.0)
        (helper-line-min -10.0)
        (helper-line-max 50.0)
        (helper-lines (make-array 44 :initial-element 0.0))
        (ticks (make-array 44 :initial-element 0.0)))
    (loop for y from 0.0 by 10.0 to 100.0
          for i from 0 do
            (let ((y-trimmed (+ y (aref tick-offsets i))))
              (setf (aref ticks (+ 0 (* 4 i))) 0.0
                    (aref ticks (+ 1 (* 4 i))) y-trimmed
                    (aref ticks (+ 2 (* 4 i))) tick-width
                    (aref ticks (+ 3 (* 4 i))) y-trimmed
                    (aref helper-lines (+ 0 (* 4 i))) helper-line-min
                    (aref helper-lines (+ 1 (* 4 i))) y-trimmed
                    (aref helper-lines (+ 2 (* 4 i))) helper-line-max
                    (aref helper-lines (+ 3 (* 4 i))) y-trimmed)))
    (loop for i from 0 to 10 do
      (render-string font-renderer-instance
                     (aref tick-labels i)
                     (+ offset 800.0 (* 10.0 (+ label-padding tick-width)))
                     (+ 25.0 50.0 (* 10.0 (aref ticks (+ 1 (* i 4)))))
                     :rgb-vector #(0.0 1.0 1.0)))
    (render renderer-instance main-line :mode :lines :scaling scale :translation origin)
    (when helper-lines-p
      (render renderer-instance helper-lines :mode :lines :scaling scale :translation origin :color #(1.0 1.0 0.0)))
    (render renderer-instance ticks :mode :lines :scaling scale :translation origin :color #(0.0 1.0 1.0))
    ))

(defun render-shapes (renderer-instance font-renderer-instance)
  (render-scale renderer-instance font-renderer-instance 0.0
                (let ((offsets (make-array 11 :initial-element 0.0)))
                  (loop for i from 0 to 10 do
                        (setf (aref offsets i) (* 10.0 (- (slot (+ 4 i)) 0.5))))
                  offsets)
                t)
  (render-scale renderer-instance
                font-renderer-instance
                200.0
                (make-array 11 :initial-element 0.0)
                nil)
  )

(defun clear-global-background ()
  (gl:clear-color (aref *global-background-color* 0)
                  (aref *global-background-color* 1)
                  (aref *global-background-color* 2)
                  1.0)
  (gl:clear :color-buffer-bit))

(defun main ()
  (init-faderfox-communication)
  (setf *root-position* (cons 0.0 0.0))
  (setf *mix-amount* 0.5)
  (glfw:with-init-window (:title "Arcimoog Display" :width *screen-width* :height *screen-height*)
    (glfw:set-framebuffer-size-callback 'framebuffer-size-callback)
    (gl:viewport 0 0 *screen-width* *screen-height*)

    (update-global-projection)

    (let* ((font-shader (make-instance 'shader-class
                                       :vertex-source (concatenate 'string *shader-path*
                                                                   "font-shader.vert")
                                       :fragment-source (concatenate 'string *shader-path*
                                                                     "font-shader.frag")))
           (font (make-instance 'font-render-class
                                :character-set *global-character-set*
                                :shader-instance font-shader))
           (shape-drawer (make-instance 'graphics-renderer-2d)))

      (let ((our-shader (make-instance 'shader-class
                                       :vertex-source (concatenate 'string *shader-path*
                                                                   "shader-texture.vs")
                                       :fragment-source (concatenate 'string *shader-path*
                                                                     "shader-texture.fs"))))

        (let ((vertices (vector 0.5 0.5 0.0    1.0 0.0 0.0    1.0 1.0
                                0.5 -0.5 0.0   0.0 1.0 0.0    1.0 0.0
                                -0.5 -0.5 0.0  0.0 0.0 1.0    0.0 0.0
                                -0.5 0.5 0.0   1.0 1.0 0.0    0.0 1.0))
              (indices (vector 0 1 3
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
                (clear-global-background)
                (update-global-projection)
                (gl:active-texture :texture0)
                (gl:bind-texture :texture-2d texture1)
                (gl:active-texture :texture1)
                (gl:bind-texture :texture-2d texture2)
                (use our-shader)
                (gl:bind-vertex-array vao)
                (set-uniform our-shader "mixAmount" 'float *mix-amount*)
                (setf *view-matrix* (create-identity-matrix 4))
                (transform-matrix *view-matrix* translate (vector 0.0 0.0 -0.5))
                (update-global-projection)
                (set-uniform-matrix our-shader "projection" (lisp-to-gl-matrix *projection-matrix*))
                (set-uniform-matrix our-shader "view" (lisp-to-gl-matrix *view-matrix*))
                (setf *model-matrix* (create-identity-matrix 4))
                (transform-matrix *model-matrix* scale (vector (* 0.2 (car image-size))
                                                               (* 0.2 (cdr image-size))
                                                               1.0))
                (transform-matrix *model-matrix* translate (vector 400.0 300.0 0.0))
                (transform-matrix *model-matrix* translate (vector (car *root-position*)
                                                                   (cdr *root-position*)
                                                                   0.0))


                (render-vicentinos our-shader)

                (render-all-texts font)

                (render-shapes shape-drawer font)


                (glfw:swap-buffers)
                (glfw:poll-events)))
            (gl:delete-vertex-arrays (list vao))
            (gl:delete-buffers (list vbo ebo))
            (destroy our-shader)))))))

(defun test ()
  (bt:make-thread (lambda () (main)) :name "test-window"))

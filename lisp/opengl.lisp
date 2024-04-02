;; testing two options for opengl: glut/glu and gflw3. glfw3 seems more suitable, since support for
;; multiple windows is stable, and www.learnopengl is compatible. minimal examples and font
;; rendering are still to be tested.

(in-package :arcimoog)

;; Necessary, because gl:draw-elements contains a problematic implementation of the offset
;; parameter.
(defun my-gl-draw-elements (mode count type &key (offset 0))
  (%gl:draw-elements mode count type offset))


;; (defclass minimal-window (glut:window)
;;   ()
;;   (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
;;                      :mode '(:double :rgb :depth) :title "Minimal OpenGL window"
;;                      :tick-interval (round 1000 60)))

;; (defmethod glut:tick ((w minimal-window))
;;   (declare (ignore w))
;;   (glut:post-redisplay))

;; (defmethod glut:display ((w minimal-window))
;;   (gl:clear-color 1.0 1.0 1.0 1.0)
;;   (gl:clear :color-buffer-bit :depth-buffer-bit)
;;   (glut:swap-buffers))

;; (defmethod glut:close ((w minimal-window)))

;; (defun minimal ()
;;   (let ((w (make-instance 'minimal-window)))
;;     (unwind-protect
;;          (glut:display-window w)
;;       (when (not (glut::destroyed w))
;;         (setf (glut::destroyed w) t)
;;         (glut:destroy-window (glut:id w))))))


;; (defun minimal-test ()
;;   (bt:make-thread (lambda () (minimal)) :name "minimal-window-test-1")
;;   )


;; (defparameter *test* (ft2:new-face "/usr/share/fonts/TTF/DejaVuSans.ttf"))

;; (ft2:set-char-size *test* (* 24 64) 0 72 72)

;; (ft2:print-with-face *test* "ſ")



;; (defclass shader-vao-window (glut:window)
;;   ((vbuff :accessor vertex-buffer)
;;    (ibuff :accessor index-buffer)
;;    (vs :accessor vertex-shader)
;;    (fs :accessor fragment-shader)
;;    (va :accessor vertex-array)
;;    (program :accessor program)
;;    (angle :accessor angle :initform 0.0))
;;   (:default-initargs :width 500 :height 500 :pos-x 100 :pos-y 100
;;                      :mode '(:double :rgb :depth) :title "shader-vao-window"
;;                      :tick-interval (round 1000 60)))


;; ;;; Initialization

;; ;;; First, we create buffers for our vertex and index
;; ;;; data. Then, we create the vertex array object that we actually use
;; ;;; for rendering directly. Finally, we load the shader objects.
;; (defmethod glut:display-window :before ((w shader-vao-window))
;;   ;; An array buffer can be used to store verex position, colors,
;;   ;; normals, or other data. We need to allocate an GL array, copy the
;;   ;; data to the array, and tell OpenGL that the buffers data comes
;;   ;; from this GL array. Like most OpenGL state objects, we bind the
;;   ;; buffer before we can make changes to its state.
;;   (unless (gl::features-present-p (>= :glsl-version 3.3))
;;     (log:warn "Window destroyed.")
;;     (glut:destroy-window (glut:id w))
;;     (return-from glut:display-window nil))
;;   (let ((buffers (gl:gen-buffers 2)))
;;     (setf (vertex-buffer w) (elt buffers 0)
;;           (index-buffer w) (elt buffers 1)))
;;   (gl:bind-buffer :array-buffer (vertex-buffer w))
;;   (let ((arr (gl:alloc-gl-array :float 12))
;;     (verts #(-0.5 -0.5 0.0
;;          -0.5 0.5 0.0
;;          0.5 -0.5 0.0
;;          0.5 0.5 0.0)))
;;     (dotimes (i (length verts))
;;       (setf (gl:glaref arr i) (aref verts i)))
;;     (gl:buffer-data :array-buffer :static-draw arr)
;;     (gl:free-gl-array arr))

;;   ;; 0 is always reserved as an unbound object.
;;   (gl:bind-buffer :array-buffer 0)

;;   ;; An element array buffer stores vertex indices. We fill it in the
;;   ;; same way as an array buffer.
;;   (gl:bind-buffer :element-array-buffer (index-buffer w))
;;   (let ((arr (gl:alloc-gl-array :unsigned-short 6))
;;     (indexes #(0 2 1 1 2 3)))
;;     (dotimes (i (length indexes))
;;       (setf (gl:glaref arr i) (aref indexes i)))
;;     (gl:buffer-data :element-array-buffer :static-draw arr)
;;     (gl:free-gl-array arr))
;;   (gl:bind-buffer :element-array-buffer 0)

;;   ;; Vertex array objects manage which vertex attributes are
;;   ;; associated with which data buffers.
;;   (setf (vertex-array w) (gl:gen-vertex-array))
;;   (gl:bind-vertex-array (vertex-array w))

;;   ;; To associate our VBO data with this VAO, we bind it, specify
;;   ;; which vertex attribute we want to associate it with, and specify
;;   ;; where the data comes from.
;;   (gl:bind-buffer :array-buffer (vertex-buffer w))
;;   ;; In this program, we use attribute 0 for position. If you had
;;   ;; per-vertex normals, you could use a different attribute for those
;;   ;; as well.
;;   (gl:enable-vertex-attrib-array 0)
;;   ;; Using a null pointer as the data source indicates that we want
;;   ;; the vertex data to come from the currently bound array-buffer.
;;   (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

;;   ;; To associate an element array with this VAO, all we need to do is
;;   ;; bind the element array buffer we want to use.
;;   (gl:bind-buffer :element-array-buffer (index-buffer w))

;;   ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
;;   (gl:bind-vertex-array 0)

;;   ;; A program object is a collection of shader objects to be used
;;   ;; together in a single pipeline for rendering objects. To create a
;;   ;; program, you first create the individual shaders. Then you attach
;;   ;; the shaders to the program and link the program together.
;;   (let ((vs (gl:create-shader :vertex-shader))
;;     (fs (gl:create-shader :fragment-shader)))
;;     (setf (vertex-shader w) vs)
;;     (setf (fragment-shader w) fs)
;;     (gl:shader-source vs (uiop:read-file-string "/home/johannes/common-lisp/prototypes/opengl/vertex-shader.vert"))
;;     (gl:compile-shader vs)
;;     (gl:shader-source fs (uiop:read-file-string "/home/johannes/common-lisp/prototypes/opengl/fragment-shader.frag"))
;;     (gl:compile-shader fs)
;;     ;; If the shader doesn't compile, you can print errors with:
;;     ;; (print (gl:get-shader-info-log vs))
;;     ;; (print (gl:get-shader-info-log fs))

;;     (setf (program w) (gl:create-program))
;;     ;; You can attach the same shader to multiple different programs.
;;     (gl:attach-shader (program w) vs)
;;     (gl:attach-shader (program w) fs)
;;     ;; Don't forget to link the program after attaching the
;;     ;; shaders. This step actually puts the attached shader together
;;     ;; to form the program.
;;     (gl:link-program (program w))
;;     ;; If we want to render using this program object, or add
;;     ;; uniforms, we need to use the program. This is similar to
;;     ;; binding a buffer.
;;     (gl:use-program (program w))))

;; (defmethod glut:tick ((w shader-vao-window))
;;   (when (slot-boundp w 'program)
;;     (let ((seconds-per-revolution 12))
;;              (incf  (angle w)
;;                     (/ (* 2 pi) (* 60 seconds-per-revolution))))
;;     (gl:uniformf (gl:get-uniform-location (program w) "angle") (angle w))
;;     (glut:post-redisplay)))

;; (defmethod glut:display ((w shader-vao-window))
;;   (gl:clear-color 0.0 0.0 0.0 1.0)
;;   (gl:clear :color-buffer-bit :depth-buffer-bit)

;;   ;; Since we never use any other program object, this is unnecessary
;;   ;; in this program. Typically, though, you'll have multiple program
;;   ;; objects, so you'll need to 'use' each one to activate it.
;;   (gl:use-program (program w))
;;   (gl:bind-vertex-array (vertex-array w))

;;   ;; This call actually does the rendering. The vertex data comes from
;;   ;; the currently-bound VAO. If the input array is null, the indices
;;   ;; will be taken from the element array buffer bound in the current
;;   ;; VAO.
;;   (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)

;;   (glut:swap-buffers))

;; (defmethod glut:reshape ((w shader-vao-window) width height)
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   ;; Ensure that projection matrix ratio always matches the window size ratio,
;;   ;; so the polygon will always look square.
;;   (let ((right (max (float (/ width height)) 1.0))
;;     (top (max (float (/ height width)) 1.0)))
;;     (glu:ortho-2d (- right) right (- top) top))
;;   (when (program w)
;;       (let ((proj-mat (gl:get-float :projection-matrix)))
;;     (gl:uniform-matrix
;;      (gl:get-uniform-location (program w) "projectionMatrix")
;;      4
;;      (vector proj-mat))))
;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity))

;; (defmethod glut:keyboard ((w shader-vao-window) key x y)
;;   (declare (ignore x y))
;;   (case key
;;     (#\Esc (glut:destroy-window (glut:id w)))))

;; ;; Cleanup.
;; ;; Most of the objects we created have analogous deletion function.
;; (defmethod glut:close ((w shader-vao-window))
;;   ;; Note: It doesn't matter whether we delete the program or the
;;   ;; linked shaders first. If a shader is linked to a program, the
;;   ;; shader isn't destroyed until after the program is
;;   ;; destroyed. Similarly, if the program is destroyed, the shaders
;;   ;; are detached.
;;   (when (slot-boundp w 'vs)
;;    (gl:delete-shader (vertex-shader w)))
;;   (when (slot-boundp w 'fs)
;;     (gl:delete-shader (fragment-shader w)))
;;   (when (slot-boundp w 'program)
;;    (gl:delete-program (program w)))

;;   (when (slot-boundp w 'vbuff)
;;     (gl:delete-buffers (list (vertex-buffer w) (index-buffer w))))
;;   (when (slot-boundp w 'va)
;;    (gl:delete-vertex-arrays (list (vertex-array w)))))

;; (defun shader-vao ()
;;   (let ((w (make-instance 'shader-vao-window)))
;;     (unwind-protect
;;          (glut:display-window w)
;;       (when (not (glut::destroyed w))
;;          (setf (glut::destroyed w) t)
;;          (glut:destroy-window (glut:id w))))))

;; (defun test ()
;;   (bt:make-thread (lambda () (shader-vao)) :name "opengl-pipeline-1"))




;; testing basic window from learnopengl.com

;; (defparameter *scr-width* 800)
;; (defparameter *scr-height* 600)

;; (defparameter *vertex-shader-source* (uiop:read-file-string "/home/johannes/common-lisp/arcimoog/lisp/basic-vertex-shader.vert"))

;; (defparameter *fragment-shader-source* (uiop:read-file-string "/home/johannes/common-lisp/arcimoog/lisp/basic-fragment-shader.frag"))

;; (defun set-viewport (width height)
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   (gl:ortho -50 50 -50 50 -1 1)
;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity))

;; (defun render ()
;;   (gl:clear :color-buffer)
;;   (gl:with-pushed-matrix
;;     (gl:color 1 1 1)
;;     (gl:rect -25 -25 25 25)))

;; (glfw:def-window-size-callback update-viewport (window w h)
;;   (declare (ignore window))
;;   (set-viewport w h))

;; (glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
;;   (declare (ignore window scancode mod-keys))
;;   (when (and (eq key :escape) (eq action :press))
;;     (glfw:set-window-should-close)))

;; (defun main ()
;;   (glfw:with-init-window
;;       (:title "Minimal OpenGL window" :width *scr-width* :height *scr-height*)
;;     (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
;;     (glfw:set-key-callback 'quit-on-escape)
;;     (glfw:set-window-size-callback 'update-viewport)
;;     (gl:clear-color 1 0 0 1)
;;     (set-viewport *scr-width* *scr-height*)
;;     (loop until (glfw:window-should-close-p)
;;           do (render)
;;           do (glfw:swap-buffers)
;;           do (glfw:poll-events))))

;; (defun main-thread ()
;;   (bt:make-thread 'main))

;; testing font rendering


;; (defparameter *scr-width* 800)
;; (defparameter *scr-height* 600)

;; (defclass character-class ()
;;   ((texture-id :accessor texture-id :initarg :texture-id)
;;    (size :accessor size :initarg :size)
;;    (bearing :accessor bearing :initarg :bearing)
;;    (advance :accessor advance :initarg :advance)))

;; (defparameter *characters* nil)





;;;;;;;;;;;;;;;;;;;;;
;; TESTING SHADERS ;;
;;;;;;;;;;;;;;;;;;;;;


;; (define-condition compile-error (error)
;;   ((message
;;     :initform nil
;;     :initarg :message
;;     :reader compile-error-message
;;     :documentation "The reason given for the error")))

;; (defparameter *vertex-shader-source* "

;; // Input our our time variable
;; uniform float time;

;; // Pass to frag shader
;; varying vec2 vUv;

;; attribute vec4 vert;

;; void main()
;; {
;;     vUv = vert.xy;
;;     vec4 offsets = vec4(cos(time), sin(time), 1., 1.);
;;     gl_Position = gl_ModelViewProjectionMatrix * vert * offsets;
;; }
;; ")

;; (defparameter *fragment-shader-source* "

;; varying vec2 vUv;

;; void main()
;; {
;;     gl_FragColor = vec4(vUv.x, vUv.y, 1., 1.);
;; }
;; ")

;; (defvar *shader-prog* -1)
;; (defvar *frag-shader* nil)
;; (defvar *vert-shader* nil)

;; (defvar *shader-time* 0)

;; (defun render ()
;;   (gl:clear :color-buffer)

;;   (gl:uniformf
;;    (gl:get-uniform-location *shader-prog* "time")
;;    (incf *shader-time* 0.01))

;;   (gl:with-pushed-matrix
;;     (gl:translate 0 0 -800)
;;     (gl:rect -25 -25 25 25)))

;; (defun check-shader-error (shader)
;;   "Get the current error status of a shader, throw error if status"
;;   (let ((error-string (gl:get-shader-info-log shader)))
;;     (unless (equalp error-string "")
;;       (progn
;;         (format t "~A~%" error-string)
;;         (error 'compile-error :message error-string)))))

;; (defun is-invalid-shader (shader)
;;   (= shader -1))

;; (defun setup-shader ()
;;   (loop
;;     while (is-invalid-shader *shader-prog*) do
;;       (with-simple-restart
;;           (retry "Retry compiling shaders.")
;;         (setf *vert-shader* (gl:create-shader :vertex-shader))
;;         (setf *frag-shader* (gl:create-shader :fragment-shader))

;;         (gl:shader-source *vert-shader*
;;                           (uiop:read-file-string
;;                            "/home/johannes/common-lisp/arcimoog/lisp/basic-vertex-shader.vert"))
;;         (gl:shader-source *frag-shader*
;;                           (uiop:read-file-string
;;                            "/home/johannes/common-lisp/arcimoog/lisp/basic-fragment-shader.frag"))

;;         (gl:compile-shader *vert-shader*)
;;         (gl:compile-shader *frag-shader*)

;;         (check-shader-error *vert-shader*)
;;         (check-shader-error *frag-shader*)

;;         (setf *shader-prog* (gl:create-program))

;;         (gl:attach-shader *shader-prog* *vert-shader*)
;;         (gl:attach-shader *shader-prog* *frag-shader*)

;;         (gl:link-program *shader-prog*)

;;         (gl:use-program *shader-prog*))))


;; (glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
;;   (declare (ignore window scancode mod-keys))
;;   (when (and (eq key :escape) (eq action :press))
;;     (glfw:set-window-should-close)))

;; (defun set-viewport (width height)
;;   (gl:clear-color 0.2 0.2 0.2 0.2)
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)

;;   (let ((h (/ height width)))
;;     (gl:frustum -1 1 (- h) h 9 50000))

;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity))

;; (glfw:def-window-size-callback update-viewport (window w h)
;;   (declare (ignore window))
;;   (set-viewport w h))

;; (defun fragment-shader-example ()
;;   (glfw:with-init-window (:title "OpenGL test" :width 600 :height 400)
;;     (glfw:set-key-callback 'quit-on-escape)

;;     (glfw:set-window-size-callback 'update-viewport)
;;     (set-viewport 800 400)

;;     (setup-shader)

;;     (loop until (glfw:window-should-close-p)
;;           do (render)
;;           do (glfw:swap-buffers)
;;           do (glfw:poll-events))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARTING FROM BEGINNING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; old stuff, should be avoided
;; (defun set-viewport (width height)
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   (gl:ortho -50 50 -50 50 -1 1)
;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity))




;; (defparameter *character-set* (mapcar (lambda (char-string)
;;                                         (char-code (char char-string 0)))
;;                                       (list "a" "b" "c" "d" "e" "f" "g" "h")))


(defparameter *screen-width* 800)

(defparameter *screen-height* 600)

(defparameter *character-set* "abcdefgh")

(defparameter *face* (ft2:new-face "/usr/share/fonts/TTF/DejaVuSans.ttf"))

(defparameter *texture-ids* nil)

(defun lookup-texture-ids (character)
  (let ((result (cdr (assoc character *texture-ids*))))
    (values (first result) ; texture-id
            (second result) ; width
            (third result) ; height
            )))

;; (let ((result))
;;   (ft2:do-string-render (*face* "M" bitmap x y :with-char i)
;;     (declare (ignore x y))
;;     (setf result (ft2:bitmap-to-array bitmap))
;;     (format t "~&~a" (type-of i)))
;;   (let ((dims (array-dimensions result)))
;;     (format t "~&: height=~a, width=~a" (first dims) (second dims)))
;;   result)


(defun generate-characters ()
  (gl:pixel-store :unpack-alignment 1)

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
                                  (coerce (second dimensions) 'float)
                                  (coerce (first dimensions) 'float)))
            *texture-ids*))))



(glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window w h))
  ;; ??
  )

(defparameter *shader* nil)

(defparameter *vertices* (list 0.4 0.4 0.0    1.0 0.0 0.0   1.0 1.0
                               0.4 -0.4 0.0   0.0 1.0 0.0   1.0 0.0
                               -0.4 -0.4 0.0  0.0 0.0 1.0   0.0 0.0
                               -0.4 0.4 0.0   1.0 0.0 1.0   0.0 1.0))

(defparameter *vertices-gl-array* nil)

(defparameter *indices* (list 0 1 3
                              1 2 3))

(defparameter *indices-gl-array* nil)

(defparameter *ebo* -1)

(defparameter *vbo* -1)

(defparameter *vao* -1)

(defparameter *texture* -1)

(defun make-gl-array (data-list data-type)
  (let ((arr (gl:alloc-gl-array data-type (length data-list))))
    (loop for item in data-list
          for i from 0
          do (setf (gl:glaref arr i) item))
    arr))

(defun make-gl-array-from-array (data-list data-type)
  (let ((arr (gl:alloc-gl-array data-type (length data-list))))
    (loop for item across data-list
          for i from 0
          do (setf (gl:glaref arr i) item))
    arr))



(defun create-identity-matrix (&optional (value 1.0))
  (let ((result (make-array '(4 4) :initial-element 0.0)))
    (setf (aref result 0 0) value)
    (setf (aref result 1 1) value)
    (setf (aref result 2 2) value)
    (setf (aref result 3 3) value)
    result))

(defun print-matrix (matrix-4-4)
  (format t "~&~a ~a ~a ~a~%~a ~a ~a ~a~%~a ~a ~a ~a~%~a ~a ~a ~a"
          (aref matrix-4-4 0 0)
          (aref matrix-4-4 0 1)
          (aref matrix-4-4 0 2)
          (aref matrix-4-4 0 3)
          (aref matrix-4-4 1 0)
          (aref matrix-4-4 1 1)
          (aref matrix-4-4 1 2)
          (aref matrix-4-4 1 3)
          (aref matrix-4-4 2 0)
          (aref matrix-4-4 2 1)
          (aref matrix-4-4 2 2)
          (aref matrix-4-4 2 3)
          (aref matrix-4-4 3 0)
          (aref matrix-4-4 3 1)
          (aref matrix-4-4 3 2)
          (aref matrix-4-4 3 3)))

(defun print-vector (vec4)
  (format t "~&~a~%~a~%~a~%~a"
          (aref vec4 0)
          (aref vec4 1)
          (aref vec4 2)
          (aref vec4 3)))

(let* ((vec #(1.0 0.0 0.0 1.0))
       (trans (create-identity-matrix)))
  (setf trans (lla:mm trans #(1.0 1.0 0.0 1.0)))
  (print-vector vec)
  (print-matrix trans))

(defun setup ()
  (gl:enable :cull-face)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)


  ;; (multiple-value-bind (data height width)
  ;;     (cl-jpeg:decode-image "/home/johannes/common-lisp/arcimoog/lisp/vicentino-test.jpg")
  ;;   (gl:active-texture :texture0)
  ;;   (setf *texture* (gl:gen-texture))
  ;;   (gl:bind-texture :texture-2d *texture*)

  ;;   (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
  ;;   (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
  ;;   (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  ;;   (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

  ;;   (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb :unsigned-byte data)
  ;;   (gl:generate-mipmap :texture-2d))

  (setf *shader*
        (make-instance 'shader-class
                       :vertex-source "/home/johannes/common-lisp/arcimoog/lisp/vertex-shader-font.vert"
                       :fragment-source "/home/johannes/common-lisp/arcimoog/lisp/fragment-shader-font.frag"))


  ;; projection-stuff

  ;; glm::mat4 projection = glm::ortho(0.0f, static_cast<float>(SCR_WIDTH), 0.0f, static_cast<float>(SCR_HEIGHT));

  ;;   shader.use();

  ;;   glUniformMatrix4fv(glGetUniformLocation(shader.ID, "projection"), 1, GL_FALSE, glm::value_ptr(projection));



  (generate-characters)


  (setf *vao* (gl:gen-vertex-array))
  (setf *vbo* (gl:gen-buffer))
  (gl:bind-vertex-array *vao*)
  (gl:bind-buffer :array-buffer *vbo*)
  (gl:buffer-data :array-buffer
                  :dynamic-draw
                  (make-gl-array (make-list (* 6 4) :initial-element 0.0) :float))
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 4 :float nil (* 4 (cffi:foreign-type-size :float)) (cffi:null-pointer))
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0)

  ;; (setf *vertices-gl-array* (make-gl-array *vertices* :float))
  ;; (setf *vbo* (gl:gen-buffer))
  ;; (gl:bind-buffer :array-buffer *vbo*)
  ;; (gl:buffer-data :array-buffer :static-draw *vertices-gl-array*)

  ;; (setf *indices-gl-array* (make-gl-array *indices* :unsigned-int))
  ;; (setf *ebo* (gl:gen-buffer))
  ;; (gl:bind-buffer :element-array-buffer *ebo*)
  ;; (gl:buffer-data :element-array-buffer :static-draw *indices-gl-array*)


  ;; (gl:vertex-attrib-pointer 0 3 :float nil
  ;;                           (* 8 (cffi:foreign-type-size :float))
  ;;                           (cffi:null-pointer))
  ;; (gl:enable-vertex-attrib-array 0)
  ;; (gl:vertex-attrib-pointer 1 3 :float nil
  ;;                           (* 8 (cffi:foreign-type-size :float))
  ;;                           (cffi:inc-pointer (cffi:null-pointer)
  ;;                                             (* 3 (cffi:foreign-type-size :float))))
  ;; (gl:enable-vertex-attrib-array 1)
  ;; (gl:vertex-attrib-pointer 2 2 :float nil
  ;;                           (* 8 (cffi:foreign-type-size :float))
  ;;                           (cffi:inc-pointer (cffi:null-pointer)
  ;;                                             (* 6 (cffi:foreign-type-size :float))))
  ;; (gl:enable-vertex-attrib-array 2)
  ;; (gl:bind-vertex-array 0)
  )


(defmethod render-text ((shader shader-class) text-string global-x global-y scale r g b)
  (declare (type float global-x global-y scale r g b))
  (use shader)
  (set-uniform shader "textColor" :float r g b)
  (gl:active-texture :texture0)
  (gl:bind-vertex-array *vao*)
  (ft2:do-string-render (*face* text-string bitmap local-x local-y :with-char current-character)
    (multiple-value-bind (texture-id original-width original-height)
        (lookup-texture-ids current-character)
      (declare (type float original-width original-height))
      (let ((x-pos (* scale (+ global-x local-x)))
            (y-pos (* scale (- global-y local-y)))
            (scaled-width (* scale original-width))
            (scaled-height (* scale original-height)))
        (let ((vertices
                (make-array '(6 4) :element-type 'float
                                   :initial-contents
                                   (list (list x-pos (+ scaled-height y-pos) 0.0 0.0)
                                         (list x-pos y-pos 0.0 1.0)
                                         (list (+ x-pos scaled-width) y-pos 1.0 1.0)

                                         (list x-pos (+ y-pos scaled-height) 0.0 0.0)
                                         (list (+ x-pos scaled-width) y-pos 1.0 1.0)
                                         (list (+ x-pos scaled-width) (+ y-pos scaled-height) 1.0 0.0)))))

          (gl:bind-texture :texture-2d texture-id)
          (gl:bind-buffer :array-buffer *vbo*)
          (gl:buffer-sub-data :array-buffer (make-gl-array-from-array (aops:flatten vertices) :float))
          (gl:bind-buffer :array-buffer 0)
          (gl:draw-arrays :triangles 0 6)))))
  (gl:bind-vertex-array 0)
  (gl:bind-texture :texture-2d 0))


(defun draw ()
  (render-text *shader* "ab" 0.0 0.0 1.0 1.0 1.0 1.0)
  ;; (my-gl-draw-elements :triangles 6 :unsigned-int)
  ;;(gl:draw-arrays :triangles 0 3)
  )

(defun render ()
  (with-shader *shader*
    ;; (gl:active-texture :texture0)
    ;; (gl:bind-texture :texture-2d *texture*)
    (gl:bind-vertex-array *vao*)
    ;; (set-uniform *shader* "uniColor" :float 1.0 1.0 1.0 1.0)
    (draw)
    (gl:bind-vertex-array 0)))

(defun fundamentals ()
  (log:debug "Starting window creation.")
  (glfw:with-init-window (:title "OpenGL test" :width *screen-width* :height *screen-height*)
    (glfw:set-key-callback 'quit-on-escape)
    (log:debug "Starting GL setup.")
    (setup)
    (loop until (glfw:window-should-close-p)
          do (render)
          do (glfw:swap-buffers)
          do (glfw:poll-events)))
  (gl:delete-vertex-arrays (list *vao*))
  (gl:delete-buffers (list *vbo*))
  (destroy *shader*)
  (setf *vbo* -1 *vao* -1)
  (log:info "OpenGL successfully destroyed."))

(log:config :debug)

(defun test ()
  (bt:make-thread (lambda () (fundamentals)) :name "test-window"))



;; from here: experimental

;; (cffi:load-foreign-library "libgl.so" :search-path "/usr/lib/")

;; (defun glm:ortho (left right bottom top near far)
;;   (cffi:defcfun ortho :void
;;     (left :float)
;;     (right :float)
;;     (bottom :float)
;;     (top :float)
;;     (near :float)
;;     (far :float)
;;     :name "glm_ortho"
;;     :library "libm.so"))




;; (cffi:defcffi-module glm
;;     (:default "libglm")

;;   (:include "glm/glm.hpp")
;;   (:include "glm/gtc/matrix_transform.hpp"))

;; (cffi:defcfun "glm::ortho" :void
;;   ((left :float)
;;    (right :float)
;;    (bottom :float)
;;    (top :float)
;;    (near :float)
;;    (far :float))
;;   :library "libglm")

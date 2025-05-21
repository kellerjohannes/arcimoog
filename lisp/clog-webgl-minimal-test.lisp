(defpackage :webgl-example
  (:use :cl :clog :clog-webgl))

(in-package :webgl-example)

(ql:quickload :clog)


(defgeneric create-webgl (clog-canvas &key context attributes)
  (:documentation "Create a new CLOG-WebGL from a CLOG-Canvas. Context
can be webgl (version 1) or webgl2 (default). Attributes must be a
plist like (\"attribute\" value ...). The values can be booleans or
strings."))

(defmethod create-webgl ((obj clog-canvas) &key (context "webgl2") attributes)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('~A'~@[,{~{~A: ~A~^, ~}}~])"
                            web-id (html-id obj) context
                            (loop for (key value) on attributes by #'cddr
                                  append (list key (if (typep value 'boolean)
                                                       (if value "true" "false")
                                                       (format nil "\"~A\"" value))))))
    (make-instance 'clog-webgl
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))



(defparameter *v-shader* "#version 300 es
in vec2 position;
out vec3 Color;

void main() {
  Color = vec3(1.0, 0.0, 1.0);
  gl_Position = vec4(position.x, position.y, 0.0, 1.0);
}")

(defparameter *f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  outColor = vec4(Color, 1.0);
}")


(defun draw (gl)
  (clear-webgl gl :COLOR_BUFFER_BIT)
  (draw-arrays gl :LINE_STRIP 0 2)

  (format t "~&drawing.~%")
  )


;; (defparameter *webgl* nil)

;; (defun animation-handler (clog-obj time-string)
;;   (format t "~&time: ~a.~%" time-string)
;;   (draw *webgl*)
;;   (request-animation-frame (connection-data-item clog-obj "window"))
;;   )

(defun on-main (body)
  (load-css (html-document body) "styles.css")
  (setf (title (html-document body)) "WebGL minimal example")
  (setf (connection-data-item body "window") (window body))
  (let* ((main-container (create-div body :class "main-container"))
         (webgl-container (create-div main-container
                                      :class "main-title"
                                      :content "WebGL Minimal Example"))
         (canvas (create-canvas webgl-container :width 1200 :height 700))
         (gl (create-webgl canvas :attributes '("preserveDrawingBuffer" t
                                                "powerPreference" "default"
                                                "antialias" t)))
         (program (compile-webgl-program
                   gl
                   (compile-shader-source gl :VERTEX_SHADER *v-shader*)
                   (compile-shader-source gl :FRAGMENT_SHADER *f-shader*)))
         (vao (create-vertex-array gl))
         (vbo (create-webgl-buffer gl)))

    (set-border canvas :medium :solid :green)
    (enable-capability gl :BLEND)
    (disable-capability gl :DEPTH_TEST)
    (blend-function gl :ONE :ONE_MINUS_SRC_ALPHA)
    (clear-color gl 0.0f0 0.2f0 0.2f0 1.0f0)
    (clear-depth gl 1)
    (viewport gl 0 0 1200 700)

    (use-program program)
    (bind-vertex-array vao)
    (bind-buffer vbo :ARRAY_BUFFER)
    (enable-vertex-attribute-array gl (attribute-location program "position"))
    (vertex-attribute-pointer gl (attribute-location program "position") 2 :float nil 0 0)
    (buffer-data vbo (list 0.0 0.0 1.0 1.0) "Float32Array" :STATIC_DRAW)

    (setf *webgl* gl)

    ;; (set-on-animation-frame (window body) #'animation-handler)
    ;; (request-animation-frame (window body))
    (bt:make-thread (lambda ()
                      (loop (draw gl)
                            (sleep 1/5))))
    ))

(defun init ()
  (initialize #'on-main
              :static-root (merge-pathnames "clog/static-files/"
                                            (asdf/system:system-source-directory :arcimoog)))
  (open-browser))

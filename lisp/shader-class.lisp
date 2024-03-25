(in-package :arcimoog)


(defun validp (shader)
  (> shader -1))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn
        (format t "~A~%" error-string)
        (error 'compile-error :message error-string)))))


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

(defgeneric set-uniform (shader type))


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


(defmacro with-shader (shader &body body)
  `(progn
     (use ,shader)
     ,@body
     (gl:use-program 0)))

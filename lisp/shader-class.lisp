(in-package :arcimoog)

(defclass shader-class ()
  ((id :accessor id
       :initform -1
       :documentation "OpenGL-identifier for shader program.")
   (vertex-source-path :initarg :vertex-source
                       :reader vertex-source)
   (fragment-source-path :initarg :fragment-source
                         :reader fragment-source-path)
   ))

(defgeneric validp (shader))

(defgeneric use (shader))

(defgeneric set-uniform (shader type))



(defmethod initialize-instance :after ((shader shader-class) &key)
  (setf (id shader) (gl:create-program)))

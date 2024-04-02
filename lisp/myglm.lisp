(in-package :arcimoog)

(defun vec-sc-op (operation vec value)
  (declare (type simple-vector vec)
           (type number value))
  (let ((result (make-array (length vec))))
    (loop for element across vec
          for i from 0
          do (setf (aref result i) (funcall operation element value)))
    result))

(defun vec-negate (vec)
  (vec-sc-op #'* vec -1))

(defun vec-vec-op (operation vec1 vec2)
  (declare (type simple-vector vec1 vec2))
  (assert (= (length vec1) (length vec2)))
  (let ((result (make-array (length vec1))))
    (loop for element1 across vec1
          for element2 across vec2
          for i from 0
          do (setf (aref result i) (funcall operation element1 element2)))
    result))

(defun vec-len (vec)
  (declare (type simple-vector vec))
  (sqrt (reduce #'+ (vec-vec-op #'* vec vec))))

(defun unit-vec (vec)
  (declare (type simple-vector vec))
  (vec-sc-op #'/ vec (vec-len vec)))

(defun vec-dot (vec1 vec2)
  (declare (type simple-vector vec1 vec2))
  (reduce #'+ (vec-vec-op #'* vec1 vec2)))

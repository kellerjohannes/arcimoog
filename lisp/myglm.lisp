(in-package :arcimoog)

(defun vec-sc-op (operation vec sc)
  (declare (type simple-vector vec)
           (type number sc))
  (let ((result (make-array (length vec))))
    (loop for element across vec
          for i from 0
          do (setf (aref result i) (funcall operation element sc)))
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

(defun vec-cross (vec3-a vec3-b)
  (declare (type simple-vector vec3-a vec3-b))
  (assert (= (length vec3-a) 3))
  (assert (= (length vec3-b) 3))
  (let ((result (make-array 3)))
    (macrolet ((a (index)
                 `(aref vec3-a ,(1- index)))
               (b (index)
                 `(aref vec3-b ,(1- index))))
      (setf (aref result 0) (- (* (a 2) (b 3))
                               (* (a 3) (b 2)))
            (aref result 1) (- (* (a 3) (b 1))
                               (* (a 1) (b 3)))
            (aref result 2) (- (* (a 1) (b 2))
                               (* (a 2) (b 1)))))
    result))

(defmacro loop-across-matrix (matrix &body body)
  "Only works for 2-dimensionsl matrices."
  (let ((n (gensym))
        (m (gensym)))
    `(destructuring-bind (,n ,m) (array-dimensions ,matrix)
       (loop for i from 0 below ,n do
             (loop for j from 0 below ,m do
                   ,@body)))))

(defun mat-mat-op (operation mat1 mat2)
  (declare (type (simple-array t (* *)) mat1 mat2))
  (assert (equalp (array-dimensions mat1) (array-dimensions mat2)))
  (let ((result (make-array (array-dimensions mat1))))
    (loop-across-matrix mat1
      (setf (aref result i j) (funcall operation (aref mat1 i j) (aref mat2 i j))))
    result))

(defun mat-sc-op (operation sc mat)
  (declare (type number sc)
           (type (simple-array t (* *)) mat))
  (let ((result (make-array (array-dimensions mat))))
    (loop-across-matrix mat
      (setf (aref result i j) (funcall operation sc (aref mat i j))))
    result))

(defun mat-mat-op (reducing-operation pairing-operation mat-a mat-b)
  (declare (type (simple-array t (* *)) mat-a mat-b))
  (assert (= (array-rank mat-a) (array-dimension mat-b 0)))
  (let ((result (make-array (list (array-rank mat-a) (array-dimension mat-b 0)))))
    (loop-across-matrix result
      (setf (aref result i j) (reduce reducing-operation
                                      (loop ))))))


(defun ortho (left right bottom top near far)
  (let ((result (make-array '(4 4) :element-type 'float :initial-element 1.0)))
    (setf (aref result 0 0) (/ 2.0 (- right left))
          (aref result 1 1) (/ 2.0 (- top bottom))
          (aref result 2 2) (/ -2.0 (- far near))
          (aref result 3 0) (/ (- (+ right left)) (- right left))
          (aref result 3 1) (/ (- (+ top bottom)) (- top bottom))
          (aref result 3 1) (/ (- (+ far near)) (- near far)))
    result))


(defun create-identity-matrix (&optional (value 1.0))
  (let ((result (make-array '(4 4) :initial-element 0.0)))
    (setf (aref result 0 0) value)
    (setf (aref result 1 1) value)
    (setf (aref result 2 2) value)
    (setf (aref result 3 3) value)
    result))

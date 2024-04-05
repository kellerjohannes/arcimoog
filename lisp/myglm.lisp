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




;;; from here serious

(defun rad->deg (rad)
  (* 180 (/ rad PI)))

(defun deg->rad (deg)
  (* PI (/ deg 180)))

;;;; Taken from https://stackoverflow.com/questions/12230312/is-glmortho-actually-wrong
;;
;; This matches that: https://en.wikipedia.org/wiki/Orthographic_projection
;; Result[0][0] = valType(2) / (right - left);
;; Result[1][1] = valType(2) / (top - bottom);
;; Result[2][2] = - valType(2) / (zFar - zNear);
;; Result[3][0] = - (right + left) / (right - left);
;; Result[3][1] = - (top + bottom) / (top - bottom);
;; Result[3][2] = - (zFar + zNear) / (zFar - zNear);
;;;; ➙ doesn't work, because row-first vs. column-first
(defun ortho (left right bottom top near far)
  (let ((result (make-array '(4 4) :element-type 'float :initial-element 0.0)))
    (setf (aref result 0 0) (/ 2.0 (- right left))
          (aref result 1 1) (/ 2.0 (- top bottom))
          (aref result 2 2) (- (/ 2.0 (- far near)))
          (aref result 3 3) 1.0
          (aref result 3 0) (- (/ (+ right left) (- right left)))
          (aref result 3 1) (- (/ (+ top bottom) (- top bottom)))
          (aref result 3 2) (- (/ (+ far near) (- far near)))
          )
    result))

(defun create-identity-matrix (dimension)
  (let ((result (make-array (list dimension dimension) :initial-element 0.0)))
    (loop for i from 0 below dimension do
      (setf (aref result i i) 1.0))
    result))

(defun translate (matrix vector)
  (let ((translation-matrix (create-identity-matrix 4)))
    (setf (aref translation-matrix 3 0) (aref vector 0)
          (aref translation-matrix 3 1) (aref vector 1)
          (aref translation-matrix 3 2) (aref vector 2))
    (lla:mm matrix translation-matrix)))

(defun scale (matrix vector)
  (let ((scale-matrix (make-array '(4 4) :initial-element 0.0)))
    (setf (aref scale-matrix 0 0) (aref vector 0)
          (aref scale-matrix 1 1) (aref vector 1)
          (aref scale-matrix 2 2) (aref vector 2)
          (aref scale-matrix 3 3) 1.0)
    (lla:mm matrix scale-matrix)))

(defun rotate (matrix angle-deg vector)
  (let ((rotate-matrix (create-identity-matrix 4))
        (angle-rad (coerce (deg->rad angle-deg) 'single-float)))
    (unless (zerop (aref vector 0))
      (setf (aref rotate-matrix 1 1) (cos angle-rad)
            (aref rotate-matrix 2 1) (- (sin angle-rad))
            (aref rotate-matrix 1 2) (sin angle-rad)
            (aref rotate-matrix 2 2) (cos angle-rad)))
    (unless (zerop (aref vector 1))
      (setf (aref rotate-matrix 0 0) (cos angle-rad)
            (aref rotate-matrix 0 2) (- (sin angle-rad))
            (aref rotate-matrix 2 0) (sin angle-rad)
            (aref rotate-matrix 2 2) (cos angle-rad)))
    (unless (zerop (aref vector 2))
      (setf (aref rotate-matrix 0 0) (cos angle-rad)
            (aref rotate-matrix 1 0) (- (sin angle-rad))
            (aref rotate-matrix 0 1) (sin angle-rad)
            (aref rotate-matrix 1 1) (cos angle-rad)))
    (lla:mm matrix rotate-matrix)))


(defmacro loop-across-matrix (matrix &body body)
  "Only works for 2-dimensionsl matrices."
  (let ((n (gensym))
        (m (gensym)))
    `(destructuring-bind (,n ,m) (array-dimensions ,matrix)
       (loop for i from 0 below ,n do
             (loop for j from 0 below ,m do
                   ,@body)))))

(defun lisp-to-gl-matrix (lisp-matrix)
  (let ((result (make-array (array-dimensions lisp-matrix))))
    (loop-across-matrix lisp-matrix
      (setf (aref result j i) (aref lisp-matrix i j)))
    (aops:flatten result)))

(defmacro transform (matrix transformation &rest parameters)
  `(setf ,matrix (,transformation ,matrix ,@parameters)))

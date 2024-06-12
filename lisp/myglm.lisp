(in-package :arcimoog.myglm)

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


;;;; Taken from https://stackoverflow.com/questions/22623405/replicating-glmperspective-in-code
;;
;; With swapped row / column
;;
;; valType const rad = glm::radians(fovy);
;; valType tanHalfFovy = tan(rad / valType(2));
;; detail::tmat4x4<valType, defaultp> Result(valType(0));
;; Result[0][0] = valType(1) / (aspect * tanHalfFovy);
;; Result[1][1] = valType(1) / (tanHalfFovy);
;; Result[2][2] = - (zFar + zNear) / (zFar - zNear);
;; Result[2][3] = - valType(1);
;; Result[3][2] = - (valType(2) * zFar * zNear) / (zFar - zNear);

;; TODO
;; does not work currently
(defun perspective (fovy aspect z-near z-far)
  (format t "~&Constructing perspective: ~a ~a ~a ~a" fovy aspect z-near z-far)
  (assert (not (zerop aspect)))
  (assert (not (= z-far z-near)))
  (let* ((fovy-rad (deg->rad fovy))
         (tan-half-fovy (tan (/ fovy-rad 2.0)))
         (result (make-array '(4 4) :element-type 'float :initial-element 0.0)))
    (setf (aref result 0 0) (/ 1.0 (* aspect tan-half-fovy))
          (aref result 1 1) (/ 1.0 tan-half-fovy)
          (aref result 2 2) (- (/ (+ z-far z-near) (- z-far z-near)))
          (aref result 3 2) -1.0
          (aref result 2 3) (- (/ (* 2.0 z-far z-near) (- z-far z-near))))
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

(defmacro transform-matrix (matrix transformation &rest parameters)
  `(setf ,matrix (,transformation ,matrix ,@parameters)))

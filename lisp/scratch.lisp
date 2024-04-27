(in-package :arcimoog)


(defparameter *nodes* nil)

(defun add-node (node)
  (push node *nodes*))

(defun make-node (whole larger smaller)
  (list whole larger smaller))

(defun find-node-by-whole (whole)
  (remove-if-not (lambda (interval) (eq interval whole))
                 *nodes*
                 :key #'first))

(defun find-node-by-larger (larger)
  (remove-if-not (lambda (interval) (eq interval larger))
                 *nodes*
                 :key #'second))

(defun find-node-by-smaller (smaller)
  (remove-if-not (lambda (interval) (eq interval smaller))
                 *nodes*
                 :key #'third))


(defun populate ()
  (setf *nodes* nil)
  (add-node (make-node :ottava :quinta :quarta))
  (add-node (make-node :quinta :terza-maggiore :terza-minore))
  (add-node (make-node :quarta :terza-maggiore :semitono-maggiore))
  (add-node (make-node :quarta :terza-minore :tono-minore))
  (add-node (make-node :terza-maggiore :tono-maggiore :tono-minore))
  (add-node (make-node :terza-minore :tono-maggiore :semitono-maggiore))
  (add-node (make-node :tono-minore :semitono-maggiore :semitono-minore)))


(defun add (interval-a interval-b)
  ())

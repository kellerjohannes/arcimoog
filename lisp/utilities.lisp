(in-package :arcimoog)

(defun reduce-equal-keyword-list (keyword-list)
  "Expects a list of identical keywords, returns this keyword."
  (if (and (listp keyword-list)
           (plusp (length keyword-list)))
      (let ((simple-list (remove-duplicates keyword-list)))
        (cond ((= 1 (length simple-list))
               (cond ((keywordp (car simple-list))
                      (car simple-list))
                     (t (log:warn "The simplified KEYWORD-LIST" keyword-list "doesn't consist of a keyword and is therefore ignored. This might produce unwanted results.")
                        nil)))
              (t (log:warn "The KEYWORD-LIST" keyword-list "doesn't consist of exclusively identical keywords and will therefore be ignored. This might produce unwanted results.")
                 nil)))
      (progn
        (log:warn "The KEYWORD-LIST" keyword-list "doesn't contain any elements. NIL will be returned.")
        nil)))


(defun shrink (number)
  "Reduces positive integers / increases negative integers by 1."
  (cond ((zerop number) number)
        ((plusp number) (1- number))
        (t (1+ number))))

(defun coerce-vector (vec type)
  (declare (type simple-vector vec))
  (let ((result (make-array (length vec) :element-type type)))
    (loop for element across vec
          for i from 0 do
          (setf (aref result i) (coerce element type)))
    result))

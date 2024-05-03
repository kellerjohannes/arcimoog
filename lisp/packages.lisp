(defpackage :arcimoog.utilities
  (:use :cl)
  (:nicknames :utility)
  (:export #:reduce-equal-keyword-list
           #:shrink
           #:coerce-vector
           #:param!
           #:param
           #:with-params))

(defpackage :arcimoog.parameters
  (:use :cl)
  (:nicknames :apar)
  (:export #:defp
           #:setp
           #:getp
           #:resp
           #:write-parameters
           #:read-parameters))

(defpackage :arcimoog.myglm
  (:use :cl)
  (:nicknames :glm)
  (:export #:rad->deg
           #:deg->rad
           #:ortho
           #:create-identity-matrix
           #:translate
           #:scale
           #:rotate
           #:loop-across-matrix
           #:lisp-to-gl-matrix
           #:transform-matrix))

(defpackage :arcimoog.display
  (:use :cl)
  (:export #:start))

(defpackage :arcimoog
  (:use :cl)
  (:nicknames :arci))

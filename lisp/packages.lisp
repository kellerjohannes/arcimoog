(defpackage :arcimoog.utilities
  (:use :cl)
  (:nicknames :utility)
  (:export #:reduce-equal-keyword-list
           #:shrink
           #:coerce-vector
           #:param!
           #:param
           #:with-params))

(defpackage :arcimoog.conditions
  (:use :cl)
  (:nicknames :acond)
  (:export #:incudine-is-not-in-rt
           #:compile-error
           #:pm-error
           #:faderfox-id-not-found
           #:get-faderfox-name-from-user
           #:instantiate-empty-parameter
           #:no-parameter-found
           #:get-parameter-value-from-user
           #:buffer-file-not-found
           #:buffer-file-not-loaded
           #:key-not-supported
           #:parameter-not-found
           #:parameter-data-invalid
           #:parameter-data-type-unsupported
           ;; probably obsolete, figuring out
           #:parameter-data-is-not-number
           #:parameter-data-is-not-string
           #:parameter-data-out-of-range
           #:parameter-type-unsupported
           ))


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
           #:perspective
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

(defpackage :symbolic-intervals
  (:use :cl))

(defpackage :score-model
  (:use :cl)
  (:nicknames :sc)
  (:export #:def
           #:val
           #:of
           #:calc
           #:rel
           #:chain
           #:series
           #:diff
           #:asc
           #:desc
           #:temper
           #:edx))

(defpackage :ly-parser
  (:use :cl)
  (:nicknames :ly)
  (:export #:parse-ly-notation))

(defpackage :arcimoog
  (:use :cl)
  (:nicknames :arci))

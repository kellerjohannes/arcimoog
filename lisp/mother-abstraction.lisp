(in-package :arcimoog.mothers)

(defclass mother ()
  ((pitch-ratio :initform 1/1 :accessor ratio)
   (natura :initform 4 :accessor natura)
   (vco-name :initarg :vco-name :reader vco-name)
   (vcf-name :initarg :vcf-name :reader vcf-name)
   (res-name :initarg :res-name :reader res-name)
   (vca-name :initarg :vca-name :reader vca-name)
   (gate-name :initarg :gate-name :reader gate-name)))

(defmethod update-pitch (interval-ratio)
  ())

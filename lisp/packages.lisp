(defpackage :arcimoog
  (:use :cl)
  (:nicknames :am))

;; (defpackage :arcimoog.utilities
;;   (:use :cl)
;;   (:nicknames :utility)
;;   (:export #:reduce-equal-keyword-list
;;            #:shrink
;;            #:coerce-vector
;;            #:param!
;;            #:param
;;            #:with-params))

(defpackage :arcimoog.conditions
  (:use :cl)
  (:nicknames :acond)
  (:export #:incudine-is-not-in-rt
           ;;#:compile-error
           #:midi-subscripts-out-of-range
           #:pm-error
           #:faderfox-id-not-found
           #:get-faderfox-name-from-user
           ;; #:instantiate-empty-parameter
           ;; #:no-parameter-found
           ;; #:get-parameter-value-from-user
           ;; #:buffer-file-not-found
           ;; #:buffer-file-not-loaded
           ;; #:key-not-supported
           ;; #:parameter-not-found
           ;; #:parameter-data-invalid
           ;; #:parameter-data-type-unsupported
           ;;;; probably obsolete, figuring out
           ;; #:parameter-data-is-not-number
           ;; #:parameter-data-is-not-string
           ;; #:parameter-data-out-of-range
           ;; #:parameter-type-unsupported
           ))


(defpackage :arcimoog.parameters
  (:use :cl)
  (:nicknames :am-par)
  (:export #:register-scalar
           #:inc-scalar
           #:clear-parameter-bank
           #:print-parameter-list
           #:register-hook
           ;; #:defp
           ;; #:setp
           ;; #:getp
           ;; #:resp
           ;; #:write-parameters
           ;; #:read-parameters
           ))

;; (defpackage :arcimoog.symbolic-intervals
;;   (:use :cl))

;; (defpackage :arcimoog.score-model
;;   (:use :cl)
;;   (:nicknames :sc)
;;   (:export #:def
;;            #:val
;;            #:of
;;            #:calc
;;            #:rel
;;            #:chain
;;            #:series
;;            #:diff
;;            #:asc
;;            #:desc
;;            #:temper
;;            #:edx))

;; (defpackage :arcimoog.ly-parser
;;   (:use :cl)
;;   (:nicknames :ly)
;;   (:export #:parse-ly-notation))

(defpackage :arcimoog.midi-communication
  (:use :cl)
  (:nicknames :am-midi)
  (:export #:init-faderfox-communication
           #:register-callback))

(defpackage :arcimoog.osc-communication
  (:use :cl)
  (:nicknames :am-osc)
  (:export #:init-osc-communication))

(defpackage :arcimoog.ui
  (:use :cl :clog)
  (:nicknames :ui)
  (:export #:start
           #:restart))

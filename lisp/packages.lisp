(defpackage :arcimoog.utilities
  (:use :cl)
  (:nicknames :utility)
  (:export #:if-exists
           #:register-constant
           #:register-display-parameter
           #:register-cv
           #:shrink
           #:register-toggle-dial
           #:register-precision-dial
           #:register-precision-dial-callback
           #:reduce-equal-keyword-list))

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
           #:get-scalar
           #:set-scalar
           #:inc-scalar
           #:clear-parameter-bank
           #:print-parameter-list
           #:register-hook
           ))

(defpackage :arcimoog.symbolic-intervals
  (:use :cl)
  (:nicknames :am-si)
  (:import-from :arcimoog.utilities
                #:if-exists)
  (:export #:go-vicentino
           #:go-willaert
           #:model
           #:relative
           #:score-off
           #:reset-meantone
           #:modify-fifth
           #:set-edx
           #:modify-edx
           #:reset-ed2))

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
  (:export #:init
           #:register-callback
           #:print-callbacks))

(defpackage :arcimoog.osc-communication
  (:use :cl)
  (:nicknames :am-osc)
  (:export #:init
           #:send))

(defpackage :arcimoog.ui
  (:use :cl :clog :clog-webgl)
  (:nicknames :am-ui)
  (:export #:init
           #:reset
           #:update-value
           #:toggle-cv-autoroll
           ))

(defpackage :arcimoog.mothers
  (:use :cl)
  (:nicknames :am-mo)
  (:export #:register-mother
           #:select-mother
           #:is-valid-mother-name
           #:set-mother-pitch
           #:modify-mother-pitch
           #:set-selected-pitch
           #:modify-selected-pitch
           #:set-mother-natura
           #:modify-mother-natura
           #:set-selected-natura
           #:modify-selected-natura
           #:set-mother-pitch-and-natura
           #:modify-mother-pitch-and-natura
           #:set-selected-pitch-and-natura
           #:modify-selected-pitch-and-natura
           #:mother-on
           #:mother-off
           #:selected-on
           #:selected-off
           #:set-all-gates
           #:set-mother-cv-offset
           #:modify-mother-cv-offset
           #:set-selected-cv-offset
           #:modify-selected-cv-offset
           #:set-mother-cv-factor
           #:modify-mother-cv-factor
           #:set-selected-cv-factor
           #:modify-selected-cv-factor
           #:read-mother-tunings
           #:get-natura
           #:mother-on-p
           #:trigger-natura-accent
           #:get-cv-1/1
           #:set-cv-1/1
           #:modify-cv-1/1
           #:get-master-transpose
           #:set-master-transpose
           #:modify-master-transpose
           #:set-natura-muted-p
           ))

(defpackage :raw-pitch
  (:use :cl)
  (:nicknames :am-rp))

(defpackage :arcimoog.history-tracker
  (:use :cl)
  (:nicknames :am-ht)
  (:export #:register-tracker
           #:add-data-point
           #:loop-over-history
           #:print-all
           #:get-latest-data-point
           #:add-points
           #:dump-list
           #:dump-gl-list
           #:length-gl-data
           #:update-data-required-p
           #:data-updated))

(defpackage :arcimoog.pitch-data
  (:use :cl)
  (:nicknames :am-pd)
  (:export #:set-default-note-name-convention
           #:with-note-name-convention
           #:transform
           #:nn
           #:create-note
           #:validp
           #:pitch-equal
           #:iterator))

(defpackage :arcimoog
  (:use :cl)
  (:nicknames :am)
  (:import-from :arcimoog.utilities
                #:register-constant
                #:register-cv
                #:register-display-parameter
                #:register-toggle-dial
                #:register-precision-dial
                #:register-precision-dial-callback)
  (:import-from :arcimoog.pitch-data
                #:nn)
  (:import-from :arcimoog.symbolic-intervals
                #:go-vicentino
                #:go-willaert
                #:model
                #:relative
                #:score-off
                #:reset-meantone
                #:reset-ed2)
  (:export #:init
           #:*output-cv-updates-p*
           ;; Frome here: DSL to tune and manipulated Mothers.
           #:s
           #:offset
           #:soffset
           #:factor
           #:sfactor
           #:morel
           #:smorel
           #:moabs
           #:smoabs
           #:on
           #:off
           #:allon
           #:alloff
           #:cvdon
           #:cvdoff))



;;; Performance sets are defined in individual packages. Add more packages below. Usually it makes
;;; sense to :use the :arcimoog package, sketches can be written into main.lisp before moving the
;;; code into a separate package and a separate file.

(defpackage :arcimoog.performance.apollo
  (:use :cl :arcimoog)
  (:nicknames :apollo)
  (:import-from :arcimoog.symbolic-intervals
                #:go-vicentino
                #:go-willaert
                #:model
                #:score-off
                #:reset-meantone
                #:reset-ed2))

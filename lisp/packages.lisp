(defpackage :arcimoog.utilities
  (:use :cl)
  (:nicknames :utility)
  (:export #:register-constant
           #:register-cv
           #:register-toggle-dial
           #:register-precision-dial))

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
  (:export #:init
           #:register-callback
           #:print-callbacks))

(defpackage :arcimoog.osc-communication
  (:use :cl)
  (:nicknames :am-osc)
  (:export #:init
           #:send))

(defpackage :arcimoog.ui
  (:use :cl :clog)
  (:nicknames :am-ui)
  (:export #:init
           #:reset
           ;; TODO remove this function
           #:register-cvs-tile
           #:update-value
           ))

(defpackage :arcimoog.history-tracker
  (:use :cl)
  (:nicknames :am-ht)
  (:export #:register-tracker
           #:add-data-point
           #:loop-over-history
           #:print-all))

(defpackage :arcimoog
  (:use :cl)
  (:nicknames :am)
  (:export #:init)
  (:import-from :arcimoog.utilities
                #:register-constant
                #:register-cv
                #:register-toggle-dial
                #:register-precision-dial))

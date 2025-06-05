(in-package :raw-pitch)

(defparameter *step-delta* 10000)

;;; TODO Swiping seems to overload the system, not clear why yet.

;; (defmacro (swiping-argument origin target time-delta expression)
;;   `(let ((value-delta ()))) (labels ((swiper (time value)

;;                  (incudine:at (+ time *step-delta*) #'swiper (+ time *step-delta* ))))
;;         (swiper (incudine:now) )))


;; (line x 1/1 5/4 10 (moabs x))


;; (defun swipe (origin target time-delta)
;;   (let ((value-delta (expt (/ target origin)
;;                            (/ 1 (/ (* time-delta (incudine:rt-sample-rate)) *step-delta*)))))
;;     (labels ((swiper (time value)
;;                (cond ((>= value target) (am::moabs :s target))
;;                      (t (am::moabs :s (coerce value 'single-float))
;;                         (incudine:at (+ time *step-delta*)
;;                                      #'swiper (+ time *step-delta*) (* value value-delta))))))
;;       (swiper (incudine:now) origin))))

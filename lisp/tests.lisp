(5am:def-suite arcimoog-testing
  :description "Testing the entire arcimoog system.")

(5am:def-suite note-names
  :description "Testing note name creation and validity testing."
  :in arcimoog-testing)

(5am:in-suite note-names)

(5am:test setting-default-note-name-convention
  :description "Testing the changing of the default note name convention. Needs to be updated whenever new convention classes are defined."

  (set-default-note-name-convention 'note-name-smn)
  (5am:is (eq *default-note-name-convention* 'note-name-smn))

  (set-default-note-name-convention 'note-name-vicentino)
  (5am:is (eq *default-note-name-convention* 'note-name-vicentino))

  (set-default-note-name-convention 'note-name-arciorgano)
  (5am:is (eq *default-note-name-convention* 'note-name-arciorgano))

  (set-default-note-name-convention 'key-name-vicentino)
  (5am:is (eq *default-note-name-convention* 'key-name-vicentino))

  (set-default-note-name-convention 'key-name-arciorgano)
  (5am:is (eq *default-note-name-convention* 'key-name-arciorgano)))

(5am:test note-name-creation
  :description "Testing the function NOTE."
  (set-default-note-name-convention 'note-name-smn)
  (loop for letter in (list :a :b :c :d :e :f :g)
        do (loop for accidental in (list nil :sharp :flat)
                 do (loop for octave from -3 to 8
                          do (let ((n-oct (nn letter accidental octave))
                                   (n-pc (nn letter accidental)))
                               (5am:is (letter n-oct) letter)
                               (5am:is (letter n-pc) letter)
                               (5am:is (eq (accidental n-oct) accidental))
                               (5am:is (eq (accidental n-pc) accidental))
                               (5am:is (= (octave n-oct) octave))
                               (5am:is (= (octave n-pc) 2)))))))

;; (5am:test vicentino-note-names
;;   (let ((note1 (make-instance 'note-name-vicentino))
;;         (note2 (make-instance 'note-name-vicentino)))
;;     (setf (octave note1) 1
;;           (accidental note1) nil
;;           (letter note1) :d
;;           (enharmonic-dot note1) :dot)
;;     (setf (octave note2) 1
;;           (accidental note2) :nothing
;;           (letter note2) :d
;;           (enharmonic-dot note2) nil)
;;     (log:info "test" (validp note2))
;;     (5am:is (validp note1) t)
;;     (5am:is (validp note2) nil)))


(in-package :symbolic-intervals)

(5am:def-suite interval-construction)

(5am:in-suite interval-construction)

(5am:test adding-intervals
  (let ((interval-lists '(;; simple addition within an octave, ascending
                          ((tono ascendente 0) (tono ascendente 0)
                           (ditono ascendente 0))
                          ((diapente ascendente 0) (diatessaron ascendente 0)
                           (diapason ascendente 0))

                          ;; simple addition within an octave, descending
                          ((tono discendente 0) (tono discendente 0)
                           (ditono discendente 0))
                          ((diapente discendente 0) (diatessaron discendente 0)
                           (diapason discendente 0))

                          ;; addition beyond the octave, ascending
                          ((diapente ascendente 0) (diapente ascendente 0)
                           (tono ascendente 1))
                          ((ditono ascendente 1) (semiditono ascendente 1)
                           (diapente ascendente 2))

                          ;; addition beyond the octave, descending
                          ((diapente discendente 0) (diapente discendente 0)
                           (tono discendente 1))
                          ((ditono discendente 1) (semiditono discendente 1)
                           (diapente discendente 2))

                          ;; addition by octaves, ascending
                          ((tono ascendente 0) (diapason ascendente 0)
                           (tono ascendente 1))
                          ((diapason ascendente 0) (tono ascendente 0)
                           (tono ascendente 1))
                          ((diapason ascendente 0) (diapason ascendente 0)
                           (diapason ascendente 1))
                          ((ditono ascendente 1) (semiditono ascendente 2)
                           (diapente ascendente 3))
                          ((diapason ascendente 1) (diapason ascendente 2)
                           (diapason ascendente 4))

                          ;; addition by octaves, descending
                          ((tono discendente 0) (diapason discendente 0)
                           (tono discendente 1))
                          ((diapason discendente 0) (tono discendente 0)
                           (tono discendente 1))
                          ((diapason discendente 0) (diapason discendente 0)
                           (diapason discendente 1))
                          ((ditono discendente 1) (semiditono discendente 2)
                           (diapente discendente 3))
                          ((diapason discendente 1) (diapason discendente 2)
                           (diapason discendente 4))

                          ;; simple subtraction, within an octave, in commutative pairs
                          ((diapente ascendente 0) (diatessaron discendente 0)
                           (tono ascendente 0))
                          ((diatessaron discendente 0) (diapente ascendente 0)
                           (tono ascendente 0))
                          ((ditono ascendente 0) (diapente discendente 0)
                           (semiditono discendente 0))
                          ((diapente discendente 0) (ditono ascendente 0)
                           (semiditono discendente 0))

                          ;; subtraction, crossing octaves, in commutative pairs
                          ((ditono ascendente 1) (diapente discendente 0)
                           (sesta-maggiore ascendente 0))
                          ((ditono discendente 1) (diapente ascendente 0)
                           (sesta-maggiore discendente 0))
                          ((ditono ascendente 3) (diapente discendente 0)
                           (sesta-maggiore ascendente 2))
                          ((ditono discendente 3) (diapente ascendente 0)
                           (sesta-maggiore discendente 2))
                          ((ditono ascendente 3) (diapente discendente 1)
                           (sesta-maggiore ascendente 1))
                          ((ditono discendente 3) (diapente ascendente 1)
                           (sesta-maggiore discendente 1))
                          ((diatessaron discendente 0) (diapente ascendente 1)
                           (tono ascendente 1))
                          ((diatessaron ascendente 0) (diapente discendente 1)
                           (tono discendente 1))
                          ((diatessaron discendente 1) (diapente ascendente 2)
                           (tono ascendente 1))
                          ((diatessaron ascendente 1) (diapente discendente 2)
                           (tono discendente 1))
                          ((diatessaron discendente 2) (diapente ascendente 4)
                           (tono ascendente 2))
                          ((diatessaron ascendente 2) (diapente discendente 4)
                           (tono discendente 2))

                          ;; landing in UNISONO, in commutative pairs
                          ((diatessaron ascendente 0) (diatessaron discendente 0)
                           (unisono nil 0))
                          ((diatessaron discendente 0) (diatessaron ascendente 0)
                           (unisono nil 0))
                          ((diatessaron ascendente 3) (diatessaron discendente 3)
                           (unisono nil 0))
                          ((diatessaron discendente 3) (diatessaron ascendente 3)
                           (unisono nil 0)))))
    (dolist (trio interval-lists)
      (5am:is (equal (chain-intervals
                      (apply #'make-interval (first trio))
                      (apply #'make-interval (second trio))
                      *ordine-naturale*
                      'diapason)
                     (apply #'make-interval (third trio)))))))

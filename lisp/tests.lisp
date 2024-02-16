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

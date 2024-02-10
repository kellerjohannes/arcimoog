(5am:def-suite arcimoog-testing
  :description "Testing the entire arcimoog system.")

(5am:def-suite note-names
  :description "Testing note name creation and validity testing."
  :in arcimoog-testing)

(5am:in-suite note-names)


(5am:test vicentino-note-names
  (let ((note1 (make-instance 'note-name-vicentino))
        (note2 (make-instance 'note-name-vicentino)))
    (setf (octave note1) 1
          (accidental note1) nil
          (letter note1) :d
          (enharmonic-dot note1) :dot)
    (setf (octave note2) 1
          (accidental note2) :nothing
          (letter note2) :d
          (enharmonic-dot note2) nil)
    (log:info "test" (validp note2))
    (5am:is (validp note1) t)
    (5am:is (validp note2) nil)))

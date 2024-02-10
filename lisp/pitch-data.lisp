(in-package :arcimoog)

(defclass pitch-data ()
  ((octave :accessor octave
           :documentation "NIL: treated as pitch class, NUMBER: octave indicator, LIST of NUMBERs: simultaneous octaves"))
  (:documentation "Parent class of subclasses for any kind of pitch data."))

(defclass note-name (pitch-data)
  ((letter :accessor letter
           :documentation "Keyword describing the root note name.")
   (accidental :accessor accidental
               :documentation "KEYWORD describing an accidental, NIL for the raw root name."))
  (:documentation "Parent class for note names based on letters and alterations."))

(defclass note-name-smn (note-name)
  ()
  (:documentation "Operational subclass for note names that are part of standard music notation (multiple alterations
are accepted)."))

(defclass note-name-12 (note-name-smn)
  ()
  (:documentation "Operational subclass for note names represented by a standard keyboard with B♭, E♭ and F♯, C♯,
G♯. All other alterations are accepted as input but silently mapped onto these note names."))

(defclass note-name-vicentino (note-name)
  ((enharmonic-dot :accessor enharmonic-dot
                   :documentation "NIL if absent, :dot when raised by a 'diesis enarmonico minore', :comma when raised by a 'comma'."))
  (:documentation "Operational subclass for note names of Vicentino's staff notation."))

(defclass note-name-arciorgano (note-name-vicentino)
  ()
  (:documentation "Operational subclass representing the available notes of the Basel Arciorgano."))

(defclass key-name-vicentino (pitch-data)
  ((letter :accessor letter
           :documentation "Keyword :a-:g.")
   (ordine :accessor ordine
           :documentation "NUMBER 1-6."))
  (:documentation "Operational subclass for key names according to Vicentinos L'antica musica (Rome 1555)."))

(defclass key-name-arciorgano (key-name-vicentino)
  ()
  (:documentation "Operational subclass representing the available keys of the Basel Arciorgano."))





;; (defmethod least-specific-first-combination (methods generic-function)
;;   (destructuring-bind (&key least-specific first-class next-methods)
;;       methods
;;     (funcall least-specific)))

;; (defgeneric my-generic-function ()
;;   (:method-combination least-specific-first-combination))



(defgeneric validp (pitch-data)
  (:method-combination and :most-specific-last)
  (:documentation "Checks for validity of a pitch-data subclass."))

(defgeneric iterator (pitch-data)
  (:documentation "Returns a list with all possible pitch-data instances. This is only available for finite sets of
pitch-data."))



(defmethod validp and ((note pitch-data))
  "Checks if the octave indicator is valid."
  (log:info "Testing validity on PITCH-DATA level.")
  (or (null (octave note))
      (integerp (octave note))
      (and (listp (octave note))
           (every #'integerp (octave note)))))

(defmethod validp and ((note note-name))
  "Checks for validity of generic note name. The letter needs to be a keyword between :a and
:g. ACCIDENTAL is only checked for types, not for specific keywords: it can be NIL, a single keyword
or a list containing keywords (usually all identical, but not necessarily). Methods of subclasses
are required to do the checking of accidentals."
  (log:info "Testing validity on NOTE-NAME level.")
  (and (member (letter note) (list :a :b :c :d :e :f :g))
       (or (null (accidental note))
           (keywordp (accidental note))
           (and (listp (accidental note))
                (every #'keywordp (accidental note))))))

(defun reduce-accidental-list (accidental-list)
  "Excepts a list of identical keywords"
  (let ((simple-list (remove-duplicates accidental-list)))
    (cond ((= 1 (length simple-list))
           (cond ((keywordp (car simple-list))
                  (car simple-list))
                 (t (log:warn "The simplified ACCIDENTAL-LIST" accidental-list "doesn't consist of a keyword and is therefore ignored. This might produce unwanted results.")
                    nil)))
          (t (log:warn "The ACCIDENTAL-LIST" accidental-list "doesn't consist of exclusively identical keywords and will therefore be ignored. This might produce unwanted results.")
             nil))))

(defmethod validp and ((note note-name-smn))
  "Checks the validity for accidentals. They have to be :FLAT, :SHARP, NIL or a list containing
uniquely :FLAT or :SHARP."
  (log:info "Testing validity on NOTE-NAME-SMN level.")
  (let ((allowed-accidentals (list :sharp :flat))
        (candidate (if (listp (accidental note))
                       (reduce-accidental-list (accidental note))
                       (accidental note))))
    (member candidate allowed-accidentals)))

(defmethod validp and ((note note-name-vicentino))
  "Checks the validity of accidentals and enharmonic shifts."
  (log:info "Testing validity on NOTE-NAME-VICENTINO level.")
  (let ((allowed-accidentals (list :sharp :flat))
        (allowed-enharmonic-alterations (list :dot :comma)))
    (and (or (null (accidental note))
             (member (accidental note) allowed-accidentals))
         (or (null (enharmonic-dot note))
             (member (enharmonic-dot note) allowed-enharmonic-alterations)
             (and (listp (enharmonic-dot note))
                  (and (subsetp (enharmonic-dot note) allowed-enharmonic-alterations)
                       (subsetp allowed-enharmonic-alterations (enharmonic-dot note))))))))

(defmethod validp and ((note note-name-arciorgano))
  "Checks the validity of a note name on the Basel Arciorgano."
  (log:info "Testing validity on NOTE-NAME-ARCIORGANO level.")
  (and (< (octave note) 6)
       (> (octave note) 0))
  ;;TODO finish implementation
  )


(defun test ()
  (let ((note (make-instance 'note-name-arciorgano)))
    (setf (octave note) 1
          (letter note) :d
          (accidental note) :sharp
          (enharmonic-dot note) :comma)
    (validp note)))

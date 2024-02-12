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


;; TODO: implement

;; Challenge: some sort of range should be defined (start / end). The type of these arguments are
;; used to dispatch the method correctly. The output should ideally be ordered according to sounding
;; pitch, which requires the implementation of tonsystem and tuning system. Not clear how to deal
;; with this.

(defgeneric iterator (start end)
  (:documentation "Returns an unordered list with all possible pitch-data instances between start and end."))



(defmethod validp and ((note pitch-data))
  "Checks if the octave indicator is valid."
  (log:debug "Testing validity on PITCH-DATA level.")
  (or (null (octave note))
      (integerp (octave note))
      (and (listp (octave note))
           (every #'integerp (octave note)))))

(defmethod validp and ((note note-name))
  "Checks for validity of generic note name. The letter needs to be a keyword between :a and
:g. ACCIDENTAL is only checked for types, not for specific keywords: it can be NIL, a single keyword
or a list containing keywords (usually all identical, but not necessarily). Methods of subclasses
are required to do the checking of accidentals."
  (log:debug "Testing validity on NOTE-NAME level.")
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
  (log:debug "Testing validity on NOTE-NAME-SMN level.")
  (let ((allowed-accidentals (list :sharp :flat))
        (candidate (if (listp (accidental note))
                       (reduce-accidental-list (accidental note))
                       (accidental note))))
    (member candidate allowed-accidentals)))

(defmethod iterator ((start note-name-smn) (end note-name-smn) &optional (multiple-accidentals-depth 2))
  (cond ((and (integerp (octave start))
              (integerp (octave end)))
         (log:warn "Octave indicators of START or END or both are not integers. Returning an empty nil.")
         nil)
        ((and (integerp multiple-accidentals-depth)
              (>= multiple-accidentals-depth 0))
         (log:warn "MULTIPLE-ACCIDENTALS-DEPTH" multiple-accidentals-depth "is invalid. Returning NIL.")))
        (t (with-note-name-convention 'note-name-smn
             (let ((result nil))
               (loop for octave from (octave start) to (octave end)
                     do (dolist (letter (list :a :b :c :d :e :f :g))
                          (dolist (accidental (list nil :flat :sharp))
                            (push (note letter accidental octave) result)
                            (when (> multiple-accidentals-depth 1)
                              (dotimes (i (1- multiple-accidentals-depth))
                                (push (note letter accidental octave) result))))))
               result))))

(defmethod validp and ((note note-name-vicentino))
  "Checks the validity of accidentals and enharmonic shifts."
  (log:debug "Testing validity on NOTE-NAME-VICENTINO level.")
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
  (log:debug "Testing validity on NOTE-NAME-ARCIORGANO level.")
  (and (< (octave note) 6)
       (> (octave note) 0))
  ;;TODO finish implementation
  )



(defparameter *default-note-name-convention* 'note-name-vicentino)

(defun set-default-note-name-convention (convention-class)
  "Set the global default for the note naming convention, used to create new note name instances."
  (if (member convention-class (list 'note-name-smn
                                     'note-name-vicentino
                                     'note-name-arciorgano
                                     'key-name-vicentino
                                     'key-name-arciorgano))
      (setf *default-note-name-convention* convention-class)
      (log:warn "Convention class unknown, DEFAULT-NOTE-NAME-CONVENTION will remain unchanged: "
                *default-note-name-convention*)))

;; TODO: write tests for this macro
(defmacro with-note-name-convention (note-name-class &body body)
  `(if (member ,note-name-class (list 'note-name-smn
                                     'note-name-vicentino
                                     'note-name-arciorgano
                                     'key-name-vicentino
                                     'key-name-arciorgano))
       (let ((*default-note-name-convention* ,note-name-class))
         ,@body)
       (log:warn "Convention class unknown, DEFAULT-NOTE-NAME-CONVENTION will remain unchanged: "
                 *default-note-name-convention*)))


(defun note (letter alteration &optional (octave 2) enharmonic-dot)
  (let ((result (make-instance *default-note-name-convention*)))
    (setf (letter result) letter
          (accidental result) alteration
          (octave result) octave)
    (when (member *default-note-name-convention* (list 'note-name-vicentino
                                                       'note-name-arciorgano))
      (setf (enharmonic-dot result) enharmonic-dot))
    result))

(defun test ()
  (validp (note :x nil)))

(in-package :arcimoog)
;; When adding a new PITCH-DATA subclass:
;; 1. Create a new section in the code below.
;; 2. Define a appropriate subclass.
;; 3. Implement all generic functions (see PITCH-DATA implementation). Implement the PRINT-OBJECT method.
;; 4. Add the class identifier (type of PITCH-DATA) to *valid-note-name-conventions*.
;; 5. Add case statements to various TRANSFORM methods, to translate other PITCH-DATA instances into the newly created one.
;; 6. Add a TRANSFORM method to the newly created class.


;; TODO
;; - write 5am tests for everything

(defparameter *valid-note-name-conventions* (list 'note-name-smn
                                                  'note-name-12
                                                  'note-name-vicentino
                                                  'note-name-arciorgano
                                                  'key-name-vicentino
                                                  'key-name-arciorgano))

(defparameter *default-note-name-convention* 'note-name-smn)

(defun set-default-note-name-convention (convention-class)
  "Set the global default for the note naming convention, used to create new note name instances."
  (if (member convention-class *valid-note-name-conventions*)
      (setf *default-note-name-convention* convention-class)
      (log:warn "Convention class unknown, DEFAULT-NOTE-NAME-CONVENTION will remain unchanged: "
                *default-note-name-convention*)))

(defmacro with-note-name-convention (note-name-class &body body)
  "Dynamically rebind the global symbol *default-note-name-convention*."
  `(if (member ,note-name-class *valid-note-name-conventions*)
       (let ((*default-note-name-convention* ,note-name-class))
         ,@body)
       (progn
         (log:warn "Convention class unknown, DEFAULT-NOTE-NAME-CONVENTION will remain unchanged: "
                   *default-note-name-convention*)
         ,@body)))


;;;;;;;;;;;;;;;;
;; PITCH-DATA ;;
;;;;;;;;;;;;;;;;

;; Parent class for all kinds of pitch data, from the beginning to the end of the pitch
;; pipeline. The OCTAVE slot is always present.

(defclass pitch-data ()
  ((octave :accessor octave
           :initarg :octave
           :documentation "NIL: treated as pitch class, NUMBER: octave indicator, LIST of NUMBERs: simultaneous octaves"))
  (:documentation "Parent class of subclasses for any kind of pitch data. The generic functions of PITCH-DATA need to be implemented for each functional subclass. Some can be omitted if the parent class handles all relevant cases already."))

(defgeneric pitch-equal (pitch-data-1 pitch-data-2 &optional octavep)
  (:method-combination and)
  (:documentation "Checks for equality of two instances of subclasses of PITCH-DATA. When OCTAVEP is nil, the octave equality will be ignored. This is useful when testing for equality of pitch classes."))

(defgeneric validp (pitch-data)
  (:method-combination and :most-specific-last)
  (:documentation "Checks for validity of a pitch-data subclass. Starts with least specific method in order to test for the rough things first, and get more and more refined throughout the dispatch chain."))

(defgeneric iterator (start end)
  (:documentation "Returns an unordered list with all possible PITCH-DATA instances between start and end. Since in many cases (for example NOTE-NAME) the pitch is unknown, the list is not sorted."))

(defgeneric transform (pitch-data target-format)
  (:documentation "Translates pitch data into another pitch data format."))



(defmethod pitch-equal and ((note-1 pitch-data) (note-2 pitch-data) &optional (octavep t))
  "Checks for equality of the OCTAVE slot."
  (log:debug "Testing equality on PITCH-DATA level: OCTAVE slot.")
  (if octavep
      (equalp (octave note-1) (octave note-2))
      t))

(defmethod validp and ((note pitch-data))
  "Checks if the octave indicator is valid."
  (log:debug "Testing validity on PITCH-DATA level.")
  (with-accessors ((oct octave))
      note
    (or (null oct) (integerp oct) (and (listp oct) (every #'integerp oct)))))


;;;;;;;;;;;;;;;
;; NOTE-NAME ;;
;;;;;;;;;;;;;;;

;; Note names based on letter/accidental.

(defclass note-name (pitch-data)
  ((letter :accessor letter
           :initarg :letter
           :documentation "Keyword describing the root note name.")
   (accidental :accessor accidental
               :initarg :accidental
               :documentation "KEYWORD describing an accidental, NIL for the raw root name."))
  (:documentation "Parent class for note names based on letters and alterations."))

(defmethod pitch-equal and ((note-1 note-name) (note-2 note-name) &optional (octavep t))
  "Checks the equality of the LETTER and the ACCIDENTAL slots."
  (declare (ignore octavep))
  (log:debug "Testing equality on NOTE-NAME level: LETTER and ACCIDENTAL slots.")
  (and (eq (letter note-1) (letter note-2))
       (equalp (accidental note-1) (accidental note-2))))

(defparameter *note-name-valid-letters*
  (list :a :b :c :d :e :f :g)
  "Valid letters for the NOTE-NAME subclasses.")

(defun letter-shift (origin distance)
  "Expects a keyword (member of *note-name-valid-letters*) and a delta (integer), returns a keyword."
  (log:debug "Letter translation:" origin distance)
  (unless (member origin *note-name-valid-letters*)
    (log:warn "ORIGIN letter" origin "is not valid."))
  (unless (integerp distance)
    (log:warn "DISTANCE" distance "is not an integer."))
  (let ((pos (position origin *note-name-valid-letters*)))
    (cond ((zerop distance) origin)
          ((zerop pos)
           (if (minusp distance)
               (letter-shift (car (last *note-name-valid-letters*)) (shrink distance))
               (letter-shift (nth (1+ pos) *note-name-valid-letters*) (shrink distance))))
          ((= pos (1- (length *note-name-valid-letters*)))
           (if (minusp distance)
               (letter-shift (nth (1- pos) *note-name-valid-letters*) (shrink distance))
               (letter-shift (first *note-name-valid-letters*) (shrink distance))))
          (t (letter-shift (nth (if (plusp distance)
                                          (1+ pos)
                                          (1- pos))
                                      *note-name-valid-letters*)
                                 (shrink distance))))))

(defmethod validp and ((note note-name))
  "Checks for validity of generic note name. The letter needs to be a keyword between :a and
:g. ACCIDENTAL is only checked for types, not for specific keywords: it can be NIL, a single keyword
or a list containing keywords (usually all identical, but not necessarily). Methods of subclasses
are required to do the checking of accidentals."
  (log:debug "Testing validity on NOTE-NAME level.")
  (with-accessors ((acc accidental))
      note
    (and (member (letter note) *note-name-valid-letters*)
         (or (null acc)
             (keywordp acc)
             (and (listp acc)
                  (every #'keywordp acc))))))


;;;;;;;;;;;;;;;;;;;
;; NOTE-NAME-SMN ;;
;;;;;;;;;;;;;;;;;;;

;; Note names in standard music notation with single and multiple accidentals.

(defclass note-name-smn (note-name)
  ()
  (:documentation "Operational subclass for note names that are part of standard music notation (multiple alterations
are accepted)."))

(defun print-smn-accidental (accidental)
  "Returns a string based on a valid SMN-accidental."
  (cond ((null accidental) nil)
        ((keywordp accidental)
         (ecase accidental
           (:sharp "♯")
           (:flat "♭")))
        ((listp accidental)
         (make-string (length accidental)
                      :initial-element (ecase (first accidental)
                                         (:sharp #\SHARP)
                                         (:flat #\FLAT))))))

(defmethod print-object ((note note-name-smn) out)
  (print-unreadable-object (note out :type t)
    (format out "~a~@[~a~][~a]"
            (letter note)
            (print-smn-accidental (accidental note))
            (octave note))))

;; pitch-equal is not needed, parent class implementation covers all slots.

(defparameter *note-name-smn-valid-accidentals* (list :sharp :flat))

(defmethod validp and ((note note-name-smn))
  "Checks the validity for accidentals. They have to be :FLAT, :SHARP, NIL or a list containing
uniquely :FLAT or :SHARP."
  (log:debug "Testing validity on NOTE-NAME-SMN level.")
  (with-accessors ((acc accidental))
      note
    (let ((candidate (if (and acc (listp acc))
                         (reduce-equal-keyword-list acc)
                         acc)))
      (or (null acc) (member candidate *note-name-smn-valid-accidentals*)))))

(defparameter *note-name-letter-circle*
  (list :f :c :g :d :a :e :b)
  "Valid NOTE-NAME letters, arranged according to their position in a column of fifths.")

(defmethod note-name-smn-fifth-above ((note note-name-smn))
  "Returns a new instance of NOTE-NAME-SMN that represents a note a fifth above the given note."
  (with-accessors ((acc accidental))
      note
    (let ((pos (position (letter note) *note-name-letter-circle*)))
      (cond ((= pos (1- (length *note-name-letter-circle*)))
             (create-note 'note-name-smn
                          (first *note-name-letter-circle*)
                          (cond ((null acc) :sharp)
                                ((eq acc :flat) nil)
                                ((eq acc :sharp) (list :sharp :sharp))
                                ((and (listp acc)
                                      (eq (first acc) :flat))
                                 (rest acc))
                                (t (cons :sharp acc)))
                          (octave note)))
            (t (create-note 'note-name-smn
                            (nth (1+ pos) *note-name-letter-circle*)
                            acc
                            (octave note)))))))

(defmethod note-name-smn-fifth-below ((note note-name-smn))
  "Returns a new instance of NOTE-NAME-SMN that represents a note a fifth below the given note."
  (with-accessors ((acc accidental))
      note
    (let ((pos (position (letter note) *note-name-letter-circle*)))
      (cond ((zerop pos)
             (create-note 'note-name-smn
                          (car (last *note-name-letter-circle*))
                          (cond ((null acc) :flat)
                                ((eq acc :flat) (list :flat :flat))
                                ((eq acc :sharp) nil)
                                ((and (listp acc)
                                      (eq (first acc) :sharp))
                                 (rest acc))
                                (t (cons :flat acc)))
                          (octave note)))
            (t (create-note 'note-name-smn
                            (nth (1- pos) *note-name-letter-circle*)
                            acc
                            (octave note)))))))

(defmethod note-name-smn-fifth-shift ((note note-name-smn) delta)
  "Returns a new instance of NOTE-NAME-SMN that represents a pitch class transposed by DELTA fifths."
  (cond ((zerop delta) note)
        ((plusp delta) (note-name-smn-fifth-shift (note-name-smn-fifth-above note)
                                                        (1- delta)))
        ((minusp delta) (note-name-smn-fifth-shift (note-name-smn-fifth-below note)
                                                         (1+ delta)))))


;;;;;;;;;;;;;;;;;;
;; NOTE-NAME-12 ;;
;;;;;;;;;;;;;;;;;;

;; Note names, projected to a keyboard with C♯, E♭, F♯, G♯ and B♭. When importing, enharmonics will
;; be applied.

(defclass note-name-12 (note-name-smn)
  ()
  (:documentation "Operational subclass for note names represented by a standard keyboard with B♭, E♭ and F♯, C♯,
G♯. All other alterations are accepted as input but silently mapped onto these note names."))

;; print-object is not necessary, it is covered by the print-object of parent class
;; pitch-equal is not needed, parent class implementation covers all slots.

(defparameter *note-name-12-black-notes*
  (list (make-instance 'note-name-12 :letter :c :accidental :sharp :octave nil)
        (make-instance 'note-name-12 :letter :e :accidental :flat :octave nil)
        (make-instance 'note-name-12 :letter :f :accidental :sharp :octave nil)
        (make-instance 'note-name-12 :letter :g :accidental :sharp :octave nil)
        (make-instance 'note-name-12 :letter :b :accidental :flat :octave nil)))

(defmethod validp and ((note note-name-12))
  "Checks validity of a already valid NOTE-NAME-SMN."
  (or (null (accidental note))
      (member note *note-name-12-black-notes* :test (lambda (a b) (pitch-equal a b nil)))))

(defmethod note-name-smn-enharmonic-equivalent-12 ((note note-name-smn) direction)
  "Returns a new instance of NOTE-NAME-SMN with the next enharmonic equivalent in a 12-fold circle of fifths. DIRECTION is :UP or :DOWN."
  (if (eq direction :up)
      (note-name-smn-fifth-shift note 12)
      (note-name-smn-fifth-shift note -12)))

(defmethod note-name-smn-remap-multiple-accidental ((note note-name-smn))
  "Returns a new instance of NOTE-NAME-SMN with a note name that represents the same Pitch as NOTE on a 12-fold circle of fifths."
  (cond ((or (null (accidental note)) (keywordp (accidental note))) note)
        (t (note-name-smn-remap-multiple-accidental
            (note-name-smn-enharmonic-equivalent-12 note (if (eq (first (accidental note)) :sharp)
                                                             :down
                                                             :up))))))

(defparameter *note-name-smn-12-sharps* (list :f :c :g)
  "The letters that can have valid sharps in a strict 12-keyboard.")

(defparameter *note-name-smn-12-flats* (list :b :e)
  "The letters that can have valid flats in a strict 12-keyboard.")


;; TODO: iterator




;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE-NAME-VICENTINO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note names based on Nicola Vicentinos convention, where every conventional note name can be
;; further altered with an enharmonic dot or a comma (or both).

(defclass note-name-vicentino (note-name)
  ((enharmonic-dot :accessor enharmonic-dot
                   :initarg :enharmonic-dot
                   :documentation "NIL if absent, :dot when raised by a 'diesis enarmonico minore', :comma when raised by a 'comma'."))
  (:documentation "Operational subclass for note names of Vicentino's staff notation."))

(defparameter *vicentino-dotted-letters*
  '((:a "Ȧ" #\LATIN_CAPITAL_LETTER_A_WITH_DOT_ABOVE)
    (:b "Ḃ" #\LATIN_CAPITAL_LETTER_B_WITH_DOT_ABOVE)
    (:c "Ċ" #\LATIN_CAPITAL_LETTER_C_WITH_DOT_ABOVE)
    (:d "Ḋ" #\LATIN_CAPITAL_LETTER_D_WITH_DOT_ABOVE)
    (:e "Ė" #\LATIN_CAPITAL_LETTER_E_WITH_DOT_ABOVE)
    (:f "Ḟ" #\LATIN_CAPITAL_LETTER_F_WITH_DOT_ABOVE)
    (:g "Ġ" #\LATIN_CAPITAL_LETTER_G_WITH_DOT_ABOVE)))


(defun lookup-vicentino-letter-string (letter-keyword)
  (second (assoc letter-keyword *vicentino-dotted-letters*)))

(defun lookup-vicentino-letter-char (letter-keyword)
  (third (assoc letter-keyword *vicentino-dotted-letters*)))

(defmethod print-object ((note note-name-vicentino) out)
  (print-unreadable-object (note out :type t)
    (format out "~a~@[~a~]~@[~a~][~a]"
            (if (or (eq (enharmonic-dot note) :dot)
                    (and (listp (enharmonic-dot note))
                         (member :dot (enharmonic-dot note))))
                (lookup-vicentino-letter-string (letter note))
                (letter note))
            (print-smn-accidental (accidental note))
            (when (or (eq (enharmonic-dot note) :comma)
                      (and (listp (enharmonic-dot note))
                           (member :comma (enharmonic-dot note))))
              "❜")
            (octave note))))

(defmethod pitch-equal and ((note-1 note-name-vicentino) (note-2 note-name-vicentino) &optional octavep)
  "Checks equality of the ENHARMONIC-DOT slot."
  (declare (ignore octavep))
  (log:debug "Testing equality on NOTE-NAME-VICENTINO level: ENHARMONIC-DOT slot.")
  (equalp (enharmonic-dot note-1) (enharmonic-dot note-2)))

(defparameter *note-name-vicentino-valid-accidentals* (list :flat :sharp))

(defparameter *note-name-vicentino-valid-enharmonic-dots* (list :dot :comma))

(defmethod validp and ((note note-name-vicentino))
  "Checks the validity of accidentals and enharmonic shifts."
  (log:debug "Testing validity on NOTE-NAME-VICENTINO level.")
  (and (or (null (accidental note))
           (member (accidental note) *note-name-vicentino-valid-accidentals*))
       (or (null (enharmonic-dot note))
           (member (enharmonic-dot note) *note-name-vicentino-valid-enharmonic-dots*)
           (and (listp (enharmonic-dot note))
                (and (subsetp (enharmonic-dot note) *note-name-vicentino-valid-enharmonic-dots*)
                     (subsetp *note-name-vicentino-valid-enharmonic-dots* (enharmonic-dot note)))))))


;; TODO: iterator


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE-NAME-ARCIORGANO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note names that are available on the Basel reconstruction of Vicentino's Arciorgano (Fleig 2016).

(defclass note-name-arciorgano (note-name-vicentino)
  ()
  (:documentation "Operational subclass representing the available notes of the Basel Arciorgano."))

(defmethod validp and ((note note-name-arciorgano))
  "Checks the validity of a note name on the Basel Arciorgano."
  (log:debug "Testing validity on NOTE-NAME-ARCIORGANO level.")
  (and (< (octave note) 6)
       (> (octave note) 0))
  ;;TODO finish implementation
  )

;; print-object not necessary, covered by parent class

;; pitch-equal is not needed, parent class implementation covers all slots.

;; TODO: validp, iterator





;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY-NAME-VICENTINO ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Key names based on Nicola Vicentino's convention, using the letters :a - :g and the indices 1-6.

(defclass key-name-vicentino (pitch-data)
  ((letter :accessor letter
           :initarg :letter
           :documentation "Keyword :a-:g.")
   (ordine :accessor ordine
           :initarg :ordine
           :documentation "NUMBER 1-6."))
  (:documentation "Operational subclass for key names according to Vicentinos L'antica musica (Rome 1555)."))

;; TODO: pitch-equal

;; TODO: validp, print-object, iterator



;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY-NAME-ARCIORGANO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Key names based on the Basel reconstruction of Vicentino's Arciorgano (Fleig 2016).

(defclass key-name-arciorgano (key-name-vicentino)
  ()
  (:documentation "Operational subclass representing the available keys of the Basel Arciorgano."))

;; print-object not necessary, covered by parent class

;; pitch-equal is not needed, parent class implementation covers all slots.


;; TODO: validp, iterator












;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Universal constructor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nn (letter &optional (alteration nil) (octave 2) (enharmonic-dot nil))
  "Creates an instance of a PITCH-DATA subclass."
  (let ((result (make-instance *default-note-name-convention*)))
    (setf (letter result) letter
          (accidental result) alteration
          (octave result) octave)
    (when (member *default-note-name-convention* (list 'note-name-vicentino
                                                       'note-name-arciorgano))
      (setf (enharmonic-dot result) enharmonic-dot))
    (unless (validp result)
      (log:warn "Validity for newly created PITCH-INFO" result "failed. The pitch convention is" *default-note-name-convention* "This might create unwanted results. The PITCH-DATA instance is returend nevertheless."))
    result))

(defun create-note (note-name-convention letter &optional (alteration nil) (octave 2) (enharmonic-dot nil))
  (with-note-name-convention note-name-convention
    (nn letter alteration octave enharmonic-dot)))






;;;;;;;;;;;;;;;;;;;;;
;; Transformations ;;
;;;;;;;;;;;;;;;;;;;;;

(defparameter *dict-smn-vicentino*
  (macrolet ((smn (letter acc)
               `(make-instance 'note-name-smn :letter ,letter
                                              :accidental ,acc
                                              :octave nil))
             (vic (letter acc dot)
               `(make-instance 'note-name-vicentino :letter ,letter
                                                    :accidental ,acc
                                                    :octave nil
                                                    :enharmonic-dot ,dot))
             (entry (smn-arguments vic-arguments)
               `(cons (smn ,@smn-arguments) (vic ,@vic-arguments))))
    (list (entry (:c nil) (:c nil nil))
          (entry (:d '(:flat :flat)) (:c nil :dot))
          (entry (:c :sharp) (:c :sharp nil))
          (entry (:d :flat) (:d :flat nil))
          (entry (:c '(:sharp :sharp)) (:d :flat :dot))
          (entry (:d nil) (:d nil nil))
          (entry (:e '(:flat :flat)) (:d nil :dot))
          (entry (:d :sharp) (:d :sharp nil))
          (entry (:e :flat) (:e :flat nil))
          (entry (:d '(:sharp :sharp)) (:e :flat :dot))
          (entry (:e nil) (:e nil nil))
          (entry (:f :flat) (:e nil :dot))
          (entry (:e :sharp) (:e :sharp nil))
          (entry (:f nil) (:f nil nil))
          (entry (:g '(:flat :flat)) (:f nil :dot))
          (entry (:f :sharp) (:g :sharp nil))
          (entry (:g :flat) (:g :flat nil))
          (entry (:f '(:sharp :sharp)) (:g :flat :dot))
          (entry (:g nil) (:g nil nil))
          (entry (:a '(:flat :flat)) (:g nil :dot))
          (entry (:g :sharp) (:g :sharp nil))
          (entry (:a :flat) (:a :flat nil))
          (entry (:g '(:sharp :sharp)) (:a :flat :dot))
          (entry (:a nil) (:a nil nil))
          (entry (:b '(:flat :flat)) (:a nil :dot))
          (entry (:a :sharp) (:a :sharp nil))
          (entry (:b :flat) (:b :flat nil))
          (entry (:a '(:sharp :sharp)) (:b :flat :dot))
          (entry (:b nil) (:b nil nil))
          (entry (:c :flat) (:b nil :dot))
          (entry (:b :sharp) (:b :sharp nil)))))

(defmethod lookup-smn-vicentino ((note note-name-smn))
  (cdr (assoc note *dict-smn-vicentino* :test (lambda (a b) (pitch-equal a b nil)))))

(defmethod transform ((note note-name-smn) target)
  "Returns a new PITCH-DATA instance based on the PITCH-DATA subclass specified in TARGET. Some conversions might cause a loss of information. In this case, no warning is issued."
  (ecase target
    (note-name-12
     (let* ((simple-note (note-name-smn-remap-multiple-accidental note))
            (result (cond ((eq (accidental simple-note) :sharp)
                           (if (member (letter simple-note) *note-name-smn-12-sharps*)
                               simple-note
                               (note-name-smn-enharmonic-equivalent-12 simple-note :down)))
                          ((eq (accidental simple-note) :flat)
                           (if (member (letter simple-note) *note-name-smn-12-flats*)
                               simple-note
                               (note-name-smn-enharmonic-equivalent-12 simple-note :up)))
                          (t simple-note))))
       (create-note 'note-name-12
                    (letter result) (accidental result) (octave result))))
    (note-name-vicentino (lookup-smn-vicentino note))))



;;;;;;;;;;;;;;;
;; Iterators ;;
;;;;;;;;;;;;;;;


;; TODO: finish implementation, and implement others
(defmethod iterator ((start note-name-smn) (end note-name-smn))
  "Returns an unordered list of possible note names of the type NOTE-NAME-SMN."
  (cond ((and (integerp (octave start))
              (integerp (octave end)))
         (log:warn "Octave indicators of START or END or both are not integers. Returning an empty nil.")
         nil)
        ((and (integerp multiple-accidentals-depth)
              (>= multiple-accidentals-depth 0))
         (log:warn "MULTIPLE-ACCIDENTALS-DEPTH" multiple-accidentals-depth "is invalid. Returning NIL."))
        (t (with-note-name-convention 'note-name-smn
             (let ((result nil))
               (loop for octave from (octave start) to (octave end)
                     do (dolist (letter (list :a :b :c :d :e :f :g))
                          (dolist (accidental (list nil :flat :sharp))
                            (push (nn letter accidental octave) result)
                            (when (> multiple-accidentals-depth 1)
                              (dotimes (i (1- multiple-accidentals-depth))
                                (push (nn letter accidental octave) result))))))
               result)))))

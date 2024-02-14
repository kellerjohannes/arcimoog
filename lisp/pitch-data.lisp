(in-package :arcimoog)

;; TODO
;; Allowed accidentals etc.: define as global parameters for each class



;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;
;; PITCH-DATA ;;
;;;;;;;;;;;;;;;;

;; Parent class for all kinds of pitch data, from the beginning to the end of the pitch
;; pipeline. The OCTAVE slot is always present.

(defclass pitch-data ()
  ((octave :accessor octave
           :documentation "NIL: treated as pitch class, NUMBER: octave indicator, LIST of NUMBERs: simultaneous octaves"))
  (:documentation "Parent class of subclasses for any kind of pitch data."))

(defgeneric validp (pitch-data)
  (:method-combination and :most-specific-last)
  (:documentation "Checks for validity of a pitch-data subclass. Starts with least specific method in order to test for the rough things first, and get more and more refined throughout the dispatch chain."))

(defgeneric iterator (start end)
  (:documentation "Returns an unordered list with all possible PITCH-DATA instances between start and end. Since in many cases (for example NOTE-NAME) the pitch is unknown, the list is not sorted."))

(defgeneric transform (pitch-data target-format)
  (:documentation "Translates pitch data into another pitch data format."))

(defmethod validp and ((note pitch-data))
  "Checks if the octave indicator is valid."
  (log:debug "Testing validity on PITCH-DATA level.")
  (or (null (octave note))
      (integerp (octave note))
      (and (listp (octave note))
           (every #'integerp (octave note)))))


;;;;;;;;;;;;;;;
;; NOTE-NAME ;;
;;;;;;;;;;;;;;;

;; Note names based on letter/accidental.

(defclass note-name (pitch-data)
  ((letter :accessor letter
           :documentation "Keyword describing the root note name.")
   (accidental :accessor accidental
               :documentation "KEYWORD describing an accidental, NIL for the raw root name."))
  (:documentation "Parent class for note names based on letters and alterations."))

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


;;;;;;;;;;;;;;;;;;;
;; NOTE-NAME-SMN ;;
;;;;;;;;;;;;;;;;;;;

;; Note names in standard music notation with single and multiple accidentals.

(defclass note-name-smn (note-name)
  ()
  (:documentation "Operational subclass for note names that are part of standard music notation (multiple alterations
are accepted)."))

(defun print-smn-accidental (accidental)
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

(defmethod validp and ((note note-name-smn))
  "Checks the validity for accidentals. They have to be :FLAT, :SHARP, NIL or a list containing
uniquely :FLAT or :SHARP."
  (log:debug "Testing validity on NOTE-NAME-SMN level.")
  (let ((allowed-accidentals (list :sharp :flat))
        (candidate (if (listp (accidental note))
                       (reduce-accidental-list (accidental note))
                       (accidental note))))
    (member candidate allowed-accidentals)))

(defmethod transform ((note note-name-smn) target)
  (ecase target
    (note-name-12 (with-note-name-convention 'note-name-12
                    (note (letter note) nil (octave note))))))

;; TODO: finish implementation, and implement others
(defmethod iterator ((start note-name-smn) (end note-name-smn))
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
                            (push (note letter accidental octave) result)
                            (when (> multiple-accidentals-depth 1)
                              (dotimes (i (1- multiple-accidentals-depth))
                                (push (note letter accidental octave) result))))))
               result)))))


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

;; TODO: validp, iterator




;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE-NAME-VICENTINO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note names based on Nicola Vicentinos convention, where every conventional note name can be
;; further altered with an enharmonic dot or a comma (or both).

(defclass note-name-vicentino (note-name)
  ((enharmonic-dot :accessor enharmonic-dot
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

;; TODO: validp, iterator





;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY-NAME-VICENTINO ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Key names based on Nicola Vicentino's convention, using the letters :a - :g and the indices 1-6.

(defclass key-name-vicentino (pitch-data)
  ((letter :accessor letter
           :documentation "Keyword :a-:g.")
   (ordine :accessor ordine
           :documentation "NUMBER 1-6."))
  (:documentation "Operational subclass for key names according to Vicentinos L'antica musica (Rome 1555)."))

;; TODO: validp, print-object, iterator



;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY-NAME-ARCIORGANO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Key names based on the Basel reconstruction of Vicentino's Arciorgano (Fleig 2016).

(defclass key-name-arciorgano (key-name-vicentino)
  ()
  (:documentation "Operational subclass representing the available keys of the Basel Arciorgano."))

;; print-object not necessary, covered by parent class

;; TODO: validp, iterator
















(defparameter *default-note-name-convention* 'note-name-smn)

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

;; TODO: write 5am-tests for this macro
(defmacro with-note-name-convention (note-name-class &body body)
  "Dynamically rebind the global symbol *default-note-name-convention*."
  `(if (member ,note-name-class (list 'note-name-smn
                                      'note-name-12
                                      'note-name-vicentino
                                      'note-name-arciorgano
                                      'key-name-vicentino
                                      'key-name-arciorgano))
       (let ((*default-note-name-convention* ,note-name-class))
         ,@body)
       (progn
         (log:warn "Convention class unknown, DEFAULT-NOTE-NAME-CONVENTION will remain unchanged: "
                   *default-note-name-convention*)
         ,@body)))

(defun note (letter &optional (alteration nil) (octave 2) enharmonic-dot)
  "Create an instance of a PITCH-DATA subclass."
  (let ((result (make-instance *default-note-name-convention*)))
    (setf (letter result) letter
          (accidental result) alteration
          (octave result) octave)
    (when (member *default-note-name-convention* (list 'note-name-vicentino
                                                       'note-name-arciorgano))
      (setf (enharmonic-dot result) enharmonic-dot))
    (unless (validp result)
      (log:warn "Validity for newly created PITCH-INFO" result "failed. This might create unwanted results. The PITCH INFO instance is returend nevertheless."))
    result))

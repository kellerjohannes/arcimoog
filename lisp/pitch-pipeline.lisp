(defclass pipeline () ())

(defclass pitch-convention (pipeline) ()
  (:documentation "Contains definitions relevant to a specific note naming or other pitch data convention. For example valid letters or alterations that are used to create an instance of PITCH-DATA."))

(defgeneric valid-data-p (pitch-data pitch-convention)
  (:documentation "Takes pitch data of a particular convention and checks whether it is in a valid form."))


(defclass pitch-data (pipeline) ()
  (:documentation "Contains for example a note name, following a specific convention."))

(defgeneric create-pitch-data (pitch-convention &rest data)
  (:documentation "Creates an instance of the PITCH-DATA class. It involved checking for validity using VALID-DATA-P."))

(defgeneric equal-pitch-data-p (pitch-data-a pitch-data-b))


(defclass tonsystem (pipeline) ()
  (:documentation "Contains parameters that define a specific Tonsystem, like lookup tables that map note names to indices that can be used to apply a tuning model."))

(defgeneric populate-lookup-table (tonsystem note-name-list index-list)
  (:documentation "Creates a lookup table that translates between note names and tonsystem-inherent pitch data (some kind of index)."))

(defgeneric note-name-to-tonsystem (pitch-data tonsystem)
  (:documentation "Takes pitch data in the form of a note name and converts it into a form that can be used to calculate by TUNING-MODEL."))


(defclass tuning-model (pipeline) ()
  (:documentation "Contains parameters that define a tuning model, like the size of the generator interval, or the configuration of a Tonnetz."))

(defgeneric tonsystem-to-tuning-model (pitch-data tuning-model)
  (:documentation "Takes data coming from a TONSYSTEM and converts it according to a TUNING-MODEL."))


(defclass output-module (pipeline) ()
  (:documentation "Contains parameters that define the appearance of the final result of the pipeline, like the reference pitch and oscillator configurations for audio output."))

(defgeneric tuning-model-to-output-module (pitch-data output-module)
  (:documentation "Takes data coming from a TUNING-MODEL and transforms it to its final apperance, defined by an OUTPUT-MODULE."))


(defun create-pipeline (tonsystem tuning-model output-module)
  "Creates a function that transforms origin data into sound or graphics."
  (lambda (pitch-data)
    (tuning-model-to-output-module
     (tonsystem-to-tuning-model (pitch-information-to-tonsystem
                                 pitch-data tonsystem)
                                tuning-model)
     output-module)))



(defclass common-notation (pitch-convention)
  ((valid-letters :accessor valid-letters :initarg :valid-letters :documentation "A string containing all valid note letters, normally \"abcdefg\".")
   (valid-alterations :accessor valid-alterations :initarg :valid-alterations :documentation "A list containing all valid alterations, given as keywords or nil.")
   (valid-octaves :accessor valid-octaves :initarg :valid-octaves :documentation "A list containing all valid octave values, while 0 is the central octave of a system."))
  (:documentation "Intended for standard note name conventions consisting of a root letter, alteration and octave indicator."))

(defparameter *common-chromatic-note-name-convention*
  (make-instance 'common-notation
                 :valid-letters "abcdefg"
                 :valid-octaves (list -3 -2 -1 0 1 2)
                 :valid-alterations (list :flat :sharp :natural nil))
  "Common pitch notation with single alterations.")

(defparameter *common-multiple-alteration-name-convention*
  (make-instance 'common-notation
                 :valid-letters "abcdefg"
                 :valid-octaves (list -3 -2 -1 0 1 2)
                 :valid-alterations (list :flat :sharp :natural
                                          :double-sharp :double-flat :triple-sharp :triple-flat
                                          nil))
  "Common pitch notation with double and triple alterations.")

(defparameter *common-quarter-tone-name-convention*
  (make-instance 'common-notation
                 :valid-letters "abcdefg"
                 :valid-octaves (list -3 -2 -1 0 1 2)
                 :valid-alterations (list :flat :sharp :natural
                                         :double-sharp :double-flat :triple-sharp :triple-flat
                                         :quarter-sharp :three-quarter-sharp
                                         :quarter-flat :three-quarter-flat
                                         nil))
  "Common pitch notation for quarter tone music, including multiple alterations.")

(defclass note-name (pitch-data)
  ((letter :accessor letter :initarg :letter)
   (octave :accessor octave :initarg :octave)
   (alteration :accessor alteration :initarg :alteration)))

(defmethod equal-pitch-data-p ((note-a note-name) (note-b note-name))
  (and (char= (letter note-a) (letter note-b))
       (= (octave note-a) (octave note-b))
       (eql (alteration note-a) (alteration note-b))))

(defmethod valid-data-p ((convention common-notation) (data note-name))
  (and (find (letter data) (valid-letters convention))
       (member (alteration data) (valid-alterations convention))
       (member (octave data) (valid-octaves convention))))

(defmethod create-pitch-data ((convention common-notation) &rest data)
  "For the COMMON-NOTATION convention, the data consists of a letter (CHAR), an alteration (keyword) and an octave indicator (integer)."
  (let ((result (make-instance 'note-name
                               :letter (first data)
                               :alteration (second data)
                               :octave (third data))))
    (if (valid-data-p result convention)
        result
        (format t "~%Invalid pitch data, ~a." data))))


(defun std-note (letter alteration octave)
  (create-pitch-data *common-chromatic-note-name-convention* letter alteration octave))

(defun ext-note (letter alteration octave)
  (create-pitch-data *common-multiple-alteration-name-convention* letter alteration octave))

(defun quarter-note (letter alteration octave)
  (create-pitch-data *common-quarter-tone-name-convention* letter alteration octave))



(defclass chain-of-fifths (tonsystem)
  ((lookup-table :initarg :lookup-table
                 :initform (make-hash-table :test #'equal-pitch-data-p)
                 :accessor lookup-table)))

(defmethod populate-lookup-table ((system chain-of-fifths) note-names index-start)
  (loop for note-name in note-names
        for index from index-start
        do (setf (gethash note-name (lookup-table system)) index)))


(defparameter *chain-of-fifths* (make-instance 'chain-of-fifths))



(defclass linear-system (tuning-model)
  ((generator-interval :initform 3/2 :initarg :generator-interval :accessor generator-interval)
   (identity-interval :initform 2/1 :initarg :identity-interval :accessor identity-interval)))

(defmethod tonsystem-to-tuning-model ((data note-name) (tuning linear-system))
  ())

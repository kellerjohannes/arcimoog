(in-package :arcimoog)

(incudine:enable-sharp-square-bracket-syntax)

(defparameter *midi-in* nil)
(defparameter *midi-responder* nil)
(defparameter *parameter-slots* (make-array 256 :initial-element 0))

(defparameter *global-character-set*
  " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZȧḃċḋėḟġȦḂĊḊĖḞĠ♯♭♮❜ʼ'\"«»[]#{}/\\,.!?:;➙➚➘12345674890-+*·")

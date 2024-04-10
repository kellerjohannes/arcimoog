(in-package :arcimoog)


;;;; Testing sample playback

(vug:dsp! sample-player ((buffer incudine:buffer) rate start-position (loopp boolean))
  (vug:foreach-channel
    (vug:cout (vug:buffer-play buffer rate start-position loopp #'incudine:stop))))

(defparameter *sample-1* (incudine:buffer-load "/home/johannes/Vicentino21/wallbrecher/audio-final/b01-c05-m01-mv01-a0-v01-20240105.wav"))

(defparameter *sample-2* (incudine:buffer-load "/home/johannes/Downloads/BabyElephantWalk60.wav"))

(sample-player *sample-1* 1 0 t :id 1)
(sample-player *sample-2* 1 0 t :id 2)

(incudine:set-control 1 :rate 1)
(incudine:set-control 2 :rate 1)

(incudine:set-control 1 :loopp nil)
(incudine:set-control 2 :loopp nil)

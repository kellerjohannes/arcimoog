(in-package :arcimoog.symbolic-intervals)

;;;; Transform shorthand notations

(defparameter *interval-dict* '((st . (:semitono-maggiore :➚))
                                (std . (:semitono-maggiore :➘))
                                (to . (:tono :➚))
                                (tod . (:tono :➘))
                                (tmin . (:terza-minore :➚))
                                (tmind . (:terza-minore :➘))
                                (tmag . (:terza-maggiore :➚))
                                (tmagd . (:terza-maggiore :➘))
                                (qua . (:quarta :➚))
                                (quad . (:quarta :➘))
                                (qui . (:quinta :➚))
                                (quid . (:quinta :➘))
                                (smin . (:sesta-minore :➚))
                                (smind . (:sesta-minore :➘))
                                (smag . (:sesta-maggiore :➚))
                                (smagd . (:sesta-maggiore :➘))
                                (settmin . (:settima-minore :➚))
                                (settmind . (:settima-minore :➘))
                                (settmag . (:settima-maggiore :➚))
                                (settmagd . (:settima-maggiore :➘))
                                (o . (:ottava :➚))
                                (od . (:ottava :➘))))

(defun interpret-interval (interval-symbol)
  (if-exists (cdr (assoc interval-symbol *interval-dict*))
             (error "Interval symbol ~a not found." interval-symbol)))


(defparameter *duration-dict* '((l . (:longa))
                                (ld . (:longa :dot))
                                (b . (:brevis))
                                (bd . (:brevis :dot))
                                (sb . (:semibrevis))
                                (sbd . (:semibrevis :dot))
                                (m . (:minima))
                                (md . (:minima :dot))
                                (sm . (:semiminima))
                                (smd . (:semiminima :dot))
                                (f . (:fusa))
                                (fd . (:fusa :dot))))

(defun interpret-duration (duration-symbol)
  (if-exists (cdr (assoc duration-symbol *duration-dict*))
             (error "Duration symbol ~a not found." duration-symbol)))

(defparameter *attack-dict* '((wo . :word)
                              (si . :sillable)))

(defun interpret-attack (attack-symbol)
  (if-exists (cdr (assoc attack-symbol *attack-dict*))
             (error "Attack symbol ~a not found." attack-symbol)))


(defun voice-loop (rest-data new-voice-data)
  (if (null rest-data)
      (reverse new-voice-data)
      (case (first rest-data)
        (ta (voice-loop (rest (rest rest-data))
                        (cons (cons :t (interpret-duration (second rest-data)))
                              new-voice-data)))
        (s (cond ((member (third rest-data) '(wo si))
                  (voice-loop (rest (rest (rest rest-data)))
                              (cons (append '(:s)
                                            (interpret-duration (second rest-data))
                                            (list (interpret-attack (third rest-data))))
                                    new-voice-data)))
                 (t (voice-loop (rest (rest rest-data))
                                (cons (cons :s (interpret-duration (second rest-data)))
                                      new-voice-data)))))
        (i (voice-loop (rest (rest rest-data))
                       (cons (append '(:i) (interpret-interval (second rest-data)))
                             new-voice-data)))
        (otherwise (error "Expected 't, 's or 'i, but got ~a." (first rest-data))))))

(defun parse-shorthand (shorthand-data)
  (mapcar (lambda (voice) (cons (first voice) (voice-loop (rest voice) nil))) shorthand-data))


(defparameter *mirabile*
  (parse-shorthand
   '((:cantus
      ;; Mirabile
      ta ld i o s b wo s sbd si i tod s m si s b si i std s b
      ;; mysterium
      i tod s b wo i tmin s sbd si i std s sm i tod s sm i to s m i st s sb i std s m si i st s sb si
      ;; declaratur
      ta m s sb wo i to s m si i to s sb si i tod s b si
      ;; hodie,
      ta m i to s md wo i to s sm i st s sb i std s sm si i tod s sm i tod s m si
      ;; hodie,
      i qua s md wo i std s sm i tod s sb i tod s m si i to s sb si
      ;; declaratur
      ta b ta m s sb wo i to s m si i st s m si i tmind s m si
      ;; hodie,
      i to s m wo s m si i tod s m si
      ;; hodie
      i tmin s md wo i std s sm i tod s sb i tod s sm i tod s sm i std s md i tod s sm i to s m i st s m si i tmind s sb si
      ;; hodie
      i tod s md wo i to s sm i to s m i st s md i to s sm i to s md i to s sm i st s sb i std s sm si i tod s sm i tod s sb si
      ;; innovantur
      ta sb i tod s sb wo i tmag s sb si i tmin s b si s sb si i std s sb
      ;; naturae
      ta m i tod s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm s sb i std s m i st s b si



      ;; REPEAT
      ta sb ta ld ; filler



      ;; Mirabile
      ta ld s b wo s sbd si i tod s m si s b si i std s b
      ;; mysterium
      i tod s b wo i tmin s sbd si i std s sm i tod s sm i to s m i st s sb i std s m si i st s sb si
      ;; declaratur
      ta m s sb wo i to s m si i to s sb si i tod s b si
      ;; hodie,
      ta m i to s md wo i to s sm i st s sb i std s sm si i tod s sm i tod s m si
      ;; hodie,
      i qua s md wo i std s sm i tod s sb i tod s m si i to s sb si
      ;; declaratur
      ta b ta m s sb wo i to s m si i st s m si i tmind s m si
      ;; hodie,
      i to s m wo s m si i tod s m si
      ;; hodie
      i tmin s md wo i std s sm i tod s sb i tod s sm i tod s sm i std s md i tod s sm i to s m i st s m si i tmind s sb si
      ;; hodie
      i tod s md wo i to s sm i to s m i st s md i to s sm i to s md i to s sm i st s sb i std s sm si i tod s sm i tod s sb si
      ;; innovantur
      ta sb i tod s sb wo i tmag s sb si i tmin s b si s sb si i std s sb
      ;; naturae
      ta m i tod s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm s sb i std s m i st s b si

      ta l ; dummy to make the last note sound

      ;; ;; Deus
      ;; ta sb ta l ta m i tmind s sb wo s m si
      ;; ;; homo
      ;; i st s sb wo i tmind s m si
      ;; ;; factus
      ;; i to s md wo i tmagd s sm i qua s sb
      ;; ;; est,
      ;; i std s m si
      ;; ;; Deus
      ;; i tmag s sb wo i tmagd s m si
      ;; ;; homo
      ;; i to s sb wo i to s sb i tod s sb si
      ;; ;; factus
      ;; i tod s md wo i std s sm i st s m i tmag s m
      ;; ;; est
      ;; i tod s sb wo i tod s sb
      ;; ;; Deus
      ;; ta sb ta m tmag sb wo

      )
     (:altus
      ;; Mirabile
      ta sb i qui s b wo s sbd si i tod s m si s b si
      ;; mirabile
      i std s b wo i tmin s sbd si i tod s m si s b si i std s b
      ;; mysterium
      i tod s sb wo s sb si i tmagd s sb si i qui s b si
      ;; declaratur
      ta sb s sb wo i to s m si i to s sb si i tod s m si
      ;; hodie
      i to s m wo s m si i tod s md si i tod s sm i tmind s m
      ;; declaratur
      i tmin s m wo i to s m si i to s sb si i tod s m si
      ;; hodie
      i to s sb wo s m si i st s sb si i std s sm i tod s sm i tod s m
      ;; hodie
      i qua s md wo i std s sm i tod s sb i std s m si i st s md si i tod s sm i tmind s m
      ;; declaratur
      i qui s sb wo i st s m si i to s m si i quad s m si i tmin s m wo s m si i std s sb si
      ;; innovatur
      ta sb ta l ta l ta sb i tod s sb wo s sb si i tmin s sb si i std s sb si
      ;; naturae
      ta m s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm i tod s sm i std s sm i st s m i to s m i to s m i tod s sb i tod s m i to s sb si i to s sb
      ;; innovantur
      ta m s m wo i tod s m si i tod s sb si i std s m si i st s m wo i to s md si i tod s sm s sb i std s m i st s b si



      ;; REPEAT


      ;; Mirabile
      ta sb i to s b wo s sbd si i tod s m si s b si
      ;; mirabile
      i std s b wo i tmin s sbd si i tod s m si s b si i std s b
      ;; mysterium
      i tod s sb wo s sb si i tmagd s sb si i qui s b si
      ;; declaratur
      ta sb s sb wo i to s m si i to s sb si i tod s m si
      ;; hodie
      i to s m wo s m si i tod s md si i tod s sm i tmind s m
      ;; declaratur
      i tmin s m wo i to s m si i to s sb si i tod s m si
      ;; hodie
      i to s sb wo s m si i st s sb si i std s sm i tod s sm i tod s m
      ;; hodie
      i qua s md wo i std s sm i tod s sb i std s m si i st s md si i tod s sm i tmind s m
      ;; declaratur
      i qui s sb wo i st s m si i to s m si i quad s m si i tmin s m wo s m si i std s sb si
      ;; innovatur
      ta sb ta l ta l ta sb i tod s sb wo s sb si i tmin s sb si i std s sb si
      ;; naturae
      ta m s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm i tod s sm i std s sm i st s m i to s m i to s m i tod s sb i tod s m i to s sb si i to s sb
      ;; innovantur
      ta m s m wo i tod s m si i tod s sb si i std s m si i st s m wo i to s md si i tod s sm s sb i std s m i st s b si



      ta sb ; end piece to make the last note sound



      ;; Deus

      )
     (:tenor
      ;; Mirabile
      s b wo i tmin s sbd si i std s m si s b si
      ;; mirabile
      ta sb i tod s b wo i tmin s sbd si i std s m si s b si i tod s b si
      ;; mysterium
      i tod s b wo i tmag s sbd si i tod s sm i tod s sm i to s m s m si i tod s sb si
      ;; mysterium
      ta m i tmag s sb wo i tmin s sb si i std s sm i tod s sm i to s m i st s sb i std s m si i st s m si
      ;; declaratur
      i quid s sb wo i to s m si i to s md si i to s sm i st s m i tmind s m si
      ;; hodie
      i tmin s m wo i to s m i tmind s sb si i tod s sb si
      ;; declaratur
      ta b ta m i tmagd s sb wo i to s m si i to s sb si i tod s sb si
      ;; hodie
      ta m i to s md wo i to s sm i st s sb i std s sm si i tod s sm i tod s m si
      ;; hodie
      i qua s md wo i std s sm i tod s sb i std s m si i st s b si
      ;; innovantur
      ta m i tmagd s m wo i tmag s sb si i tmin s sb si s sb si i std s sb
      ;; naturae
      ta m i tod s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm s md i std s sm s sm i tod s sm i tmin s sb si
      ;; innovantur
      ta bd s sb wo i to s sb si i tmin s sb si i std s sb si
      ;; naturae
      ta m i tod s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm s sb i tod s m i to

      ;; s sb si ; original
      s b si ; adapted to fill the last bar before repeat


      ;; REPEAT


      ;; Mirabile
      s b wo i tmin s sbd si i std s m si s b si
      ;; mirabile
      ta sb i tod s b wo i tmin s sbd si i std s m si s b si i tod s b si
      ;; mysterium
      i tod s b wo i tmag s sbd si i tod s sm i tod s sm i to s m s m si i tod s sb si
      ;; mysterium
      ta m i tmag s sb wo i tmin s sb si i std s sm i tod s sm i to s m i st s sb i std s m si i st s m si
      ;; declaratur
      i quid s sb wo i to s m si i to s md si i to s sm i st s m i tmind s m si
      ;; hodie
      i tmin s m wo i to s m i tmind s sb si i tod s sb si
      ;; declaratur
      ta b ta m i tmagd s sb wo i to s m si i to s sb si i tod s sb si
      ;; hodie
      ta m i to s md wo i to s sm i st s sb i std s sm si i tod s sm i tod s m si
      ;; hodie
      i qua s md wo i std s sm i tod s sb i std s m si i st s b si
      ;; innovantur
      ta m i tmagd s m wo i tmag s sb si i tmin s sb si s sb si i std s sb
      ;; naturae
      ta m i tod s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm s md i std s sm s sm i tod s sm i tmin s sb si
      ;; innovantur
      ta bd s sb wo i to s sb si i tmin s sb si i std s sb si
      ;; naturae
      ta m i tod s sb wo i tod s sm i tod s sm i to s m i to s md si i tod s sm s sb i tod s m i to

      ;; s sb si ; original
      s b si ; adapted to fill the last bar before repeat

      ta sb ; end piece to make the last note sound



      ;; Deus
      ;; ta m
      )
     (:bassus
      ;; mirabile
      s b wo s sbd si i tod s m si s b si i std s b
      ;; mysterium
      ta ld ta sb i tod s sb wo s sbd si s m si i tod s b si
      ;; declaratur
      ta m i smag s md wo i tod s sm i tod s sm i std s sm i tod s m i tod s m si i qua s sb si i std s md si i st s sm i to s sb
      ;; hodie
      i tod s md wo i std s sm i tod s m i tod s m si i qua s sb si
      ;; declaratur
      ta sb ta m i tmind s sb wo i to s m si i st s m si i tmind s m si
      ;; hodie
      i tmin s m wo i to s m si i tmind s sb si i tod s sb
      ;; declaratur
      ta l i quad s sbd wo i to s m si i st s m si i tmind s m si
      ;; hodie
      i tmin s m wo i to s m si i tmind s sb si i tod s sb
      ;; innovantur
      ta m i qua s m wo s sb si i tmin s sb si s sb si i std s md i st s sm i to s m
      ;; innovantur
      i od s m wo i qui s sb si s sb si i tod s m si
      ;; naturae
      i tmind s m wo i tmin s md si i std s sm i tod s sb i tod s m si
      ;; naturae
      i o s m wo i to s md si i tod s sm i tod s m i std s sm i tod s sm i tod s m si
      ;; naturae
      i qua s m wo i std s sb si i tod s sb si
      ;; innovantur
      ta m i tod s m wo i tmag s sb si i st s sb si i std s md si i tod s sm i to s m
      ;; naturae
      i st s m wo i quad s m si i tmag s m i tod s sb i tod s b si

      ;; REPEAT



      ;; mirabile
      i qui s b wo s sbd si i tod s m si s b si i std s b
      ;; mysterium
      ta ld ta sb i tod s sb wo s sbd si s m si i tod s b si
      ;; declaratur
      ta m i smag s md wo i tod s sm i tod s sm i std s sm i tod s m i tod s m si i qua s sb si i std s md si i st s sm i to s sb
      ;; hodie
      i tod s md wo i std s sm i tod s m i tod s m si i qua s sb si
      ;; declaratur
      ta sb ta m i tmind s sb wo i to s m si i st s m si i tmind s m si
      ;; hodie
      i tmin s m wo i to s m si i tmind s sb si i tod s sb
      ;; declaratur
      ta l i quad s sbd wo i to s m si i st s m si i tmind s m si
      ;; hodie
      i tmin s m wo i to s m si i tmind s sb si i tod s sb
      ;; innovantur
      ta m i qua s m wo s sb si i tmin s sb si s sb si i std s md i st s sm i to s m
      ;; innovantur
      i od s m wo i qui s sb si s sb si i tod s m si
      ;; naturae
      i tmind s m wo i tmin s md si i std s sm i tod s sb i tod s m si
      ;; naturae
      i o s m wo i to s md si i tod s sm i tod s m i std s sm i tod s sm i tod s m si
      ;; naturae
      i qua s m wo i std s sb si i tod s sb si
      ;; innovantur
      ta m i tod s m wo i tmag s sb si i st s sb si i std s md si i tod s sm i to s m
      ;; naturae
      i st s m wo i quad s m si i tmag s m i tod s sb i tod s b si


      ta sb ; end piece to make the last note sound



      ;; Deus
      ;; ta sbd
      ))))

(defparameter *soave* '((:soprano
                         ;; Soav'e dolc' ardore,
                         (:s :brevis :word)
                         (:i :semitono-minore :➘)
                         (:s :semibrevis :dot :sillable)
                         (:s :minima :sillable)
                         (:i :diesis-minore :➘)
                         (:s :semibrevis :word)
                         (:s :semibrevis :word)
                         (:i :tono-minore)
                         (:s :brevis :sillable)
                         (:i :tono-minore)
                         (:s :semibrevis :sillable)
                         ;; %%
                         (:t :semibrevis)
                         (:i :terza-minima :➘)
                         (:s :semibrevis :word)
                         (:s :minima :sillable)
                         (:s :minima :sillable)
                         (:i :semitono-minore)
                         (:s :minima :word)
                         (:s :minima :word)
                         (:i :terza-minore :➘)
                         (:s :semibrevis :sillable)
                         (:i :quarta-propinqua)
                         (:s :semibrevis :sillable)
                         ;; che fra piant' e sospiri
                         (:t :semibrevis)
                         (:i :terza-minore :➘)
                         (:s :semibrevis :word)
                         (:i :tono-minore)
                         (:s :semibrevis :word)
                         (:i :diesis-minore :➘)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :word)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :sillable)
                         (:i :quarta :➘)
                         (:s :semibrevis :sillable)
                         ;; %%
                         (:t :minima)
                         (:i :diesis-minore)
                         (:s :semibrevis :word)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :minima :word)
                         (:i :diesis-minore)
                         (:s :minima :word)
                         (:i :quarta)
                         (:s :semibrevis :sillable)
                         (:i :semitono-maggiore :➘)
                         (:s :semibrevis)
                         (:i :semitono-minore)
                         ;; (:s :semibrevis :sillable) ; original
                         (:s :semibrevis :sillable) ; adapted for clean ending
                         (:t :brevis))
                        (:alto
                         (:t :brevis)
                         (:i :quarta-minima :➘)
                         (:s :brevis :word)
                         (:i :diesis-minore)
                         (:s :semibrevis :dot :sillable)
                         (:s :minima :sillable)
                         (:i :tono-minore)
                         (:s :semibrevis :word)
                         (:s :semibrevis :word)
                         (:i :terza-più-di-maggiore :➘)
                         (:s :minima :dot :sillable)
                         (:i :semitono-maggiore :➘)
                         (:s :semiminima)
                         (:i :tono :➘)
                         (:s :semibrevis)
                         (:i :diesis-minore)
                         (:s :brevis :sillable)
                         ;; %%
                         (:t :semibrevis)
                         (:i :settima-minima)
                         (:s :semibrevis :word)
                         (:i :semitono-minore :➘)
                         (:s :minima :dot :sillable)
                         (:s :semiminima :sillable)
                         (:i :diesis-minore :➘)
                         (:s :minima :word)
                         (:s :minima :word)
                         (:i :quinta-imperfetta-propinqua :➘)
                         (:s :semibrevis :sillable)
                         (:i :diesis-minore :➘)
                         (:s :semibrevis :sillable)
                         ;; che fra piant' e sospiri
                         (:t :minima)
                         (:i :quarta-propinqua)
                         (:s :semibrevis :word)
                         (:s :minima :word)
                         (:i :terza-maggiore-propinqua :➘)
                         (:s :semibrevis :word)
                         (:s :minima :word)
                         (:i :quarta :➘)
                         (:s :minima :word)
                         (:i :semitono-maggiore)
                         (:s :semibrevis :sillable)
                         (:i :semitono-maggiore)
                         (:s :semibrevis)
                         (:i :diesis-minore :➘)
                         (:s :semibrevis :sillable)
                         ;; che fra piant' e
                         (:t :minima)
                         (:i :terza-minima :➘)
                         (:s :semibrevis :word)
                         (:s :minima :word)
                         (:i :diesis-minore :➘)
                         (:s :minima :word)
                         (:i :ottava)
                         (:s :semibrevis :word)
                         (:t :brevis))
                        (:tenore
                         (:t :longa)
                         (:i :sesta-minore :➘)
                         (:s :brevis :word)
                         (:i :diesis-minore)
                         (:s :semibrevis :dot :sillable)
                         (:i :semitono-maggiore)
                         (:s :minima :sillable)
                         (:i :terza-più-di-minore :➘)
                         (:s :minima :dot :word)
                         (:i :tono)
                         (:s :semiminima)
                         (:i :semitono-maggiore)
                         (:s :minima)
                         (:i :terza-minore :➘)
                         (:s :minima :word)
                         (:i :quarta-propinqua)
                         (:s :brevis :sillable)
                         (:i :terza-più-di-maggiore :➘)
                         (:s :semibrevis :sillable)
                         ;; %%
                         (:t :semibrevis)
                         (:i :terza-più-di-maggiore)
                         (:s :semibrevis :word)
                         (:i :diesis-minore)
                         (:s :minima :dot :sillable)
                         (:s :semiminima :sillable)
                         (:i :diesis-minore)
                         (:s :minima :word)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :sillable)
                         (:i :quarta-minima :➘)
                         (:s :brevis :sillable)
                         ;; che fra piant' e sospiri
                         (:t :minima)
                         (:i :tono-maggiore :➘)
                         (:s :semibrevis :word)
                         (:i :terza-minore)
                         (:s :minima :word)
                         (:i :semitono-minore :➘)
                         (:s :semibrevis :word)
                         (:i :tono)
                         (:s :minima :word)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :sillable)
                         (:i :semitono-maggiore)
                         (:s :minima :sillable)
                         ;; che fra piant' e
                         (:i :semitono-minore :➘)
                         (:s :minima :sillable)
                         (:i :semitono-maggiore :➘)
                         (:s :semibrevis :word)
                         (:i :semitono-minore)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :word)
                         (:t :brevis))
                        (:basso
                         (:t :longa :dot)
                         (:i :ottava :➘)
                         (:i :terza-minima :➘)
                         (:s :brevis :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :dot :sillable)
                         (:s :minima :sillable)
                         (:i :terza-minima :➘)
                         (:s :semibrevis :dot :word)
                         (:s :minima :word)
                         (:i :semitono-minore)
                         (:s :semibrevis :sillable)
                         (:i :terza-minore :➘)
                         (:s :semibrevis :sillable)
                         ;; %%
                         (:t :semibrevis)
                         (:i :semitono-maggiore :➘)
                         (:s :semibrevis :word)
                         (:i :diesis-minore)
                         (:s :minima :dot :sillable)
                         (:s :semiminima :sillable)
                         (:i :quarta-minima)
                         (:s :minima :word)
                         (:s :minima :word)
                         (:i :diesis-minore)
                         (:s :brevis :sillable)
                         (:i :semitono-minore)
                         (:s :semibrevis :sillable)
                         ;; che fra pianti e sospiri
                         (:t :minima)
                         (:i :quarta :➘)
                         (:s :minima :word)
                         (:i :terza-più-di-maggiore)
                         (:s :minima :word)
                         (:s :minima :word)
                         (:i :quinta :➘)
                         (:s :minima :sillable)
                         (:s :minima :word)
                         (:i :quarta-minima)
                         (:s :semibrevis :sillable)
                         (:i :terza-minima :➘)
                         (:s :semibrevis :sillable)
                         ;; pian-
                         (:t :semibrevis)
                         (:i :diesis-minore :➘)
                         (:s :semibrevis :dot :word)
                         (:t :brevis))))

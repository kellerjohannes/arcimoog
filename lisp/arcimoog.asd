(asdf:defsystem "arcimoog"
  :depends-on (:alexandria :log4cl)
  :serial t
  :components ((:file "package")
               (:file "pitch-data")))

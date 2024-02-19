(asdf:defsystem "arcimoog"
  :depends-on (:alexandria :log4cl :bordeaux-threads :cl-opengl :cl-glut :cl-glu)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "pitch-data")
               (:file "opengl")))

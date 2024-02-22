(asdf:defsystem "arcimoog"
  :depends-on (:alexandria :log4cl :bordeaux-threads :cl-opengl :cl-glfw3 :cl-freetype2 :incudine)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "pitch-data")
               (:file "opengl")))

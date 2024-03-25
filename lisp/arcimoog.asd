(asdf:defsystem "arcimoog"
  :depends-on (:uiop :alexandria :log4cl :bordeaux-threads
               :cl-opengl :cl-glfw3 :cl-freetype2 :incudine)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "pitch-data")
               (:file "shader-class")
               (:file "opengl")))

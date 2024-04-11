(asdf:defsystem "arcimoog"
  :depends-on (:uiop :alexandria :log4cl :bordeaux-threads :array-operations
                     :cl-opengl :cl-glfw3 :cl-freetype2 :lla :cl-jpeg
                     :incudine)
  :serial t
  :components ((:file "packages")
               (:file "global-definitions")
               (:file "utilities")
               (:file "myglm")
               (:file "midi")
               (:file "pitch-data")
               (:file "shader-class")
               (:file "opengl")))

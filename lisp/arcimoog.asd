;; * TODO Move OpenGL abstractions into separate packages
;; * TODO Implement condition handling everywhere
;; * TODO Test monospace fonts
;; * TODO Solve text alignment (https://tonsky.me/blog/centering/)
;; * TODO Start to add doc strings


(asdf:defsystem "arcimoog"
  :depends-on (:uiop :alexandria :log4cl :bordeaux-threads :array-operations
                     :cl-opengl :cl-glfw3 :cl-freetype2 :lla :cl-jpeg
               :incudine)
  :serial t
  :components ((:file "packages")
               (:file "global-definitions")
               (:file "faderfox")
               (:file "utilities")
               (:file "myglm")
               (:file "midi")
               (:file "pitch-data")
               (:file "shader-class")
               (:file "opengl")
               (:file "configuration")))

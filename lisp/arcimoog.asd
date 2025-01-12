(asdf:defsystem "arcimoog"
  :depends-on (:uiop :alexandria :log4cl :bordeaux-threads
               :array-operations :cl-jpeg :clog
               :incudine)
  :serial t
  :components ((:file "packages")
               (:file "conditions")
               (:file "global-definitions")
               ;;(:file "symbolic-intervals")
               ;;(:file "score-model")
               ;;(:file "faderfox")
               ;;(:file "utilities")
               (:file "midi")
               ;;(:file "pitch-data")
               ;;(:file "sampler")
               (:file "parameters")
               ;;(:file "configuration")
               ;; TODO Rename this into something with OSC, create a package
               ;; for osc-communication and properly implement it.
               ;; (:file "vc-client-test")
               (:file "ui")
               (:file "main")
               ))

(asdf:defsystem "arcimoog"
  :depends-on (:uiop :alexandria :log4cl :bordeaux-threads
               :array-operations ;;:cl-jpeg
               :clog
               :incudine)
  :serial t
  :components ((:file "clog-webgl-patch")
               (:file "packages")
               (:file "utilities")
               (:file "conditions")
               (:file "global-definitions")
               (:file "symbolic-intervals")
               ;;(:file "score-model")
               ;;(:file "faderfox")
               ;;(:file "utilities")
               (:file "midi")
               (:file "pitch-data")
               ;;(:file "sampler")
               (:file "parameters")
               ;;(:file "configuration")
               (:file "osc-communication")
               (:file "history-tracker")
               (:file "mother-abstraction")
               (:file "raw-pitch")
               (:file "ui")
               (:file "setup")
               (:file "main")
               ))

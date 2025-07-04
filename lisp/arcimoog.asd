(asdf:defsystem "arcimoog"
  :depends-on (:uiop :alexandria :log4cl :bordeaux-threads
               :array-operations ;;:cl-jpeg
               :clog
               :incudine)
  :serial t
  :components ((:file "clog-webgl-patch")
               (:file "packages")
               (:file "global-definitions")
               (:file "utilities")
               (:file "conditions")
               (:file "interval-trees")
               (:file "symbolic-scores")
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
               ;; From here: all performance sets, all based on main.lisp.
               (:file "performance-01-apollo")
               ;; Add more performance sets here, in their separate packages.
               ))

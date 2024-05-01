(defpackage :arcimoog.parameters
  (:use :cl)
  (:export "defp"
           "setp"
           "getp"
           "write-parameters"
           "read-parameters"))

(defpackage :arcimoog
  (:use :cl)
  (:nicknames "arcie")
  (:import-from "arcimoog.parameters" "defp" "setp" "getp"))

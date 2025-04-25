(in-package :clog-webgl)

;; Thank you, https://github.com/josrr/minskyclogtron/blob/main/clog-webgl-patch.lisp

(defgeneric create-webgl (clog-canvas &key context attributes)
  (:documentation "Create a new CLOG-WebGL from a CLOG-Canvas. Context
can be webgl (version 1) or webgl2 (default). Attributes must be a
plist like (\"attribute\" value ...). The values can be booleans or
strings."))

(defmethod create-webgl ((obj clog-canvas) &key (context "webgl2") attributes)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('~A'~@[,{~{~A: ~A~^, ~}}~])"
                            web-id (html-id obj) context
                            (loop for (key value) on attributes by #'cddr
                                  append (list key (if (typep value 'boolean)
                                                       (if value "true" "false")
                                                       (format nil "\"~A\"" value))))))
    (make-instance 'clog-webgl
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

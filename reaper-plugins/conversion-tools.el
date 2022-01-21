;;;; elisp tools to quickly convert data into Reaper plugins


(defun jk/convert-freq-list-to-reaper (num-items)
  "Converts a set of values into array definitions. Needs one number per line in a buffer. This will edit the buffer directly."
  (interactive "nNumber of lines to convert: ")
  (dotimes (i num-items)
    (let ((value (thing-at-point 'number)))
      (kill-whole-line)
      (insert (format "array[%d] = %s;\n" (+ 10 i) value)))))

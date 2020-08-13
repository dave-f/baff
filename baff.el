;;
;; baff
;; byte array from file

(require 'f)

(defun baff (arg)
  (interactive "FFile: ")
  (if (not (f-file-p arg))
      (error "File error"))
  (switch-to-buffer (get-buffer-create "*baff*"))
  (erase-buffer)
  (let ((bytes (string-to-list (f-read-bytes arg)))
        (count 0))
    (insert "std::array<uint8_t," (number-to-string (length bytes)) "> bytes = {\n")
    (cl-loop for i in bytes do
             (setq count (1+ count))
             (insert (format "0x%02x" i) (if (= count (length bytes)) "" ","))
             (when (= (% count 16) 0)
               (insert "\n")))
  (insert "\n};"))
  t)

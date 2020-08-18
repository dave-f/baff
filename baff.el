;;; baff.el --- Create a byte array from a file
;;; -*- lexical-binding: t; -*-

;;; Copyright (C) 2020 Dave Footitt
;;;
;;; Author: Dave Footitt <dave.footitt@gmail.com>
;;; URL: https://github.com/dave-f/baff/
;;; Package-Requires: ((emacs "24.3") (f "0.20.0"))
;;; Version: 1.0
;;; Keywords: convenience, usability

;;; This file is not part of GNU Emacs.

;;; License:
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;; Use the following command:
;;; M-x baff RET filename RET
;;; This will insert the bytes contained in filename as a byte array,
;;; into its own buffer, *baff* which can be pasted elsewhere.

;;; Code:

(require 'f)

(defgroup baff nil
  "Make a byte array from a file"
  :group 'programming)

(defcustom baff-header-function (lambda (filename contents)
                                   (insert "#include <array>\n\n"
                                           "// source : " filename "\n"
                                           "// sha256 : "
                                           (secure-hash 'sha256 contents)
                                           "\n\nstd::array<uint8_t,"
                                           (number-to-string (length contents))
                                           "> bytes = {\n"))
  "Function to run before any bytes are inserted."
  :type 'function
  :group 'baff)

(defcustom baff-footer-function (lambda (filename contents) (insert "\n};"))
  "Function to run after all bytes have been inserted."
  :type 'function
  :group 'baff)

(defcustom baff-indent-function (lambda () (insert "    "))
  "Function to indent each line."
  :type 'function
  :group 'baff)

(defcustom baff-bytes-per-line 16
  "Number of bytes per line before a line break."
  :type 'integer
  :group 'baff)

(defun baff (arg)
  "Read file `ARG` into a buffer containing a byte array of its contents."
  (interactive "FFile to insert: ")
  (if (not (f-file-p arg))
      (error "File open error"))
  (let* ((unibytes (f-read-bytes arg))
         (bytes (string-to-list unibytes))
         (count 0)
         (pr (make-progress-reporter "Working" 0 (length bytes))))
    (switch-to-buffer (get-buffer-create "*baff*"))
    (erase-buffer)
    (funcall baff-header-function arg unibytes)
    (funcall baff-indent-function)
    (cl-loop for i in bytes do
             (setq count (1+ count))
             (insert (format "0x%02x" i) (if (= count (length bytes)) "" ", "))
             (when (= (% count baff-bytes-per-line) 0)
               (progress-reporter-update pr count)
               (insert "\n")
               (funcall baff-indent-function)))
    (progress-reporter-done pr)
  (funcall baff-footer-function arg unibytes))
  t)

(provide 'baff)

;;; baff.el ends here

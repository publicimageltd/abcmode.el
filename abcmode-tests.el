;;; abcmode-tests.el --- tests for abcmode           -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022

;; Author:  <joerg@joergvolbers.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for abcmode.el. This is not a regular test suite, just
;; run `abc--testsuite' manually in the buffer.

;;; Code:
(require 'abcmode)
(require 'cl-lib)

(defun abc--test-for-x (regexp string match-expected)
  "Insert result of applying REGEXP to STRING in current buffer.
Colorize output green if MATCH is equal to MATCH-EXPECTED."
  (let* ((case-fold-search nil)
         (match (not (not (string-match regexp string))))
         (result (if match (capitalize string) string))
         (expected? (eql match match-expected))
         (pad       (make-string (- 20 (length string)) ? ))
         (s (concat string
                    pad
                    (propertize result
                                'face
                                (if expected? 'success 'error))
                    pad
                    (format "(%s, %s)"
                            (if match "match" "no match")
                            (propertize 
                             (if match-expected "match expected" "no match expected")
                             'face
                             (if expected? 'success 'error))))))
    (let (start end)
      (setq start (point))
      (insert s "\n")
      (setq end (point))
      (if (cl-oddp (line-number-at-pos start))
          (add-face-text-property start end 'hl-line)))))

(defun abc--test (s expected)
  "Test S against `abc--regexp', inserting the results.
S is the string, and EXPECTED if a match is expected."
  (abc--test-for-x abc--regexp s expected))

(defun abc--testsuite ()
  "Show a buffer with matches against `abc--regexp'."
  (interactive)
  (let ((tempbufname "**abc testsuite***"))
  (with-output-to-temp-buffer tempbufname
    (set-buffer tempbufname)
    (pcase-dolist (`(,s ,exp)
                    '(("Normal" nil)
                      ("BLatt"  t)
                      ("BLatts" t)
                      ("BLätter" t)
                      ("bLätter" t)
                      ("BLätters" t)
                      ("Cn" nil)
                      ("CC" nil)
                      ("CCn" t)
                      ("CDs" nil)
                      ("PDFs" nil)
                      ("xPDF" t)
                      ("xPDFs" t)
                      ("PDFn" t)
                      ("kleinUND" nil)
                      ("PDFmitBINNEN" t)
                      ("PDFmitBINNENs" t)
                      ("CamelCase" nil)
                      ("McDOwell" t)
                      ("McDowell" nil)
                      ("GROSS" nil)
                      ("klein" nil)
                      ("12xyaa" nil)))
      (abc--test s exp))
    (insert "\n "))))


(provide 'abcmode-tests)
;;; abcmode-tests.el ends here

;;; abcmode.el --- autocorrect mistyping capital letters at the beginning of the word  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: convenience

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

;; 

;;; Code:

(define-minor-mode abc-mode
  "Toggle ABc mode. Minor mode enabling automatic correction of
misspelled capital words, i.e. WIlliam -> William."
  ;; inital value:
  nil
  ;; indicator for mode line:
  " ABc"
  ;; bindings:
  nil
  ;; body:
  (if (not abc-mode)
      (abc-remove-change-hooks)
    (abc-setup-change-hooks)))

(defun abc-setup-change-hooks ()
  (interactive)
  (add-hook 'after-change-functions 'abc-after-change-function  nil  t))

(defun abc-remove-change-hooks ()
  (interactive)
  (remove-hook 'after-change-functions  'abc-after-change-function  t))

(defvar abc--regexp "\\b[[:upper:]][[:upper:]]\\([[:lower:]]+\\)\\b"
  "Regexp used to identify these words which ABC-modes is supposed to correct.")

(defun abc-after-change-function (start end length)
 (when  (zerop length) ;; insertion
   (save-match-data
     (let* ((point (point))
	    (case-fold-search nil) ;; do not ignore case
	    (match (re-search-backward abc--regexp (point-at-bol) t)))
       (when (and match (not (string= "s" (match-string 1)))) ;; Korrektur nur wenn kein abschließendes s
	 (goto-char match)
	 (capitalize-word 1))
       (goto-char point)))))
 
(provide 'abcmode)
;;; abcmode.el ends here

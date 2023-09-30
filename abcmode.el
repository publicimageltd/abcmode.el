;;; abcmode.el --- autocorrect mistyping capital letters at the beginning of the word  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))

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

;; A minor mode which automatically corrects misspelled capital words,
;; e.g. 'WOrd' -> 'Word'.

;;; Code:

(require 'rx)

(define-minor-mode abc-mode
  "Toggle ABc mode.
Minor mode enabling automatic correction of misspelled capital
words, i.e. WIlliam -> William."
  :lighter " ABc"
  (if (not abc-mode)
      (remove-hook 'after-change-functions  'abc-maybe-capitalize  t)
    (add-hook 'after-change-functions 'abc-maybe-capitalize  nil  t)))

(defvar abc--regexp
  (rx word-boundary
      (or
       ;; case 1: 2 or more times uppercase at the beginning
       (>= 2 upper)
       ;; case 2: some lower, then upper
       (and (one-or-more lower)
            (one-or-more upper)))
      ;; in any case: there has to be a lower ending
      (group (1+ lower))
      (not ?s) ;; but NO ending S
      word-boundary)
  "Regexp used to identify misspelled words.")

(defun abc-maybe-capitalize (start end length)
  "Maybe capitalize the word at point.
Use START, END and LENGTH as it is passed by the hook
`after-change-functions'."
  (ignore start end)
  (when  (zerop length) ;; insertion
    (save-match-data
      (let* ((point (point))
             (case-fold-search nil) ;; do not ignore case
             (match (re-search-backward abc--regexp (line-beginning-position) t)))
        (when match
          (goto-char match)
          (capitalize-word 1))
        (goto-char point)))))

;; # Correct the words in the regionn from START to END

(provide 'abcmode)
;;; abcmode.el ends here

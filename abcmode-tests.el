;;; abcmode-tests.el --- tests for abcmode           -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

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

;; Tests for abcmode.el.

;;; Code:

(defun abc--test-for-x (regexp string-to-test)
  (let ((case-fold-search nil))
  (insert (format "For string '%s': %s\t\n" 
		  string-to-test 
		  (if (string-match regexp string-to-test) 
		      (if (match-string 1 string-to-test)
			  (capitalize (match-string 0 string-to-test))
			(downcase (match-string 0 string-to-test)))
		    (concat string-to-test " (no match)"))))))

(defun abc--testsuite ()
  (interactive)
  (let ((tempbufname "**abc testsuite***"))
  (with-output-to-temp-buffer tempbufname
    (set-buffer tempbufname)
    (abc--test-for-x abc--regexp "Normal")
    (abc--test-for-x abc--regexp "BLatt")
    (abc--test-for-x abc--regexp "BLatts")
    (abc--test-for-x abc--regexp "BLätter")    
    (abc--test-for-x abc--regexp "BLätters")    
    (abc--test-for-x abc--regexp "CDs")
    (abc--test-for-x abc--regexp "PDFs")
    (abc--test-for-x abc--regexp "kAnt")
    (abc--test-for-x abc--regexp "McDowell")
    (abc--test-for-x abc--regexp "MCDowell")    
    (abc--test-for-x abc--regexp "GROSS")
    (abc--test-for-x abc--regexp "nichts")
    (abc--test-for-x abc--regexp "12XYaa"))))



(provide 'abcmode-tests)
;;; abcmode-tests.el ends here

;; ABc minor mode:
;;

(define-minor-mode ABc-mode
  "Toggle ABc mode. Minor mode enabling automatic correction of
misspelled capital words, i.e. WIlliam -> William."
  ;; inital value:
  nil
  ;; indicator for mode line:
  " ABc"
  ;; bindings:
  nil
  ;; body:
  (if (not ABc-mode)
      (ABc-remove-change-hooks)
    (ABc-setup-change-hooks)))

(defun ABc-setup-change-hooks ()
  (interactive)
  (add-hook 'after-change-functions 'ABc-after-change-function  nil  t))

(defun ABc-remove-change-hooks ()
  (interactive)
  (remove-hook 'after-change-functions  'ABc-after-change-function  t))

(defvar abc--regexp "\\b[[:upper:]][[:upper:]]\\([[:lower:]]+\\)\\b"
  "Regexp used to identify these words which ABC-modes is supposed to correct.")

(defun ABc-after-change-function (start end length)
 (when  (zerop length) ;; insertion
   (save-match-data
     (let* ((point (point))
	    (case-fold-search nil) ;; do not ignore case
	    (match (re-search-backward abc--regexp (point-at-bol) t)))
       (when (and match (not (string= "s" (match-string 1)))) ;; Korrektur nur wenn kein abschließendes s
	 (goto-char match)
	 (capitalize-word 1))
       (goto-char point)))))

;; --------------------------------------------------------------------------------
;; Ab hier Tests.

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
    

(provide 'abcmode)

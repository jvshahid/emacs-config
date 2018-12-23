;;; -*- lexical-binding: t; -*-

(defun jump-to-file-and-line ()
  (interactive)
  (let ((thing (thing-at-point 'filename)))
    (save-match-data
      (if (not (string-match "^\\([^:]*\\)\\(:\\([0-9]+\\)\\)?$" thing))
          (message "%s doesn't look like a filename" thing)
        (let ((name (match-string 1 thing))
              (line (match-string 3 thing)))
          (if (not (file-exists-p name))
              (message "Cannot find file %s" name)
            (find-file-other-window name)
            (goto-char (point-min))
            (if line (forward-line (1- (string-to-number line))))))))))

(global-set-key (kbd "C-x 4 j") #'jump-to-file-and-line)

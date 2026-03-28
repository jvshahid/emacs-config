;;; -*- lexical-binding: t; -*-

;;; time utilities

(defun epoch-to-string (start end)
  (interactive "r")
  (let ((seconds (string-to-number (buffer-substring start end))))
    (delete-region start end)
    (insert (format-time-string "%d %b %Y %H:%M:%S %Z"
                                (seconds-to-time seconds)))))

(defun string-to-epoch (str)
  (unix-time (apply 'encode-time (parse-time-string str))))

(defun unix-time (&optional optional-time)
  (let ((time (if optional-time optional-time (current-time))))
    (+ (ash (car time) 16)
       (cadr time))))

;;; buffer/file utilities

(defun show-all-buffers ()
  (interactive)
  (display-buffer (list-buffers-noselect nil (buffer-list))))

(defun omg-this-is-a-dos-file ()
  (interactive)
  (revert-buffer-with-coding-system 'us-ascii-dos))

(defun toggle-indent-longer-lines (&optional delta)
  "Trigger selective display to hide lines that have more indentation than the current line.
If DELTA was provided it will be added to the current line's
indentation."
  (interactive "P")
  (let ((indentation (current-indentation)))
    (if selective-display
        (set-selective-display nil)
      (set-selective-display (+ indentation 1
                                (if delta delta 0))))))

(global-set-key (kbd "C-x t") #'toggle-indent-longer-lines)

;;; search

(defun find-grep-current-word (ignore-case)
  "Interactive way to grep for the word under the cursor using rg."
  (interactive "P")
  (let* ((extra-arg (if ignore-case "-i " ""))
         (grep-cmd "rg --color=never --no-heading ")
         (prompt (if ignore-case "search for (ignore case): " "search for: "))
         (word (read-string prompt (current-word)))
         (directory (read-directory-name "in: " default-directory)))
    (let ((default-directory directory))
      (grep-find (concat grep-cmd extra-arg (shell-quote-argument word))))))

(global-set-key (kbd "C-c C-g") #'find-grep-current-word)

;;; help extensions

;; Add the version of Emacs when a symbol was added
;; Stefan Monnier: http://lists.gnu.org/archive/html/emacs-devel/2018-09/msg00959.html
(defun help-fns--first-release (function)
  "Try and find the first release that defined this function."
  ;; Code below relies on the etc/NEWS* files.
  (when (symbolp function)
    (let* ((name (symbol-name function))
           (re (concat "\\_<" (regexp-quote name) "\\_>"))
           (news (directory-files data-directory t "\\`NEWS.[1-9]"))
           (first nil))
      (with-temp-buffer
        (dolist (f news)
          (erase-buffer)
          (insert-file-contents f)
          (goto-char (point-min))
          (search-forward "\n*")
          (while (re-search-forward re nil t)
            (save-excursion
              (if (re-search-backward "^\\*.*in Emacs \\([0-9.]+\\)"
                                      nil t)
                (let ((version (match-string 1)))
                  (when (or (null first) (version< version first))
                    (setq first version))))))))
      (when first
        (insert (format "\nIntroduced at or before Emacs version %s.\n"
                        first))))))

(add-hook 'help-fns-describe-function-functions #'help-fns--first-release)

;;; concourse pipeline imenu support

(defun concourse-imenu-index-function ()
  (let (title
        elems
        index)
    (beginning-of-buffer)
    (save-match-data
      (while (search-forward-regexp "^\\([^: \n]*\\):$\\|^- name: \\(.*\\)$" nil t)
        (if (match-string 2)
            (push (cons (match-string-no-properties 2)
                        (line-beginning-position))
                  elems)
          (when title
            (push (cons title (reverse elems)) index))
          (setq elems nil)
          (setq title (match-string-no-properties 1))))
      (push (cons title (seq-reverse elems)) index)
      index)))

(defun setup-concourse-index-function ()
  (interactive)
  (add-dir-local-variable 'yaml-mode 'imenu-create-index-function #'concourse-imenu-index-function))

(push '(imenu-create-index-function . concourse-imenu-index-function) safe-local-variable-values)

(defun term-move-to-column (column)
  (setq term-current-column column)
  (let ((point-at-eol (line-end-position)))
    (move-to-column term-current-column)))
    ;; If move-to-column extends the current line it will use the face
    ;; from the last character on the line, set the face for the chars
    ;; to default.
    ;; (when (> (point) point-at-eol)
    ;;   (put-text-property point-at-eol (point) 'font-lock-face 'default))

(defun term-emulate-terminal (proc str)
  (with-current-buffer (process-buffer proc)
    (let* ((i 0) funny
           decoded-substring
           save-point save-marker win
           (inhibit-read-only t)
           (buffer-undo-list t)
           (selected (selected-window))
           last-win
           (str-length (length str)))
      (save-selected-window

        (when (marker-buffer term-pending-delete-marker)
          ;; Delete text following term-pending-delete-marker.
          (delete-region term-pending-delete-marker (process-mark proc))
          (set-marker term-pending-delete-marker nil))

        (when (/= (point) (process-mark proc))
          (setq save-point (point-marker)))

        (setf term-vertical-motion
              (if (eq (window-buffer) (current-buffer))
                  'vertical-motion
                'term-buffer-vertical-motion))
        (setq save-marker (copy-marker (process-mark proc)))
        (goto-char (process-mark proc))

        (save-restriction
          ;; If the buffer is in line mode, and there is a partial
          ;; input line, save the line (by narrowing to leave it
          ;; outside the restriction ) until we're done with output.
          (when (and (> (point-max) (process-mark proc))
                     (term-in-line-mode))
            (narrow-to-region (point-min) (process-mark proc)))

          (when term-log-buffer
            (princ str term-log-buffer))
          (when term-terminal-undecoded-bytes
            (setq str (concat term-terminal-undecoded-bytes str))
            (setq str-length (length str))
            (setq term-terminal-undecoded-bytes nil))

          (while (< i str-length)
            (setq funny (string-match term-control-seq-regexp str i))
            (let ((ctl-params (and funny (match-string 1 str)))
                  (ctl-params-end (and funny (match-end 1)))
                  (ctl-end (if funny (match-end 0)
                             (setq funny (string-match term-control-seq-prefix-regexp str i))
                             (if funny
                                 (setq term-terminal-undecoded-bytes
                                       (substring str funny))
                               (setq funny str-length))
                             ;; The control sequence ends somewhere
                             ;; past the end of this string.
                             (1+ str-length))))
              (when (> funny i)
                (when term-do-line-wrapping
                  (term-down 1 t)
                  (term-move-to-column 0)
                  (setq term-do-line-wrapping nil))
                ;; Handle non-control data.  Decode the string before
                ;; counting characters, to avoid garbling of certain
                ;; multibyte characters (bug#1006).
                (setq decoded-substring
                      (decode-coding-string
                       (substring str i funny)
                       locale-coding-system t))
                ;; Check for multibyte characters that ends
                ;; before end of string, and save it for
                ;; next time.
                (when (= funny str-length)
                  (let ((partial 0)
                        (count (length decoded-substring)))
                    (while (eq (char-charset (aref decoded-substring
                                                   (- count 1 partial)))
                               'eight-bit)
                      (cl-incf partial))
                    (when (> partial 0)
                      (setq term-terminal-undecoded-bytes
                            (substring decoded-substring (- partial)))
                      (setq decoded-substring
                            (substring decoded-substring 0 (- partial)))
                      (cl-decf str-length partial)
                      (cl-decf funny partial))))

                ;; Insert a string, check how many columns
                ;; we moved, then delete that many columns
                ;; following point if not eob nor insert-mode.
                (let ((old-column (term-horizontal-column))
                      (old-point (point))
                      columns)
                  (unless term-suppress-hard-newline
                    (while (> (+ (length decoded-substring) old-column)
                              term-width)
                      (insert (substring decoded-substring 0
                                         (- term-width old-column)))
                      ;; Since we've enough text to fill the whole line,
                      ;; delete previous text regardless of
                      ;; `term-insert-mode's value.
                      (delete-region (point) (line-end-position))
                      (term-down 1 t)
                      (term-move-columns (- (term-current-column)))
                      (setq decoded-substring
                            (substring decoded-substring (- term-width old-column)))
                      (setq old-column 0)))
                  (insert decoded-substring)
                  (setq term-current-column (current-column)
                        columns (- term-current-column old-column))
                  ;; TODO: what is this trying to do
                  (when (not (or (eobp) term-insert-mode))
                    (let ((pos (point)))
                      (term-move-columns columns)
                      (delete-region pos (point))
                      (term-move-columns (- columns))))
                  ;; In insert mode if the current line
                  ;; has become too long it needs to be
                  ;; chopped off.
                  (when term-insert-mode
                    (let ((pos (point)))
                      (end-of-line)
                      (when (> (current-column) term-width)
                        (delete-region (- (point) (- (current-column) term-width))
                                       (point)))
                      (goto-char pos)))

                  (put-text-property old-point (point)
                                     'font-lock-face term-current-face))
                ;; If the last char was written in last column,
                ;; back up one column, but remember we did so.
                ;; Thus we emulate xterm/vt100-style line-wrapping.
                ;; TODO: (when (eq (current-column) term-width))))))))))
                (when (eq (term-current-column) term-width)
                  (term-move-columns -1)
                  ;; We check after ctrl sequence handling if point
                  ;; was moved (and leave line-wrapping state if so).
                  (setq term-do-line-wrapping (point)))
                (setq term-current-column nil)
                (setq i funny))
              (pcase-exhaustive (and (<= ctl-end str-length) (aref str i))
                (?\t ;; TAB (terminfo: ht)
                 ;; The line cannot exceed term-width. TAB at
                 ;; the end of a line should not cause wrapping.
                 (let ((col (term-current-column)))
                   (term-move-to-column
                    (min (1- term-width)
                         (+ col 8 (- (mod col 8)))))))
                (?\r ;; (terminfo: cr)
                 (term-vertical-motion 0)
                 (setq term-current-column term-start-line-column))
                (?\n ;; (terminfo: cud1, ind)
                 (unless (and term-kill-echo-list
                              (term-check-kill-echo-list))
                   (term-down 1 t)))
                (?\b ;; (terminfo: cub1)
                 (term-move-columns -1))
                (?\C-g                  ;; (terminfo: bel)
                 (beep t))
                (?\032 ; Emacs specific control sequence.
                 (funcall term-command-hook
                          (decode-coding-string
                           (substring str (1+ i)
                                      (- ctl-end
                                         (if (eq (aref str (- ctl-end 2)) ?\r)
                                             2 1)))
                           locale-coding-system t)))
                (?\e
                 (pcase (aref str (1+ i))
                   (?\[
                    ;; We only handle control sequences with a single
                    ;; "Final" byte (see [ECMA-48] section 5.4).
                    (when (eq ctl-params-end (1- ctl-end))
                      (term-handle-ansi-escape
                       proc
                       (mapcar ;; We don't distinguish empty params
                        ;; from 0 (according to [ECMA-48] we
                        ;; should, but all commands we support
                        ;; default to 0 values anyway).
                        #'string-to-number
                        (split-string ctl-params ";"))
                       (aref str (1- ctl-end)))))
                   (?D ;; Scroll forward (apparently not documented in
                    ;; [ECMA-48], [ctlseqs] mentions it as C1
                    ;; character "Index" though).
                    (term-handle-deferred-scroll)
                    (term-down 1 t))
                   (?M ;; Scroll reversed (terminfo: ri, ECMA-48
                    ;; "Reverse Linefeed").
                    (if (or (< (term-current-row) term-scroll-start)
                            (>= (1- (term-current-row))
                                term-scroll-start))
                        ;; Scrolling up will not move outside
                        ;; the scroll region.
                        (term-down -1)
                      ;; Scrolling the scroll region is needed.
                      (term-down -1 t)))
                   (?7 ;; Save cursor (terminfo: sc, not in [ECMA-48],
                    ;; [ctlseqs] has it as "DECSC").
                    (term-handle-deferred-scroll)
                    (setq term-saved-cursor
                          (list (term-current-row)
                                (term-horizontal-column)
                                term-ansi-current-bg-color
                                term-ansi-current-bold
                                term-ansi-current-color
                                term-ansi-current-invisible
                                term-ansi-current-reverse
                                term-ansi-current-underline
                                term-current-face)))
                   (?8 ;; Restore cursor (terminfo: rc, [ctlseqs]
                    ;; "DECRC").
                    (when term-saved-cursor
                      (term-goto (nth 0 term-saved-cursor)
                                 (nth 1 term-saved-cursor))
                      (setq term-ansi-current-bg-color
                            (nth 2 term-saved-cursor)
                            term-ansi-current-bold
                            (nth 3 term-saved-cursor)
                            term-ansi-current-color
                            (nth 4 term-saved-cursor)
                            term-ansi-current-invisible
                            (nth 5 term-saved-cursor)
                            term-ansi-current-reverse
                            (nth 6 term-saved-cursor)
                            term-ansi-current-underline
                            (nth 7 term-saved-cursor)
                            term-current-face
                            (nth 8 term-saved-cursor))))
                   (?c ;; \Ec - Reset (terminfo: rs1, [ctlseqs] "RIS").
                    ;; This is used by the "clear" program.
                    (term-reset-terminal))
                   (?A ;; An \eAnSiT sequence (Emacs specific).
                    (term-handle-ansi-terminal-messages
                     (substring str i ctl-end)))))
                ;; Ignore NUL, Shift Out, Shift In.
                ((or ?\0 #xE #xF 'nil) nil))
              ;; Leave line-wrapping state if point was moved.
              (unless (eq term-do-line-wrapping (point))
                (setq term-do-line-wrapping nil))
              (if (term-handling-pager)
                  (progn
                    ;; Finish stuff to get ready to handle PAGER.
                    (if (> (% (current-column) term-width) 0)
                        (setq term-terminal-undecoded-bytes
                              (substring str i))
                      ;; We're at column 0.  Goto end of buffer; to compensate,
                      ;; prepend a ?\r for later.  This looks more consistent.
                      (if (zerop i)
                          (setq term-terminal-undecoded-bytes
                                (concat "\r" (substring str i)))
                        (setq term-terminal-undecoded-bytes (substring str (1- i)))
                        (aset term-terminal-undecoded-bytes 0 ?\r))
                      (goto-char (point-max)))
                    (make-local-variable 'term-pager-old-filter)
                    (setq term-pager-old-filter (process-filter proc))
                    (set-process-filter proc term-pager-filter)
                    (setq i str-length))
                (setq i ctl-end)))))

        (when (>= (term-current-row) term-height)
          (term-handle-deferred-scroll))

        (set-marker (process-mark proc) (point))
        (when save-point
          (goto-char save-point)
          (set-marker save-point nil))

        ;; Check for a pending filename-and-line number to display.
        ;; We do this before scrolling, because we might create a new window.
        (when (and term-pending-frame
                   (eq (window-buffer selected) (current-buffer)))
          (term-display-line (car term-pending-frame)
                             (cdr term-pending-frame))
          (setq term-pending-frame nil))

        ;; Scroll each window displaying the buffer but (by default)
        ;; only if the point matches the process-mark we started with.
        (setq win selected)
        ;; Avoid infinite loop in strange case where minibuffer window
        ;; is selected but not active.
        (while (window-minibuffer-p win)
          (setq win (next-window win nil t)))
        (setq last-win win)
        (while (progn
                 (setq win (next-window win nil t))
                 (when (eq (window-buffer win) (process-buffer proc))
                   (let ((scroll term-scroll-to-bottom-on-output))
                     (select-window win)
                     (when (or (= (point) save-marker)
                               (eq scroll t) (eq scroll 'all)
                               ;; Maybe user wants point to jump to the end.
                               (and (eq selected win)
                                    (or (eq scroll 'this) (not save-point)))
                               (and (eq scroll 'others)
                                    (not (eq selected win))))
                       (goto-char term-home-marker)
                       (recenter 0)
                       (goto-char (process-mark proc))
                       (if (not (pos-visible-in-window-p (point) win))
                           (recenter -1)))
                     ;; Optionally scroll so that the text
                     ;; ends at the bottom of the window.
                     (when (and term-scroll-show-maximum-output
                                (>= (point) (process-mark proc)))
                       (save-excursion
                         (goto-char (point-max))
                         (recenter -1)))))
                 (not (eq win last-win))))

        ;; Stolen from comint.el and adapted -mm
        (when (> term-buffer-maximum-size 0)
          (save-excursion
            (goto-char (process-mark (get-buffer-process (current-buffer))))
            (forward-line (- term-buffer-maximum-size))
            (beginning-of-line)
            (delete-region (point-min) (point))))
        (set-marker save-marker nil)))
    ;; This might be expensive, but we need it to handle something
    ;; like `sleep 5 | less -c' in more-or-less real time.
    (when (get-buffer-window (current-buffer))
      (redisplay))))

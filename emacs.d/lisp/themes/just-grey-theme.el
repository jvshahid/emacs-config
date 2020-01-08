;;; just-grey-theme.el --- Tango-based custom theme for faces

;; Authors: John Shahid <jvshahid@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; The colors and code in this theme are based on the tango-dark theme, which
;; had the following copyright notice:

;; Copyright (C) 2010-2019 Free Software Foundation, Inc.

;; Authors: Chong Yidong <cyd@stupidchicken>
;;          Jan Moringen <jan.moringen@uni-bielefeld.de>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(deftheme just-grey
  "Face colors using the Tango palette (dark background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#4E5760") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#e090d7") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#919ba5") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e9b2e3")
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526"))

  (custom-theme-set-faces
   'just-grey
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
               (:foreground ,alum-1 :background ,alum-6))
              (((class color) (min-colors 256))
               (:foreground ,alum-1 :background "#222"))
              (,class
               (:foreground ,alum-1 :background "black"))))
   `(cursor ((,class (:background ,butter-1))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,alum-7))))
   `(highlight ((,class (:foreground ,alum-6 :background ,butter-2))))
   `(region ((,class (:background ,alum-5))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,alum-1 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,choc-3))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   ;; ido
   `(ido-only-match ((,class (:inherit ido-first-match))))
   `(ido-subdir ((,class (:underline t))))
   ;; Mode line faces
   `(mode-line ((,class
                 (:box (:line-width -1 :style released-button)
                       :background ,alum-2 :foreground ,alum-6))))
   `(mode-line-inactive ((,class
                          (:box (:line-width -1 :style released-button)
                                :background ,alum-5 :foreground ,alum-1))))
   `(compilation-mode-line-fail ((,class (:foreground ,red-3))))
   `(compilation-mode-line-run  ((,class (:foreground ,orange-3))))
   `(compilation-mode-line-exit ((,class (:foreground ,cham-3))))
   `(compilation-info ((,class (:foreground ,alum-1))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,cham-0))))
   `(escape-glyph ((,class (:foreground ,butter-3))))
   `(homoglyph ((,class (:foreground ,butter-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,cham-1))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,alum-1))))
   `(font-lock-comment-face ((,class (:foreground ,cham-2))))
   `(font-lock-constant-face ((,class (:foreground ,alum-1))))
   `(font-lock-function-name-face ((,class (:foreground ,alum-1))))
   `(font-lock-keyword-face ((,class (:foreground ,alum-1))))
   `(font-lock-string-face ((,class (:foreground ,alum-1))))
   `(font-lock-type-face ((,class (:foreground ,alum-1))))
   `(font-lock-variable-name-face ((,class (:foreground ,alum-1))))
   ;; shell
   `(sh-quoted-exec ((,class (:foreground ,alum-1))))
   `(sh-heredoc ((,class (:foreground ,alum-1 :weight semi-bold))))
   `(eshell-prompt ((,class (:foreground ,alum-1))))
   `(eshell-ls-directory ((,class (:foreground ,alum-1 :weight bold))))
   `(eshell-ls-backup ((,class (:foreground ,alum-1))))
   `(eshell-ls-readonly ((,class (:foreground ,alum-1))))
   `(eshell-ls-unreadable ((,class (:foreground ,alum-1))))
   `(eshell-ls-executable ((,class (:foreground ,alum-1))))
   `(eshell-ls-archive ((,class (:foreground ,alum-1))))
   `(term-color-black ((,class (:foreground ,alum-1))))
   `(term-color-red   ((,class (:foreground ,alum-1))))
   `(term-color-green ((,class (:foreground ,alum-1))))
   `(term-color-yellow ((,class (:foreground ,alum-1))))
   `(term-color-blue  ((,class (:foreground ,alum-1))))
   `(term-color-magenta ((,class (:foreground ,alum-1))))
   `(term-color-cyan  ((,class (:foreground ,alum-1))))
   `(term-color-white ((,class (:foreground ,alum-1))))
   ;; dired
   `(dired-header ((,class (:foreground ,alum-1))))
   `(dired-directory ((,class (:foreground ,alum-1 :weight bold))))
   `(dired-perm-write ((,class (:foreground ,alum-1))))
   ;; org mode
   `(org-todo ((,class (:foreground ,alum-1 :weight bold))))
   `(org-done ((,class (:foreground ,alum-1 :weight bold))))
   `(org-level-2 ((,class (:foreground ,alum-1))))
   `(org-table ((,class (:foreground ,alum-1 :weight bold))))
   `(org-time-grid ((,class (:foreground ,alum-1))))
   `(org-agenda-date-today ((,class (:foreground ,alum-1 :slant italic :weight bold))))
   `(org-agenda-date ((,class (:foreground ,alum-1 :weight bold))))
   `(org-scheduled-previously ((,class (:foreground ,alum-1 :weight bold))))
   ;; markdown
   `(markdown-inline-code-face ((,class (:foreground ,alum-1 :weight semi-bold))))
   `(markdown-pre-face ((t)))
   `(markdown-code-face ((,class (:foreground ,alum-1 :weight semi-bold))))
   `(markdown-table-face ((,class (:foreground ,alum-1 :weight semi-bold))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))
   ;; mu4e
   `(mu4e-unread-face ((,class (:inherit link :underline nil))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-news-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-news-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-news-4 ((,class (:foreground ,plum-0))))
   `(gnus-group-news-4-low ((,class (:foreground ,choc-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,butter-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-mail-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-mail-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-mail-low ((,class (:foreground ,butter-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,butter-3))))
   `(gnus-header-from ((,class (:foreground ,butter-2))))
   `(gnus-header-subject ((,class (:foreground ,cham-1))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,choc-2))))
   ;; company mode
   `(company-template-field ((t (:inherit region))))
   `(company-tooltip ((t (:background "lightgrey" :foreground "black"))))
   `(company-tooltip-annotation ((t (:inherit copmany-tooltip))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
   `(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
   ;; Message faces
   `(message-mml ((,class (:foreground ,alum-1 :weight semi-bold))))
   `(message-header-name ((,class (:foreground ,blue-1))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,cham-1))))
   `(message-header-to ((,class (:foreground ,butter-2))))
   `(message-cited-text ((,class (:foreground ,cham-1))))
   `(message-separator ((,class (:foreground ,plum-1))))
   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))
   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,alum-5))))
   `(ediff-fine-diff-A ((,class (:background ,blue-3))))
   `(ediff-even-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-current-diff-B ((,class (:background ,alum-5))))
   `(ediff-fine-diff-B ((,class (:background ,choc-3))))
   `(ediff-even-diff-B ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-B ((,class (:background ,alum-5.5))))
   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,orange-1))))
   `(flyspell-incorrect ((,class (:underline ,red-1)))))

  (custom-theme-set-variables
   'just-grey
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
                                      ,blue-1 ,plum-1 ,blue-0 ,alum-1])))

(provide-theme 'just-grey)

;;; just-grey-theme.el ends here

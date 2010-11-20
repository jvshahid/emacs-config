;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-enable-slime-on-existing-buffers clojure-mode)
;;;;;;  "clojure-mode" "clojure-mode.el" (19554 44917))
;;; Generated autoloads from clojure-mode.el

(autoload (quote clojure-mode) "clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode..
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(autoload (quote clojure-enable-slime-on-existing-buffers) "clojure-mode" "\
Not documented

\(fn)" t nil)

(add-hook (quote slime-connected-hook) (quote clojure-enable-slime-on-existing-buffers))

(add-to-list (quote auto-mode-alist) (quote ("\\.clj$" . clojure-mode)))

;;;***

;;;### (autoloads nil nil ("clojure-mode-pkg.el") (19554 44917 859030))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-mode-autoloads.el ends here

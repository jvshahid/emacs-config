;;; -*- lexical-binding: t; -*-

;; allow the test to use untrusted CA cert for the test server
(setq network-security-level 'low)

;; TODO: how can i do this ?
(load-file "../../straight/build/hierarchy/hierarchy.el")

(defun fake-server-port ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp ":[0-9]")
    (backward-char)
    (if (looking-at "[0-9]+$")
        (buffer-substring (nth 0 (match-data))
                          (nth 1 (match-data)))
      (ert-fail "cannot find server port"))))

(defun -with-fake-server (args body)
  (let* ((proc (start-process "fake-server"
                              "concourse-fake-server"
                              "go"
                              "run" "src/server/main.go"))
         (orig-url concourse-url)
         (port (progn
                 (accept-process-output proc 5)
                 (with-current-buffer "concourse-fake-server"
                   (fake-server-port)))))
    (setq concourse-url (format "https://localhost:%s" port))
    (unwind-protect
        (funcall body)
      (setq concourse-url orig-url)
      (kill-process proc)
      (accept-process-output proc 5)
      (kill-buffer (process-buffer proc)))))


(defmacro with-fake-server (args &rest body)
  `(-with-fake-server ,args
                      (lambda () ,@body)))

(defun -with-flet (fname func body)
  (let ((orig (symbol-function fname)))
    (fset fname func)
    (unwind-protect
        (funcall body)
      (fset fname orig))))

(defmacro with-flet (args &rest body)
  (let ((fname (nth 0 args))
        (fargs (nth 1 args))
        (fbody (nth 2 args)))
    `(-with-flet ',fname (lambda ,fargs ,fbody) (lambda () ,@body))))

(ert-deftest pipeline-render-proper-root-name ()
  (with-fake-server nil
                    (with-temp-buffer
                      (let ((concourse-pipeline "somepipeline"))
                        (concourse-view-pipeline (current-buffer))
                        (goto-char (point-min))
                        (should (looking-at "\\[\\+\\] +somepipeline"))
                        (should (equal 1 (count-lines (point-min) (point-max))))
                        (widget-button-press (point-min))
                        (should (looking-at "\\[\\-\\] +somepipeline"))
                        (should (equal 4 (count-lines (point-min) (point-max))))
                        (forward-line)
                        (should (looking-at " +|\\-\\[\\+\\] +first"))
                        (forward-line)
                        (should (looking-at " +|\\-\\[\\+\\] +second"))
                        (forward-line)
                        (should (looking-at " +`\\-\\[\\+\\] +third"))))))

(ert-deftest pipeline-render-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (concourse-view-pipeline (current-buffer))
                      (widget-button-press (point-min))
                      (search-forward "first")
                      (backward-word)
                      ;; test button actions by replacing concourse-view-job
                      (let (button-pressed)
                        (with-flet (concourse-view-job (_)
                                                       (setq button-pressed t))
                          (button-activate (button-at (point)))
                          (should button-pressed))))))

(ert-deftest job-render-test ()
  (with-fake-server nil
                    (with-temp-buffer
                      (concourse-view-job '((name "first")) (current-buffer))
                      (should (looking-at "\\[\\+\\] +first"))
                      (widget-button-press (point-min))
                      (should (equal 3 (count-lines (point-min) (point-max))))
                      (forward-line 1)
                      (should (looking-at " +|\\-\\[\\+\\] +1"))
                      (forward-line 1)
                      (should (looking-at " +`\\-\\[\\+\\] +20")))))

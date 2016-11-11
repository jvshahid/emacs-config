;;; -*- lexical-binding: t; -*-
;;; package --- Summary
;;; Commentary:
(require 'json)

;;; Code:

(defun concourse-bind (f &rest fs)
  (if fs
      (let ((g (car fs)))
        (apply 'concourse-bind
               (lambda (x)
                 (funcall g (funcall f x)))
               (cdr fs)))
    f))

(defun concourse-get-url (url callback)
  "Depending on the value of `noninteractive' calls either
`url-retrieve' or `url-retrieve-synchronously'"
  (if noninteractive
      (let ((buffer (url-retrieve-synchronously url t)))
        (with-current-buffer buffer
          (funcall callback nil)))
    (url-retrieve url callback nil t)))

(defun concourse-parse-response (status)
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((data (json-read)))
    (kill-buffer)
    data))

(defun concourse-filter-jobs-by-group (group jobs)
  "Remove JOBS that don't belong to the given GROUP."
  (if group
      (cl-remove-if-not (lambda (job)
                          (let ((groups (assoc-default 'groups job)))
                            (cl-find-if (lambda (g) (string-equal g group)) groups)))
                        jobs)
    jobs))

(defun concourse-job-hanging-p (build duration-limit)
  "Return true if the BUILD has been running for more than DURATION-LIMIT seconds."
  (let ((start-time (assoc-default 'start_time build)))
    (when start-time
      (let* ((current-time (round (float-time)))
             (duration (- current-time start-time)))
        (> duration duration-limit)))))

(defun concourse-job-status (duration-limit job)
  "Return an alist containing the name and status of the given JOB.
A JOB is considered hanging if it has been running for more than
DURATION-LIMIT seconds"
  (let ((next-build (assoc-default 'next_build job))
        (previous-build (assoc-default 'finished_build job))
        (name (assoc 'name job)))
    (cl-labels ((status-from-job (job)
                                 (if next-build
                                     (if (concourse-job-hanging-p next-build duration-limit)
                                         '(status . hanging)
                                       '(status . pending))
                                   ;; otherwise, check the previous build
                                   (let ((status (assoc-default 'status previous-build)))
                                     (if (string-equal status "succeeded")
                                         '(status . succeeded)
                                       '(status . failed))))))
      (list (status-from-job job) name))))

(defun concourse-jobs-status (duration-limit jobs)
  (cl-map 'list (apply-partially 'concourse-job-status duration-limit) jobs))

(defun concourse-pipeline-summary-from-status (data)
  (cl-labels ((has-status (status) (lambda (x) (eq status (assoc-default 'status x))))
              (count-for-status (status list)
                                (length (cl-remove-if-not (has-status status) list))))
    (list (cons 'succeeded (count-for-status 'succeeded data))
          (cons 'failed    (count-for-status 'failed data))
          (cons 'hanging   (count-for-status 'hanging data)))))

(defvar concourse-pipeline-color nil)
(defvar concourse-url "diego.ci.cf-app.com")
(defvar concourse-pipeline "main")
(defvar concourse-team "main")
(defvar concourse-group "diego")
(defvar concourse-duration-limit (* 60 60))

(defun concourse-pipeline-summary (group)
  (concourse-bind
   'concourse-parse-response
   (apply-partially 'concourse-filter-jobs-by-group group)
   (apply-partially 'concourse-jobs-status concourse-duration-limit)
   'concourse-pipeline-summary-from-status))

;;; Emacs mode line update
(defun concourse-mode-line ()
  (concat
   (if concourse-pipeline-color
       (propertize concourse-url 'face (cons 'background-color concourse-pipeline-color))
     (propertize "Updating" 'face (cons 'background-color "orange")))
   " "))

(defun concourse-update-mode-line-from-status (status)
  (cond
   ((> (assoc-default 'hanging status) 0) (setq concourse-pipeline-color "orange"))
   ((> (assoc-default 'failed status) 0) (setq concourse-pipeline-color "red"))
   (t (setq concourse-pipeline-color "green")))
  (force-mode-line-update t))

(defun concourse-update-mode-line ()
  (concourse-get-url
   (format "https://%s/api/v1/teams/%s/pipelines/%s/jobs"
           concourse-url
           concourse-team
           concourse-pipeline)
   (concourse-bind
    (concourse-pipeline-summary concourse-group)
    'concourse-update-mode-line-from-status)))

(defvar concourse-timer nil)

(defun concourse-update-mode-line-bg ()
  (add-to-list 'mode-line-modes '(:eval (concourse-mode-line)) t)
  (setq concourse-timer (run-at-time 1 60 'concourse-update-mode-line))
  (force-mode-line-update t))

(defun concourse-stop-updating-mode-line ()
  (delete '(:eval (concourse-mode-line)) mode-line-modes)
  (cancel-timer concourse-timer))

;;; Tests:

(ert-deftest concourse-test-partial ()
  (let ((second (apply-partially 'nth 1))
        (append-to-foo (apply-partially 'append '("foo"))))
    (should (eql (funcall second '(foo bar baz)) 'bar))
    (should (equal (funcall append-to-foo '("bar")) '("foo" "bar")))))

(ert-deftest concourse-test-binding ()
  (let ((noop (concourse-bind
               (apply-partially 'cons "foo")
               'cdr)))
    (should (equal (funcall noop '("bar")) '("bar")))))

(defun construct-successful-job ()
  '((name . "successful") (groups . ["diego"]) (finished_build (status . "succeeded"))))

(defun construct-hanging-job ()
  '((name . "hanging") (groups . ["diego"]) (next_build (start_time . 0))))

(defun construct-pending-job ()
  '((name . "hanging") (groups . ["diego"]) (next_build (status . pending))))

(ert-deftest concourse-job-status ()
  (let ((job (construct-successful-job))
        (hanging (construct-hanging-job))
        (pending (construct-pending-job)))
    (should (eql (assoc-default 'status (concourse-job-status 0 job)) 'succeeded))
    (should (eql (assoc-default 'status (concourse-job-status 0 hanging)) 'hanging))
    (should (eql (assoc-default 'status (concourse-job-status 0 pending)) 'pending))))

(ert-deftest concourse-job-hanging-p ()
  (let* ((hanging (construct-hanging-job))
         (hanging-build (assoc-default 'next_build hanging))
         (pending (construct-pending-job))
         (pending-build (assoc-default 'next_build pending)))
    (should (eql (concourse-job-hanging-p hanging-build 60) t))
    (should (eql (concourse-job-hanging-p pending-build 60) nil))))

(ert-deftest concourse-jobs-status ()
  (let* ((jobs (list (construct-successful-job)))
         (status (concourse-jobs-status 0 jobs)))
    (should (eql (length status) 1))))

(defun concourse-mode ()
  (interactive)
  (if concourse-timer
      (progn
        (concourse-stop-updating-mode-line)
        (setq concourse-timer nil)
        (setq concourse-pipeline-color nil))
    (concourse-update-mode-line-bg)))

(provide 'concourse-mode)
;;; concourse.el ends here

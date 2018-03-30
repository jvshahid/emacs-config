;;; -*- lexical-binding: t; -*-

(autoload 'hierarchy-new "hierarchy")

(defun concourse-colored-name (job _)
  (insert " " (cdr (assoc 'name job)))
  (let* ((build (assoc 'finished_build job))
         (status (cdr (assoc 'status build))))
    (let ((property (cond
                     ((string-equal status "failed") '(:foreground "red"))
                     ((string-equal status "succeeded") '(:foreground "green")))))
      (and property
           (add-face-text-property (point-min)
                                   (point-max)
                                   property)))))

(defun concourse-display-json (root data func buffer)
  (let ((root `((name . ,root))))
    (let ((h (hierarchy-new)))
      (hierarchy-add-tree h root nil (lambda (n)
                                       (if (equal root n)
                                           (reverse data))))
      (let ((buffer (hierarchy-tree-display h
                                            (hierarchy-labelfn-button
                                             #'concourse-colored-name
                                             (lambda (elem _) (funcall func elem)))
                                            buffer)))
        (with-current-buffer buffer
          (local-set-key (kbd "q") #'kill-current-buffer)
          (local-set-key (kbd "g") (lambda ()
                                     (interactive)
                                     (funcall concourse-refresh-func)))
          buffer)))))

(defun concourse-refresh ()
  (interactive)
  (let* ((data      (json-read-file source))
         (curpoint  (point))
         (open      (concourse-> (point-min)
                                 (widget-at)
                                 (widget-get :parent)
                                 (widget-get :open))))
    (apply concourse-refresh-func concourse-refresh-args)
    (if open (widget-button-press (point-min)))
    (goto-char curpoint)))

(defun concourse-view-build (url &optional buffer))
;; TODO: display the build resource followed by the logs (truncated may be to
;; the first 1000 lines or so)

(defun concourse-view-job (job &optional buffer)
  (interactive)
  (let ((name (cadr (assoc 'name job))))
    (concourse-get-url (concat concourse-url (concourse-builds-path name))
                       (lambda (x)
                         (let ((data (concourse-parse-response x))
                               (buffer (or buffer
                                           (get-buffer-create "concourse-job-view"))))
                           (concourse-display-json name data #'concourse-view-build buffer)
                           (switch-to-buffer buffer)
                           (setq-local concourse-refresh-func #'concourse-view-job)
                           (setq-local concourse-refresh-args name))))))

(defun concourse-view-pipeline (&optional buffer)
  (interactive)
  (concourse-get-url (concat concourse-url (concourse-jobs-path))
                     (lambda (x)
                       (let ((data (concourse-parse-response x))
                             (buffer (or buffer
                                         (get-buffer-create "concourse-pipeline-view"))))
                         (concourse-display-json concourse-pipeline
                                                 data
                                                 #'concourse-view-job buffer)
                         (switch-to-buffer buffer)
                         (setq-local concourse-refresh-func #'concourse-view-pipeline)))))

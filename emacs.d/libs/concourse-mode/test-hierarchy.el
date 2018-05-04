;;; -*- lexical-binding: t; -*-

(autoload 'hierarchy-new "hierarchy")

(defun concourse-colored-name (elem _)
  (insert " " (cdr (assoc 'name elem)))
  (let* ((build (or (assoc 'finished_build elem)
                    ;; when looking at a job's builds
                    elem))
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

(defun concourse/assoc (x key)
  (alist-get key x))

(defun concourse/get-in (x &rest keys)
  (seq-reduce #'concourse/assoc keys x))

(defun concourse/event-id (event)
  (concourse/get-in event 'data 'data 'origin 'id))

(defun concourse/event-version (event)
  (concourse/get-in event 'data 'data 'version))

(defun concourse/insert-event (event)
  (insert (format "Event: %S" (concourse/event-id event)))
  (insert "\n")
  (if-let ((version (concourse/event-version event)))
      (insert (format "\tversion: %S\n" version)))
  (cl-loop for m across (concourse/get-in event 'data 'data 'metadata)
           do (insert (format "\t%s: %s\n"
                              (concourse/assoc m 'name)
                              (concourse/assoc m 'value)))))

(defun concourse-view-build (build &optional buffer)
  (let ((buffer (or buffer
                    (get-buffer-create "concourse-build-view"))))
    (with-current-buffer buffer (erase-buffer))
    (let ((buf "")
          id
          event
          data
          events)
      (make-process :name "test-event-stream"
                    :buffer buffer
                    :command (list "curl" "-k" "-N" (concat concourse-url (concourse/assoc build 'api_url) "/events"))
                    :connection-type 'pipe
                    :filter (lambda (proc str)
                              (setq buf (concat buf str))
                              (let ((newline? (string-suffix-p "\n" buf))
                                    (lines (split-string buf "\n")))
                                (setq buf "")
                                (unless newline?
                                  (setq lines (butlast lines))
                                  (setq buf (car (last lines))))
                                (dolist (l lines)
                                  (cond
                                   ((string-prefix-p "id: " l) (setq id (substring l (length "id: "))))
                                   ((string-prefix-p "data: " l) (setq data (substring l (length "data: "))))
                                   ((string-prefix-p "event: " l) (setq event (substring l (length "event: ")))))
                                  (if (and id event data)
                                      (let ((parsed-data (json-read-from-string data)))
                                        (when (string-prefix-p "finish-" (cdr (assoc 'event parsed-data)))
                                          (with-current-buffer buffer
                                            (concourse/insert-event `((id . ,id)
                                                                      (event . ,event)
                                                                      (data . ,parsed-data))))
                                          (setq id nil
                                                event nil
                                                data nil))))
                                  (if (string-suffix-p "end" event)
                                      (kill-process (get-buffer-process buffer))))))
                    :sentinel (lambda (_ str)
                                (when (string-prefix-p "killed" str)
                                  (switch-to-buffer buffer)
                                  (setq buffer-read-only t)
                                  (local-set-key (kbd "q") #'kill-current-buffer)
                                  (local-set-key (kbd "g") (lambda ()
                                                             (interactive)
                                                             (funcall concourse-refresh-func)))))))))


(defun concourse-view-job (job &optional buffer)
  (interactive)
  (let ((name (cdr (assoc 'name job))))
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


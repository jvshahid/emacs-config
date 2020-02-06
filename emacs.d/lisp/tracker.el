(defun tracker--notification-unread-p (notification)
  (not (alist-get 'read_at notification)))

;; (defun tracker--notification-project-id (notification)
;;   (let ((project (alist-get 'project notification)))
;;     (alist-get 'id project)))

(defun tracker--notification-project (notification)
  (alist-get 'project notification))

(defun tracker--notification-story (notification)
  (or (alist-get 'story notification)
      (alist-get 'epic notification)))

(defun tracker--story-url (story)
  (format "https://www.pivotaltracker.com/%s/show/%s"
          (alist-get 'kind story)
          (alist-get 'id story)))

(defun tracker--indent-string-rigidly (str arg)
  "Indent all ines in the given string sideways by ARG columns."
  (with-temp-buffer
    (insert str)
    (indent-code-rigidly (point-min)
                         (point-max)
                         arg)
    (buffer-string)))

(defun tracker--story-notifications-to-org (notifications)
  (pcase-let ((`(,story . ,notifications) notifications))
    (let ((name (alist-get 'name story))
          (id   (alist-get 'id story))
          (url  (tracker--story-url story)))
      (insert (format "** [[%s][%s]] (%s)\n"
                      url
                      name
                      id))
      (seq-do (lambda (notification)
                (let ((message (alist-get 'message notification))
                      (context (alist-get 'context notification)))
                  (insert (format "*** %s\n%s\n"
                                  message
                                  (tracker--indent-string-rigidly (or context "")
                                                                  1)))))
              notifications))))

(defun tracker--proj-notifications-to-org (notifications)
  (pcase-let ((`(,proj . ,notifications) notifications))
    (let ((grouped-by-story (seq-group-by #'tracker--notification-story
                                          notifications)))
      (insert (format "* %s\n" (alist-get 'name proj)))
      (seq-do #'tracker--story-notifications-to-org
              grouped-by-story))))

(defun tracker--notifications-to-org (notifications)
  (if (seq-empty-p notifications)
      (insert "No notifications at this time")
    (let ((grouped-by-proj (seq-group-by #'tracker--notification-project
                                         notifications)))
      (seq-do #'tracker--proj-notifications-to-org
              grouped-by-proj))))

(defun tracker--parser-response (status)
  (search-forward "\n\n")
  (let* ((data (json-parse-buffer :object-type 'alist))
         (unread (seq-into (seq-filter #'tracker--notification-unread-p data)
                           'vector))
         (sorted-unread (seq-sort (lambda (not1 not2)
                                    (string<
                                     (alist-get 'created_at not1)
                                     (alist-get 'created_at not2)))
                                  unread))
         (buf (get-buffer-create "*tracker updates*")))
    (kill-buffer)
    (with-current-buffer buf
      (erase-buffer)
      (tracker--notifications-to-org sorted-unread)
      (org-mode)
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defgroup tracker nil
  "Customization for tracker")

(defcustom tracker-username nil
  "Username to use when displaying notifications"
  :group 'tracker
  :type 'string)

(defun tracker--get-password (username)
  (let ((found (nth 0 (auth-source-search :max 1
                                          :host "pivotaltracker.com"
                                          :user username
                                          :require '(:secret)))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun tracker-notifications ()
  (interactive)
  (let* ((token (tracker--get-password tracker-username))
         (url-request-extra-headers `(("X-TrackerToken" . ,token))))
    (url-retrieve "https://www.pivotaltracker.com/services/v5/my/notifications?notification_types=:all"
                  #'tracker--parser-response)))

;;; -*- lexical-binding: t; -*-

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook #'company-mode)
(add-hook 'java-mode-hook #'hs-minor-mode)
(defun shahid/java-mode-hook ()
  (setq-local c-basic-offset 2))
(add-hook 'java-mode-hook #'shahid/java-mode-hook)

(defun eglot-java-eclipse-jdt-p (server)
  (cl-find "jdtls" (process-command
                    (jsonrpc--process server))
                   :test #'string-search))

(defun eglot-java--find-server ()
  "Find the LSP server of type eglot-java-eclipse-jdt for the
current project. In the strange event that there are multiple,
return the first one."
  (when-let* ((project (project-current))
              (servers (gethash project eglot--servers-by-project)))
    (cl-find-if #'eglot-java-eclipse-jdt-p servers)))

(defun eglot-java--make-path (root-dir &rest path-elements)
  "Compose a path from a base folder ROOT-DIR and a set of items PATH-ELEMENTS."
  (let ((new-path          (expand-file-name root-dir))
        (new-path-elements path-elements))
    (dolist (p new-path-elements)
      (setq new-path (concat (file-name-as-directory new-path) p)))
    new-path))

(defun eglot-java--jdt-uri-handler (_operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-java" (project-root (project-current t))))
         (source-file
          (expand-file-name
           (eglot-java--make-path
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-java--find-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-java--jdt-uri-handler))

(cl-defmethod eglot-initialization-options :around (server)
  (let ((base (cl-call-next-method)))
    (when (eglot-java-eclipse-jdt-p server)
      `(:extendedClientCapabilities
        (:classFileContentsSupport t)))))

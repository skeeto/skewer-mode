;;; skewer-bower.el --- -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'skewer-mode)
(require 'simple-httpd)
(require 'magit nil t) ; optional

(defvar skewer-bower-cache-dir (locate-user-emacs-file "skewer-cache")
  "Location of library cache (git repositories).")

(defvar skewer-bower-endpoint "https://bower.herokuapp.com"
  "Endpoint for accessing package information.")

; Try to match Magit's configuration if available
(if (boundp 'magit-git-executable)
    (defvaralias 'skewer-bower-git-executable 'magit-git-executable)
  (defvar skewer-bower-git-executable "git"
    "Name of the git executable."))

(defvar skewer-bower-packages nil
  "Alist of all packages known to bower.")

(defvar skewer-bower-refreshed nil
  "List of packages that have been refreshed recently. This keeps
them from hitting the network frequently.")

;;;###autoload
(defun skewer-bower-refresh ()
  "Update the package listing and packages synchronously."
  (setf skewer-bower-refreshed nil)
  (with-current-buffer
      (url-retrieve-synchronously (concat skewer-bower-endpoint "/packages"))
    (re-search-forward "\r?\n\r?\n")
    (setf skewer-bower-packages
          (sort*
           (loop for package across (json-read)
                 collect (cons (cdr (assoc 'name package))
                               (cdr (assoc 'url package))))
           #'string< :key #'car))))

;; Git functions

(defun skewer-bower-cache (package)
  "Return the cache repository directory for PACKAGE."
  (unless (file-exists-p skewer-bower-cache-dir)
    (make-directory skewer-bower-cache-dir t))
  (expand-file-name package skewer-bower-cache-dir))

(defun skewer-bower-git (package &rest args)
  "Run git for PACKAGE's repository with ARGS."
  (with-temp-buffer
    (when (zerop (apply #'call-process skewer-bower-git-executable nil t nil
                        (format "--git-dir=%s" (skewer-bower-cache package))
                        args))
      (buffer-string))))

(defun skewer-bower-git-clone (url package)
  "Clone or fetch PACKAGE's repository from URL if needed."
  (if (member package skewer-bower-refreshed)
      t
    (let* ((cache (skewer-bower-cache package))
           (status
            (if (file-exists-p cache)
                (when (skewer-bower-git package "fetch")
                  (push package skewer-bower-refreshed))
              (skewer-bower-git package "clone" "--bare" url cache))))
      (not (null status)))))

(defun skewer-bower-git-show (package version file)
  "Grab FILE from PACKAGE at version VERSION."
  (skewer-bower-git package "show" (format "%s:%s" version file)))

(defun skewer-bower-git-tag (package)
  "List all the tags in PACKAGE's repository."
  (split-string (skewer-bower-git package "tag")))

;; Bower functions

(defun skewer-bower-package-ensure (package)
  "Ensure a package is installed in the cache and up to date."
  (let ((url (cdr (assoc package skewer-bower-packages))))
    (when (null url)
      (error "Unknown package: %s" package))
    (when (null (skewer-bower-git-clone url package))
      (error "Failed to fetch: %s" url))
    t))

(defun skewer-bower-package-versions (package)
  "List the available versions for a package."
  (skewer-bower-package-ensure package)
  (or (sort (skewer-bower-git-tag package) #'string<)
      (list "master")))

(defun skewer-bower-get-config (package &optional version)
  "Get the configuration alist for PACKAGE at VERSION."
  (skewer-bower-package-ensure package)
  (unless version (setf version "master"))
  (json-read-from-string
   (or (skewer-bower-git-show package version "bower.json")
       (skewer-bower-git-show package version "package.json")
       (skewer-bower-git-show package version "component.json")
       "null")))

;; Serving the library

(defvar skewer-bower-history ()
  "Library selection history.")

(defun skewer-bowser--path (package version main)
  "Return the hosted path for PACKAGE."
  (format "/skewer/bower/%s/%s/%s" package (or version "master") main))

;;;###autoload
(defun skewer-bower-load (package &optional version)
  "Dynamically load a library from bower into the current page."
  (interactive
   (let* ((packages (mapcar #'car skewer-bower-packages))
          (selection (delete-duplicates
                      (append skewer-bower-history packages) :from-end t))
          (package (completing-read "Library: " selection nil t nil
                                    'skewer-bower-history))
          (versions (skewer-bower-package-versions package))
          (version (completing-read "Version: " (reverse versions))))
     (list package version)))
  (let* ((config (skewer-bower-get-config package))
         (deps (cdr (assoc 'dependencies config)))
         (main (cdr (assoc 'main config))))
    (unless main
      (error "Could not load %s (%s)" package version))
    (loop for (dep . version) in deps
          do (skewer-bower-load (format "%s" dep) version))
    (let ((path (skewer-bowser--path package version main)))
      (skewer-eval path nil :type "script"))))

(defservlet skewer/bower application/javascript (path)
  "Serve a file from the local repository cache."
  (destructuring-bind (_ skewer bower package version . parts)
      (split-string path "/")
    (let* ((file (mapconcat #'identity parts "/"))
           (contents (skewer-bower-git-show package version file)))
      (if contents
          (insert contents)
        (httpd-error t 404)))))

(provide 'skewer-bower)

;;; skewer-bower.el ends here

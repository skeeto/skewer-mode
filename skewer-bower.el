;;; skewer-bower.el --- -*- lexical-binding: t; -*-

(require 'cl)
(require 'magit nil t)

(defvar skewer-bower-cache-dir (locate-user-emacs-file "skewer-cache"))

(defvar skewer-bower-endpoint "https://bower.herokuapp.com"
  "Endpoint for accessing package information.")

; Try to use Magit's configuration
(if (boundp 'magit-git-executable)
    (defvaralias 'skewer-bower-git-executable 'magit-git-executable)
  (defvar skewer-bower-git-executable "git"
    "Name of the git executable."))

(defvar skewer-bower-packages nil
  "Alist of all packages known to bower.")

(defvar skewer-bower-refreshed nil
  "List up packages that have been refreshed recently.")

(defun skewer-bower-refresh ()
  "Update the package listing synchronously."
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
  "Return the cache directory for PACKAGE."
  (unless (file-exists-p skewer-bower-cache-dir)
    (make-directory skewer-bower-cache-dir t))
  (expand-file-name package skewer-bower-cache-dir))

(defun skewer-bower-git (package &rest args)
  (with-temp-buffer
    (when (zerop (apply #'call-process skewer-bower-git-executable nil t nil
                        (format "--git-dir=%s" (skewer-bower-cache package))
                        args))
      (buffer-string))))

(defun skewer-bower-git-clone (url package)
  "Clone or update a package's repository if needed."
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
  "Show a file from a repository."
  (skewer-bower-git package "show" (format "%s:%s" version file)))

(defun skewer-bower-git-tag (package)
  "List all the tags in a repository."
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
  (sort (skewer-bower-git-tag package) #'string<))

(defun skewer-bower-get-config (package &optional version)
  "Get the configuration for a package."
  (skewer-bower-package-ensure package)
  (json-read-from-string
   (skewer-bower-git-show package (or version "master") "bower.json")))

;; Serving the library

(defun skewer-bower-load (package &optional version)
  "Dynamically load a library from bower into the page."
  (interactive
   (let* ((packages (mapcar #'car skewer-bower-packages))
          (package (completing-read "Library: " packages nil t))
          (versions (skewer-bower-package-versions package))
          (version (completing-read "Version: " (reverse versions) nil t)))
     (list package version)))
  (let* ((config (skewer-bower-get-config package))
         (main (cdr (assoc 'main config))))
    (skewer-eval (concat "/skewer/bower/" package "/" (or version "master")
                         "/" main) nil
                 :type "script")))

(defservlet skewer/bower application/javascript (path)
  "Serve a file from the local repository cache."
  (destructuring-bind (_ skewer bower package version . parts)
      (split-string path "/")
    (let* ((file (mapconcat #'identity parts "/"))
           (contents (skewer-bower-git-show package version file)))
      (if contents
          (insert contents)
        (httpd-error t 404)))))

(setf skewer-bower-refreshed nil))
(skewer-bower-get-config "angular")
(skewer-bower-package-versions "angular")
skewer-bower-packages

(skewer-bower-git-show "angular" "master" "bower.json")
(skewer-bower-git-show "jquery" "master" "bower.json")
(skewer-bower-git-show "jquery" "master" "package.json")
(skewer-bower-git-show "jade" "master" "bower.json")

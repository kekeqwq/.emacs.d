;;; elpaca

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; end


;;; editor

(global-auto-revert-mode 1)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(recentf-mode 1)
(savehist-mode 1)
(delete-selection-mode 1)

;;; end


;;; user interface

(pixel-scroll-precision-mode 1)
(global-hl-line-mode 1)
(load-theme 'modus-vivendi t)

;;; end


;;; packages

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;;; end


;;; org-static-blog

(use-package org-static-blog
  :ensure t
  :config
  (setq org-static-blog-publish-title "K")
  (setq org-static-blog-publish-url "")
  (setq org-static-blog-publish-directory "~/blog/")
  (setq org-static-blog-posts-directory "~/blog/posts/")
  (setq org-static-blog-drafts-directory "~/blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)

  ;; This header is inserted into the <head> section of every page:
  ;;   (you will need to create the style sheet at
  ;;    ~/projects/blog/static/style.css
  ;;    and the favicon at
  ;;    ~/projects/blog/static/favicon.ico)
  (setq org-static-blog-page-header
	"
          <meta name=\"author\" content=\"k\">
          <meta name=\"referrer\" content=\"no-referrer\">
          <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
          <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
          <link rel=\"icon\" href=\"static/favicon.ico\">
")
  (setq org-static-blog-page-preamble
	"
          <div class=\"header\">
              <a href=\"index.html\">homepage</a>
              <a href=\"archive.html\">all posts</a>
          </div>
")
  (setq org-static-blog-page-postamble
	"<center>kkk@2024</center>")
  (setq org-static-blog-index-front-matter
	"<h1> Welcome to my blog </h1>\n")
  )

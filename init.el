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

;;; Keymap
(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p s") 'project-search)
(global-set-key (kbd "C-c v v") 'vc-next-action)
(global-set-key (kbd "C-c v c") 'vc-dir)
(global-set-key (kbd "C-c v d") 'vc-diff)
(global-set-key (kbd "C-c w w") 'windmove-up)
(global-set-key (kbd "C-c w a") 'windmove-left)
(global-set-key (kbd "C-c w s") 'windmove-down)
(global-set-key (kbd "C-c w d") 'windmove-right)
(global-set-key (kbd "C-c w q") 'delete-window)
(global-set-key (kbd "C-c w 1") 'delete-other-windows)
(global-set-key (kbd "C-c w 2") 'split-window-below)
(global-set-key (kbd "C-c w 3") 'split-window-right)
(global-set-key (kbd "C-c b b") 'ido-switch-buffer)
(global-set-key (kbd "C-c b k") 'ido-kill-buffer)
(global-set-key (kbd "C-c t t") 'org-agenda-show-all-todo)

;;; end


;;; Completion

(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (global-set-key (kbd "C-c r") 'fido-recentf-open)))

;;; end


;;; user interface

(pixel-scroll-precision-mode 1)
(global-hl-line-mode 1)

;;; end


;;; packages

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;;; end

;;; meow
(use-package meow
  :ensure t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))
;;; end

;;; ef-themes
(use-package ef-themes
  :ensure t
  :config (load-theme 'ef-elea-dark t))


;;; org-static-blog

(use-package org-static-blog
  :ensure t
  :config
  (setq org-static-blog-publish-title "K")
  (setq org-static-blog-publish-url "")
  (setq org-static-blog-publish-directory "~/Repos/blog/")
  (setq org-static-blog-posts-directory "~/Repos/blog/posts/")
  (setq org-static-blog-drafts-directory "~/Repos/blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)

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

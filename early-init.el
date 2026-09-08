;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Emacs 31: site-start.el now loads *before* early-init.el.
;; package.el 必须在这里关掉，Elpaca 才不会和它抢启动。

(setq package-enable-at-startup nil)
(setq inhibit-splash-screen t)

;; typst-ts-mode autoloads call define-compilation-mode (macro in compile.el).
(require 'compile)

(setq default-frame-alist
      (append
       '((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars . nil)
         (alpha-background . 80))
       default-frame-alist))

(when (display-graphic-p)
  (set-face-attribute
   'default nil
   :family "MonoLisa Nasy"
   :height
   (pcase system-type
     ('darwin 150)
     ('windows-nt 140)
     ('gnu/linux 130)
     (_ 130))))

;; system-type 在 macOS 上是 darwin，没有 'macos。
(when (eq system-type 'darwin)
  (dolist (entry '((undecorated-round . t)
                   (alpha . 80)))
    (add-to-list 'default-frame-alist entry))

  (condition-case err
      (let ((path
             (with-temp-buffer
               (insert-file-contents-literally (expand-file-name "~/.path"))
               (string-trim (buffer-string)))))
        (setenv "PATH" path)
        (setq exec-path
              (append (parse-colon-path path)
                      (list exec-directory))))
    (error
     (warn "%s" (error-message-string err)))))

;;; early-init.el ends here

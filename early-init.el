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

(let ((size (pcase system-type
              ('darwin 15)
              ('windows-nt 14)
              ('gnu/linux 13)
              (_ 13)))
      (font "MonoLisa Nasy"))
  (dolist (alist '(default-frame-alist initial-frame-alist))
    (setf (alist-get 'font (symbol-value alist))
          (format "%s-%d" font size))))

;; 英文仍用 MonoLisa Nasy；仅 Windows GUI 把汉字落到微软雅黑。
(when (eq system-type 'windows-nt)
  (defun my/windows-cjk-font (&optional frame)
    (let ((frame (or frame (selected-frame))))
      (when (display-graphic-p frame)
        (dolist (charset '(han cjk-misc))
          (set-fontset-font t charset "Microsoft YaHei" frame)))))
  (add-hook 'after-make-frame-functions #'my/windows-cjk-font)
  (add-hook 'window-setup-hook #'my/windows-cjk-font))

(when (memq system-type '(darwin windows-nt))
  (add-to-list 'default-frame-alist '(alpha . 80)))

;; Windows emacs -nw：w32console 会设成 OEM（常为 cp936），
;; ▶ / … 等编不进去就会显示成 \u25B6。TTY 改为 UTF-8。
(when (eq system-type 'windows-nt)
  (defun my/windows-tty-utf-8 ()
    (when (fboundp 'w32-set-console-output-codepage)
      (w32-set-console-output-codepage 65001))
    (when (fboundp 'w32-set-console-codepage)
      (w32-set-console-codepage 65001))
    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8))
  (add-hook 'tty-setup-hook #'my/windows-tty-utf-8))

;; system-type 在 macOS 上是 darwin，没有 'macos。
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated-round . t))

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

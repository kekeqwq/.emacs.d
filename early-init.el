;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Emacs 31: site-start.el now loads *before* early-init.el.
;; package.el 必须在这里关掉，Elpaca 才不会和它抢启动。

(setq package-enable-at-startup nil)
(setq inhibit-splash-screen t)

(setq default-frame-alist
      (append
       '((menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars . nil)
         (alpha-background . 80))
       default-frame-alist))

(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("MonoLisa Nasy" . 15) ("Monaco" . 13) ("Menlo" . 13)))
   ((eq system-type 'windows-nt)
    '(("Iosevka Term Curly" . 14) ("Consolas" . 12) ("Cascadia Mono" . 11)))
   (t
    '(("MonoLisa Nasy" . 11) ("SF Mono" . 11) ("Consolas" . 12))))
  "Fonts to try.  First available entry is used for the default face.")

(add-to-list 'default-frame-alist
             (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))
(set-fontset-font t 'unicode "Iosevka Term Curly")

;; system-type 在 macOS 上是 darwin，没有 'macos。
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(alpha . 80))
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

;;; early-init.el ends here

;;; user interface

(setq package-enable-at-startup nil)
(setq inhibit-splash-screen t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(alpha-background . 80) default-frame-alist)

;;; end


;;; font

(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("MonoLisa Nasy" . 15) ("Monaco" . 13) ("Menlo" . 13)))
   ((eq system-type 'windows-nt)
    '(("Iosevka Term Curly" . 14) ("Consolas" . 12) ("Cascadia Mono" . 11)))
   (t
    '(("MonoLisa Nasy" . 11) ("SF Mono" . 11) ("Consolas" . 12))))
  "List of fonts and sizes.  The first one available will be used.")

(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))

(set-fontset-font t 'unicode "Iosevka Term Curly")
;;; end


(if (or (eq system-type 'darwin) (eq system-type 'macos))
    (progn
      (push '(alpha . 80) default-frame-alist)
      (condition-case err
	  (let ((path (with-temp-buffer
			(insert-file-contents-literally "~/.path")
			(buffer-string))))
	    (setenv "PATH" path)
	    (setq exec-path (append (parse-colon-path path) (list exec-directory))))
	(error (warn "%s" (error-message-string err))))))

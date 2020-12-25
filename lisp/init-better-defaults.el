;; configs to improve the default settings



;; bind the ~/.emacs.d/init.el to F1 keys
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; config for yes=y, no=n
(fset 'yes-or-no-p 'y-or-n-p)

;; avoid the warning: There is an existing Emacs server, named "server".xs
(require 'server)
(or (server-running-p)
    (server-start))

(setq make-backup-files nil)
(setq auto-save-default nil)

;; enable recent files in the buffers
(require 'recentf)
(recentf-mode 1)
(setq recent-max-menu-file-item 15) ;;set number of items in recent files.


;;highlight matching parenthesis
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; define advice named fix-show-paren-function
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "highlight enclosing parens"
  (cond ((looking-at-p "\\s(") (funcall fn)) ;; looking for (, if found call function fn
	 (t (save-excursion ;; fix cursor
	      (ignore-errors (backward-up-list))
	      (funcall fn)))))

;;global-auto-revert-mode is an interactive autoloaded lisp function
;; when init file is modified by other sources, it autoload the modifications
(global-auto-revert-mode t)

;; turn off the ring bell for error
(setq ring-bell-function 'ignore)


(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ("zy" "Zhou Yang")
					    ))

;; indent region or buffer
(defun indent-buffer ()
  "Indent current buffer"
  (interactive)
  (indent-region (point-min) (point-max)))
(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p);; p:predicate
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented buffer.")))))


;; config for dired-mode-always copy or delete recursively
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)
;; dired-mode: disable multi dired buffer
;; dired-reuse current buffer by pressign 'a'
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired-x)


;; remove dos end of line
(defun remove-dos-eol ()
  "replace DOS eols with UNIX CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; allows M-s o to search the word at cursor
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; accurate search and goto in js mode;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun js2-imenu-make-index ()
      (interactive)
      (save-excursion
        ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
        (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'js2-imenu-make-index)))
(global-set-key (kbd "M-s i") 'counsel-imenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; change the color of selected text: 64ff44
(set-face-attribute 'region nil :background "#1e8619" :foreground "#ffffff")
;; change the cursor color: 5fec66 green
(set-cursor-color "#e31c00")
(setq default-cursor-type 'box)

;; set the encoding as uft-8
(set-language-environment "UTF-8")

;; org capture: capture contents from web pages
(defun zhou/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
      (let ((result (do-applescript
                     (concat
                      "set frontmostApplication to path to frontmost application\n"
                      "tell application \"Google Chrome\"\n"
                      " set theUrl to get URL of active tab of first window\n"
                      " set theResult to (get theUrl) \n"
                      "end tell\n"
                      "activate application (frontmostApplication as text)\n"
                      "set links to {}\n"
                      "copy theResult to the end of links\n"
                      "return links as string\n"))))
        (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))






(provide 'init-better-defaults)

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


(provide 'init-better-defaults)

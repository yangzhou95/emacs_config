

(package-initialize)

;; enable configs from org mode file: using configs in zhou_emacs_config.org
(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "zhou_emacs_config.org" user-emacs-directory))

;; add load-path for config files in .emacs/lisp/
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )



(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-hippie-expand)
(require 'init-latex)
(require 'init-locales)
(require 'custom)
(require 'init-keybindings)
;; save the user customized configs in custom.el
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
;; load lisp file custom-file
(load-file custom-file)
(put 'upcase-region 'disabled nil)

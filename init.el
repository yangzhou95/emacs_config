

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )



(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-keybindings)
(require 'org)
(require 'init-hippie-expand)
(require 'init-latex)
(require 'init-locales)
(require 'custom)
;; save the user customized configs in custom.el
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
;; load lisp file custom-file
(load-file custom-file)
(put 'upcase-region 'disabled nil)

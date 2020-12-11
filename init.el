

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
(require 'custom)


(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

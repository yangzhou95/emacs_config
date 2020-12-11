(setq ns-pop-up-frames nil)
(set-keyboard-coding-system nil)
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

;; settings for the cdlatex
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp/")
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;;set the appearance for emacs

(tool-bar-mode -1);;disable tool bar on top

;; set full screen when initialize. initial-frame-alist is a variable defined in ‘frame.el’.
;;Its value is ((fullscreen . maximized)).Original value was nil
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(set-face-attribute 'default nil :height 130) ;; set font size for emacs
(setq inhibit-splash-screen t) ;; disable the help landing page


(linum-mode t);; line number
(global-linum-mode t);; line number mode
(setq-default cursor-type 'bar)

;; maxmize windows
;; Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Start fullscreen (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)


;;highlights current line
(global-hl-line-mode 1)

(linum-mode t);; line number
(global-linum-mode t);; line number mode





(provide 'init-ui)

;; keybiding configs
;; ---------customized keybindings start with C-c ----------

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;set find-function, find-variable, find-function-on-key
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

;;keybindings for hippie
(global-set-key (kbd "s-/") 'hippie-expand)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Find file in the current Git repository.
(global-set-key (kbd "C-c p f") 'counsel-git)



(provide 'init-keybindings)

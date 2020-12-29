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
;; (global-set-key (kbd "C-c p f") 'counsel-git)

;; indent region or buffer
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)


;;hippi expand
(global-set-key (kbd "s-/") 'hippie-expand);; s stands for command on Mac os

;; dired-mode: disable multi dired buffer
;; dired-reuse current buffer by pressign 'a'
(require 'dired);; since the define-key must be under the dire-mode, we should require it first
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(with-eval-after-load 'dired ;; avoid using (require 'dired) and (define-key ....) to accelerate init
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-;") 'iedit-mode)

;; r aka remember: keybindings for org-capture
(global-set-key (kbd "C-c r") 'org-capture)


;; change the M-n/p for selection
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


;;helm-ag: search
;;(global-set-key (kbd "C-c p s") 'helm-do-ag-project-root)


(global-set-key (kbd "C-z") 'evil-emacs-state)

;;evil keybiding
(evil-leader/set-key
  "ff" 'find-file
  "fr" 'recentf-open-files
  "bb" 'switch-to-buffer
  "bk" 'kill-buffer
  "ps" 'helm-do-ag-project-root
  "0"  'select-window-0
  "1"  'select-window-1
  "2"  'select-window-2
  "3"  'select-window-3
  "w/" 'split-window-right
  "w-" 'split-window-below
  ":"  'counsel-M-x
  "wm" 'delete-other-windows
  "qq" 'save-buffers-kill-terminal
  ) 



;; enable the evil commenter in both evil-mode and non-evil mode
(evilnc-default-hotkeys)
(define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)














(provide 'init-keybindings)




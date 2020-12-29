;;this is the package-related configs

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;set the Melpa sources
(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu_china"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa_china" . "http://elpa.emacs-china.org/melpa/"))))

;; cl - Common Lisp Extension
(require 'cl)

 ;; Add Packages to my/packages (the packages you want)
(defvar my/packages '(
		;; --- Auto-completion ---
		company
		;; --- Better Editor -
		hungry-delete
		smex
		swiper
		counsel
		smartparens
		company-tabnine
		;; --- Major Mode ---
		js2-mode
		;; --- Minor Mode ---
		nodejs-repl
		exec-path-from-shell
		;; --- Themes ---
		dracula-theme
		auctex
		auctex-latexmk
		;;windows management
		popwin
		reveal-in-osx-finder
		web-mode ;; for html
		expand-region
		iedit
		htmlize ;; org file convert to html
		org-pomodoro ;; gtd
		helm-ag;; fast search
		flycheck ;; syntax checking
		yasnippet;; code snippets
		auto-yasnippet ;; code snippets
		evil ;; evil mode
		evil-leader;;
		window-numbering ;; use M-1(9) to control windows
		;;powerline-evil ;; statusline
		evil-surround;;
		evil-nerd-commenter;;comment code
		which-key
		) "my default packages")
;;Store here packages installed explicitly by user.
(setq package-selected-packages my/packages)
;; install packages in the var: my/packages
(defun my/packages-installed-p ()
  ;; loop for is from (require 'cl)
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (returnnil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))

;; add a theme to my/packages-it will install
(add-to-list 'my/packages 'monokai-theme)

;;load theme when you open emacs
(load-theme 'monokai 1)
(load-theme 'dracula 1)

;;config for the smartparens 
(require 'smartparens-config);;enable default config
(smartparens-global-mode t)
;; in emacs lisp mode, when typing "'",it won't add another ","
(sp-local-pair 'emacs-list-mode "'" nil :actions nil)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)



;;config for popwin
(require 'popwin)
(popwin-mode 1)
(setq popwin:popup-window-position 'right)
(setq popwin:popup-window-width 0.5)

(global-company-mode 1) ;;enable COMPlete ANY thing mode
;; some variables are 'buffer-local", each buffer is allowed to have a separate value for that variable that override teh global default.
;; For buffer-local variable, setq sets its local value in current buffer
;; setq-default sets the global default value
;; if a variable is not buffer-local, setq=setq-default

;; enable recent files in the buffers
(require 'recentf)
(recentf-mode 1)
(setq recent-max-menu-file-item 15) ;;set number of items in recent files.


(delete-selection-mode 1);;when selected, insertion will replate the orig




;; for hungry delte
(require 'hungry-delete)
(global-hungry-delete-mode)

;;config for swiper
(ivy-mode 1);; enable Ivy completion everywhere
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)

;;quit the minibuffers with ESC key
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
;; set the minibuffer hight


;; disable autosave
(setq auto-save-default nil)

;;config for js2-mode
(setq auto-mode-alist
      (append '(("\\.js\\'" . js2-mode))  auto-mode-alist)
      )

;;set JS IDE
(setq auto-mode-alist ;; auto-mode-alist is used to associate major mode with different types of file matching by regular expression
      (append
       '(("\\.js\\'" . js2-mode);; use js2-mode to replate the default major mode
	 ("\\.html\\'" . web-mode)) ;; use web-mode to open html
       auto-mode-alist))
;;config for js requiring nodejs and outpu error messages on Macq
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)) ;; enable emacs to find nodejs on Mac
;; need to install nodejs on Mac



;; config for smex-enhance M-x
;;To auto-start Smex every time you open Emacs add these lines to your .EMACS FILE:
(require 'smex) ;; NOT NEEDED IF YOU USE PACKAGE.EL
(smex-initialize) ;; CAN BE OMITTED. THIS MIGHT CAUSE A (MINIMAL) DELAY
;; WHEN SMEX IS AUTO-INITIALIZED ON ITS FIRST RUN.BIND SOME KEYS:

;;You can now use smartparens with M-x smartparens-mode.
;;To automatically enable smartparens in a programming mode:
;; Always start smartparens mode in js-mode.
(add-hook 'js-mode-hook #'smartparens-mode)



;; porodoro for gtd
(require 'org-pomodoro)


;; flycheck
;;(global-flycheck-mode)
(add-hook 'js2-mode 'flycheck-mode)

;; configs for snippets
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode) ;; only for prog-mode


;; activate gloabal-evil-leader-mode before activating evil-mode; otherwise, evil-mode won't work on severl init buffers
;; such as *scratch*, *Message*
(global-evil-leader-mode)
;; configs for evil mode
(evil-mode 1)
;; empty the key-mapping in the "insert state map", enable the Fallback to the emacs states
;; such that the keybindings in the Emacs won't be covered by the "evil insert minor mode state"
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)


;;window-numbering mode
(window-numbering-mode 1)

;; evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; powerline is a statusline plugin for vim.
;;(require 'powerline)
;;(powerline-default-theme)
;;(powerline-center-theme)
;; (require 'powerline-evil)


;; Enable window-numbering-mode and use M-1 through M-0 to navigate.
(setq window-numbering-assign-func
      (lambda () (when (equal (buffer-name) "*Calculator*") 9)))



;; configs for which-key
(which-key-mode 1)
(which-key-setup-side-window-right) 


(add-hook 'occur-mode-hook
	  (lambda()
	    (evil-add-hjkl-bindings occur-mode-map 'emacs
	      (kbd "/") 'evil-search-forward
	      (kbd "n") 'evil-search-next
	      (kbd "N") 'evil-search-previous
	      (kbd "C-d") 'evil-scroll-down
	      ) ))



;; use emacs as the default mode 
(dolist (mode '(ag-mode
		flycheck-error-list-mode
		occur-mode
		git-rebase-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;;
(provide 'init-packages)

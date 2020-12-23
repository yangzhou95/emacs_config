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
		;; solarized-theme
		) "my default packages")
;;Store here packages installed explicitly by user.
(setq package-selected-packages my/packages)
;; install packages in the var: my/packages
(defun my/packages-installed-p ()
  ;; loop for is from (require 'cl)
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
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
       '(("\\.js\\'" . js2-mode));; use js2-mode to replate the default major mode
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





(provide 'init-packages)

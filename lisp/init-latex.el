;;;;;;;;;;;;;;;;;; configs for the latex;;;;;;;;;;;;;;;;;;
;;add to path
(setenv "PATH"
	(concat (getenv "PATH") ":/Library/TeX/texbin:/usr/local/bin"))

;; config the latex
;;If you want to make AUCTeX aware of style files and multi-file documents right away, insert the following in your ‘.emacs’ file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up latex mode. Forward/inverse search with evince using D-bus.
;; Tells emacs where to find LaTeX.
(let ((my-path (expand-file-name "/usr/local/bin:/usr/local/texlive/2020/bin/x86_64-darwin")))
    (setenv "PATH" (concat my-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-path))

(custom-set-variables
     '(TeX-source-correlate-mode t)
     '(TeX-source-correlate-start-server t))
     '(TeX-source-correlate-method 'synctex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;set for cdlatex
(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp")
(require 'cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
(add-hook 'LaTex-mode-hook 'turn-on-reftex)

;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; This line tells emacs to create pdf files instead of dvi files.
(setq-default TeX-PDF-mode t)
;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ; auto enable cdlatex-mode when use auctex
(setq reftex-plug-into-AUCTeX t)
;; add "-shell-escape" option to LaTeX command, which is needed by packages like minted
(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
           '("%`%l%(mode) -shell-escape%' %t"
             TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
           )
  )




;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
(server-start); start emacs in server mode so that skim can talk to it
 

;;;;;;;;;;;;; config from skim's offical sites
;; The following only works with AUCTeX loaded
(require 'tex-site)


;; enable the smartparens in latex-mode
(add-hook 'text-mode-hook 'smartparens-mode)



;; start emacs in server mode so that skim can talk to it
(server-start)
'(LaTeX-command "latex -synctex=1")




(add-hook 'LaTeX-mode-hook 'outline-minor-mode)



;; use arara to compile, which indiate the errors in ``runscript.tlu''
;; by pressing ``C-c `'''
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
        '("Arara" "arara %s" TeX-run-TeX nil t :help "Run Arara.")))





(provide 'init-latex)

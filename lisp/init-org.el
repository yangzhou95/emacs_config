;; config for org mode
;;----------- config for org -------------
;;highligh for org mode
(require 'org)


;;config for org-agenda
(setq org-agenda-files '("~/org"))

;; configs in org mode
(with-eval-after-load 'org
  (setq org-agenda-files '("~/org/"))
  (setq org-src-fontify-natively t)
  ;; set templates for todo
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "schedule")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)
	("c" "Chrome"entry (file+headline "~/org/org_notes/notes.org" "quick notes")
	 "* TODO [#C] %?\n %(zhou/retrieve-chrome-current-tab-url) \n %i\n %U"
	 :empty-lines 1)

	))
)


;;------------end config for org--------



;;如果不想每次都在 org 文件头指定，可以在 Emacs 配置文件中进行如下设定：#+LATEX_CMD: xelatex
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))










(provide 'init-org)

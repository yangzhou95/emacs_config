;; config for org mode
;;----------- config for org -------------
;;highligh for org mode
(require 'org)
(setq org-src-fontify-natively t)

;;config for org-agenda
(setq org-agenda-file '("~/org"))

(provide 'org)

;;------------end config for org--------



;;如果不想每次都在 org 文件头指定，可以在 Emacs 配置文件中进行如下设定：#+LATEX_CMD: xelatex
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))


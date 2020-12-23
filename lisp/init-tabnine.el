;; config for tabnine comletion
(add-to-list 'company-backends #'company-tabnine)

(def-package! company-tabnine
  :when (featurep! :completion company)
  :config
  (set-company-backend! '(c-mode
                          c++-mode
                          java-mode
                          haskell-mode
                          emacs-lisp-mode
                          lisp-mode
                          sh-mode
                          perl-mode
                          php-mode
                          python-mode
                          go-mode
                          ruby-mode
                          rust-mode
                          js-mode
                          css-mode
                          web-mode
                          org-mode)
    '(company-tabnine))

  ;; Trigger completion immediately.
  ;; (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  )

;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq +format-on-save-enabled-modes '(emacs-lisp-mode nix-mode))

(use-package! which-func
  :defer t
  :commands which-function)

(after! company
  (setq company-idle-delay 0.2)
  (setq company-format-margin-function #'company-detect-icons-margin)
  (setq  company-show-quick-access t))
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
;; improve memory
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; ess
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP & DAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use format-all by default
(setq +format-with-lsp nil)

(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 3000)
  (dolist (dir '("[/\\\\]\\.ccls-cache$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]\\.pytest_cache$"
                 "[/\\\\]\\.cache$"
                 "[/\\\\]\\.clwb$"
                 "[/\\\\]_build$"
                 "[/\\\\]__pycache__$"
                 "[/\\\\]bazel-bin$"
                 "[/\\\\]bazel-code$"
                 "[/\\\\]bazel-genfiles$"
                 "[/\\\\]bazel-out$"
                 "[/\\\\]bazel-testlogs$"
                 "[/\\\\]third_party$"
                 "[/\\\\]third-party$"
                 ))
    (push dir lsp-file-watch-ignored-directories))
  )

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))

(add-hook! 'lsp-mode-hook (lsp-headerline-breadcrumb-mode 1))

(use-package lsp-docker
  :defer t
  :commands lsp-docker-init-clients
  :config
  (defvar lsp-docker-client-packages
    '(lsp-css lsp-clients lsp-bash lsp-go lsp-pyls lsp-html lsp-typescript
              lsp-terraform lsp-cpp))

  (defvar lsp-docker-client-configs
    (list
     (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
     (list :server-id 'clangd :docker-server-id 'clangd-docker :server-command "ccls")
     (list :server-id 'css-ls :docker-server-id 'cssls-docker :server-command "css-languageserver --stdio")
     (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio")
     (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
     (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "html-languageserver --stdio")
     (list :server-id 'pyls :docker-server-id 'pyls-docker :server-command "pyls")
     (list :server-id 'ts-ls :docker-server-id 'tsls-docker :server-command "typescript-language-server --stdio")))

  ;; (lsp-docker-init-clients
  ;;  :path-mappings `((,(file-truename "~/av") . "/code"))
  ;;  ;; :docker-image-id "my-lsp-docker-container:1.0"
  ;;  :client-packages '(lsp-pyls)
  ;;  :client-configs lsp-docker-client-configs)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! realgud (advice-remove #'realgud:terminate #'+debugger--cleanup-after-realgud-a))

(defun +my/dap-start ()
  (interactive)
  (dap-mode 1)
  (call-interactively #'dap-debug))

;; (add-hook! dap-mode-hook ((tooltip-mode 1)))

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-segments '(file symbols)))

(after! dap-mode
  ;; (setq dap-auto-configure-features '(sessions locals expressions controls tooltip))
  (setq lsp-enable-dap-auto-configure nil)

  ;; use M-u to exit dap-hydra
  ;; (after! dap-hydra
  ;;   (defhydra+ dap-hydra () ("M-u" nil)))

  ;; Toggle dap-hydra whenever breakpoint is triggered
  ;; (add-hook 'dap-stopped-hook
  ;;           (lambda (arg) (call-interactively #'dap-hydra)))
  )

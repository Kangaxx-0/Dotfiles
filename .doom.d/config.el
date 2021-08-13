;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(load! "+programmer")
(load! "+ui")
(load! "+misc")
(load! "+bindings")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Frank Zhang"
      user-mail-address "damn.frank2z@gmail.com")

;; Set projectile project search path
(setq projectile-project-search-path '("~/CommunityLeader/" "~/Repo/"))

;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))

;; ;; LSP keybinding
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook (lsp-mode . efs/lsp-mode-setup)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (lsp-enable-which-key-integration t))

;; ;; tab -> autocompletion
;; (use-package company
;;   :after lsp-mode
;;   :hook (lsp-mode . company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))
;;
;;


;; https://tecosaur.github.io/emacs-config/config.html
;; (setq-default
;;  delete-by-moving-to-trash t                      ; Delete files to trash
;;  window-combination-resize t                      ; take new window space from all other windows (not just current)
;;  x-stretch-cursor t)                              ; Stretch cursor to the glyph width


;; (unless (string-match-p "^Power N/A" (battery))   ; On laptops...
;;   (display-battery-mode 1))                       ; it's nice to know how much power you have

;; (global-subword-mode 1)                           ; Iterate through CamelCase words

;; Kjkey variants of the window navigation/swapping commands.
;; (map! :map evil-window-map
;;       "SPC" #'rotate-layout
;;       ;; Navigation
;;       "<left>"     #'evil-window-left
;;       "<down>"     #'evil-window-down
;;       "<up>"       #'evil-window-up
;;       "<right>"    #'evil-window-right
;;       ;; Swapping windows
;;       "C-<left>"       #'+evil/window-move-left
;;       "C-<down>"       #'+evil/window-move-down
;;       "C-<up>"         #'+evil/window-move-up
;;       "C-<right>"      #'+evil/window-move-right)

;; (use-package abbrev
;;   :init
;;   (setq-default abbrev-mode t)
;;   ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
;;   ;; whenever the major mode is a text mode
;;   (defun tec/set-text-mode-abbrev-table ()
;;     (if (derived-mode-p 'text-mode)
;;         (setq local-abbrev-table org-mode-abbrev-table)))
;;   :commands abbrev-mode
;;   :hook
;;   (abbrev-mode . tec/set-text-mode-abbrev-table)
;;   :config
;;   (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
;;   (setq save-abbrevs 'silently))

;; Centaur Tab
;; (after! centaur-tabs
;;   (centaur-tabs-mode -1)
;;   (setq centaur-tabs-height 20
;;         centaur-tabs-set-icons t
;;         centaur-tabs-modified-marker "o"
;;         centaur-tabs-close-button "×"
;;         centaur-tabs-set-bar 'above
;;         centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)


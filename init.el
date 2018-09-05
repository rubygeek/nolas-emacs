(menu-bar-mode   -1)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package diminish
  :ensure t
  :config
  (diminish 1)
  )

;; instead of ESC for evil mode use tt 
(use-package key-chord
  :ensure t
  :init
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "tt" 'evil-normal-state)
  :config
  (key-chord-mode 1))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (setq cider-show-error-buffer t
	ceinter-auto-select-error-buffer t)
  )

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :ensure t
  :init (auto-dim-other-buffers-mode 1)
  )

(use-package clojure-mode-extra-font-locking
  :after clojure-mode
  :ensure t
  )

(use-package paredit
  :ensure t
  )

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  )

(use-package magit
  :ensure t
  )

;; enhances M-x
(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex)
  )

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package highlight-parentheses
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
  )

(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
  helm-mode-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t
  helm-locate-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-candidate-number-list 150
  helm-split-window-in-side-p t
  helm-move-to-line-cycle-in-source t
  helm-echo-input-in-header-line t
  helm-autoresize-max-height 0
  helm-autoresize-min-height 20)
  :config
  (helm-mode 1))


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Hello Nola") ; print a default message in the empty scratch buffer opened at startup

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "color-234")))))

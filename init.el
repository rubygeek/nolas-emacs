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


(setq use-package-always-ensure t)      ; always install packages 
(setq sentence-end-double-space nil)    ; sentence SHOULD end with only a point.
(setq default-fill-column 80)           ; toggle wrapping text at the 80th character
(setq initial-scratch-message "Hello Nola") ; print a default message in the empty scratch buffer opened at startup

(use-package diminish
  :config
  (diminish 1)
  )

;(use-package evil
;  :diminish  undo-tree-mode
;  :config
;  (evil-mode 1)
;  )

;; instead of ESC for evil mode use tt 
;(use-package key-chord
;  :init
;  (setq key-chord-two-keys-delay 0.2)
;  (key-chord-define evil-insert-state-map "tt" 'evil-normal-state)
;  :config
;  (key-chord-mode 1)
;  )

(use-package cider
  )
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (setq cider-show-error-buffer t
	ceinter-auto-select-error-buffer t)
  )

(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :init (auto-dim-other-buffers-mode 1)
  )

(use-package clojure-mode-extra-font-locking
  :after clojure-mode
  )

(use-package paredit
  )

(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  )

(use-package ag
  )

;; enhances M-x
(use-package smex
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex)
  )

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package highlight-parentheses
  :init
  (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
  )

(use-package helm
  :diminish helm-mode
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
  (helm-mode 1)
  )


(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  )



 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "color-234")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package smex projectile paredit neotree markdown-mode magit key-chord highlight-parentheses helm evil doom-themes diminish clojure-mode-extra-font-locking auto-dim-other-buffers))))

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

(use-package cider
  )

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (setq cider-show-error-buffer t
	ceinter-auto-select-error-buffer t
	show-paren-delay 0
	)
  (show-paren-mode 1))

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

(use-package rainbow-delimiters 
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

;;(use-package highlight-parentheses
;;  :init
;;  (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
;;  )

(use-package ivy
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

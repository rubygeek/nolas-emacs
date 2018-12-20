
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

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


(use-package diminish
  :config
  (diminish 1))

;; Used to select a group of text and +/- to grab bigger or smaller chunks
(use-package expand-region
  :init
  (global-unset-key (kbd "C-\\"))
  (global-set-key   (kbd "C-\\") 'er/expand-region))

;; For clojure <3
(use-package cider)

;; Multiple points of typing
(use-package multiple-cursors
  :init (progn (setq mc/list-file "./.mc-lists.el"))
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; For Clojure duh
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (setq cider-show-error-buffer t
	ceinter-auto-select-error-buffer t
	show-paren-delay 0)
  (show-paren-mode 1))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package paredit
  :after clojure-mode)


;; Dim the non active buffer
(use-package auto-dim-other-buffers
  :diminish auto-dim-other-buffers-mode
  :init    (auto-dim-other-buffers-mode 1))

;; Get around a project, make sure you have a .projectile empty file
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'hybrid)  
  (setq projectile-enable-caching t)
  (projectile-mode +1))

;; Find inside of packages
(use-package ag)

;; fuzzy matching
(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; enhances M-x
(use-package smex
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex))

;; like nerdtree for vim
(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle))

;; highlights delimitors in rainbows
(use-package rainbow-delimiters 
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; For Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; For navigating around a project
(use-package projectile
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config
  (projectile-mode +1))

(use-package highlight-symbol
  :bind ("<C-return>" . highlight-symbol))


;; from Bryan

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil)))


;; Themes https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  (load-theme 'doom-vibrant t)
  )

;; dont make backup files
(setq make-backup-files nil)


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
    (zprint-mode magit key-chord highlight-parentheses helm diminish clojure-mode-extra-font-locking))))


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

(use-package which-key
  :config
  (diminish 1)
  (which-key-mode 1))

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
	show-paren-delay 0
	cider-default-cljs-repl 'shadow)
  (show-paren-mode 1))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package paredit
  :after clojure-mode)

(use-package company
  :config
  (diminish 1)
  :init
  (global-company-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

  
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
 '(ansi-color-names-vector
   ["#1c1f24" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#DFDFDF"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes
   (quote
    ("b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" default)))
 '(fci-rule-color "#62686E")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(objed-cursor-color "#ff665c")
 '(package-selected-packages
   (quote
    (undo-tree json-navigator zprint-mode magit key-chord highlight-parentheses helm diminish clojure-mode-extra-font-locking)))
 '(pdf-view-midnight-colors (cons "#242730" "#bbc2cf"))
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))

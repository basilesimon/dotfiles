;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "Input-12"))
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 80))

;; Package configs
;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# filesrequire 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

;; Vim mode
;; first line is witchcraft
;; and fixes tab to expand in org mode
(setq evil-want-C-i-jump nil)
(use-package evil
  :ensure t
  :config
  (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(box "white")
      evil-emacs-state-cursor '(box "white")
      evil-normal-state-cursor '(box "#97C150"))
  (evil-mode 1))

;; relative line numbering
(use-package nlinum
  :ensure t
  :config (add-hook 'prog-mode-hook '(lambda ()
				       (nlinum-mode t))))

(use-package evil-surround
  :ensure t
  :config
  (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

;; Which Key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virutal-bufferst t)
  (setq ivy-height 10))

;; counsel
(use-package counsel
  :requires ivy
  :ensure t)

;; NeoTree
(use-package neotree
  :config
  (progn
  ;;  will try to find current file and jump to node.
  (setq-default neo-smart-open t)
  ;; Do not allow neotree to be the only open window
  (setq-default neo-dont-be-alone t))
  (setq neo-show-hidden-files t)
  (progn
    (setq neo-theme 'nerd))
  (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "gr") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "R") 'neotree-change-root)
                (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
                (define-key evil-normal-state-local-map (kbd "C") 'neotree-copy-node)
                (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
                (define-key evil-normal-state-local-map (kbd "K") 'neotree-select-up-node)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

(use-package ox-reveal
  :ensure ox-reveal)

(use-package htmlize
  :ensure t)

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (setq org-directory "~/org")
    (setq org-todo-keywords
	  '((sequence "TODO" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . "#dc322f") ("DONE" . "#859900")))
  (setq org-agenda-files (list "~/org/org.org"))))

(use-package solarized-theme
  :ensure t
  :init
  (progn
    (setq solarized-use-less-bold t
	  solarized-emphasize-indicators nil)
    (load-theme 'solarized-dark t)))
;; Spaceline - A mode line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator 'arrow-fade)
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'utf-8)
  :config
  (progn
    (spaceline-spacemacs-theme)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-hud-off)
    (spaceline-toggle-version-control-on)
    (spaceline-toggle-org-clock-on)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-line-column-off)))
;; powerline
;; (use-package powerline
;;   :init
;;   (powerline-evil-vim-color-theme)
;;   :config
;;   (setq powerline-default-separator 'utf-8))
;; (add-hook 'after-init-hook 'powerline-reset)
;; 
;; (use-package powerline-evil
;;     :ensure powerline-evil
;;     :demand powerline-evil)

(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-responsive t)
  (setq indent-guide-recursive t))

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (use-package evil-magit
    :ensure t
    :config
    (add-hook 'magit-mode-hook 'evil-local-mode)
    (add-hook 'git-rebase-mode-hook 'evil-local-mode)
    (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
        (let ((mm-key ","))
          (dolist (state '(normal motion))
            (evil-define-key state with-editor-mode-map
              (concat mm-key mm-key) 'with-editor-finish
              (concat mm-key "a")    'with-editor-cancel
              (concat mm-key "c")    'with-editor-finish
	      (concat mm-key "k") 'with-editor-cancel))))
  (use-package magit-popup
    :ensure t))

;; insert closing parens automatically
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package autorevert
  :diminish auto-revert-mode)
(use-package undo-tree
  :diminish undo-tree-mode)

;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/private/snippets"))
  (yas-global-mode 1))

(use-package prettier-js
  :ensure t
  :config
  (progn
  ;; Set JS Prettier plugin
  (setq prettier-js-args '(
                           "--trailing-comma" "es5"
                           "--bracket-spacing" "true"
                           "--single-quote" "true"
			   ))))

(use-package js2-mode
  :ensure t
  :interpreter (("node" . js2-mode))
  :mode (("\\.js$" . js2-mode))
  :config
  (progn
    (add-hook 'js2-mode-hook 'prettier-js-mode)))

(defun bs/load-init ()
  "Reloads init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(counsel-M-x :which-key "M-x")
  "ff"  '(counsel-find-file :which-key "find files")
  "ft"  '(neotree-toggle :which-key "neotree")
  "fed" '((lambda () (interactive) (counsel-find-file "~/dotfiles/init.el")) :which-key "edit init")
  "feR" '(bs/load-init :which-key "reload init")
  ;; Buffers
  "bb"  '(ivy-switch-buffer :which-key "buffers list")
  "bd"  '(kill-this-buffer :which-key "kill current buffer")
  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  ;; Org
  "mI" '(org-clock-in :which-key "org clock in")
  "mO" '(org-clock-out :which-key "org clock out")
  "me" '(org-export-dispath :which-key "org despatch")
  ;; Others
  "gs" '(magit-status :which-key "git status")
  "gm" '(magit-dispatch-popup :which-key "git status")
  "t"  '(counsel-load-theme :which-key "change theme")
  "ll" '(nlinum-mode :which-key "toggle line numbering")
))


;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (R . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (diminish powerline-evil telephone-line highlight-indent-guides ivy which-key use-package neotree general evil all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:foreground "#cb4b16" :height 1.1 :family "Input"))))
 '(org-level-2 ((t (:foreground "#859900" :height 1.1 :family "Input"))))
 '(org-level-3 ((t (:foreground "#268bd2" :height 1.1 :family "Input"))))
 '(org-level-4 ((t (:foreground "#268bd2" :height 1.1 :family "Input"))))
 '(show-paren-match ((t (:background "gold"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))

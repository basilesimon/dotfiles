;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "Input"))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 80))

;; Package configs
;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# filesrequire 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
    (package-initialize)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

;; fix GUI lost with package paths
;;(when (memq window-system '(mac ns x))
;; (exec-path-from-shell-initialize))

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

(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (setq linum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; surround selection with 's'
(use-package evil-surround
  :ensure t
  :config
  (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

;; multiple cursors
(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode  1))

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
  (setq ivy-height 20)
  ;; a single ESC quits virtual buffers
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  )

;; counsel
(use-package counsel
  :requires ivy
  :ensure t)

;; NeoTree
(use-package neotree
  :config
  ;;  will try to find current file and jump to node.
  (setq-default neo-smart-open t)
  ;; Do not allow neotree to be the only open window
  (setq-default neo-dont-be-alone t)
  (setq neo-show-hidden-files t)
  (setq neo-theme 'nerd)
  (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "R") 'neotree-change-root)
                (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
                (define-key evil-normal-state-local-map (kbd "C") 'neotree-copy-node)
                (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
                (define-key evil-normal-state-local-map (kbd "K") 'neotree-select-up-node)
                (define-key evil-normal-state-local-map (kbd "-") 'neotree-enter-horizontal-split)
                (define-key evil-normal-state-local-map (kbd "|") 'neotree-enter-vertical-split)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

;; org-mode exports to reaveal presentations
(use-package ox-reveal
  :ensure ox-reveal)

(use-package htmlize
  :ensure t)

;; diff markers in left-hand side
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :config
  (add-hook 'vc-checkin-hook 'diff-hl-update))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-hook 'org-mode-hook 'olivetti-mode)
  (progn
    (setq org-directory "~/org")
    (setq org-ellipsis " ⤵")
    (setq org-agenda-span 14)
    (setq org-agenda-start-on-weekday nil)
    (setq calendar-week-start-day 1)
    (setq org-todo-keywords
	  '((sequence "TODO" "DONE")))
    (setq org-todo-keyword-faces
          '(("TODO" . "#dc322f") ("DONE" . "#859900")))
    (setq org-agenda-files "~/org")
    (setq org-capture-templates
	  '(("t" "todo" entry (file+headline "~/org/org.org" "todo")
             "* TODO %?\n  %i\n")
	    ("m" "meeting" entry (file+headline "~/org/org.org" "meetings")
             "* TODO %?\n  %i\n")
	    ))))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
	'("◉" "◎" ">" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; solarized
(use-package solarized-theme
  :ensure t
  :init
  (progn
    (setq solarized-use-less-bold t
	  solarized-emphasize-indicators nil)
    (load-theme 'solarized-light t)))

;; Spaceline - A mode line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator 'wave)
  (setq ns-use-srgb-colorspace nil)
; (setq powerline-default-separator 'utf-8)
  (setq powerline-raw " ")
  ; (setq powerline-height 20)
  (setq powerline-text-scale-factor 0.9)
  :config
  (progn
    (spaceline-emacs-theme)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-hud-off)
    (spaceline-toggle-version-control-on)
    (spaceline-toggle-org-clock-on)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-line-column-off)))

(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-responsive nil)
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
  (use-package magit-popup
    :ensure t)
  ;; (use-package transient)
  (setq transient-enable-popup-navigation t)
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
  )

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

(use-package rjsx-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'"
	 "\\.tsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2)
  (electric-pair-mode 1))

(use-package add-node-modules-path
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

(use-package prettier-js
  :config
    (setq prettier-js-args '(
                             "--trailing-comma" "es5"
                             "--bracket-spacing" "true"
                             "--single-quote" "true"
			     ))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(add-to-list 'exec-path "${HOME}/.nvm/versions/node/v10.22.1/bin")

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil)
    (setq-default company-tooltip-minimum-width 15)
    (setq-default company-idle-delay 0.1))
  :diminish company-mode)

(use-package simpleclip :ensure t
  :config
  (simpleclip-mode 1))

;; lorem ipsum insert in copy
(use-package lorem-ipsum
  :ensure t
  :config
  (progn
    (setq-default lorem-ipsum-use-default-bindings)))

;; R mode
;; (use-package ess
;;   :ensure t
;;   :init (require 'ess-site))

;; Olivetti writing environment
(use-package olivetti
  :ensure olivetti
  :config
  (progn
    (setf olivetti-body-width 80)
    (visual-line-mode)))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'linum-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode))

(defun bs/reload-init ()
  "Reloads init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun bs/new-buffer ()
  "Creates new empty buffer"
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer )
    (other-window 1)))

(defun bs/truncate-and-wrap ()
  "Toggles both line truncating and word wrapping"
  (interactive)
  (toggle-truncate-lines)
  (toggle-word-wrap))

(defun bs/find-personal-org ()
  "Loads default org file"
  (interactive)
  (counsel-find-file "~/org/org.org"))

(defun bs/find-init ()
  (interactive)
  (counsel-find-file "~/dotfiles/init.el"))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "SPC" '(counsel-M-x :which-key "M-x")
  "f"   '(:which-key "files")
  "ff"  '(counsel-find-file :which-key "find files")
  "ft"  '(neotree-toggle :which-key "neotree")
  "fe"  '(:which-key "init files")
  "fed" '(bs/find-init :which-key "edit init")
  "feR" '(bs/reload-init :which-key "reload init")
  "feo" '(bs/find-personal-org :which-key "open org.org")
  ;; Buffers
  "b"   '(:which-key "buffers")
  "bb"  '(ivy-switch-buffer :which-key "buffers list")
  "bd"  '(kill-this-buffer :which-key "kill current buffer")
  "bs"  '(bs/new-buffer :which-jey "new buffer")
  ;; Window
  "w"   '(:which-key "window")
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  ;; Org
  "m"  '(:which-key "org")
  "ma" '(org-agenda :which-key "org agenda")
  "mI" '(org-clock-in :which-key "org clock in")
  "mO" '(org-clock-out :which-key "org clock out")
  "mR" '(org-clock-report :which-key "org clock report")
  "me" '(org-export-dispatch :which-key "org despatch")
  "mn" '(org-narrow-to-subtree :which-key "org narrow")
  "mN" '(widen :which-key "org widen")
  "mc" '(counsel-org-capture :which-key "org capture")
  "ms" '(org-schedule :which-key "org schedule")
  ;; cursors
  "c"  '(:which-key "cursors")
  "cu"  '(evil-mc-undo-all-cursors :which-key "undo all")
  "cj"  '(evil-mc-make-cursor-move-next-line :which-key "undo all")
  "ck"  '(evil-mc-make-cursor-move-prev-line :which-key "undo all")
  "cma"  '(evil-mc-undo-all-cursors :which-key "make all")
  ;; Others
  "g"  '(:which-key "magit")
  "gs" '(magit-status :which-key "git status")
  "gm" '(magit-dispatch-popup :which-key "git status")
  "t"  '(:which-key "toggles")
  "tt"  '(counsel-load-theme :which-key "change theme")
  "tr" '(bs/truncate-and-wrap :toggle-truncate-lines :which-key "toggle truncate")
  "to" '(olivetti-mode :which-key"toggle olivetti")
  "tl" '(nlinum-mode :which-key "toggle line numbering")
  "lp" '(lorem-ipsum-insert-paragraphs :which-key "insert lipsum paras")
  "ls" '(lorem-ipsum-insert-sentences :which-key "insert lipsum sentences")
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

;; on mac, make Alt (meta, M-) on the right-hand side Alt key only
;; left key does standard stuff, eg M-[ for curly braces
(setq ns-alternate-modifier 'none)
(setq ns-right-alternate-modifier 'meta)

;; prevent emacs from quitting too easily
(setq confirm-kill-emacs 'y-or-n-p)

;; open org on startup
(find-file "~/org/org.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(line-number-mode nil)
 '(org-agenda-files '("~/org"))
 '(org-babel-load-languages '((emacs-lisp . t) (R . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-archives
   '(("org" . "http://orgmode.org/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(markdown-mode clojure-mode undo-tree yaml-mode spaceline-all-the-icons org-bullets rjsx-mode add-node-modules-path prettier olivetti web-mode darkroom ess lorem-ipsum simpleclip company exec-path-from-shell prettier-js evil-mc nlinum-relative diff-hl diminish powerline-evil telephone-line highlight-indent-guides ivy which-key use-package neotree general evil all-the-icons))
 '(spaceline-all-the-icons-clock-always-visible t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:foreground "#cb4b16" :height 1.0 :family "Input"))))
 '(org-level-2 ((t (:foreground "#859900" :height 1.0 :family "Input"))))
 '(org-level-3 ((t (:foreground "#268bd2" :height 1.0 :family "Input"))))
 '(org-level-4 ((t (:foreground "#268bd2" :family "Input"))))
 '(org-level-5 ((t (:foreground "#268bd2" :height 1.0 :family "Input"))))
 '(show-paren-match ((t (:background "gold"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))

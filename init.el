;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "Input-13"))
(add-to-list 'default-frame-alist '(height . 100))
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
    (setq org-agenda-files (list "~/org/org.org" "~/org/fwd.org"))
    (setq org-capture-templates
	  '(("t" "todo" entry (file+headline "~/org/org.org" "todo")
             "* TODO %?\n  %i\n")
	    ("m" "meeting" entry (file+headline "~/org/org.org" "meetings")
             "* TODO %?\n  %i\n")
	    ))))

;; solarized
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

(use-package web-mode
  :ensure t
  :mode (("\\.svelte$" . web-mode)))

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

(defun bs/load-init ()
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

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(counsel-M-x :which-key "M-x")
  "f"   '(:which-key "files")
  "ff"  '(counsel-find-file :which-key "find files")
  "ft"  '(neotree-toggle :which-key "neotree")
  "fe"  '(:which-key "init files")
  "fed" '((lambda () (interactive) (counsel-find-file "~/dotfiles/init.el")) :which-key "edit init")
  "feR" '(bs/load-init :which-key "reload init")
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
  "tr" '(toggle-truncate-lines :which-key "toggle truncate")
  "tl" '(nlinum-mode :which-key "toggle line numbering")
  "pp" '(prettier-js :which-key "prettier run")
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#88999b"])
 '(beacon-color "#c61b6e")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#11948b")
 '(cua-normal-cursor-color "#596e76")
 '(cua-overwrite-cursor-color "#a67c00")
 '(cua-read-only-cursor-color "#778c00")
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(fci-rule-color "#f4eedb")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#c42475" "#5e65b6"))
 '(highlight-symbol-colors
   ("#ed7ddb24b29e" "#cd82e29fd17c" "#fc9acadfb443" "#d974d4beddd6" "#df07dfc6b349" "#f76ccd6eaf2a" "#d132db91e15a"))
 '(highlight-symbol-foreground-color "#5d737a")
 '(highlight-tail-colors
   '(("#f4eedb" . 0)
     ("#a8b84b" . 20)
     ("#66c1b3" . 30)
     ("#6fa5e7" . 50)
     ("#d6a549" . 60)
     ("#ed6e3e" . 70)
     ("#f46495" . 85)
     ("#f4eedb" . 100)))
 '(hl-bg-colors
   '("#d6a549" "#ed6e3e" "#ff6243" "#f46495" "#837bdf" "#6fa5e7" "#66c1b3" "#a8b84b"))
 '(hl-fg-colors
   '("#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9"))
 '(hl-paren-colors '("#11948b" "#a67c00" "#007ec4" "#5e65b6" "#778c00"))
 '(line-number-mode nil)
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   '("#cc1f24" "#bb3e06" "#a67c00" "#4f6600" "#a8b84b" "#005797" "#11948b" "#c42475" "#5e65b6"))
 '(org-babel-load-languages '((emacs-lisp . t) (R . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(color-theme web-mode lorem-ipsum simpleclip company exec-path-from-shell prettier-js evil-mc nlinum-relative diff-hl diminish powerline-evil telephone-line highlight-indent-guides ivy which-key use-package neotree general evil all-the-icons))
 '(pos-tip-background-color "#f4eedb")
 '(pos-tip-foreground-color "#5d737a")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#778c00" "#f4eedb" 0.2))
 '(term-default-bg-color "#fffce9")
 '(term-default-fg-color "#596e76")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#cc1f24")
     (40 . "#bbbc5a0718b8")
     (60 . "#b1ce6c680f6e")
     (80 . "#a67c00")
     (100 . "#980682770000")
     (120 . "#905f85330000")
     (140 . "#887187c90000")
     (160 . "#802a8a3b0000")
     (180 . "#778c00")
     (200 . "#69798ef83f8d")
     (220 . "#5f5f904353a2")
     (240 . "#51b991a0669e")
     (260 . "#3d7893107927")
     (280 . "#11948b")
     (300 . "#1b098bd9a288")
     (320 . "#1963876fadf3")
     (340 . "#129882fbb95b")
     (360 . "#007ec4")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fffce9" "#f4eedb" "#990001" "#cc1f24" "#4f6600" "#778c00" "#785700" "#a67c00" "#005797" "#007ec4" "#93004d" "#c42475" "#006d68" "#11948b" "#596e76" "#88999b"))
 '(xterm-color-names
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#002b37"])
 '(xterm-color-names-bright
   ["#fffce9" "#bb3e06" "#98a6a6" "#88999b" "#596e76" "#5e65b6" "#5d737a" "#00212b"]))
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

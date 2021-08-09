;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono"))
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

;; Vim mode
;; first line is witchcraft
;; and fixes tab to expand in org mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (setq evil-default-cursor t)
  (setq evil-insert-state-cursor '(box "white")
	evil-emacs-state-cursor  '(box "white")
	evil-normal-state-cursor '(box "#97C150"))
  (evil-mode 1))

;; smooth(er) scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; line numbering
;; absolute and enabled by default,
;; also relative
(use-package nlinum
  :hook (prog-mode . nlinum-mode))

(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (setq linum-relative-redisplay-delay 0)
  :hook (prog-mode . nlinum-relative-mode))

;; surround selection with 's'
(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  :init
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))

;; multiple cursors
(use-package evil-mc
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

;; rainbow delimters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-use-virutal-bufferst t)
  (setq ivy-height 20)
  ;; a single ESC quits virtual buffers
  :bind (([escape] . minibuffer-keyboard-quit)))

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

(use-package htmlize
  :ensure t)

;; diff markers in left-hand side
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :hook (vc-checkin . diff-hl-update))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . olivetti-mode)
  :config
  (progn
    (setq org-startup-folded t)
    (setq org-directory "~/org")
    (setq org-ellipsis " ⤵")
    (setq org-agenda-span 14)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-use-time-grid nil)
    (setq calendar-week-start-day 1)
    (setq org-todo-keywords
	  '((sequence "TODO" "DONE")))
    (setq org-todo-keyword-faces
          '(("TODO" . "#dc322f") ("DONE" . "#859900")))
    (setq org-agenda-files
	  '("~/org"
	    "~/SynologyDrive/_notes"
	    "~/SynologyDrive/_notes/daily"))))

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list
	'("◉" "◎" ">" "○" "►" "◇"))
  :hook (org-mode . org-bullets-mode))

;; roam
(use-package org-roam
  :ensure t
  :config
  (progn
    (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "${slug}-%<%Y%m>.org"
			      "#+title: ${title} \n")
	   :unnarrowed t)))
    (setq org-roam-node-display-template "${tags:10} ${title:100} ${backlinkscount:6}"))
  :config
  (org-roam-setup)
  :custom
  (org-roam-directory "~/SynologyDrive/_notes")
  (org-roam-completion-system 'ivy))
(setq org-roam-v2-ack t)

(cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
  (let* ((count (caar (org-roam-db-query
                       [:select (funcall count source)
                                :from links
                                :where (= dest $s1)
                                :and (= type "id")]
                       (org-roam-node-id node)))))
    (format "[%d]" count)))

(require 'websocket)
(add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
(load-library "org-roam-ui")

;; solarized
(setq solarized-use-less-bold t)
(setq solarized-emphasize-indicators nil)
(load-theme 'solarized-light t)

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
  (setq highlight-indent-guides-auto-enabled nil)
  (setq indent-guide-recursive t))

;; highlight the current line
(use-package hl-line
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
  ;; (use-package evil-magit
  ;;   :ensure t
  ;;   :config
  ;;   (add-hook 'magit-mode-hook 'evil-local-mode)
  ;;   (add-hook 'git-rebase-mode-hook 'evil-local-mode)
  ;;   (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
  ;;   (let ((mm-key ","))
  ;;     (dolist (state '(normal motion))
  ;; 	(evil-define-key state with-editor-mode-map
  ;; 	  (concat mm-key mm-key) 'with-editor-finish
  ;; 	  (concat mm-key "a")    'with-editor-cancel
  ;; 	  (concat mm-key "c")    'with-editor-finish
  ;; 	  (concat mm-key "k") 'with-editor-cancel))))
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
  :hook
  (js2-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  (rjsx-mode . prettier-js-mode))

(add-to-list 'exec-path "${HOME}/.nvm/versions/node/v14.17.3/bin")

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
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-capf)
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

;; Olivetti writing environment
(use-package olivetti
  :ensure olivetti
  :config
  (setq olivetti-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'linum-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package ranger
  :config
  (setq ranger-max-preview-size 10)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-dont-show-binary t))

(defun bs/web-mode-hook ()
    "Setup indentation when loading `web-mode`."
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2))

(use-package web-mode
  :hook
  (web-mode . bs/web-mode-hook)
  :config
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode)))

;; TypeScript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :mode (("\\.tsx\\'" . typescript-mode)))

(defun setup-tide-mode ()
  (interactive)
  (defun tide-imenu-index () nil)
  (tide-setup)
  (tide-hl-identifier-mode +1))

(use-package tide
  :config
  (progn
    (add-hook 'typescript-mode-hook #'setup-tide-mode)))

;; Flycheck
(use-package flycheck
  :defer 1
  :init (setq
         flycheck-checkers
         '(typescript-tide
           javascript-tide
           jsx-tide
           javascript-eslint
           css-csslint
           emacs-lisp
           haml
           json-jsonlint
           yaml-jsyaml))
  :config (global-flycheck-mode))

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
  "fg"  '(counsel-ag :which-key "find str in project")
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
  ;; Roam
  "mr"  '(:which-key "ROAM")
  "mrf" '(org-roam-node-find :which-key "find node")
  "mri" '(org-roam-node-insert :which-key "insert node")
  "mrn" '(org-roam-dailies-capture-today :which-key "new daily journal")
  "mrg" '(org-roam-dailies-goto-today :which-key "go to daily")
  "mrt" '(org-roam-buffer-toggle :which-key "toggle sidebar")
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
  "tt" '(counsel-load-theme :which-key "change theme")
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
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(compilation-message-face 'default)
 '(default-input-method "latin-postfix")
 '(line-number-mode nil)
 '(org-babel-load-languages '((emacs-lisp . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "~/org/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-roam-completion-system 'ivy)
 '(org-roam-directory "~/SynologyDrive/_notes")
 '(package-archives
   '(("org" . "http://orgmode.org/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(simple-httpd websocket tide typescript-mode solarized-theme evil-surround magit ranger smooth-scrolling rainbow-delimiters clipetty org-roam cider markdown-mode clojure-mode undo-tree yaml-mode spaceline-all-the-icons org-bullets rjsx-mode add-node-modules-path prettier olivetti web-mode darkroom ess lorem-ipsum simpleclip company exec-path-from-shell prettier-js evil-mc nlinum-relative diff-hl diminish powerline-evil telephone-line highlight-indent-guides ivy which-key use-package neotree general evil all-the-icons))
 '(spaceline-all-the-icons-clock-always-visible t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Ubuntu Mono"))))
 '(org-level-1 ((t (:foreground "#cb4b16" :height 1.0 :family "Ubuntu Mono"))))
 '(org-level-2 ((t (:foreground "#859900" :height 1.0 :family "Ubuntu Mono"))))
 '(org-level-3 ((t (:foreground "#268bd2" :height 1.0 :family "Ubuntu Mono"))))
 '(org-level-4 ((t (:foreground "#268bd2" :family "Ubuntu Mono"))))
 '(org-level-5 ((t (:foreground "#268bd2" :height 1.0 :family "Ubuntu Mono"))))
 '(show-paren-match ((t (:background "gold"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))))

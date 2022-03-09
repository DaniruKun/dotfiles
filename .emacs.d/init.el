;;; init.el -*- lexical-binding: t; -*-
;; Author: Daniils Petrovs

(require 'cl-lib)

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(100 . 100))
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;; The default is 800 kilobytes.  Measured in bytes.
;; (setq gc-cons-threshold (* 50 2000 2000))

(setq native-comp-deferred-compilation nil)

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(defconst *is-work-laptop* (cl-search "sora" (system-name)))
(defconst *work-email* "daniils@platogo.com")
(defconst *personal-email* "thedanpetrov@gmail.com")

;; Set my personal details
(setq user-full-name "Daniils Petrovs"
      user-mail-address (if *is-work-laptop* *work-email* *personal-email*)
      calendar-latitude 48.208
      calendar-longitude 16.37
      calendar-location-name "Vienna, AU")

(setq frame-resize-pixelwise t)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(global-hl-line-mode +1)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Allow to install packages manually from this dir
(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "10:20"))

(use-package exec-path-from-shell)

(pcase system-type
  ('gnu/linux (exec-path-from-shell-initialize))
  ('windows-nt "It's Windows!")
  ('darwin (exec-path-from-shell-initialize)))

;; Setup startup dashboard
(use-package dashboard
            :ensure t
            :config
            (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "GNU Emacs")
;; Set the banner
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(setq dashboard-set-init-info t)

(setq confirm-kill-emacs #'yes-or-no-p)

(use-package elcord :defer t)

(use-package google-this :defer t)

(use-package ns-auto-titlebar)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

;; Disable the annoying package.cl deprecated warning
(setq byte-compile-warnings '(cl-functions))

(use-package indent-guide :defer t)

;; To generate automatic tables of content in MarkDown docs
(use-package markdown-toc)

;; Diagrams
(use-package mermaid-mode
  :mode "\\.mermaid\\'")

(use-package ob-mermaid)

(use-package vega-view)

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar")))

(use-package dash-at-point)

(with-eval-after-load 'flycheck
  (use-package flycheck-plantuml)
  (flycheck-plantuml-setup))

;; Highlight TODO, FIXME blocks in code etc.
(use-package hl-todo)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)

(use-package restart-emacs)

(set-fontset-font t '(#x1f000 . #x1faff) (font-spec :family "Apple Color Emoji"))

;; Fun
(use-package hnreader :defer t)
(use-package fireplace :defer t)
(use-package nyan-mode :defer t)
(use-package dilbert :defer t)
(use-package xkcd :defer t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
;; (global-display-line-numbers-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Display line numbers and fill col for some modes

(setq my-programming-modes '(elixir-mode-hook
                             erlang-mode-hook
                             emacs-lisp-mode-hook
                             common-lisp-mode-hook
                             lisp-mode-hook
                             clojure-mode-hook
                             ruby-mode-hook
                             yaml-mode-hook
                             html-mode-hook
                             shell-mode-hook
                             shell-script-mode-hook
                             sql-mode-hook
                             typescript-mode-hook
                             javascript-mode-hook
                             js-mode-hook
                             makefile-bsdmake-mode
                             github-review-mode-hook))

(dolist (mode my-programming-modes)
  (add-hook mode (lambda () (display-line-numbers-mode t)
                   (display-fill-column-indicator-mode t))))

(use-package elisp-slime-nav)
;; Enable Slime navigation for Elisp filed
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Org extensions

;; Allow to easily paste images into Org
(use-package org-download :defer t)
;; Make presentations in Org
(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

;; Snippets and enable them globally
(use-package yasnippet)
(yas-global-mode 1)

;; Drag-and-drop to `org`
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "~/org/img")
(setq-default org-image-actual-width 800)
(setq-default org-startup-with-inline-images t)
;; Adds all-the-icons support to dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(defun set-font-faces ()
  (message "Setting faces")
  (set-face-attribute 'default nil :font "Menlo" :height efs/default-font-size)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Menlo" :height efs/default-font-size)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Menlo" :height efs/default-variable-font-size :weight 'regular))

(defun my-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-nord-light t))
    ('dark (load-theme 'doom-nord t))))

;; Make sure stuff happens for each frame when running as server
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (set-font-faces))))

  (when (eq system-type 'darwin)
    (add-hook 'ns-system-appearance-change-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my-apply-theme 'ns-system-appearance)))))
  (my-apply-theme ns-system-appearance)
  (set-font-faces))

;; Auto trigger theme change based on system appearance
(when (eq system-type 'darwin)
  (add-hook 'ns-system-appearance-change-functions #'my-apply-theme))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package ag)

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode -1)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(centaur-tabs-headline-match)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-modified-marker t)
(centaur-tabs-change-fonts "menlo" 200)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)
           (doom-modeline-icon t)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package gif-screencast)

(with-eval-after-load 'gif-screencast
  (setq gif-screencast-convert-program "convert")
  (setq gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (setq gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (setq gif-screencast-capture-format "ppm")) ;; Optional: Required to crop captured images.

(use-package elisp-lint)

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package smartparens)
(require 'smartparens-config)

;; Enable smartparens and fill column for other programming languages
(dolist (mode '(ruby-mode-hook
                python-mode
                html-mode-hook
                javascript-mode-hook
                elixir-mode-hook
                erlang-mode-hook))
  (add-hook mode (lambda ()
                   (smartparens-mode)
                   (hl-todo-mode))))

(setq display-fill-column-indicator-column 80)

;; ParEdit configuration for everything to do with parens

(use-package paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;; Enable ParEdit for Lisp languages
(dolist (mode '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                ielm-mode-hook
                lisp-mode-hook
                clojure-mode-hook
                cider-repl-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook))
  (add-hook mode (lambda () (enable-paredit-mode))))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Stop SLIME's REPL from grabbing DEL,
          ;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
      (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Menlo" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(setq org-directory "~/org/")

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files '("~/org/tasks.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "STARTED(s)" "WAITING(w@/!)" "HOLD(h)" "|" "CANC(k@)" "OBSOLETE(o)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/.emacs.d/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-real)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Make Markdown mode less ugly
(defun markdown-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(defun rfc-mode-visual-prettify ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package markdown-mode
  :hook (markdown-mode . markdown-mode-visual-fill))

(use-package rfc-mode
  :hook (rfc-mode . rfc-mode-visual-prettify))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)
        (plantuml . t)
        (mermaid . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (push '("plantuml" . plantuml) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(defun efs/lsp-mode-setup ()
  (ruby-mode . lsp)
  (clojure-mode . lsp)
  (elixir-mode . lsp)
  (erlang-mode . lsp)
  (javascript-mode . lsp)
  (typescript-mode . lsp))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure t
  :diminish lsp-mode
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  ;; (add-to-list 'exec-path "/usr/local/opt/elixir-ls/libexec")
  :config
  (setq lsp-headerline-enable nil
        lsp-headerline-breadcrumb-enable nil ;; disable headerline breadcrumbs
        lsp-lens-enable t
        lsp-elixir-server-command '("elixir-ls")
        lsp-elixir-suggest-specs t
        lsp-elixir-fetch-deps nil
        lsp-elixir-mix-env "dev"
        lsp-solargraph-use-bundler t)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-code-actions t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'at-point))

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  (setq dap-ui-mode 1
      dap-tooltip-mode 1
      tooltip-mode 1
      dap-ui-controls-mode 1)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; Enable folding
(setq lsp-enable-folding t)

;; Add origami and LSP integration
(use-package lsp-origami)
(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2
        js-indent-level 2))

(use-package erlang
  :hook (erlang-mode . lsp-deferred))

(use-package company-erlang)
(add-hook 'erlang-mode-hook #'company-erlang-init)

(use-package elixir-mode
  :hook (elixir-mode . lsp-deferred))

(use-package exunit)

(use-package lsp-tailwindcss)

;; Ruby Development

(use-package ruby-mode
  :hook (ruby-mode . lsp-deferred))

(use-package rspec-mode)
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

(use-package yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . lsp-deferred))

(use-package slime-company)

(use-package slime
  :ensure t
  :config
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-company slime-quicklisp slime-asdf))
  (slime-setup '(slime-company slime-quicklisp slime-asdf)))

(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run")

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(use-package company
  :commands company-mode
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode common-lisp-mode emacs-lisp-mode slime-repl-mode) . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
         (:map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2)
  (company-global-modes '(prog-mode org-mode lisp-mode elisp-mode slime-repl-mode sly-mrepl-mode)))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Start company mode automatically in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(use-package restclient)
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/projects/git" "~/projects/local" "~/GIT"))
  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package github-review)

(setq auth-sources '("~/.authinfo"))

(use-package diff-hl
  :init (global-diff-hl-mode) (diff-hl-flydiff-mode t))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(use-package kubernetes-helm
  :after kubernetes)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

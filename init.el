;; Do compatibility checks
(if (version< emacs-version "29.0")
    (warn "Emacs version is %s.  This init assumes at least Emacs 29 release series." emacs-version))
(let ((warn-message "Feature `%s' is not available.  It is highly recommended by the author of this init file to enable it."))
  (mapcar (lambda (feature) (if (not (featurep feature)) (warn warn-message feature)))
          '(native-compile json threads)))
(let ((warn-message "Feature `%s' is not available.  It is highly recommended by the author of this init file to enable it.")
      (comp-features (split-string system-configuration-features " ")))
  (mapcar (lambda (feature) (if (not (member feature comp-features)) (warn warn-message feature)))
          '("SQLITE3" "FREETYPE")))

;; Setup MELPA support
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setf package-archive-priorities
      '(("gnu" . 20)
        ("nongnu" . 20)
        ("melpa-stable" . 10)
        ("melpa" . 0)))
;; Auto install configured packages
(require 'use-package)
(setf use-package-always-ensure t)
;;(setf use-package-always-demand t) ; When enabled, this will for immediate loading of packages

(load-theme 'kunagawa-dark-julian-mix t)

;; Set font
(set-face-attribute 'default nil :family "Fira Code")

;; Customize UI
(setf inhibit-startup-screen t)
(setf initial-major-mode 'org-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setf split-width-threshold 80)
(column-number-mode)
(global-display-line-numbers-mode 1)
(setf display-line-numbers-type 'relative)
(setq-default display-line-numbers-widen t)
(use-package telephone-line
  :pin nongnu
  :demand t
  :config
  (telephone-line-mode 1))
(setf frame-resize-pixelwise t)

;; Disable backup files.
(setf make-backup-files nil)

;; Load Evil Mode.
(use-package evil
  :pin nongnu
  :ensure t
  :demand t
  :init
  (setf evil-search-module 'evil-search)
  :config
  (evil-mode 1))

;; Custom keymap.
(defvar-keymap custom-search-map
  :doc "Custom keymap for aggregating search actions.")
(defvar-keymap custom-workspace-map
  :doc "Custom keymap for managing projects and workspaces.")
(defvar-keymap custom-leader-map
  :doc "Custom keymap for triggering various actions."
  "s" custom-search-map
  "w" custom-workspace-map)
(define-key evil-normal-state-map (kbd "SPC") custom-leader-map)
(define-key evil-visual-state-map (kbd "SPC") custom-leader-map)
(define-key evil-motion-state-map (kbd "SPC") custom-leader-map)

;; More fancy auto-completion and matching than default.
(use-package flx
  :pin melpa
  :ensure t
  :demand t)

(use-package counsel
  :pin gnu
  :ensure t
  :demand t
  :after (flx)
  :bind
  (:map custom-leader-map
   ("?" . counsel-buffer-or-recentf)
   ("SPC" . counsel-switch-buffer)
   :map custom-search-map
   ("f" . counsel-find-file))
  :config
  (setf ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (ivy-mode 1)
  (counsel-mode 1))

;; Fuzzy search in buffer.
(use-package swiper
  :bind (:map custom-leader-map
         ("/" . swiper)))

;; Better in buffer completion.
(use-package company
  :pin gnu
  :ensure t
  :demand t
  :config
  (global-company-mode 1))

;; Eldoc customization.
(use-package inline-docs
  :ensure t
  :init
  (setf inline-docs-border-symbol 9472))

(use-package eldoc
  :pin manual
  :ensure nil
  :defer t
  :after (inline-docs)
  :bind (:map custom-leader-map
         ("h" . eldoc-mode)
         ("H" . eldoc-print-current-symbol-info))
  :config
  (setf eldoc-message-function #'inline-docs)
  (setf eldoc-echo-area-use-multiline-p 3)
  (setf eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :disabled ; enable for gui-only Eldoc overlay under point
  :pin melpa-stable
  :ensure t
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)))

;; Folding in programming modes.
(use-package hideshow ;hs-minor-mode
  :pin manual
  :ensure nil
  :hook ((prog-mode . hs-minor-mode)))

;; Configure dired
(use-package dired
  :pin manual
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-alh"))

;; enable dired-x
(use-package dired-x
  :disabled
  :pin manual
  :ensure nil
  :defer t
  :custom
  (dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

;; Start the emacsclient server.
(server-start)

;; Disable auto-fill-mode globally.
(auto-fill-mode -1)

;;(require 'printing)
(use-package printing
  :pin manual
  :ensure nil)

;; Emacs calc configuration.
(use-package calc
  :pin manual
  :ensure nil
  :defer t
  :config
  (add-to-list 'calc-language-alist
               '(org-mode . latex)))

(use-package org
  :pin manual
  :ensure nil
  :demand t
  :custom
  (org-startup-folded t)
  ;; Set org-mode TODO keywords workflow.
  (org-todo-keywords
   '((sequence "TODO(t)" "CURRENT(s!)" "|" "CANCELED(c!)"
               "DONELATE(l!)" "PARTIALCOMPLETE(p!)" "FAILED(f!)"
               "DONE(d!)")))
  ;; Set org-mode to log items being marked as DONE.
  (org-log-done 'time)
  (org-link-frame-setup
   '((vm . vm-visit-folder)
     (vm-imap . vm-visit-imap-folder)
     (gnus . gnus)
     (file . find-file)
     (wl . wl-other-frame)))
  ;; Configure clocking.
  (org-clock-persist t)
  ;; Setup org-mode capture.
  (org-directory "~/doc")
  ;; Set org-mode capture templates.
  (org-capture-templates
   '(("n" "Note to self" entry (file+headline "~/doc/notes.org" "Note to self/yet to be filed")
      "* %u %?")
     ("N" "Note to self with context" entry (file+headline "~/doc/notes.org" "Note to self/yet to be filed")
      "* %u %?\n  %l\n:  %i")
     ("m" "Note to self, with time" entry (file+headline "~/doc/notes.org" "Note to self/yet to be filed")
      "* %U %?")
     ("M" "Note to self with context, with time" entry (file+headline "~/doc/notes.org" "Note to self/yet to be filed")
      "* %U %?\n  %l\n:  %i")))
  ;; Set org-mode agenda and default notes files.
  (org-agenda-files
   '("~/doc/notes.org"))
  (org-export-backends '(ascii html icalendar latex md odt texinfo))
  (org-format-latex-header
   "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;;(asymptote . t)
     (awk . t)
     (C . t)
     (calc . t)
     (clojure . t)
     (comint . t)
     ;;(cpp . t)
     (css . t)
     (ditaa . t)
     (dot . t)
     (eshell . t)
     (gnuplot . t)
     (groovy . t)
     (haskell . t)
     (latex . t)
     (java . t)
     (js . t)
     (lisp . t)
     (makefile . t)
     (org . t)
     (perl . t)
     ;;(php . t)
     (plantuml . t)
     (python . t)
     (R . t)
     ;;(redis . t)
     (ruby . t)
     (sass . t)
     (scheme . t)
     (screen . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))
  ;; Setup org-mode habits
  (add-to-list 'org-modules 'org-habit)
  ;; Configure clocking.
  (org-clock-persistence-insinuate)
  ;; Setup org-mode capture.
  (setf org-default-notes-file (concat org-directory "/notes.org"))
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ;; org-mode style links in all buffers.
         ("C-c o" . org-open-at-point-global)))

;; Enable org-mode structure template expansions.
(use-package org-tempo
  :pin manual
  :ensure nil
  :after (org))

;; Org-babel tmux
(use-package ob-tmux
  :pin melpa-stable
  :after (org)
  :custom
  (org-babel-default-header-args:tmux
      '((:results . "silent")
        (:session . "default")
        (:socket . nil)))
  (org-babel-tmux-terminal "urxvt"))

(use-package orgit
  :pin nongnu
  :after (org magit)
  :config
  (use-package orgit-forge
    :pin melpa-stable
    :after (org magit forge)))

(use-package org-modern
  :disabled
  :pin gnu
  :after (org))

(use-package htmlize
  :disabled
  :pin nongnu
  :init
  (setf org-src-fontify-natively t)
  :config
  (setf org-html-htmlize-output-type 'css)
  :after (org))

;; Setup org-mode with which-function-mode.
;; Originally taken from https://emacs.stackexchange.com/a/30901
(use-package which-func
  :pin manual
  :ensure nil
  :config
  (defun org-which-function ()
    (interactive)
    (when (eq major-mode 'org-mode)
      (mapconcat 'identity (org-get-outline-path t)
                 " ⟼ ")))
  (add-to-list 'which-func-functions #'org-which-function))

;; Use transparent encryption.
(use-package epa-file
  :pin manual
  :ensure nil
  :config
  (epa-file-enable)
  (setf epa-pinentry-mode 'loopback))

(use-package pinentry
  :pin gnu)

;; Customize bs-show.
(use-package bs
  :pin manual
  :ensure nil
  :custom
  (bs-configurations
   '(("all" nil nil "^.*\\.#.*" nil nil)
     ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last)))
  (bs-alternative-configuration "files")
  (bs-default-configuration "all")
  :bind
  ;; Swap list-buffers with bs-show.
  ("C-x C-b" . bs-show))

(use-package magit
  :pin nongnu
  :ensure t)
(use-package forge
  :pin melpa-stable
  :after (magit))
(use-package magit-annex
  :pin melpa-stable
  :after (magit))
(use-package magit-lfs
  :pin melpa
  :after (magit))

(use-package diff-hl
  :disabled
  :config
  (global-diff-hl-mode))

(use-package dap-mode
  :pin melpa-stable
  :defer
  :bind (:map custom-leader-map
         ("b" . dap-breakpoint-toggle)
         ("B" . dap-breakpoint-condition))
  :custom
  (dap-auto-configure-mode t))

;; Setup sr-speedbar.
(use-package sr-speedbar
  :pin melpa-stable
  :bind (("s-s" . sr-speedbar-toggle))
  :custom
  (sr-speedbar-right-side nil))

;; show-paren-mode configuration.
(setf show-paren-delay 0)
(setf show-paren-context-when-offscreen 'overlay)

(use-package smartparens
  :pin nongnu
  :ensure t
  :hook ((lisp-mode . smartparens-strict-mode)
         (emacs-lisp-mode . smartparens-strict-mode)
         (eval-expression-minibuffer-setup . (lambda ()
                                               (sp-local-pair 'minibuffer-pairs "'" nil :actions nil)
                                               (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
                                               (sp-update-local-pairs 'minibuffer-pairs)
                                               (smartparens-strict-mode)))
         (lisp-interaction-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode)
         (slime-repl-mode . smartparens-strict-mode))
  :config
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)))

(use-package slime
  :pin manual
  :ensure nil
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :custom
  ;; Set inferior lisp program(s).
  (inferior-lisp-program "sbcl")
  (slime-lisp-implementations
   '((sbcl ("sbcl"))
     (ecl ("ecl"))
     (clisp ("clisp"))
     (r5rs ("plt-r5rs"))))
  :config
  ;; Slime documentation.
  (add-to-list 'Info-directory-list (expand-file-name "~/quicklisp/dists/quicklisp/software/slime-2.13/doc")))

;; Setup integration for Common Lisp Documentation.
(use-package info-look
  :pin manual
  :ensure nil)

;; Setup run-scheme.
(use-package scheme
  :pin manual
  :ensure nil
  :defer t
  :custom
  (scheme-program-name "plt-r5rs"))

;;; JDEE configuration
;; JDE
;;(load "jde")
;; decompile.el
;;(require 'decompile)

(use-package treesit
  :pin manual
  :ensure nil
  :init
  (setf treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (make "https://github.com/alemuller/tree-sitter-make")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rego "https://github.com/FallenAngel97/tree-sitter-rego")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/ikatyang/tree-sitter-toml")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setf major-mode-remap-alist
        '(;;(bash-mode . bash-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (c-mode . c-ts-mode)
          ;;(cmake-mode . cmake-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          ;;(dockerfile-mode . dockerfile-ts-mode)
          ;;(go-mod-mode . go-mod-ts-mode)
          ;;(go-mode . go-ts-mode)
          (java-mode . java-ts-mode)
          ;;(js-mode . js-ts-mode)
          ;;(javascript-mode . js-ts-mode)
          (js-mode . typescript-ts-mode)
          (javascript-mode . typescript-ts-mode)
          ;;(json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          ;;(rust-mode . rust-ts-mode)
          ;;(toml-mode . toml-ts-mode)
          ;;(tsx-mode . tsx-ts-mode)
          ;;(typescript-mode . typescript-ts-mode)
          ;;(yaml-mode . yaml-ts-mode)
          ))
  (setf treesit-font-lock-level 4)
  :config
  (mapc #'treesit-install-language-grammar
        (seq-filter #'(lambda (language)
                        (not (treesit-language-available-p language)))
                    (mapcar #'car
                            treesit-language-source-alist))))
(use-package eglot
  :pin manual
  :ensure nil
  :hook ((js-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)))

(use-package verb
  :pin melpa-stable
  :after (org)
  :defer nil
;;  :bind (:map org-mode-map
;;         ("C-c C-r" . verb-command-map))
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package which-key
  :pin gnu
  :config
  (which-key-mode 1))
(use-package yaml-ts-mode
  :pin manual
  :ensure nil
  :mode "\\.yaml\\'")
(use-package arduino-mode
  :pin nongnu
  :defer t)
(use-package platformio-mode
  :pin melpa-stable
  :defer t)
(use-package nhexl-mode
  :pin gnu
  :defer t)
(use-package elf-mode
  :pin melpa-stable
  :defer t)
(use-package pcap-mode
  :pin melpa
  :defer t)
(use-package gnuplot
  :pin nongnu
  :defer t)
(use-package graphviz-dot-mode
  :pin melpa-stable
  :defer t)
(use-package scad-mode
  :pin nongnu
  :defer t)
(use-package rainbow-mode
  :pin gnu)
(use-package markdown-mode
  :pin nongnu
  :defer t)
;;(use-package bbdb
;;  :pin gnu
;;  :defer t)
;;(use-package bbdb-vcard)
;;(use-package ebdb
;;  :pin gnu
;;  :defer t)
(use-package haskell-mode
  :pin nongnu
  :defer t)
;;(use-package docker-compose-mode)
(use-package web-mode
  :pin nongnu
  :defer t)
;;(use-package js2-mode)
(use-package sass-mode
  :pin nongnu
  :defer t)
;;(use-package pipenv) ; unsure if needed
(use-package jinja2-mode
  :pin nongnu
  :defer t)
(use-package csv-mode
  :pin gnu
  :defer t)
(use-package ess
  :pin melpa-stable
  :defer t)
(use-package sed-mode
  :pin gnu
  :defer t)
(use-package php-mode
  :pin nongnu
  :defer t)
(use-package irony
  :pin melpa-stable
  :defer t)
(use-package emms
  :pin gnu
  :defer t)
;; Unsure if edbi is still relevant or if there is something better.
;;(use-package edbi
;;  :pin melpa-stable
;;  :defer t)
;;(use-package edbi-sqlite) ; possibly redundant with 29.1
;;(use-package edbi-minor-mode)
;;(use-package edbi-database-url)
(use-package biblio
  :pin melpa-stable
  :defer t)
;;(use-package autodisass-java-bytecode
;;  :pin melpa-stable
;;  :defer t)
(use-package tex
  :ensure auctex
  :pin gnu
  :defer t)
(use-package apache-mode
  :pin nongnu
  :defer t)
(use-package nginx-mode
  :pin nongnu
  :defer t)
(use-package nix-mode
  :pin nongnu
  :defer t)
(use-package dired-du
  :pin gnu
  :defer t)
(use-package dired-git-info
  :pin gnu
  :defer t)
(use-package dired-preview
  :pin gnu
  :defer t)
;;(use-package dired-sidebar)
;;(use-package dired-subtree)
;;(use-package dired-toggle)
;;(use-package nix-sandbox)
;;(use-package neotree)
(use-package editorconfig
  :pin nongnu
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))
(use-package projectile
  :pin nongnu
  :ensure t
  :demand t
  :after (counsel)
  :bind
  (:map projectile-mode-map
   ("C-c p" . projectile-command-map)
   :map custom-leader-map
   ("p" . projectile-command-map)
   :map custom-workspace-map
   ("pa" . projectile-add-known-project)
   ("pp" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1))
(use-package treemacs
  :pin melpa-stable
  :ensure t
  :hook ((treemacs-mode . (lambda () (eldoc-mode -1)))) ; Since inline-docs is used, this prevents annoying popups
  :bind
  (:map custom-leader-map
   ("t" . treemacs)
   :map custom-workspace-map
   ("ta" . treemacs-add-and-display-current-project)
   ("te" . treemacs-add-and-display-current-project-exclusively)
   ("tp" . treemacs-project-map)
   ("tw" . treemacs-workspace-map)
   ("wp" . treemacs-create-workspace-from-project))
  :custom
  (treemacs-project-follow-cleanup t)
  (treemacs-recenter-after-file-follow t)
  :config
  (treemacs-project-follow-mode 1))
(use-package treemacs-projectile
  :pin melpa-stable
  :after (treemacs projectile)
  :ensure t)
(use-package treemacs-magit
  :pin melpa-stable
  :after (treemacs magit)
  :ensure t)
(use-package minimap
  :disabled
  :pin gnu
  :custom-face
  (minimap-font-face ((t (:height 18))))
  :custom
  (minimap-minimum-width 18)
  (minimap-window-location 'right)
  (minimap-mode 1))
(use-package lorem-ipsum
  :pin nongnu
  :defer t)
(use-package osm
  :pin gnu)

;; Packages pulled in as dependencies.

(use-package async
  :pin gnu
  :defer t)
(use-package biblio-core
  :pin melpa-stable
  :defer t)
(use-package closql
  :pin melpa-stable
  :defer t)
(use-package compat
  :pin gnu
  :defer t)
(use-package dash
  :pin gnu
  :defer t)
(use-package emacsql
  :pin melpa-stable
  :defer t)
(use-package emacsql-sqlite
  :pin melpa-stable
  :defer t)
(use-package ghub
  :pin melpa-stable
  :defer t)
(use-package git-commit
  :pin nongnu
  :defer t)
(use-package haml-mode
  :pin nongnu
  :defer t)
(use-package julia-mode
  :pin nongnu
  :defer t)
(use-package magit-section
  :pin nongnu
  :defer t)
(use-package s
  :pin melpa-stable
  :defer t)
(use-package spinner
  :pin gnu
  :defer t)
(use-package tablist
  :pin nongnu
  :defer t)
(use-package transient
  :pin gnu
  :defer t)
(use-package treepy
  :pin melpa-stable
  :defer t)
(use-package with-editor
  :pin nongnu
  :defer t)
(use-package yaml
  :pin melpa-stable
  :defer t)
(use-package ace-window
  :pin gnu
  :defer t)
(use-package avy
  :pin gnu
  :defer t)
(use-package pfuture
  :pin melpa-stable
  :defer t)
(use-package hydra
  :pin gnu
  :defer t)
(use-package lv
  :pin gnu
  :defer t)
(use-package ht
  :pin melpa-stable
  :defer t)
(use-package cfrs
  :pin melpa-stable
  :defer t)
(use-package posframe
  :pin gnu
  :defer t)

;; Configuring gnus.
(use-package gnus
  :pin manual
  :ensure nil
  :defer t
  :custom
  (gnus-select-method '(nntp "news.gwene.org"))
  (gnus-secondary-select-methods `((nntp "news.eternal-september.org")
                                   (nntp "news.gmane.io"))))

(use-package keyfreq
  :pin melpa-stable
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; Various variable customizations.

;; Use decimals instead of octals to select characters with C-q.
(setf read-quoted-char-radix 10)

;; Set welcome file.
(setf initial-buffer-choice t)

;; Set *scratch* initial text.
(setf initial-scratch-message "# Scratch buffer for trying things out.\n\n")

;; Set preferred date format.
(setf calendar-date-style "iso")

;; Set indent mode to no tabs.
(setq-default indent-tabs-mode nil)

(setf mouse-yank-at-point t)

;; Enable whitespace mode.
(use-package whitespace
  :pin manual
  :ensure nil
  :demand t
  :custom
  (whitespace-style '(face tabs trailing space-before-tab empty space-after-tab tab-mark))
  :config
  (global-whitespace-mode))

(use-package highlight-indent-guides
  :pin melpa
  :hook ((prog-mode . (lambda ()
                        (highlight-indent-guides-mode 1)
                        (set-face-foreground 'highlight-indent-guides-character-face "#727169")
                        (set-face-background 'highlight-indent-guides-even-face nil)
                        (set-face-background 'highlight-indent-guides-odd-face nil))))
  :config
  (setf highlight-indent-guides-method 'character)
  (setf highlight-indent-guides-character ?\┊)
  (set-face-foreground 'highlight-indent-guides-character-face "#727169")
  (set-face-background 'highlight-indent-guides-even-face nil)
  (set-face-background 'highlight-indent-guides-odd-face nil))

;; Window cycling.
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "C-'") 'last-window)

(defun last-window ()
  (interactive)
  (other-window -1))

;;(setf ps-header-lines 1)

;;; Evil mode customization
(use-package evil-numbers
  :pin nongnu
  :after (evil))
(use-package evil-surround
  :pin melpa-stable
  :after (evil)
  :config
  (global-evil-surround-mode 1))
(use-package evil-textobj-tree-sitter
  :pin melpa
  :after (evil))
(use-package evil-org
  :pin melpa
  :after (evil))

(use-package treemacs-evil
  :pin melpa-stable
  :after (treemacs evil))

;;;; Customizer settings

(setf custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

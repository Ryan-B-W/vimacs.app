;; Do compatibility checks.
(unless (file-exists-p (concat user-emacs-directory "suppress-feature-checks"))
  (when (version< emacs-version "29.0")
    (warn "Emacs version is %s.  This init assumes at least Emacs 29 release series." emacs-version))
  (let ((warn-message "Feature `%s' is not available.  It is highly recommended by the author of this init file to enable it."))
    (mapcar (lambda (feature) (unless (featurep feature) (warn warn-message feature)))
            '(native-compile json threads)))
  (let ((warn-message "Feature `%s' is not available.  It is highly recommended by the author of this init file to enable it.")
        (comp-features (split-string system-configuration-features " ")))
    (mapcar (lambda (feature) (unless (member feature comp-features) (warn warn-message feature)))
            '("SQLITE3" "FREETYPE"))))

;; Setup MELPA support.
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
;; Auto install configured packages.
(require 'use-package)
(setf use-package-always-ensure t)

(load-theme 'kunagawa-dark-julian-mix t)

;; Set font.
(set-face-attribute 'default nil :family "Fira Code")

;; Customize UI.
(setf inhibit-startup-screen t)
(setf initial-major-mode 'org-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (featurep 'scroll-bar) (scroll-bar-mode -1))
(setf split-width-threshold 80)
(column-number-mode)
(global-display-line-numbers-mode 1)
(setf display-line-numbers-type 'relative)
(setq-default display-line-numbers-widen t)
(use-package telephone-line
  :pin melpa-stable
  :config
  (telephone-line-mode 1))
(setf frame-resize-pixelwise t)
(when (featurep 'pixel-scroll) (pixel-scroll-precision-mode 1))

;; Enable additional mouse support.
(setf dired-mouse-drag-files t)
(setf mouse-drag-and-drop-region-cross-program t)
(xterm-mouse-mode 1)
(when (string-match-p "\\<GPM\\>" system-configuration-features) (gpm-mouse-mode 1))

;; Disable backup files.
(setf make-backup-files nil)
;; Prompt to delete autosaves when killing buffers.
(setf kill-buffer-delete-auto-save-files t)

;; Load Evil Mode.
(use-package evil
  :init
  (setf evil-want-keybinding nil)
  (setf evil-search-module 'evil-search)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after (evil)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

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
(use-package flx)

(use-package counsel
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
  :config
  (global-company-mode 1))

;; Eldoc customization.
(use-package inline-docs
  :init
  (setf inline-docs-border-symbol 9472)
  :config
  ;; Overriding internal function to fix overflow issue.
  ;; Following function is from inline-docs.el from
  ;; https://repo.or.cz/inline-docs.git and is licensed under the
  ;; GNU GPL V3.
  (defun inline-docs--string-display (string apply-face)
    "Show STRING contents below point line until next command with APPLY-FACE."
    ;; note that `display-line-numbers-mode' takes 2 + `line-number-display-width' columns
    (let* ((total-column-number (if display-line-numbers-mode
                                    (- (window-body-width) (+ 3 (line-number-display-width)))
                                  (- (window-body-width) 1)))
           (border-line (make-string total-column-number inline-docs-border-symbol))
           (offset (make-string
                    (if (= (current-indentation) 0)
                        (current-indentation)
                      (- (current-indentation) 1))
                    inline-docs-prefix-symbol))
           (str (concat (propertize border-line 'face 'inline-docs-border-face) "\n"
                        offset
                        (propertize (concat inline-docs-indicator-symbol " ") 'face 'inline-docs-indicator-face)
                        (copy-sequence string) "\n"
                        (propertize border-line 'face 'inline-docs-border-face) "\n"))
           start-pos end-pos)
      (unwind-protect
          (save-excursion
            (inline-docs--clear-overlay)
            (cl-case inline-docs-position
              (above (forward-line 0))
              (below (forward-line)))
            (setq start-pos (point))
            (end-of-line)
            (setq end-pos (point))
            (setq inline-docs-overlay (make-overlay start-pos end-pos (current-buffer)))
            (if apply-face
                (overlay-put inline-docs-overlay 'face 'inline-docs-face))
            (overlay-put inline-docs-overlay 'evaporate t)
            (overlay-put inline-docs-overlay 'before-string str))
        (add-hook 'post-command-hook 'inline-docs--clear-overlay)))))
(use-package eldoc-box
  :config
  (defun eldoc-box-hover-ensure ()
    (if eldoc-box-hover-at-point-mode
        (when (xor eldoc-mode (display-graphic-p))
          (eldoc-box-hover-at-point-mode -1))
      (if (and eldoc-mode (display-graphic-p))
          (eldoc-box-hover-at-point-mode 1)
        (eldoc-box-hover-at-point-mode -1))))
  (add-hook 'window-state-change-hook #'eldoc-box-hover-ensure))

(use-package eldoc
  :pin manual
  :ensure nil
  :after (inline-docs)
  :bind (:map custom-leader-map
         ("h" . eldoc-mode)
         ("H" . eldoc-print-current-symbol-info))
  :config
  (setf eldoc-message-function #'inline-docs)
  (setf eldoc-echo-area-use-multiline-p 3)
  (setf eldoc-echo-area-display-truncation-message nil))

;; Folding in programming modes.
(use-package hideshow ; hs-minor-mode.
  :pin manual
  :ensure nil
  :hook ((prog-mode . hs-minor-mode)))

;; Configure dired.
(use-package dired
  :pin manual
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-alh"))

;; enable dired-x.
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
  ;; Setup org-mode habits.
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
  :defer t
  :after (org))

;; Org-babel tmux.
(use-package ob-tmux
  :defer t
  :after (org)
  :custom
  (org-babel-default-header-args:tmux
      '((:results . "silent")
        (:session . "default")
        (:socket . nil)))
  (org-babel-tmux-terminal "urxvt"))

(use-package orgit
  :defer t
  :after (org magit)
  :config
  (use-package orgit-forge
    :after (org magit forge)))

(use-package org-modern
  :disabled
  :defer t
  :after (org))

(use-package htmlize
  :disabled
  :defer t
  :after (org)
  :init
  (setf org-src-fontify-natively t)
  :config
  (setf org-html-htmlize-output-type 'css))

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

(use-package pinentry)

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

(use-package magit)
(use-package forge
  :defer t
  :after (magit))
(use-package magit-annex
  :defer t
  :after (magit))
(use-package magit-lfs
  :defer t
  :after (magit))

(use-package diff-hl
  :after (magit)
  :defer nil
  :hook
  ((magit-refresh-buffer . diff-hl-update))
  :config
  (global-diff-hl-mode 1))

(use-package dap-mode
  :bind (:map custom-leader-map
         ("b" . dap-breakpoint-toggle)
         ("B" . dap-breakpoint-condition))
  :custom
  (dap-auto-configure-mode t))

;; Setup sr-speedbar.
(use-package sr-speedbar
  :bind (("s-s" . sr-speedbar-toggle))
  :custom
  (sr-speedbar-right-side nil))

;; show-paren-mode configuration.
(setf show-paren-delay 0)
(setf show-paren-context-when-offscreen 'overlay)

(use-package smartparens
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
  :defer t
  :if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
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
  :ensure nil
  :defer t)

;; Setup run-scheme.
(use-package scheme
  :pin manual
  :ensure nil
  :defer t
  :custom
  (scheme-program-name "plt-r5rs"))

;;; JDEE configuration.
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
          (cmake-mode . cmake-ts-mode)
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
  :hook ((bash-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-or-c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (csharp-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (dockerfile-ts-mode . eglot-ensure)
         (go-mod-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (php-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (toml-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)))

(use-package verb
  :defer t
  :after (org)
  :init
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package which-key
  :config
  (which-key-mode 1))
(use-package yaml-ts-mode
  :pin manual
  :ensure nil
  :defer t
  :mode "\\.ya?ml\\'")
(use-package arduino-mode
  :defer t)
(use-package platformio-mode
  :defer t)
(use-package nhexl-mode
  :defer t)
(use-package elf-mode
  :defer t)
(use-package pcap-mode
  :defer t)
(use-package gnuplot
  :defer t)
(use-package graphviz-dot-mode
  :defer t)
(use-package scad-mode
  :defer t)
(use-package rainbow-mode
  :defer t)
(use-package markdown-mode
  :defer t)
;;(use-package bbdb
;;  :defer t)
;;(use-package bbdb-vcard
;;  :defer t)
;;(use-package ebdb
;;  :defer t)
(use-package haskell-mode
  :defer t)
;;(use-package docker-compose-mode
;;  :defer t)
(use-package web-mode
  :defer t)
;;(use-package js2-mode
;;  :defer t)
(use-package sass-mode
  :defer t)
;;(use-package pipenv
;;  :defer t) ; unsure if needed
(use-package jinja2-mode
  :defer t)
(use-package csv-mode
  :defer t)
(use-package ess
  :defer t)
(use-package sed-mode
  :defer t)
(use-package php-mode
  :defer t)
(use-package irony
  :defer t)
(use-package emms
  :defer t)
;; Unsure if edbi is still relevant or if there is something better.
;;(use-package edbi
;;  :defer t)
;;(use-package edbi-sqlite
;;  :defer t) ; possibly redundant with 29.1
;;(use-package edbi-minor-mode
;;  :defer t)
;;(use-package edbi-database-url
;;  :defer t)
(use-package biblio
  :defer t)
;;(use-package autodisass-java-bytecode
;;  :defer t)
(use-package tex
  :ensure auctex
  :defer t)
(use-package apache-mode
  :defer t)
(use-package nginx-mode
  :defer t)
(use-package nix-mode
  :defer t)
(use-package dired-du
  :defer t)
(use-package dired-git-info
  :defer t)
(use-package dired-preview
  :defer t)
;;(use-package dired-sidebar
;;  :defer t)
;;(use-package dired-subtree
;;  :defer t)
;;(use-package dired-toggle
;;  :defer t)
;;(use-package nix-sandbox
;;  :defer t)
(use-package editorconfig
  :config
  (editorconfig-mode 1))
(use-package projectile
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
  :hook ((treemacs-mode . (lambda () (eldoc-mode -1)))) ; Since inline-docs is used, this prevents annoying popups.
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
  :defer t
  :after (treemacs projectile))
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
(use-package minimap
  :disabled
  :defer t
  :custom-face
  (minimap-font-face ((t (:height 18))))
  :custom
  (minimap-minimum-width 18)
  (minimap-window-location 'right)
  (minimap-mode 1))
(use-package lorem-ipsum
  :defer t)
(use-package osm
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
  :custom
  (whitespace-style '(face tabs trailing space-before-tab empty space-after-tab tab-mark))
  :config
  (global-whitespace-mode))

(use-package highlight-indent-guides
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

;;; Evil mode customization.
(use-package evil-numbers
  :defer t
  :after (evil))
(use-package evil-surround
  :defer t
  :after (evil)
  :config
  (global-evil-surround-mode 1))
(use-package evil-textobj-tree-sitter
  :defer t
  :after (evil))
(use-package evil-org
  :pin melpa
  :after (evil org))

(use-package treemacs-evil
  :defer t
  :after (treemacs evil))

;; Enabled disabled by default commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-defun-include-comments 'disabled nil)

;;;; Customizer settings.

(setf custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

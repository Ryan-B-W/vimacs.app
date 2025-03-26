;; Early UI tweaks.
(setf inhibit-startup-screen t)

;; Silence non-critical warnings and errors.
(setf native-comp-async-report-warnings-errors 'silent)
(setf byte-compile-warnings
      '(not obsolete docstrings))

;; Set customizer file location.
(setf custom-file (concat user-emacs-directory "custom.el"))

;; Vimacs.app settings.
(defgroup vimacs nil
  "Emacs configuration and distribution."
  :tag "Vimacs")

(defcustom vimacs-config-user-init-path (expand-file-name "init.d/init.el" user-emacs-directory)
  "Path to user init file.

specifies user init file to load that is loaded after the
Vimacs.app configuration initial setup but before most of the
Vimacs.app configuration runs.")

(defcustom vimacs-config-user-notes-path (file-truename "~/notes/")
  "Directory containing user's general or non-project specific notes."
  :group 'vimacs
  :type 'directory)

(defcustom vimacs-config-auto-install-packages nil
  "If not-nil, automatically install needed system packages.

If nil, don't automatically install system packages to fulfill
external dependencies for Emacs packages.  If t, auto install
system packages."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-auto-install-packages-prompt t
  "If not-nil, prompt before attempting to install system packages.

If t, prompt user before installing system packages.  If nil,
don't prompt user before installing system packages."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-setup-fonts t
  "If not-nil, configure default fonts based on availability.

If t, automatically pick font families for default faces.  If
nil, don't change the fonts."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-setup-theme t
  "If not-nil, setup Modus Themes with customizations.

If t, configure and load Modus Themes with some customizations.
If nil, don't change the theme."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-theme-deuteranopia nil
  "If not-nil, use red-green color blindness accessible theme.

If t, use red-green color blindness (deuteranopia) accessible
theme variant.  If nil, don't use it.  If
vimacs-config-setup-theme is nil, this does nothing."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-backup-policy 'minimal
  "Policy for when and where to keep backups and autosaves.

Backup policy.  A symbol.  Can be one of \"full\", \"minimal\",
or \"none\".  Effect is as follows:
Full: standard backup policy for Emacs.
Minimal: standard backup policy for vimacs.app.  Only make autosaves.
None: don't make any backups or autosaves."
  :group 'vimacs
  :type '(choice (const :tag "Standard Emacs backup behavior" full)
                 (const :tag "Autosaves only, Vimacs.app default" minimal)
                 (const :tag "Don't make backups or autosaves" none)))

(defcustom vimacs-config-inline-help nil
  "If not-nil, show ElDoc help under point instead of minibuffer.

If t, use overlay frame or text overlay, for X11 frame or TTY
frame respectively, to show ElDoc help under point instead of in
the minibuffer.  If nil, use normal ElDoc behavior."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-minimap nil
  "If not-nil, enable Minimap package for buffer overview.

Not recommended if using Emacs in TTY frame since Minimap depends
on face height attributes."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-org-modern nil
  "If non-nil, enable Org-Modern package for a simi-WYSIWYG Org Mode Experience."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-auto-fill nil
  "If not-nil, automatically insert newlines to wrap long lines.

If t, auto wrap lines at fill-column columns.  If nil, don't
automatically insert newlines to wrap lines that go over
fill-column."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-wrap-style 'fancy
  "Whether to wrap lines, on words, and with indentation alignment.

Style of line visual wrapping.  A symbol.  Can be one of
\"default\", \"words\", \"fancy\", or \"none\".  Effect is as
follows:
Default: default Emacs behavior.
Words: wrap on word boundaries.
Fancy: wrap on word boundaries and indent to same level.
None: don't do any line wrapping.  Truncate instead."
  :group 'vimacs
  :type '(choice (const :tag "Fancy wrapping on word bounds and maintaining indentation" fancy)
                 (const :tag "Default Emacs behavior" default)
                 (const :tag "Wrap on word bounds" words)
                 (const :tag "Truncate lines instead of wrapping" none)))

(defcustom vimacs-config-enable-gpm nil
  "If not-nil, enable GPM mouse mode on startup.

If t, enable GPM mode.  If nil, don't enable GPM mode
automatically.  Disabled by default since when launching Emacs in
X11 it starts GPM Mode and then when attempting to create a TTY
frame the new frame crashes failing to connect to the GPM server
if it's not running."
  :group 'vimacs
  :type 'boolean)

(defcustom vimacs-config-dap-mode-instead-of-dape nil
  "If non-nil, enable dap-mode instead of Dape.

If t, enable dap-mode for providing DAP debugger functionality.
If nil, enable Dape for DAP debugger functionality."
  :group 'vimacs
  :type 'boolean)

;; Bootstrap Elpaca.
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Default to shallow clones for Elpaca.
(setf elpaca-order-defaults '(:protocol https
                              :inherit t
                              :depth 1))

;; Load user customizations before continuing with configuration.
(when (file-exists-p custom-file)
  (load custom-file))

;; Load use-package.
(if (fboundp 'use-package)
    (require 'use-package)
  (elpaca (use-package)))

;; Enable Elpaca use-package support.
(elpaca (elpaca-use-package)
  (elpaca-use-package-mode)
  (setf elpaca-use-package-by-default t))

(elpaca-wait)

;; If insufficient Emacs version, install and load compat.
(use-package compat
  :when (version< emacs-version "29.1")
  :defer t)

;; Setup capability to auto install system packages.
(use-package system-packages
  :when vimacs-config-auto-install-packages)

;; Custom keymap.
(defvar-keymap custom-search-map
  :doc "Custom keymap for aggregating search actions.")
(defvar-keymap custom-workspace-map
  :doc "Custom keymap for managing projects and workspaces.")
(defvar-keymap custom-leader-map
  :doc "Custom keymap for triggering various actions."
  "s" custom-search-map
  "w" custom-workspace-map)

;; Load user's configuration file, if it exists.
(when (file-exists-p vimacs-config-user-init-path)
  (load vimacs-config-user-init-path))

;; Set font.
(when vimacs-config-setup-fonts
  (cond ((member "Iosevka Comfy" (font-family-list))
         (set-face-attribute 'default nil :family "Iosevka Comfy")
         (set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy")
         (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Comfy Motion")
         (set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Duo"))
        ((member "Iosevka SS09" (font-family-list))
         (set-face-attribute 'default nil :family "Iosevka SS09")
         (set-face-attribute 'fixed-pitch nil :family "Iosevka SS09")
         (cond ((member "Iosevka Aile SS09" (font-family-list))
                (set-face-attribute 'variable-pitch nil :family "Iosevka Aile SS09"))
               ((member "Iosevka Aile" (font-family-list))
                (set-face-attribute 'variable-pitch nil :family "Iosevka Aile"))))
        ((member "Iosevka SS05" (font-family-list))
         (set-face-attribute 'default nil :family "Iosevka SS05")
         (set-face-attribute 'fixed-pitch nil :family "Iosevka SS05"))
        ((member "Fira Code" (font-family-list))
         (set-face-attribute 'default nil :family "Fira Code")
         (set-face-attribute 'fixed-pitch nil :family "Fira Code"))
        ((member "Source Code Pro" (font-family-list))
         (set-face-attribute 'default nil :family "Source Code Pro")
         (set-face-attribute 'fixed-pitch nil :family "Source Code Pro"))
        ((member "Inconsolata" (font-family-list))
         (set-face-attribute 'default nil :family "Inconsolata")
         (set-face-attribute 'fixed-pitch nil :family "Inconsolata"))))

(use-package modus-themes
  :demand t
  :when vimacs-config-setup-theme
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  :config
  (if vimacs-config-theme-deuteranopia
      (progn
        (setf modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia))
        (setf modus-vivendi-deuteranopia-palette-overrides
              `((bg-main "#141617")
                ;; Use some of the warmer colors from modus-vivendi.
                (bg-mode-line-active "#505050")
                (fg-mode-line-active "#ffffff")
                (border-mode-line-active "#959595")))
        ;; Borrow colors from modus-operandi-tinted for a warmer variation.
        (setf modus-operandi-deuteranopia-palette-overrides
              `((bg-main "#fbf7f0")
                (bg-dim "#efe9dd")
                (bg-active "#c9b9b0")
                (bg-inactive "#dfd5cf")
                (border "#9f9690")
                (bg-hl-line "#f1d5d0")
                (bg-region "#c2bcb5")
                (bg-mode-line-active "#cab9b2")
                (fg-mode-line-active "#000000")
                (border-mode-line-active "#545454")
                (bg-mode-line-inactive "#dfd9cf")
                (fg-mode-line-inactive "#585858")
                (border-mode-line-inactive "#a59a94")
                (bg-tab-bar "#e0d4ce")
                (bg-tab-current "#fbf7f0")
                (bg-tab-other "#c8b8b2")
                (bg-diff-context "#efe9df")
                (bg-paren-match "#7fdfcf")
                (fringe "#efe9dd")
                (bg-button-active "#c9b9b0")
                (bg-button-inactive "#efe9dd")
                (bg-line-number-inactive "#efe9dd")
                (bg-line-number-active "#c9b9b0")
                (fg-space "#9f9690")))
        (load-theme 'modus-vivendi-deuteranopia :no-confirm))
    (setf modus-themes-to-toggle '(modus-vivendi modus-operandi-tinted))
    (setf modus-vivendi-palette-overrides `((bg-main "#141617")))
    (load-theme 'modus-vivendi :no-confirm))
  (defun custom-modus-themes-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       `(telephone-line-projectile ((,c :foreground ,accent-2)))
       `(highlight-indent-guides-character-face ((,c :foreground ,border))))))
  (add-hook 'modus-themes-after-load-theme-hook #'custom-modus-themes-faces)
  (custom-modus-themes-faces)
  :bind (:map custom-workspace-map
              ("s" . modus-themes-toggle)))

;; Customize UI.
(setf initial-major-mode 'org-mode)
(when (featurep 'scroll-bar) (scroll-bar-mode -1))
(setf split-width-threshold 80)
(column-number-mode)
(global-display-line-numbers-mode 1)
(setf display-line-numbers-type 'relative)
(setq-default display-line-numbers-widen t)
(when (member vimacs-config-wrap-style '(words fancy)) (setq-default word-wrap t))
(when (eql vimacs-config-wrap-style 'none) (setq-default truncate-lines t))
(setf frame-resize-pixelwise t)
(when (featurep 'pixel-scroll) (pixel-scroll-precision-mode 1))

(use-package telephone-line
  :custom
  (telephone-line-lhs '((evil . (telephone-line-evil-tag-segment))
                        (accent . (telephone-line-vc-segment
                                   telephone-line-erc-modified-channels-segment
                                   telephone-line-process-segment))
                        (nil . (telephone-line-projectile-segment
                                telephone-line-buffer-segment))))
  (telephone-line-rhs '((nil . (telephone-line-flymake-segment
                                telephone-line-flycheck-segment
                                telephone-line-misc-info-segment))
                        (accent . (telephone-line-major-mode-segment))
                        (evil . (telephone-line-airline-position-segment))))
  :config
  (telephone-line-mode 1))

;; Enable additional mouse support.
(setf dired-mouse-drag-files t)
(setf mouse-drag-and-drop-region-cross-program t)
(xterm-mouse-mode 1)
;; TODO: fix issue with Emacs instance/server launched in X11 having
;; new tty frames launched in a terminal crash due to GPM server not
;; running when GPM mode is enabled.  For now, disabling
;; initialization time starting of GPM mode.
(when (and vimacs-config-enable-gpm
           (string-match-p "\\<GPM\\>" system-configuration-features))
  (ignore-errors (gpm-mouse-mode 1)))

;; Configure backup policy.
(cl-case vimacs-config-backup-policy
  ;; If "'full" don't make changes.
  (minimal
   ;; Disable backup files.
   (setf make-backup-files nil))
  (none
   ;; Disable backup files and autosaves.
   (setf make-backup-files nil)
   (setf auto-save-default nil)))

;; Prompt to delete autosaves when killing buffers.
(setf kill-buffer-delete-auto-save-files t)

;; Load Evil Mode.
(use-package evil
  :demand t
  :init
  (setf evil-want-keybinding nil)
  (setf evil-search-module 'isearch)
  (setf evil-move-beyond-eol t)
  (setf evil-move-cursor-back nil)
  (defun vimacs--custom-leader-map-override (mode-map)
    "This function exists purely to override major modes that generate
their keymaps at runtime instead of load time."
    (define-key mode-map (kbd "SPC") custom-leader-map)
    (define-key mode-map (kbd "<normal-state> SPC") custom-leader-map))
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "SPC") custom-leader-map)
  (define-key evil-visual-state-map (kbd "SPC") custom-leader-map)
  (define-key evil-motion-state-map (kbd "SPC") custom-leader-map))

(use-package evil-collection
  :demand t
  :after (evil magit)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

;; Fancy icons.
(use-package nerd-icons)
(use-package octicons)

;; More fancy auto-completion and matching than default.
(use-package flx)

(use-package ivy
  :demand t
  :after (flx)
  :config
  (setf ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package ivy-hydra
  :demand t
  :after (ivy)
  :bind
  (:map ivy-minibuffer-map
   ("M-o" . hydra-ivy/body)))

(use-package counsel
  :demand t
  :after (flx ivy)
  :bind
  (("C-x b" . counsel-switch-buffer)
   :map custom-leader-map
   ("?" . counsel-buffer-or-recentf)
   ("SPC" . counsel-switch-buffer)
   :map custom-search-map
   ("f" . counsel-find-file))
  :config
  (counsel-mode 1))

;; Fuzzy search in buffer.
(use-package swiper
  :bind (:map custom-leader-map
         ("/" . swiper)))

;; Better in buffer completion.
(use-package company
  :config
  (global-company-mode 1))

;; Setup/extend pcomplete and improve eshell and comint completions.
(use-package pcmpl-args)
(use-package capf-autosuggest
  :demand t
  :hook ((commint-mode eshell-mode) . capf-autosuggest-mode))
;; Eldoc customization.
(use-package inline-docs
  :when vimacs-config-inline-help
  :ensure (inline-docs :remotes ("fork" :type git :host github :repo "Ryan-B-W/inline-docs"))
  :init
  (setf inline-docs-border-symbol 9472))

(use-package eldoc-box
  :when (and (featurep 'term/common-win)
             vimacs-config-inline-help)
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
  :ensure nil
  :bind (:map custom-leader-map
         ("h" . eldoc-mode)
         ("H" . eldoc-print-current-symbol-info))
  :config
  (when (featurep 'inline-docs) (setf eldoc-message-function #'inline-docs))
  (setf eldoc-echo-area-use-multiline-p 5)
  (setf eldoc-echo-area-display-truncation-message nil))

(use-package info
  :ensure nil
  :bind (:map Info-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

(use-package help
  :ensure nil
  :bind (:map help-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

(use-package devdocs
  :bind (:map help-map
         ("D" . devdocs-lookup)))

;; Folding in programming modes.
(use-package hideshow ; hs-minor-mode.
  :ensure nil
  :hook ((prog-mode . hs-minor-mode)))

;; Configure dired.
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil))
  :custom
  (dired-listing-switches "-alh")
  (vimacs--custom-leader-map-override 'dired-mode-map))

;; enable dired-x.
(use-package dired-x
  :disabled
  :ensure nil
  :defer t
  :custom
  (dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

;; Start the emacsclient server.
(server-start)

;; Disable auto-fill-mode globally.
(unless vimacs-config-auto-fill (auto-fill-mode -1))

(use-package adaptive-wrap
  :when (eql vimacs-config-wrap-style 'fancy)
  :hook ((text-mode . adaptive-wrap-prefix-mode)
         (prog-mode . adaptive-wrap-prefix-mode)))

(use-package image-mode
  :ensure nil
  :bind (:map image-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil))
  :config
  (defcustom image-mode-hook nil
    "Run at the very end of `image-mode'."
    :group 'image
    :type 'hook
    :version "29.1")
  (add-hook 'image-mode-hook (lambda ()
                              (define-key image-mode-map (kbd "SPC") nil)
                              (define-key image-mode-map (kbd "<normal-state> SPC") nil))))

(use-package doc-view
  :ensure nil
  :bind (:map doc-view-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package printing
  :ensure nil)

(use-package dashboard
  :after (octicons nerd-icons)
  :bind (:map custom-leader-map
         ("u" . dashboard-open))
  :custom
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-display-icons-p t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  :config
  (dashboard-setup-startup-hook))

;; Emacs calc configuration.
(use-package calc
  :ensure nil
  :defer t
  :config
  (add-to-list 'calc-language-alist '(org-mode . latex)))

(use-package org
  :demand t
  :ensure (:wait t)
  :custom
  (org-startup-folded t)
  (org-startup-truncated truncate-lines)
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
  (org-directory vimacs-config-user-notes-path)
  ;; Set org-mode capture templates.
  (org-capture-templates
   `(("n" "Note to self" entry (file+headline ,(concat org-directory "notes.org") "Note to self/yet to be filed")
      "* %u %?")
     ("N" "Note to self with context" entry (file+headline ,(concat org-directory "notes.org") "Note to self/yet to be filed")
      "* %u %?\n  %l\n:  %i")
     ("m" "Note to self, with time" entry (file+headline ,(concat org-directory "notes.org") "Note to self/yet to be filed")
      "* %U %?")
     ("M" "Note to self with context, with time" entry (file+headline ,(concat org-directory "notes.org") "Note to self/yet to be filed")
      "* %U %?\n  %l\n:  %i")))
  ;; Customize org-mode refiling.
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-outline-path-complete-in-steps nil)
  ;; Set org-mode agenda and default notes files.
  (org-export-backends '(ascii beamer html icalendar latex man md odt org texinfo))
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
  (setf org-default-notes-file (concat org-directory "notes.org"))
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("mla" "\\documentclass[12pt,letterpaper]{mla}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                 nil (lambda (x y) (equal (car x) (car y)))))
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ;; org-mode style links in all buffers.
         ("C-c o" . org-open-at-point-global)))

;; Enable org-mode structure template expansions.
(use-package org-tempo
  :ensure nil
  :after (org)
  :demand t)

(use-package org-protocol
  :after (org)
  :ensure nil
  :demand t)

(use-package citar
  :after (org)
  :demand t
  :custom
  (citar-bibliography (list (concat vimacs-config-user-notes-path "bibliography/references.bib")))
  (org-cite-global-bibliography (list (concat vimacs-config-user-notes-path "bibliography/references.bib")))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map :package org
   ("C-c b i" . org-cite-insert)
   ("C-c b o" . citar-open-entry)
   :map custom-leader-map
   ("b i" . org-cite-insert)
   ("b o" . citar-open-entry)))

(use-package emacsql)

(use-package org-roam
  :after (org emacsql org-web-tools)
  :demand t
  :bind
  (("C-c r c" . org-roam-capture)
   ("C-c r f" . org-roam-node-find)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r q" . org-roam-tag-add)
   ("C-c r w" . org-roam-refile)
   ("C-c r x" . org-roam-extract-subtree)
   ("C-c r t" . org-roam-buffer-toggle)
   ("C-c r g" . org-roam-graph)
   ("C-c r d d" . org-roam-dailies-goto-today)
   ("C-c r d y" . org-roam-dailies-goto-yesterday)
   ("C-c r d t" . org-roam-dailies-goto-tomorrow)
   ("C-c r d g" . org-roam-dailies-goto-date)
   ("C-c r d n" . org-roam-dailies-goto-next-note)
   ("C-c r d p" . org-roam-dailies-goto-previous-note)
   :map custom-leader-map
   ("r c" . org-roam-capture)
   ("r f" . org-roam-node-find)
   ("r i" . org-roam-node-insert)
   ("r q" . org-roam-tag-add)
   ("r w" . org-roam-refile)
   ("r x" . org-roam-extract-subtree)
   ("r t" . org-roam-buffer-toggle)
   ("r g" . org-roam-graph)
   ("r d c" . org-roam-dailies-capture-today)
   ("r d d" . org-roam-dailies-goto-today)
   ("r d y" . org-roam-dailies-goto-yesterday)
   ("r d t" . org-roam-dailies-goto-tomorrow)
   ("r d g" . org-roam-dailies-goto-date)
   ("r d n" . org-roam-dailies-goto-next-note)
   ("r d p" . org-roam-dailies-goto-previous-note))
  :custom
  (org-roam-directory vimacs-config-user-notes-path)
  (org-roam-file-exclude-regexp
   `("data/"
     ,(concat "^" (expand-file-name org-roam-directory) "/other/")
     ,(concat "^" (expand-file-name org-roam-directory) "/.git/")
     ".*\\.gpg"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  ;; The capture ref templates are technically unsafe due to handling
  ;; of body property expansion inside Emacs Lisp snippet.
  ;;
  ;; TODO: modify org-roam to handle function templates that take an
  ;; alist of properties and returns a pre-formatted string to use
  ;; without the usual org-capture and org-roam-capture templating
  ;; keyword hackery.
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?%(unless (string-blank-p \"${body}\") \"\n#+begin_quote\n${body}\n#+end_quote\")" :target
      (file+head "${slug}.org" "#+title: ${title}")
      :unnarrowed t)
     ("w" "web-ref" plain "%?%(unless (string-blank-p \"${body}\") (concat \"\n#+begin_quote\n\" (replace-regexp-in-string \" +$\" \"\" (replace-regexp-in-string \"^\" \"  \" (string-trim (org-web-tools--html-to-org-with-pandoc (replace-regexp-in-string \"&nbsp;\" \" \" \"${body}\"))))) \"\n#+end_quote\"))" :target
      (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :website:")
      :unnarrowed t)))
  (org-roam-graph-executable "sfdp")
  :config
  (org-roam-db-autosync-mode 1)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-hight . fit-window-to-buffer)))
  (defun custom-org-roam-refs-at-point ()
    (if (eq major-mode 'org-mode)
        (if (org--link-at-point)
            ;; The following would be cleaner to do with org-roam-node
            ;; accessors but the one for refs doesn't return a full
            ;; URI in the current version of org-roam.
            (when-let ((node (org-roam-node-from-id (url-filename (url-generic-parse-url (org--link-at-point))))))
              (with-current-buffer (find-file-noselect (org-roam-node-file node))
                (org-property-values "roam_refs")))
          (if (org-roam-node-at-point)
              (org-property-values "roam_refs")
            (message "No Org Roam node with refs or link to node with refs at point.")))
      (message "Not in an Org Mode buffer.  Nothing to do.")
      nil))
  (defun custom-org-roam-visit-first-ref-at-point ()
    (interactive)
    (when-let ((first-ref (car (custom-org-roam-refs-at-point))))
      (org-link-open-from-string first-ref)))
  (bind-key (kbd "r o") 'custom-org-roam-visit-first-ref-at-point custom-leader-map))

(use-package org-roam-protocol
  :after (org-roam)
  :ensure nil
  :demand t)

(use-package citar-org-roam
  :after (citar org-roam)
  :config
  (citar-org-roam-mode))

(use-package org-roam-ui
  :after (org-roam)
  :bind
  (("C-c r G" . org-roam-ui-mode)
   :map custom-leader-map
   ("r G" . org-roam-ui-mode)))

(use-package org-web-tools
  :after (org)
  :demand t
  :autoload
  (org-web-tools--html-to-org-with-pandoc)
  :bind
  (:map custom-leader-map
   ("o l" . org-web-tools-insert-link-for-url)
   ("o i" . org-web-tools-insert-web-page-as-entry)
   ("o r" . org-web-tools-read-url-as-org)
   ("o w" . org-web-tools-convert-links-to-page-entries)
   ("o a" . org-web-tools-archive-attach)
   ("o v" . org-web-tools-archive-view))
  :config
  (setf org-web-tools--pandoc-no-wrap-option "--wrap=none"))

;; Enable more Org-Mode link types.
(use-package ol-man
  :after (org)
  :ensure nil
  :demand t)

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
  :when vimacs-config-org-modern
  :after (org))

;; Setup org-mode with which-function-mode.
;; Originally taken from https://emacs.stackexchange.com/a/30901
(use-package which-func
  :ensure nil
  :config
  (defun org-which-function ()
    (interactive)
    (when (and (eq major-mode 'org-mode)
               (org-current-level))
      (mapconcat 'identity (org-get-outline-path t)
                 " ⟼ ")))
  (add-to-list 'which-func-functions #'org-which-function)
  (which-function-mode 1))

(use-package auth-source
  :ensure nil
  :config
  (auth-source-pass-enable))

;; Use transparent encryption.
(use-package epa-file
  :ensure nil
  :config
  (epa-file-enable)
  (setf epa-pinentry-mode 'loopback))

(use-package pinentry)

;; Customize bs-show.
(use-package bs
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

(use-package transient
  :after (seq))

(use-package magit
  :after (transient)
  :bind (:map magit-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)
         :map magit-diff-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)
         :map magit-log-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)
         :map magit-section-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)
         :map magit-repolist-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)
         :map magit-submodule-list-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)
         :map magit-blame-read-only-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

(use-package forge
  :after (magit emacsql))
(use-package magit-annex
  :after (magit))

(use-package magit-lfs
  :after (magit))

(use-package git-modes
  :defer t)

(use-package diff-hl
  :after (magit)
  :defer nil
  :hook
  ((magit-refresh-buffer . diff-hl-update))
  :config
  (global-diff-hl-mode 1))

(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

(use-package dap-mode
  :when vimacs-config-dap-mode-instead-of-dape
  :bind (:map custom-leader-map
         ("d b" . dap-breakpoint-toggle)
         ("d B" . dap-breakpoint-condition))
  :custom
  (dap-auto-configure-mode t))

(use-package jsonrpc)

(use-package dape
  :after (jsonrpc)
  :unless vimacs-config-dap-mode-instead-of-dape
  :bind (:map custom-leader-map
         ("d b" . dape-breakpoint-toggle)
         ("d B" . dape-breakpoint-expression))
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-cwd-fn 'projectile-project-root))

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
         (lisp-data-mode . smartparens-strict-mode)
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
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)))

(use-package evil-cleverparens
  :after (evil smartparens)
  :demand t
  :hook ((smartparens-mode . evil-cleverparens-mode)))

(use-package slime
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
  :ensure nil
  :defer t)

;; Setup run-scheme.
(use-package scheme
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
  :ensure nil
  :if (fboundp 'treesit-install-language-grammar)
  :init
  (setf treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
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
          (gdscript-mode . gdscript-ts-mode)
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
  :ensure nil
  :if (fboundp 'eglot)
  :hook ((bash-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-or-c++-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (csharp-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (dockerfile-ts-mode . eglot-ensure)
         (gdscript-ts-mode . eglot-ensure)
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
         (yaml-ts-mode . eglot-ensure))
  :custom
  (eglot-extend-to-xref t))

(use-package verb
  :defer t
  :after (org)
  :init
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package which-key
  :config
  (which-key-mode 1))
(use-package yaml-ts-mode
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
(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" "\\.js\\'"))
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
(use-package gdscript-mode
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
  :bind
  (:map custom-leader-map
   ("b f" . biblio-lookup)))
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
  :defer t
  :when vimacs-config-minimap
  :custom-face
  (minimap-font-face ((t (:height 0.18))))
  :custom
  (minimap-minimum-width 18)
  (minimap-window-location 'right)
  (minimap-mode 1))

(use-package lorem-ipsum
  :defer t)

(use-package osm
  :defer t)

(use-package rfc-mode
  :defer t
  :bind
  (:map rfc-mode-map
   ("SPC" . nil)
   ("<normal-state> SPC" . nil)))

;; Configuring gnus.
(use-package gnus
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

;; Setup flyspell.
(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; Setup flymake.
(use-package flymake
  :ensure nil
  :hook
  (text-mode . flymake-mode))

;; Setup LanguageTool.
(use-package flymake-languagetool
  :hook
  (text-mode . flymake-languagetool-load)
  (find-file . flymake-languagetool-maybe-load)
  :init
  (setf flymake-languagetool-server-command (list "languagetool-server" "--port" "8081" "--allow-origin")))

;; Enable whitespace mode.
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face tabs trailing space-before-tab empty space-after-tab tab-mark))
  :config
  (global-whitespace-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode . (lambda ()
                        (modus-themes-with-colors
                          (highlight-indent-guides-mode 1)
                          (set-face-foreground 'highlight-indent-guides-character-face border)
                          (set-face-background 'highlight-indent-guides-even-face nil)
                          (set-face-background 'highlight-indent-guides-odd-face nil)))))
  :config
  (setf highlight-indent-guides-method 'character)
  (setf highlight-indent-guides-character ?\┊)
  (set-face-background 'highlight-indent-guides-even-face nil)
  (set-face-background 'highlight-indent-guides-odd-face nil))

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
  :after (evil org)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package treemacs-evil
  :defer t
  :after (treemacs evil))

;; Enabled disabled by default commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-defun-include-comments 'disabled nil)

;; Load customizer settings.
(add-hook 'elpaca-after-init-hook (lambda () (when (file-exists-p custom-file)
                                               (load custom-file))))

;; Restore garbage collector settings to defaults.
(add-hook 'elpaca-after-init-hook (lambda () (setf gc-cons-threshold vimacs-gc-cons-threshold-default)))

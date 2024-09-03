;; Early UI tweaks.
(setf inhibit-startup-screen t)

;; Set customizer file location.
(setf custom-file (concat user-emacs-directory "custom.el"))

;; Default Vimacs.app settings.
(defvar vimacs-config-user-notes-path (file-truename "~/doc/")
  "Directory containing user's general or non-project specific notes.")

(defvar vimacs-config-suppress-compatibility-checks nil
  "If non-nil, do compatibility checks against Emacs features.

If nil, make compatibility checks and raise warnings when they
fail.  If t, skip compatibility checks.")
(defvar vimacs-config-auto-install-packages 'not-set
  "If not-nil, automatically install needed system packages.

If nil, don't automatically install system packages to fulfill
external dependencies for Emacs packages.  If t, auto install
system packages.  If \"not-set\", assume this setting hasn't been
customized by the user and prompt them for a setting.")
(defvar vimacs-config-auto-install-packages-prompt t
  "If not-nil, prompt before attempting to install system packages.

If t, prompt user before installing system packages.  If nil,
don't prompt user before installing system packages.")
(defvar vimacs-config-setup-fonts t
  "If not-nil, configure default fonts based on availability.

If t, automatically pick font families for default faces.  If
nil, don't change the fonts.")
(defvar vimacs-config-setup-theme t
  "If not-nil, setup Modus Themes with customizations.

If t, configure and load Modus Themes with some customizations.
If nil, don't change the theme.")
(defvar vimacs-config-theme-deuteranopia nil
  "If not-nil, use red-green color blindness accessible theme.

If t, use red-green color blindness (deuteranopia) accessible
theme variant.  If nil, don't use it.  If
vimacs-config-setup-theme is nil, this does nothing.")
(defvar vimacs-config-backup-policy 'minimal
  "Policy for when and where to keep backups and autosaves.

Backup policy.  A symbol.  Can be one of \"full\", \"minimal\",
or \"none\".  Effect is as follows:
Full: standard backup policy for Emacs.
Minimal: standard backup policy for vimacs.app.  Only make autosaves.
None: don't make any backups or autosaves.")
(defvar vimacs-config-inline-help nil
  "If not-nil, show ElDoc help under point instead of minibuffer.

If t, use overlay frame or text overlay, for X11 frame or TTY
frame respectively, to show ElDoc help under point instead of in
the minibuffer.  If nil, use normal ElDoc behavior.")
(defvar vimacs-config-minimap nil
  "If not-nil, enable Minimap package for buffer overview.

Not recommended if using Emacs in TTY frame since Minimap depends
on face height attributes.")
(defvar vimacs-config-org-modern nil
  "If non-nil, enable Org-Modern package for a simi-WYSIWYG Org Mode Experience.")
(defvar vimacs-config-auto-fill nil
  "If not-nil, automatically insert newlines to wrap long lines.

If t, auto wrap lines at fill-column columns.  If nil, don't
automatically insert newlines to wrap lines that go over
fill-column.")
(defvar vimacs-config-wrap-style 'fancy
  "Whether to wrap lines, on words, and with indentation alignment.

Style of line visual wrapping.  A symbol.  Can be one of
\"default\", \"words\", \"fancy\", or \"none\".  Effect is as
follows:
Default: default Emacs behavior.
Words: wrap on word boundaries.
Fancy: wrap on word boundaries and indent to same level.
None: don't do any line wrapping.  Truncate instead.")
(defvar vimacs-config-enable-gpm nil
  "If not-nil, enable GPM mouse mode on startup.

If t, enable GPM mode.  If nil, don't enable GPM mode
automatically.  Disabled by default since when launching Emacs in
X11 it starts GPM Mode and then when attempting to create a TTY
frame the new frame crashes failing to connect to the GPM server
if it's not running.")
(defvar vimacs-config-dap-mode-instead-of-dape nil
  "If non-nil, enable dap-mode instead of Dape.

If t, enable dap-mode for providing DAP debugger functionality.
If nil, enable Dape for DAP debugger functionality.")

;; Load Vimacs.app configuration.
(setf vimacs-config-file (concat user-emacs-directory "vimacs.app-config.el"))
(if (file-exists-p vimacs-config-file)
    (load vimacs-config-file)
  (warn "Unable to load and/or write \"%s\" config file.  Vimacs.app will not be customizable without it." vimacs-config-file))

;; On (probable) first start ask user to set system package
;; auto-install setting.
(when (eql vimacs-config-auto-install-packages 'not-set)
  (setf vimacs-config-auto-install-packages
        (yes-or-no-p "This seems to be the initial launch of this config.  Enable auto install of system packages?"))
  (when vimacs-config-auto-install-packages
    (setf vimacs-config-auto-install-packages-prompt
          (yes-or-no-p "Prompt each time before installing system packages?"))))

(unless (file-exists-p vimacs-config-file)
  (write-region (format "(setf vimacs-config-user-notes-path \"%s\"
      org-agenda-files '%s)
(setf vimacs-config-suppress-compatibility-checks '%s
      vimacs-config-auto-install-packages '%s
      vimacs-config-auto-install-packages-prompt '%s
      vimacs-config-setup-fonts '%s
      vimacs-config-setup-theme '%s
      vimacs-config-theme-deuteranopia '%s
      vimacs-config-backup-policy '%s
      vimacs-config-inline-help '%s
      vimacs-config-minimap '%s
      vimacs-config-org-modern '%s
      vimacs-config-auto-fill '%s
      vimacs-config-wrap-style '%s
      ;; Not recommended to automatically enable GPM mode.  Enable
      ;; manually from a TTY frame instead.
      vimacs-config-enable-gpm '%s)
      vimacs-config-dap-mode-instead-of-dape '%s"
                        vimacs-config-user-notes-path
                        (if org-agenda-files
                            org-agenda-files
                          (list vimacs-config-user-notes-path
                                (concat (expand-file-name vimacs-config-user-notes-path) "daily/")))
                        vimacs-config-suppress-compatibility-checks
                        vimacs-config-auto-install-packages
                        vimacs-config-auto-install-packages-prompt
                        vimacs-config-setup-fonts
                        vimacs-config-setup-theme
                        vimacs-config-theme-deuteranopia
                        vimacs-config-backup-policy
                        vimacs-config-inline-help
                        vimacs-config-minimap
                        vimacs-config-org-modern
                        vimacs-config-auto-fill
                        vimacs-config-wrap-style
                        vimacs-config-enable-gpm
                        vimacs-config-dap-mode-instead-of-dape)
                nil vimacs-config-file))

;; Do compatibility checks.
(unless vimacs-config-suppress-compatibility-checks
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
(unless (and (member '("melpa-stable" . "https://stable.melpa.org/packages/") package-archives)
             (member '("melpa" . "https://melpa.org/packages/") package-archives))
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))
(setf package-archive-priorities
      '(("gnu" . 20)
        ("nongnu" . 20)
        ("melpa-stable" . 10)
        ("melpa" . 0)))
;; Auto install configured packages.
(unless (fboundp 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setf use-package-always-ensure t)

;; If insufficient Emacs version, install and load compat.
(use-package compat
  :if (version< emacs-version "29.0"))

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
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (featurep 'scroll-bar) (scroll-bar-mode -1))
(setf split-width-threshold 80)
(column-number-mode)
(global-display-line-numbers-mode 1)
(setf display-line-numbers-type 'relative)
(setq-default display-line-numbers-widen t)
(when (member vimacs-config-wrap-style '(words fancy)) (setq-default word-wrap t))
(when (eql vimacs-config-wrap-style 'none) (setq-default truncate-lines t))
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
  (setf evil-search-module 'evil-search)
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
  :pin manual
  :ensure nil
  :when vimacs-config-inline-help
  :init
  (unless (package-installed-p (intern "inline-docs"))
    (package-vc-install '(inline-docs . (:url "https://github.com/Ryan-B-W/inline-docs.git"))))
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
  :pin manual
  :ensure nil
  :bind (:map custom-leader-map
         ("h" . eldoc-mode)
         ("H" . eldoc-print-current-symbol-info))
  :config
  (when (featurep 'inline-docs) (setf eldoc-message-function #'inline-docs))
  (setf eldoc-echo-area-use-multiline-p 5)
  (setf eldoc-echo-area-display-truncation-message nil))

(use-package info
  :pin manual
  :ensure nil
  :bind (:map Info-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

(use-package help
  :pin manual
  :ensure nil
  :bind (:map help-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

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
  :bind (:map dired-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil))
  :custom
  (dired-listing-switches "-alh")
  (vimacs--custom-leader-map-override 'dired-mode-map))

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
(unless vimacs-config-auto-fill (auto-fill-mode -1))

(use-package adaptive-wrap
  :when (eql vimacs-config-wrap-style 'fancy)
  :hook ((text-mode . adaptive-wrap-prefix-mode)
         (prog-mode . adaptive-wrap-prefix-mode)))

(use-package image-mode
  :pin manual
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
  :pin manual
  :ensure nil
  :bind (:map doc-view-mode-map
         ("SPC" . nil)
         ("<normal-state> SPC" . nil)))

(use-package printing
  :pin manual
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
  :pin manual
  :ensure nil
  :defer t
  :config
  (unless (member '(org-mode . latex) calc-language-alist)
    (add-to-list 'calc-language-alist
                 '(org-mode . latex))))

(use-package org
  :pin gnu
  :ensure nil
  :demand t
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
  (org-agenda-files
   `(,(concat org-directory "notes.org")
     ,@vimacs-config-additional-org-agenda-files))
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
  (unless (memq 'org-habit org-modules )
    (add-to-list 'org-modules 'org-habit))
  ;; Configure clocking.
  (org-clock-persistence-insinuate)
  ;; Setup org-mode capture.
  (setf org-default-notes-file (concat org-directory "notes.org"))
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
  :after (org)
  :demand t)

(use-package org-protocol
  :after (org)
  :pin manual
  :ensure nil
  :demand t)

(use-package org-roam
  :after (org)
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
   :map custom-leader-map
   ("r c" . org-roam-capture)
   ("r f" . org-roam-node-find)
   ("r i" . org-roam-node-insert)
   ("r q" . org-roam-tag-add)
   ("r w" . org-roam-refile)
   ("r x" . org-roam-extract-subtree)
   ("r t" . org-roam-buffer-toggle)
   ("r g" . org-roam-graph)
   ("r d d" . org-roam-dailies-goto-today)
   ("r d y" . org-roam-dailies-goto-yesterday)
   ("r d t" . org-roam-dailies-goto-tomorrow))
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
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?\n#+begin_quote\n${body}\n#+end_quote" :target
      (file+head "${slug}.org" "#+title: ${title}")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode 1)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-hight . fit-window-to-buffer))))

(use-package org-roam-protocol
  :after (org-roam)
  :pin manual
  :ensure nil
  :demand t)

(use-package org-roam-ui
  :after (org-roam)
  :bind
  (("C-c r w" . org-roam-ui-mode)
   :map custom-leader-map
   ("r w" . org-roam-ui-mode)))

;; Enable more Org-Mode link types.
(use-package ol-man
  :after (org)
  :pin manual
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
  :pin melpa
  :defer t
  :after (org magit)
  :config
  (use-package orgit-forge
    :pin melpa
    :after (org magit forge)))

(use-package org-modern
  :defer t
  :when vimacs-config-org-modern
  :after (org))

;; Setup org-mode with which-function-mode.
;; Originally taken from https://emacs.stackexchange.com/a/30901
(use-package which-func
  :pin manual
  :ensure nil
  :config
  (defun org-which-function ()
    (interactive)
    (when (and (eq major-mode 'org-mode)
               (org-current-level))
      (mapconcat 'identity (org-get-outline-path t)
                 " ⟼ ")))
  (unless (memql #'org-which-function which-func-functions)
    (add-to-list 'which-func-functions #'org-which-function))
  (which-function-mode 1))

(use-package auth-source
  :pin manual
  :ensure nil
  :config
  (auth-source-pass-enable))

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

(use-package magit
  :pin melpa
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
  :pin melpa
  :after (magit))
(use-package magit-annex
  :pin melpa
  :after (magit))
(use-package magit-lfs
  :pin melpa
  :after (magit))

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

(use-package dape
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
  (unless (member (expand-file-name "~/quicklisp/dists/quicklisp/software/slime-2.13/doc") Info-directory-list)
    (add-to-list 'Info-directory-list (expand-file-name "~/quicklisp/dists/quicklisp/software/slime-2.13/doc"))))

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
  :pin manual
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
(use-package typescript-ts-mode
  :pin manual
  :ensure nil
  :defer t
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
  :pin melpa
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

;; Setup flyspell.
(use-package flyspell
  :pin manual
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

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
  :ensure t
  :pin melpa
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
(when (file-exists-p custom-file)
  (load custom-file))

;; Restore garbage collector settings to defaults.
(setf gc-cons-threshold vimacs-gc-cons-threshold-default)

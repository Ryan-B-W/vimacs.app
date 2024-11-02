;; Do some startup optimizations.
(setf vimacs-gc-cons-threshold-default gc-cons-threshold)
(setf gc-cons-threshold 128000000)

;; Do some early tweaks to the UI before starting.
(menu-bar-mode -1)
(tool-bar-mode -1)
(setf initial-frame-alist '((foreground-color . "#ffffff")
                            (background-color . "#141617")
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)))

;; Don't load outdated byte-compiled files.
(setq load-prefer-newer t)

;; Disable package.el in favor of Elpaca.
(setf package-enable-at-startup nil)

;; Copyright (c) 2023 Ryan Wilcox
;;
;; Modified from original theme by Tommaso Laurenzi under the MIT license.  This
;; file is under the same MIT license and the orignal copyright notice follows.
;;
;; MIT License
;;
;; Copyright (c) 2021 Tommaso Laurenzi
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(deftheme kunagawa-dark-julian-mix
  "Created 2023-08-13, edited 2023-08-30.")

(custom-theme-set-faces
 'kunagawa-dark-julian-mix
 '(cursor ((t (:weight bold :foreground "#1d2021" :background "#A3D4D5"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((t (:foreground "#A3D4D5"))))
 '(minibuffer-prompt ((t (:foreground "#E6C384" :background "#49443C"))))
 '(highlight ((t (:foreground "#938AA9" :background "#54536D"))))
 '(region ((t (:extend t :background "#2D4F67"))))
 '(shadow ((t (:background "#1d2021"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "#54536D"))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:foreground "#7FB4CA"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :foreground "#727169"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#727169"))))
 '(font-lock-constant-face ((t (:foreground "#FFA066"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:foreground "#54536D"))))
 '(font-lock-doc-markup-face ((t (:foreground "#54536D"))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face) :foreground "#DCD7BA"))))
 '(font-lock-function-name-face ((t (:foreground "#7FB4CA"))))
 '(font-lock-keyword-face ((t (:foreground "#957FB8" :slant oblique :weight semi-bold))))
 '(font-lock-negation-char-face ((t (:foreground "#FF5D62"))))
 '(font-lock-number-face ((t nil)))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "#C0A36E"))))
 '(font-lock-property-name-face ((t (:foreground "#957FB8"))))
 '(font-lock-property-use-face ((t (:foreground "#E6C384" :inherit (font-lock-property-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#C0A36E"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#98BB6C"))))
 '(font-lock-type-face ((t (:foreground "#7AA89F"))))
 '(font-lock-variable-name-face ((t (:foreground "#957FB8"))))
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:foreground "#FF9E3B"))))
 '(button ((t (:foreground "#7AA89F"))))
 '(link ((t (:foreground "#7E9CD8"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(fringe ((t (:foreground "#1d2021"))))
 '(header-line ((t (:background "#16161D"))))
 '(tooltip ((t (:weight bold :foreground "#16161D" :background "#E6C384"))))
 '(mode-line ((t (:background "#1d2021"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "#7AA89F"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:foreground "#C0A36E"))))
 '(mode-line-inactive ((t (:foreground "#504945" :weight normal))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:distant-foreground "black" :background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:distant-foreground "white" :background "paleturquoise4")) (((class color) (min-colors 16)) (:distant-foreground "white" :background "turquoise3")) (((class color) (min-colors 8)) (:distant-foreground "white" :background "turquoise3")) (t (:underline (:color foreground-color :style line :position nil)))))
 '(match ((t (:foreground "#16161D" :background "#E6C384"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(default ((t (:background "#141617" :foreground "#DCD7BA"))))
 '(ivy-current-match ((t (:extend t :background "#7E9CD8" :foreground "#1d2021" :weight bold))))
 '(ivy-action ((t (:background unspecified :foreground "#DCD7BA"))))
 '(ivy-grep-line-number ((t (:background unspecified :foreground "#98BB6C"))))
 '(ivy-grep-info ((t (:foreground "#A3D4D5"))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#E46876"))))
 '(ivy-minibuffer-match-face-2 ((t (:background unspecified :foreground "#98BB6C"))))
 '(ivy-minibuffer-match-highlight ((t (:foreground "#A3D4D5"))))
 '(ivy-confirm-face ((t (:foreground "#7AA89F"))))
 '(swiper-line-face ((t (:foreground "#E6C384"))))
 '(swiper-match-face-1 ((t (:background "#FFA066" :foreground "#1d2021"))))
 '(swiper-match-face-2 ((t (:background "#7E9CD8" :foreground "#1d2021"))))
 '(swiper-match-face-3 ((t (:background "#C0A36E" :foreground "#1d2021"))))
 '(swiper-match-face-4 ((t (:background "#FF5D62" :foreground "#1d2021"))))
 '(company-tooltip ((t (:background "#3c3836"))))
 '(company-tooltip-common ((t (:foreground "#DCA561"))))
 '(company-tooltip-search ((t (:background "#E6C384" :distant-foreground "#DCD7BA" :foreground "#1d2021"))))
 '(company-tooltip-selection ((t (:background "#FF5D62" :foreground "#43242B" :weight bold))))
 '(company-tooltip-mouse ((t (:inherit highlight :background "#3c3836" :distant-foreground "#DCD7BA" :foreground "#1d2021"))))
 '(company-tooltip-annotation ((t (:distant-foreground "#282828" :foreground "#FF5D62"))))
 '(company-preview ((t (:foreground "#E6C384"))))
 '(company-preview-common ((t (:foreground "#FF5D62" :weight bold))))
 '(company-preview-search ((t (:inherit company-tooltip-search))))
 '(company-template-field ((t (:inherit match)))))

(provide-theme 'kunagawa-dark-julian-mix)

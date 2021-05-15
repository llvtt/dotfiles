(deftheme text-adventure
  "Created 2021-05-13.")

(custom-theme-set-faces
 'text-adventure
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "steelblue1"))))
 '(highlight ((t (:underline t :weight bold))))
 '(region ((t (:extend t :inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "mediumpurple"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "gray25"))))
 '(font-lock-constant-face ((t (:foreground "mediumpurple2"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "orange1" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "firebrick2"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "olivedrab3"))))
 '(font-lock-type-face ((t (:foreground "steelblue1"))))
 '(font-lock-variable-name-face ((t (:foreground "#bbbe93"))))
 '(font-lock-warning-face ((t (:inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:background "gray28" :foreground "black"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray14" :foreground "black" :weight light))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(custom-variable-tag ((t (:inherit font-lock-function-name-face))))
 '(show-paren-match ((t (:background "gray15"))))
 '(diff-added ((t (:background "#113311" :extend t :inherit diff-changed))))
 '(diff-removed ((t (:background "#331111" :extend t :inherit diff-changed))))
 '(diff-refine-added ((t (:background "#336633" :inherit diff-refine-changed))))
 '(diff-refine-removed ((t (:background "#663333" :inherit diff-refine-changed))))
 '(diff-header ((t (:extend t :background "gray55"))))
 '(flycheck-warning ((t (:inherit warning :inverse-video t :underline t))))
 '(flycheck-error ((t (:inherit error))))
 '(flycheck-info ((t (:underline t :inverse-video t))))
 '(line-number ((t (:inherit (shadow default) :background "gray7" :foreground "wheat4"))))
 '(line-number-major-tick ((t (:inherit line-number-minor-tick :weight bold))))
 '(line-number-minor-tick ((t (:inherit line-number :background "gray10" :weight normal))))
 '(smerge-upper ((t (:inherit diff-removed :extend t))))
 '(smerge-lower ((t (:inherit diff-added :extend t))))
 '(sml/projectile ((t (:slant italic :inherit sml/git))))
 '(sml/modified ((t (:inherit sml/not-modified :foreground "indianred"))))
 '(sml/git ((t (:inherit sml/prefix))))
 '(sml/col-number ((t (:foreground "snow4"))))
 '(undo-tree-visualizer-active-branch-face ((t (:foreground "dodgerblue" :weight bold))))
 '(undo-tree-visualizer-unmodified-face ((((class color)) (:foreground "cyan"))))
 '(hl-line ((t (:background "#101000" :weight bold))))
 '(error ((t (:foreground "Red1" :inverse-video t :underline t :weight bold))))
 '(tree-sitter-hl-face:operator ((t (:inherit default))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#3a4b20"))))
 '(lsp-ui-doc-background ((t (:background "#202083"))))
 '(lsp-ui-peek-peek ((t (:background "#202043"))))
 '(lsp-ui-peek-header ((((background light)) (:foreground "white" :background "grey30")) (t (:foreground "black" :background "white"))))
 '(lsp-ui-peek-list ((t (:background "#202083"))))
 '(default ((t (:inherit nil :extend nil :stipple nil :foreground "#eae5d7" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(smerge-markers ((t (:extend t :background "#363636")))))

(provide-theme 'text-adventure)

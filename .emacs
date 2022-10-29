;; Load customizations first to prevent errors about unsafe custom
;; themes loaded in the rest of this file, for example.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;;;;;;;;;;;;;;;;;;
;; package setup ;;
;;;;;;;;;;;;;;;;;;;

;; https://github.com/raxod502/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)

;;;;;;;;;;
;; evil ;;
;;;;;;;;;;

(use-package evil
  :init
  (setq
   evil-regexp-search nil
   ;; make it easier to see paren matches
   evil-show-paren-range 1
   ;; don't add replaced text to the kill ring
   evil-kill-on-visual-paste nil
   ;; try to be more like OG emacs when dealing with yanking stuff in general
   evil-want-Y-yank-to-eol t
   ;; try to be more like emacs when dealing with window management
   evil-split-window-below t
   evil-vsplit-window-right t
   ;; use M-(number) for prefix argument instead and retain vim movement behavior
   evil-want-C-u-scroll t
   evil-want-C-u-delete t

   ;; these need to be set for 'evil-collection to work properly
   evil-want-integration t
   evil-want-keybinding nil

   evil-undo-system 'undo-tree
   )

  :hook
  ;; TODO - add keybindings for "backward"
  ;; - abstract this somehow to a keymap?
  (
   (help-mode . (lambda () (evil-local-set-key 'normal (kbd "C-j") 'forward-button)))
   (custom-new-theme-mode . (lambda () (evil-local-set-key 'normal (kbd "C-j") 'forward-button)))
   (compilation-mode . (lambda () (evil-local-set-key 'normal (kbd "C-j") 'compilation-next-error)))
   (log-view-mode . (lambda ()
                      (evil-local-set-key 'normal (kbd "C-j") 'log-view-msg-next)
                      (evil-local-set-key 'normal (kbd "C-k") 'log-view-msg-prev)))
   )
  :config
  ;; must be activated after setting evil-want-* variables
  (evil-mode t)
  )
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init)
  (add-to-list 'evil-collection-mode-list 'ripgrep)
  )
;; https://github.com/emacs-evil/evil-surround
;; cs<old><new>
;; ds<operator to delete>
(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )
(use-package evil-terminal-cursor-changer
  :config
  (setq
   evil-normal-state-cursor 'hbar)
  (unless (display-graphic-p) (evil-terminal-cursor-changer-activate)))

;;;;;;;;;
;; LSP ;;
;;;;;;;;;

(use-package lsp-mode
  :hook ((ruby-mode . lsp)
         (web-mode . lsp)
         (python-mode . lsp)
         (sh-mode . lsp) ; https://github.com/bash-lsp/bash-language-server
         (go-mode . lsp))
  :config
  (setq
   lsp-response-timeout 2
   lsp-headerline-breadcrumb-icons-enable nil)
  :bind
  (:map evil-normal-state-map
        ("<SPC>lr" . lsp-rename)
        ("<SPC>ll" . lsp-workspace-restart)
        ("<SPC>la" . lsp-execute-code-action)
        ))

(use-package lsp-ui
  :bind
  (:map evil-normal-state-map
        ("<SPC>li" . lsp-ui-imenu)
        ("<SPC>lfr" . lsp-ui-peek-find-references)
        ("<SPC>lfg" . lsp-find-references))
  :config
  (add-hook 'lsp-ui-peek-mode-hook
            (lambda ()
              (evil-define-key nil lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev)
              (evil-define-key nil lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next)
              (evil-define-key nil lsp-ui-peek-mode-map (kbd "M-k") 'lsp-ui-peek--select-prev-file)
              (evil-define-key nil lsp-ui-peek-mode-map (kbd "M-j") 'lsp-ui-peek--select-next-file)
              )))

;;;;;;;;;;;;;
;; graphql ;;
;;;;;;;;;;;;;

(use-package request) ; dependency of emacs-graphql
; npm i graphql-language-service-cli
(use-package graphql)

;;;;;;;;;;;;;;;;;;;;;;
;; general packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package sql
  :config
  (setq sql-dialect "postgres"))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package undo-tree
  :init
  (global-undo-tree-mode nil)
  :config
  (setq undo-tree-history-directory-alist
        '((".*" . "~/.emacs.d/backups")))
  )

;; TODO - this is broken under emacs 28:
;; https://www.reddit.com/r/emacs/comments/kqb9s9/cannot_recompile_packagess_error_wrong_number_of/
(use-package smart-mode-line
  :init
  (setq sml/theme 'respectful)
  (sml/setup)
  :config
  (setq
   sml/mule-info nil
   sml/vc-mode-show-backend nil
   sml/modified-char "δ"
   sml/shorten-directory nil
   )
  )

(use-package yaml-mode)
(use-package magit)
(use-package flycheck
  :config
  (global-flycheck-mode t)
  )
(use-package string-inflection)
(use-package yasnippet
  :requires (company)
  :config
  (add-to-list 'company-backends '(company-dabbrev-code company-yasnippet))
  )
(use-package yasnippet-snippets)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode))
  )
(use-package yasnippet
  :init
  (yas-global-mode t)
  )
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :hook ((ruby-mode . dumb-jump-mode)
	 (python-mode . dumb-jump-mode)
	 (js-mode . dumb-jump-mode)
	 (web-mode . dumb-jump-mode)
	 (terraform-mode . dumb-jump-mode)
	 )
  )

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook
            (lambda ()
              (terraform-format-on-save-mode t)
              (require 'semantic/symref/grep)
              (add-to-list 'semantic-symref-filepattern-alist '(terraform-mode "*.tf")))))
(use-package protobuf-mode
  :init
  (add-hook 'protobuf-mode-hook
	    (lambda ()
	      (add-to-list 'flycheck-protoc-import-path
			   (expand-file-name "definitions" (projectile-project-root)))
	      ))
  )
(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package git-link
  :config
  (setq git-link-use-commit t)
  :bind
  (:map evil-normal-state-map
        ("<SPC>gl" . git-link)
        )
  (:map evil-visual-state-map
        ("<SPC>gl" . git-link)
        )
 )

;;;;;;;;;;;;;;;;
;; completion ;;
;;;;;;;;;;;;;;;;

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2)
  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )
(use-package prescient
  :config
  (setq prescient-persist-mode t)
  )
(use-package company-prescient
  :requires (company)
  :config
  (add-hook 'company-mode-hook 'company-prescient-mode)
  )

;;;;;;;;;;
;; rust ;;
;;;;;;;;;;

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
            (lambda() (setq
                       rust-format-on-save t
                       flycheck-checker 'rust-clippy
                       ))
            ))
(use-package cargo)

;;;;;;;;;;;
;; scala ;;
;;;;;;;;;;;

(use-package scala-mode)
;; (use-package lsp-metals
;;   :ensure t
;;   :custom
;;   ;; Metals claims to support range formatting by default but it supports range
;;   ;; formatting of multiline strings only. You might want to disable it so that
;;   ;; emacs can use indentation provided by scala-mode.
;;   (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
;;   :hook (scala-mode . lsp))

;;;;;;;;
;; go ;;
;;;;;;;;

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda () (evil-define-key 'normal 'go-mode-map "gd" 'xref-find-definitions)))
  )
(use-package gotest
  :config
  (setq
   go-test-verbose t)
  :bind
  (:map evil-normal-state-map
        ("<SPC>gtf" . go-test-current-file)
        ("<SPC>gtp" . go-test-current-project)
        ("<SPC>gtt" . go-test-current-test)
        ))
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
(use-package go-fill-struct)

;;;;;;;;;;;;
;; python ;;
;;;;;;;;;;;;

(use-package python
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (setq fill-column 80)
                                (display-fill-column-indicator-mode t)
                                ))
  )
(use-package pyvenv)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))
(use-package python-pytest
  :config
  (setq python-pytest-unsaved-buffers-behavior nil)
  :bind
  (:map evil-normal-state-map
        ("<SPC>ptf" . python-pytest-file)
        ("<SPC>pt." . python-pytest-function)))

;;;;;;;;;;
;; ruby ;;
;;;;;;;;;;

(use-package projectile
  :bind
  (:map evil-normal-state-map
        ("<SPC>rpf" . projectile-find-file)
        ("<f12>" . projectile-find-file)
        )
  )
(use-package projectile-rails
  :bind
  (:map evil-normal-state-map
        ("<SPC>rpc" . projectile-rails-console)
        ("<SPC>rpt" . projectile-rails-find-test)
        ("<SPC>rpT" . projectile-rails-find-current-test)
        ("<SPC>rpn" . projectile-rails-find-controller)
        ("<SPC>rpN" . projectile-rails-find-current-controller)
        ("<SPC>rpM" . projectile-rails-find-current-model)
        ("<SPC>rpm" . projectile-rails-find-model)
        ("<SPC>rph" . projectile-rails-find-helper)
        ("<SPC>rpH" . projectile-rails-find-current-helper)
        ("<SPC>rpz" . projectile-rails-find-serializer)
        ("<SPC>rpZ" . projectile-rails-find-current-serializer)
        )
  )
(use-package ripgrep
  :bind
  (:map evil-normal-state-map
        ("<SPC>rpr" . projectile-ripgrep))
  )
(use-package rspec-mode
  :bind
  (:map evil-normal-state-map
        ("<SPC>rra" . rspec-verify)
        ("<SPC>rrs" . rspec-verify-single)
        )
  :config
  (rspec-install-snippets)
  )
(use-package minitest
  :bind
  (:map evil-normal-state-map
        ("<SPC>rma" . minitest-verify)
        ("<SPC>rms" . minitest-verify-single)
        ("<SPC>rmw" . minitest-verify-all)
        )
  :config
  )
(use-package ruby-mode
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  :config
  (subword-mode t)
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq-local flycheck-command-wrapper-function
                          (lambda (command) (append '("bundle" "exec") command)))

	      (let ((ruby-mode-pairs '((?| . ?|))))
		(setq-local electric-pair-pairs (append electric-pair-pairs ruby-mode-pairs))
		(setq-local electric-pair-text-pairs (append electric-pair-text-pairs ruby-mode-pairs)))

              (set (make-local-variable 'compilation-error-regexp-alist-alist)
                   (list (quote ('ruby-Test::Unit "^ *\\([^ (].*\\):\\([1-9][0-9]*\\):in " 1 2))))

              (add-hook 'lsp-after-initialize-hook (lambda () (flycheck-add-next-checker 'lsp 'ruby-rubocop)) 0 t)
              ))
  :bind
  (:map evil-normal-state-map
        ("<SPC>re" . ruby-toggle-block)
        ("<SPC>rq" . ruby-toggle-string-quotes)
	("<SPC>rpk" . inf-ruby-console-racksh)
        )
  )
(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode)
  )
(use-package rubocopfmt
  )
(use-package evil-ruby-text-objects
  :hook (ruby-mode . evil-ruby-text-objects-mode))

;;;;;;;;;
;; web ;;
;;;;;;;;;

;; https://github.com/flycheck/flycheck/issues/1428#issuecomment-591320954
(defun flycheck-node_modules-executable-find (executable)
  "Find npx binary in node_modules or globally for use with flycheck"
  (or
   (let* ((base (locate-dominating-file buffer-file-name "node_modules"))
          (cmd  (if base (expand-file-name (concat "node_modules/.bin/" executable)  base))))
     (if (and cmd (file-exists-p cmd))
         cmd))
   (flycheck-default-executable-find executable)))

(defun my-node_modules-flycheck-hook ()
  "Hook to help flycheck find node executables"
  (setq-local flycheck-executable-find #'flycheck-node_modules-executable-find))

;; Based on:
;; - https://gist.github.com/blue0513/f503c26bf5cb8a1b6fb6e75f1ec91557
;; - https://github.com/codesuki/eslint-fix/blob/master/eslint-fix.el
(defun eslint-fix-file ()
  "Run eslint --fix on the current file"
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let* ((eslint (flycheck-node_modules-executable-find "eslint")))
    (if eslint
        (progn
          (apply #'call-process eslint nil "*ESLint Errors*" nil (list "--fix" (buffer-file-name)))
          (revert-buffer t t t))
      (message (format "eslint executable ‘%s’ not found" (or "" eslint))))
    ))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js" . web-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-expanding t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-closing t
   web-mode-enable-auto-indentation t
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-content-types-alist '(("jsx" . ".*\\.js[x]?"))
   )
  (add-hook 'web-mode-hook 'my-node_modules-flycheck-hook)
  (add-hook 'web-mode-hook
            (lambda()
              (add-hook 'after-save-hook 'eslint-fix-file nil t)
              (add-hook 'lsp-ui-hook (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint)) 0 t)
              ))
  :bind
  (:map evil-normal-state-map
        ("<SPC>wer" . web-mode-element-rename)
        ("<SPC>wek" . web-mode-element-kill)
        ("<SPC>wec" . web-mode-element-close)
        ("<SPC>wes" . web-mode-surround)
        )
  )
;; (use-package tide
;;   :after
;;   (typescript-mode company flycheck)
;;   :hook
;;   (
;;    (web-mode . tide-setup)
;;    (before-save . tide-format-before-save)
;;    )
;;   :bind
;;   (:map evil-normal-state-map
;;         ("<SPC>tf" . tide-fix)
;;         )
;;   )

;;;;;;;;;;;;;;
;; mmm mode ;;
;;;;;;;;;;;;;;

;; configuration based on https://gist.github.com/rangeoshun/67cb17392c523579bc6cbd758b2315c1
(use-package mmm-mode
  :config
  ;; Add submodule for graphql blocks
  (mmm-add-classes
   '((mmm-graphql-mode
      :submode graphql-mode
      :front "gr?a?p?h?ql`"
      :back "`;")))
  (mmm-add-mode-ext-class 'web-mode nil 'mmm-graphql-mode))

(defun mmm-reapply ()
  (mmm-mode)
  (mmm-mode))

(add-hook 'after-save-hook
          (lambda ()
            (when (string-match-p "\\.tsx?" buffer-file-name)
              (mmm-reapply))))

;;;;;;;;;;;;;;;;;
;; tree sitter ;;
;;;;;;;;;;;;;;;;;

(use-package tree-sitter
  :config
  (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode)
  :hook
  (
   (go-mode . tree-sitter-mode)
   (python-mode . tree-sitter-mode)
   (sh-mode . tree-sitter-mode)
   ;; need to make some adjustments to ruby syntax highlighting first before use there
   )
  )
(use-package tree-sitter-langs)
(use-package origami
  :config (global-origami-mode 1)
  :bind
  (:map evil-normal-state-map
        ("zu" . origami-undo)
        )
  )


;;;;;;;;;;;
;; swift ;;
;;;;;;;;;;;

(use-package swift-mode)


;;;;;;;;;;;;
;; global ;;
;;;;;;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(setq
 initial-scratch-message nil ; don't tell me how to use the scratch buffer
 ;; emulate vi-style buffer scrolling
 scroll-conservatively 1
 scroll-margin 20
 ;; no tabs
 indent-tabs-mode nil
 ;; put all backups somewhere in /tmp
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; eldoc
 eldoc-echo-area-prefer-doc-buffer t
 )
(xterm-mouse-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(electric-pair-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; ido
(setq
 ido-enable-flex-matching t
 ido-everywhere t
 )
(ido-mode t)

;; line numbers
(global-display-line-numbers-mode)
(setq
 display-line-numbers-grow-only t
 display-line-numbers-major-tick 10
 display-line-numbers-minor-tick 2
 )

;; xref
(evil-global-set-key 'normal "gb" 'xref-pop-marker-stack)
(evil-global-set-key 'normal "gd" 'xref-find-definitions)
(evil-global-set-key 'normal "gr" 'xref-find-references)

;; global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; key bindings
(evil-global-set-key 'normal [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(evil-global-set-key 'normal [mouse-5] '(lambda () (interactive) (scroll-up 1)))
(evil-global-set-key 'normal ";" 'comment-thing)
(evil-global-set-key 'visual ";" 'comment-thing)
(evil-global-set-key 'normal "gz" '(lambda () (interactive) (my-increment-number-decimal -1)))
(evil-global-set-key 'normal "ga" 'my-increment-number-decimal)
;; this prevents accidentally switching to Emacs mode when attemptint to background the emacs client
(evil-global-set-key 'normal (kbd "C-z") 'suspend-frame)
(evil-global-set-key 'insert (kbd "C-z") 'suspend-frame)
(evil-global-set-key 'visual (kbd "C-z") 'suspend-frame)
(evil-define-key 'emacs 'global (kbd "<escape>") 'evil-normal-state)

(global-set-key (kbd "C-x C-k <RET>") 'kill-this-buffer)
(global-set-key (kbd "M-s M-o") 'occur-all-buffers)
(global-set-key (kbd "C-x p") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x 4 t") 'toggle-window-split)

;; aliases
(defalias 'css 'custom-theme-visit-theme)
(defalias 'ttl 'toggle-truncate-lines)
(defalias 'rr 'replace-rectangle)
(defalias 'kr 'kill-rectangle)
(defalias 'rs 'replace-string)
(defalias 'rreg 'replace-regexp)
(defalias 'rev 'revert-buffer)
(defalias 'atb 'append-to-buffer)
(defalias 'vd 'vc-diff)
(defalias 'diffbuff 'diff-buffer-with-file)
(defalias 'db 'diff-buffer-with-file)
(defalias 'vtt 'visit-tags-table)
(defalias 'vcrb 'vc-revert-buffer)
(defalias 'msf 'magit-stage-file)
(defalias 'rack 'inf-ruby-console-racksh)

;;;;;;;;;;;;
;; themes ;;
;;;;;;;;;;;;

(load "~/.emacs.d/current-theme.el" t)

;;;;;;;;;;;;;;;;;
;; other files ;;
;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/advice.el") ; advising functions
(load "~/.emacs.d/extensions.el") ; custom defuns

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; TODO:
;; - rspec always reuses the same buffer, so compilation history is lost
;; - company backend for yasnippet isn't working?
(put 'set-goal-column 'disabled nil)

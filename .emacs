;;;;;;;;;;;;;;;;;;;
;; package setup ;;
;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; core packages
(let ((packages-list '(use-package)))
  (dolist (package packages-list)
    (unless (package-installed-p package) (package-install package))))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;; general packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot)
(use-package yaml-mode)
(use-package magit)
(use-package flycheck
  :config
  (global-flycheck-mode t)
  )
(use-package yasnippet)
(use-package rainbow-delimiters)
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
	 )
  :bind
  (:map evil-normal-state-map
        ("gd" . dumb-jump-go)
        )
  )
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )
(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  )
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
  :bind
  (:map evil-normal-state-map
        ("<SPC>gl" . git-link)
        )
  (:map evil-visual-state-map
        ("<SPC>gl" . git-link)
        )
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

;;;;;;;;;;
;; evil ;;
;;;;;;;;;;

(use-package evil
  :init
  (setq
   ;; make it easier to see paren matches
   evil-show-paren-range 1
   ;; don't add replaced text to the kill ring
   evil-kill-on-visual-paste nil
   ;; try to be more like OG emacs when dealing with yanking stuff in general
   evil-want-Y-yank-to-eol t
   evil-move-beyond-eol t
   ;; try to be more like emacs when dealing with window management
   evil-split-window-below t
   evil-vsplit-window-right t
   ;; use M-(number) for prefix argument instead and retain vim movement behavior
   evil-want-C-u-scroll t

   ;; these need to be set for 'evil-collection to work properly
   evil-want-integration t
   evil-want-keybinding nil
   )
  :hook
  ;; TODO - add keybindings for "backward"
  ;; - abstract this somehow to a keymap?
  (
   (help-mode . (lambda () (evil-local-set-key 'normal (kbd "C-j") 'forward-button)))
   (custom-new-theme-mode . (lambda () (evil-local-set-key 'normal (kbd "C-j") 'forward-button)))
   (compilation-mode . (lambda () (evil-local-set-key 'normal (kbd "C-j") 'compilation-next-error)))
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
(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

;;;;;;;;
;; go ;;
;;;;;;;;

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;;;;;;;;;;
;; ruby ;;
;;;;;;;;;;

(use-package projectile
  :bind
  (:map evil-normal-state-map
        ("<SPC>rpf" . projectile-find-file)
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
  )
(use-package minitest
  :bind
  (:map evil-normal-state-map
        ("<SPC>rma" . minitest-verify)
        ("<SPC>rms" . minitest-verify-single)
        ("<SPC>rmw" . minitest-verify-all)
        )
  )
(use-package ruby-mode
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq-local flycheck-command-wrapper-function
                          (lambda (command) (append '("bundle" "exec") command)))
	      (let ((ruby-mode-pairs '((?| . ?|))))
		(setq-local electric-pair-pairs (append electric-pair-pairs ruby-mode-pairs))
		(setq-local electric-pair-text-pairs (append electric-pair-text-pairs ruby-mode-pairs)))
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
  :hook (ruby-mode . rubocopfmt-mode)
  )

;;;;;;;;;
;; web ;;
;;;;;;;;;

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx" . web-mode))
  )

;;;;;;;;;;;;
;; global ;;
;;;;;;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode t)
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
 )
(xterm-mouse-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(electric-pair-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)

;; ido
(setq
 ido-enable-flex-matching t
 ido-everywhere t
 )
(ido-mode t)

;; key bindings
(evil-global-set-key 'normal "gb" 'xref-pop-marker-stack)
(evil-global-set-key 'normal [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(evil-global-set-key 'normal [mouse-5] '(lambda () (interactive) (scroll-up 1)))
(evil-global-set-key 'normal ";" 'comment-thing)
(evil-global-set-key 'visual ";" 'comment-thing)
(evil-global-set-key 'normal "gz" '(lambda () (interactive) (my-increment-number-decimal -1)))
(evil-global-set-key 'normal "ga" 'my-increment-number-decimal)
;; this prevents accidentally switching to Emacs mode when attemptint to background the emacs client
(evil-global-set-key 'normal (kbd "C-z") 'suspend-frame)
(evil-global-set-key 'insert (kbd "C-z") 'suspend-frame)
(evil-define-key 'emacs 'global (kbd "<escape>") 'evil-normal-state)

(global-set-key (kbd "C-x C-k <RET>") 'kill-this-buffer)
(global-set-key (kbd "M-s M-o") 'occur-all-buffers)
(global-set-key (kbd "C-x p") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x 4 t") 'toggle-window-split)

;; aliases
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

(load-theme 'abyssal-blue t nil)

;;;;;;;;;;;;;;;;;
;; other files ;;
;;;;;;;;;;;;;;;;;

;; put *Customize* generated code in its own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "~/.emacs.d/advice.el") ; advising functions
(load "~/.emacs.d/extensions.el") ; custom defuns

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
(put 'downcase-region 'disabled nil)

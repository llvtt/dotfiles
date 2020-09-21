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
  (dolist (package packages-list) (package-install package)))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;; general packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package magit)
(use-package evil
  :init
  (setq
   ;; these need to be set for 'evil-collection to work properly
   evil-want-integration t
   evil-want-keybinding nil
   )
  :config
  ;; must be activated after setting evil-want-* variables
  (evil-mode t)
  )
(use-package evil-collection
  :init
  (evil-collection-init)
  )
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
  :hook
  (ruby-mode python-mode web-mode js-mode)
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
	("<SPC>rpt" . projectile-rails-find-current-test)
	)
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
  :config
  (add-hook 'ruby-mode-hook
	    (lambda ()
	      (setq-local flycheck-command-wrapper-function
			  (lambda (command) (append '("bundle" "exec") command))))
	    )
  :bind
  (:map evil-normal-state-map
	("<SPC>re" . ruby-toggle-block)
	("<SPC>rq" . ruby-toggle-string-quotes)
	)

  )

;;;;;;;;;
;; web ;;
;;;;;;;;;

(use-package web-mode)

;;;;;;;;;;;;
;; global ;;
;;;;;;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode t)
(setq
 initial-scratch-message nil ; don't tell me how to use the scratch buffer
 ; emulate vi-style buffer scrolling
 scroll-conservatively 1
 scroll-margin 20
 )
(xterm-mouse-mode t)
(show-paren-mode t)
(global-hl-line-mode t)
(electric-pair-mode t)

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
(evil-global-set-key 'normal ";" 'comment-line)

(global-set-key (kbd "C-x C-k <RET>") 'kill-this-buffer)

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

;;;;;;;;;;;;
;; themes ;;
;;;;;;;;;;;;

(load-theme 'abyssal-blue)

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

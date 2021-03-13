;; extensions.el
;;
;; Custom commands and functions


(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
	(setq inc-by (if arg arg 1))
	(skip-chars-backward "0123456789")
	(when (re-search-forward "[0-9]+" nil t)
	  (setq field-width (- (match-end 0) (match-beginning 0)))
	  (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
	  (when (< answer 0)
	    (setq answer (+ (expt 10 field-width) answer)))
	  (replace-match (format (concat "%0" (int-to-string field-width) "d")
				 answer)))))))

(defun toggle-window-split ()
  "Toggle between horizontal and vertical split windows"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; https://stackoverflow.com/questions/15692016/emacs-apply-shell-command-to-top-item-in-kill-ring-and-yank/15694531#15694531
(defun shell-command-on-str (cmd &optional str)
  "Insert result of calling CMD with STR as input.

STR is current-kill if unspecified.
"
  (interactive (list (read-shell-command "Shell command on region: ")))
  (setq str (or str (current-kill 0)))
  (with-temp-buffer
    (insert str)
    (shell-command-on-region (point-min) (point-max) cmd nil 'replace)))

(defun copy-to-mac-clipboard ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy"))

(defun paste-from-mac-clipboard ()
  (interactive)
  (shell-command "pbpaste" t))
(defalias 'copy 'copy-to-mac-clipboard)
(defalias 'paste 'paste-from-mac-clipboard)

(defun smart-move-beginning-of-line ()
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    (when (eq pt (point))
      (beginning-of-line-text))))

(defun occur-all-buffers (arg)
  (interactive "sSearch for regex: ")
  (multi-occur-in-matching-buffers ".*" arg))

(defun insert-object-id ()
  (interactive)
  (dotimes (_ 24)
    (insert (format "%x" (random 16)))))

(defun comment-thing ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun aggressive-indent ()
  (interactive)
  (let* ((start-syntax-pattern "[\[,(<{]")
	 (end-syntax-pattern "[\])>}]"))
    (save-excursion
      (save-restriction
	(narrow-to-region (line-beginning-position) (line-end-position))
	(beginning-of-line)
	(while (re-search-forward start-syntax-pattern nil t)
	  (newline-and-indent))
	(beginning-of-buffer)
	(while (re-search-forward end-syntax-pattern nil t)
	  (backward-char 1)
	  (newline-and-indent)
	  (forward-char 1))
	)
      )
    )
  )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:

;; advice.el
;;
;; Advising functions

(defadvice occur (after occur-and-shrink-window activate)
  "Make *Occur* buffer as small as possible"
  (shrink-window-if-larger-than-buffer))

(defadvice linum-update-window (around linum-dynamic activate)
  "Put space between linum and the buffer"
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

(defadvice eshell (before eshell-other-window activate)
  "Open the eshell in another window if there is only one window"
  (if (one-window-p)
      (progn
        (split-window-sensibly (car (window-list)))
        (other-window 1))))

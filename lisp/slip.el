(defun slip-god-mode-active-minibuffer-p ()
  "Return true if minibuffer is active otherwise nil"
  (if (active-minibuffer-window) t))

(defun slip-copy-line (arg)
  "Copy lines to the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
		  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun slip-org-mode-setup ()
  "Run when in org mode"
  (org-indent-mode)
  (org-superstar-mode 1)
  (prettify-symbols-mode))

(defun slip-after-init ()
  "Run after emacs after-init-hook"
  (global-company-mode)
  (doom-modeline-mode)
  (setq god-global-mode t))

(defun slip-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(provide 'slip)

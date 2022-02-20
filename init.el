(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)

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

(setq package-enable-at-startup nil)
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold nil
      doom-themes-enable-italic t)
(load-theme 'doom-one t)
(straight-use-package
 '(password-store :type git
		  :flavor melpa
		  :files ("contrib/emacs/*.el" "password-store-pkg.el")
		  :host github
		  :repo "zx2c4/password-store"))
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'all-the-icons)
(when (display-graphic-p)
  (require 'all-the-icons))
(straight-use-package 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)
(straight-use-package 'magit)
(straight-use-package 'diminish)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(straight-use-package 'vertico)
(vertico-mode)
(with-eval-after-load 'vertico
     (straight-use-package 'marginalia)
     (marginalia-mode))
(straight-use-package 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))
(savehist-mode)
(straight-use-package 'consult)
(straight-use-package 'which-key)
(which-key-mode)
(straight-use-package 'vterm)
(with-eval-after-load 'evil
  (require 'general)
  (general-evil-setup))
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(with-eval-after-load 'evil
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-C-i-jump nil))


(diminish 'which-key-mode)
(diminish 'company-mode)

(defun copy-line (arg)
  "Copy lines to the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
		  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-display-line-numbers-mode 1)

(general-define-key
 :states 'insert
 "C-g" 'evil-normal-state
 "C-h" 'evil-delete-backward-char-and-join)

(general-define-key
 "<escape>" 'keyboard-escape-quit
 "C-;" 'execute-extended-command
 "C-x b" 'consult-buffer)

(general-nmap
 :prefix "SPC"
 "." 'find-file
 "f" '(:ignore t :which-key "files")
 "f s" '(save-buffer :which-key "save"))

(general-create-definer slip-custom-def
  :prefix "C-c")

(slip-custom-def
  "t" '(:ignore t :which-key "toggle")
  "t l" '(display-line-numbers-mode :which-key "line-numbers")
  "t L" '(global-display-line-numbers-mode :which-key "global-line-numbers")
  "." 'find-file
  "C-k" 'copy-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b146631f8c24c29b555427da833ce15a028855bff3a1b7c2e79fbc9ede5568bb" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

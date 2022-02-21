(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8
      visible-bell t)

;; load slip's stuff
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'slip)
(add-hook 'after-init-hook 'slip-after-init)

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
(straight-use-package 'magit)
(straight-use-package 'diminish)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(straight-use-package 'company)
(require 'company)
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
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(straight-use-package 'general)
(straight-use-package 'org-superstar)
(straight-use-package 'org)
(with-eval-after-load 'org
  (require 'org)
  (require 'slip)
  (require 'org-tempo)
  (setq org-startup-indented t
	org-ellipsis " ⮛"
	org-pretty-entities
	prettify-symbols-unprettify-at-point 'right-edge)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
  (add-hook 'org-mode-hook 'slip-org-mode-setup))

;; adding basic god mode
(straight-use-package 'god-mode)
(with-eval-after-load 'god-mode
  (require 'god-mode)
  (require 'slip)
  (god-mode)
  (add-to-list 'god-exempt-predicates 'slip-god-mode-active-minibuffer-p)
  (add-hook 'post-command-hook 'slip-god-mode-update-cursor-type)
  (which-key-enable-god-mode-support))

(diminish 'which-key-mode)
(diminish 'company-mode)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-display-line-numbers-mode 1)

(general-define-key
 "<escape>" #'god-mode-all
 "C-;" 'execute-extended-command
 "C-x b" 'consult-buffer
 "C-s" 'consult-line
 "C-x C-1" 'delete-other-windows
 "C-x C-2" 'split-window-below
 "C-x C-3" 'split-window-right
 "C-x C-0" 'delete-window)

(general-define-key
 :keymaps 'isearch-mode-map
 "M-e" 'consult-isearch
 "M-s e" 'consult-isearch
 "M-s l" 'consult-line)

(general-create-definer slip-custom-def
  :prefix "C-c")

(require 'slip)
(slip-custom-def
  "t" '(:ignore t :which-key "toggle")
  "t l" '(display-line-numbers-mode :which-key "line-numbers")
  "t L" '(global-display-line-numbers-mode :which-key "global-line-numbers")
  "." 'find-file
  "C-l" 'slip-copy-line
  "f" '(:ignore t :which-key "file"))

(general-define-key
 :keymaps 'god-local-mode-map
 "." 'repeat
 "i" 'god-local-mode
 "[" 'backward-paragraph
 "]" 'forward-paragraph)

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

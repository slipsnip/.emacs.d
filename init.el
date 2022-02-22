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

(straight-use-package 'doom-modeline)

(straight-use-package 'all-the-icons)
(when (display-graphic-p)
  (require 'all-the-icons)
  (with-eval-after-load 'all-the-icons
    (straight-use-package 'all-the-icons-dired))
)

(straight-use-package 'god-mode)
(with-eval-after-load 'god-mode
  (require 'god-mode)
  (god-mode)
  (add-to-list 'god-exempt-predicates 'slip-god-mode-active-minibuffer-p)
  (add-hook 'post-command-hook 'slip-god-mode-update-cursor-type)
  (which-key-enable-god-mode-support))

(straight-use-package 'vertico)
(vertico-mode)
(with-eval-after-load 'vertico
  (straight-use-package 'marginalia)
  (marginalia-mode))

(straight-use-package 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(straight-use-package 'consult)

(savehist-mode)

(straight-use-package 'which-key)
(which-key-mode)

(straight-use-package 'magit)

(straight-use-package 'org-superstar)
(straight-use-package 'org)
(with-eval-after-load 'org
  (require 'org)
  (require 'org-tempo)
  (setq org-startup-indented t
	org-ellipsis " ⮛"
	org-pretty-entities t
	prettify-symbols-unprettify-at-point 'right-edge
	org-agenda-files '("~/org/agenda.org")
	org-agenda-start-with-log-mode t
	org-log-done 'time)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (add-hook 'org-mode-hook 'slip-org-mode-setup)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(straight-use-package 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(straight-use-package 'general)

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

(straight-use-package 'vterm)

(straight-use-package
 '(password-store :type git
		  :flavor melpa
		  :files ("contrib/emacs/*.el" "password-store-pkg.el")
		  :host github
		  :repo "zx2c4/password-store"))

(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(straight-use-package 'diminish)

(straight-use-package 'company)
(require 'company)

(dolist (mode '(org-mode-hook
                  term-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
(global-display-line-numbers-mode 1)

(defun slip-god-mode-active-minibuffer-p ()
  "Return true if minibuffer is active otherwise nil"
  (if (active-minibuffer-window) t))

(defun slip-copy-line (arg)
  "Copy lines to the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun slip-org-babel-tangle-config ()
  "Automaticaly tangle Config.org when saved"
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/readme.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun slip-org-mode-setup ()
  "Run when in org mode"
  (org-indent-mode)
  (org-superstar-mode 1)
  (prettify-symbols-mode)
  (add-hook 'after-save-hook #'slip-org-babel-tangle-config))

(defun slip-after-init ()
  "Run after emacs after-init-hook"
  (global-company-mode)
  (doom-modeline-mode)
  (setq god-global-mode t)
  (diminish 'which-key-mode)
  (diminish 'company-mode))

(defun slip-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(add-hook 'after-init-hook 'slip-after-init)

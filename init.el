;; straight use package
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

;; variables
(setq default-buffer-file-coding-system 'utf-8
      visible-bell t
      inhibit-startup-message t
      use-dialog-box nil
      global-auto-revert-non-file-buffers t
      custom-file (locate-user-emacs-file "custom-vars.el")
      auto-mode-case-fold nil)

;; coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(global-auto-revert-mode)
(recentf-mode)
(savehist-mode)
(save-place-mode)
(show-paren-mode)

;; (straight-use-package 'exwm)


;; font
(straight-use-package 'fira-code-mode)
(with-eval-after-load 'fira-code-mode
  (add-hook 'prog-mode (lambda () (global-fira-code-mode)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (global-fira-code-mode))))
(set-face-attribute 'default nil :font "Fira Code Retina")

;; keep things clean - no litttering
(straight-use-package 'no-littering)
(with-eval-after-load 'no-littering
  (require 'no-littering)
  (require 'recentf)
  (setq auto-save-file-name-transforms
        '((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; garbage collection magic
(straight-use-package 'gcmh)
(with-eval-after-load 'gcmh
  (gcmh-mode 1))

;; better visibility of window borders
(setq window-divider-default-places t
      window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(add-hook 'window-setup-hook #'window-divider-mode)

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
(exec-path-from-shell-copy-env "GPG_TTY")

;; helpful functions
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
  (doom-modeline-mode)
  (corfu-global-mode t)
  (require 'vertico)
  (vertico-mode)
  (load custom-file 'noerror 'nomessage))

(add-hook 'after-init-hook 'slip-after-init)

;; themes
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold nil
      doom-themes-enable-italic t)
(load-theme 'doom-one t)

;; modeline
(straight-use-package 'doom-modeline)
(with-eval-after-load 'doom-modeline
  (setq doom-modeline-minor-modes t))

;; icon fonts
(straight-use-package 'all-the-icons)
(when (display-graphic-p)
  (require 'all-the-icons)
  (with-eval-after-load 'all-the-icons
    (straight-use-package 'all-the-icons-dired))
  )

;; better vertical completions - integrates with emacs
(straight-use-package 'vertico)

(with-eval-after-load 'vertico

  (straight-use-package 'marginalia)
  (marginalia-mode))

;; completion style - out of order completion
(straight-use-package 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; helpful function replacements work with vertico
(straight-use-package 'consult)

;; autocomplete ui dropdown that stays out of way
(straight-use-package 'corfu)
(with-eval-after-load 'corfu
  (require 'corfu)
  (setq corfu-auto t)
  (dolist (mode '(prog-mode
                  shell-mode
                  eshell-mode))
    (add-hook mode corfu-mode))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))

;; verbose key bindings - discover-ability
(straight-use-package 'which-key)
(which-key-mode)

;; Magit
(straight-use-package 'magit)

;; Org mode
(straight-use-package 'org-superstar)
(straight-use-package 'org)
(with-eval-after-load 'org
  (require 'org)
  (require 'org-tempo)
  (setq org-startup-indented t
        org-ellipsis " ???"
        org-pretty-entities t
        prettify-symbols-unprettify-at-point 'right-edge
        org-agenda-files '("~/org/agenda.org")
        org-agenda-start-with-log-mode t
        org-log-done 'time)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "???")
                                         ("#+END_SRC" . "???")
                                         ("#+begin_src" . "???")
                                         ("#+end_src" . "???")
                                         (">=" . "???")
                                         ("=>" . "???")))
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
 "C-;" 'execute-extended-command
 "C-x b" 'consult-buffer
 "C-s" 'consult-line
 "C-x C-1" 'delete-other-windows
 "C-x C-2" 'split-window-below
 "C-x C-3" 'split-window-right
 "C-x C-0" 'delete-window
 "M-/" 'dabbrev-completion
 "C-M-/" 'dabbrev-expand
 )

(general-define-key
 :keymaps 'isearch-mode-map
 "M-e" 'consult-isearch
 "M-s e" 'consult-isearch
 "M-s l" 'consult-line)

(general-create-definer slip-custom-def
  :prefix "M-p")

(slip-custom-def
  "t" '(:ignore t :which-key "toggle")
  "t l" '(display-line-numbers-mode :which-key "line-numbers")
  "t L" '(global-display-line-numbers-mode :which-key "global-line-numbers")
  "C-." 'find-file
  "C-l" 'slip-copy-line
  "f" '(:ignore t :which-key "file")
  "f r" '(recentf-open-files :which-key "recent"))

(straight-use-package 'vterm)

(straight-use-package
 '(password-store :type git
                  :flavor melpa
                  :files ("contrib/emacs/*.el" "password-store-pkg.el")
                  :host github
                  :repo "zx2c4/password-store"))

(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; (straight-use-package 'diminish)
(straight-use-package 'delight)

(straight-use-package '(free-keys
                        :type git
                        :flavor melpa
                        :host github
                        :repo "Fuco1/free-keys"))

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(global-display-line-numbers-mode 1)

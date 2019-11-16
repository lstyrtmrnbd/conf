;;; MELPA & Package=============================================
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;; Platform detection==========================================
(setq current-os
      (cond ((string-equal system-type "gnu/linux")
             'linux)
            ((string-equal system-type "window-nt")
             'windows)
            ((string-equal system-type "darwin")
             'macos)))

(setq graphics (display-graphic-p))

;;; General=====================================================
;; Paths
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Feature Customization
(setq-default indent-tabs-mode nil) ; don't use tabs for indentation
(setq ring-bell-function 'ignore)   ; don't ring bell
(scroll-bar-mode -1)                ; remove scrollbar
(electric-pair-mode 1)              ; pair brackets outside of paredit
(column-number-mode 1)              ; show column num
(global-auto-revert-mode 1)         ; refresh buffer when disk copy changes

;; makes titlebar the same as theme background on OSX
(when (equal current-os 'macos)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; show full filepath of buffer in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;;; Essential Packages==========================================
(require 'use-package)

(use-package dired+)

;; BASED
(use-package xah-fly-keys
  :ensure t
  :config
  (xah-fly-keys-set-layout "qwerty") ; required
  (xah-fly-keys 1))

;; Show key commands
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org
  :config
  (setq org-src-fontify-natively t) ; color src code in org mode
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (python . t)
                               (js . t))))

;;; LISP========================================================
(use-package slime
  :ensure t
  :config
  (add-to-list 'slime-contribs 'slime-repl)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  :custom
  (inferior-lisp-program "/usr/bin/sbcl"))

(defun add-lisp-hook (lisp-mode-hooks hook)
  "Maps a hook to all lisp mode hooks"
  (mapc (lambda (lisp-mode)
          (add-hook lisp-mode hook))
        lisp-mode-hooks))

;; add hooks here to get lisp functionality
(setq lisp-mode-hooks
      '(emacs-lisp-mode-hook
        eval-expression-minibuffer-setup-hook
        ielm-mode-hook
	lisp-mode-hook
	lisp-interaction-mode-hook
	scheme-mode-hook
	racket-mode-hook
        racket-repl-mode-hook
        slime-repl-mode-hook)) ; keep in last position

(use-package paredit
  :ensure t
  :config
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))   ; add hook to slime
  (add-lisp-hook (butlast lisp-mode-hooks) #'enable-paredit-mode)) ; add hook to all but slime

(defun override-slime-repl-bindings-with-paredit ()
  "Stop SLIME's REPL from grabbing DEL, 
   which is annoying when backspacing over a ("
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;Rainbow delims
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-lisp-hook lisp-mode-hooks 'rainbow-delimiters-mode))

;;; Javascript==================================================
(use-package js-comint
  :ensure t
  :custom
  (js-comint-program-command "/usr/local/opt/node@10/bin/node")
  (js-comint-program-arguments '("--experimental-repl-await"))
  :config
  (setenv "NODE_NO_READLINE" "1")) ; fixes prompt

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

;;; Custom======================================================
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

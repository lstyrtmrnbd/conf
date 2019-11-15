;;; MELPA & package
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


;;; ORG mode
;; after package-initialize
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-support-shift-select t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C  . t)
   (js . t)
   (python . t)))


;;; LISP
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; SLIME Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))


;;; Javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'js-comint)

(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'js-load-file-and-go)))


;;; General
;; backup file location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))

;; enable image+
(eval-after-load 'image '(require 'image+))
(eval-after-load 'image+ '(imagex-auto-adjust-mode 1))


;;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   [("#181818" . "#282828")
    ("#ab4642" . "#dc9656")
    ("#a1b56c" . "#383838")
    ("#f7ca88" . "#383838")
    ("#7cafc2" . "#585858")
    ("#ba8baf" . "#b8b8b8")
    ("#86c1b9" . "#d8d8d8")
    ("#ffffff" . "#ffffff")])
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (borland-blue)))
 '(custom-safe-themes
   (quote
    ("02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "38e66a2a20fa9a27af5ffc4f4dd54f69e3fef6b51be7b351e137b24958bfebd7" "e3fc83cdb5f9db0d0df205f5da89af76feda8c56d79a653a5d092c82c7447e02" "3d5720f488f2ed54dd4e40e9252da2912110948366a16aef503f3e9e7dfe4915" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
			 (:color "#c0c0c0"))
     (implicitParams :underline
		     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000"))))
 '(fci-rule-color "#c7c7c7")
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(package-selected-packages
   (quote
    (js-comint exec-path-from-shell slime-theme slime paredit org magit intellij-theme image+ goose-theme faff-theme darcula-theme borland-blue-theme basic-theme anti-zenburn-theme)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ba8baf")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty Diminished" :foundry "unknown" :slant normal :weight normal :height 123 :width normal)))))

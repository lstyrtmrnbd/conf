;;; MELPA packaging=============================================

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;; General=====================================================

;; based
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; required
(xah-fly-keys 1)

;; don't use tabs for indentation
(setq-default indent-tabs-mode nil)

;; set margin widths
(setq-default left-margin-width 0 right-margin-width 0)

;; no menubar
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;Dired+
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "dired+")
(diredp-toggle-find-file-reuse-dir 1)

;;; LISP========================================================
;; SLIME
(require 'slime)
(add-to-list 'slime-contribs 'slime-repl)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(setq inferior-lisp-program "/usr/bin/sbcl")

;; add a mode to all lisp modes
(defun add-lisp-hook (lisp-mode-hooks hook)
  "Maps a hook to all lisp mode hooks"
  (mapc (lambda (lisp-mode)
          (add-hook lisp-mode hook))
        lisp-mode-hooks))

;; add new lisp modes hooks to get lisp functionality
(setq lisp-mode-hooks
      '(emacs-lisp-mode-hook
        eval-expression-minibuffer-setup-hook
        ielm-mode-hook
	lisp-mode-hook
	lisp-interaction-mode-hook
	scheme-mode-hook
	racket-mode-hook
        racket-repl-mode-hook
        slime-repl-mode-hook)); keep in last position

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; slime-repl-mode-hook special format, thus butlast
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-lisp-hook (butlast lisp-mode-hooks) #'enable-paredit-mode)

(defun override-slime-repl-bindings-with-paredit ()
  "Stop SLIME's REPL from grabbing DEL, 
   which is annoying when backspacing over a ("
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;Rainbow delims
(require 'rainbow-delimiters)
(add-lisp-hook lisp-mode-hooks 'rainbow-delimiters-mode)

;;; C Languages=================================================
(defun indent-the-way-i-like-please ()
  (c-set-offset 'case-label '+))

;; c-mode-common-hook is for all languages supported by CC-mode
;; c++-mode-hook, java-mode-hook, c-mode-hook are other options
(add-hook 'c-mode-common-hook 'indent-the-way-i-like-please)

;;; Custom======================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (tango)))
 '(custom-safe-themes
   (quote
    ("c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e6ccd0cc810aa6458391e95e4874942875252cd0342efd5a193de92bfbb6416b" "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "f738c3eb5cfc7e730fea413f9cd8ba0624bd8b4837451660fe169f13f77c7814" "1367672034423b1be7c81aa63575f822032185c62b27c13f621813c8d53f495c" default)))
 '(fci-rule-color "#c7c7c7")
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(package-selected-packages
   (quote
    (xah-find xah-fly-keys gandalf-theme racket-mode rainbow-delimiters anti-zenburn-theme sublime-themes monotropic-theme clojure-mode cider magit paredit slime faff-theme)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
   (quote
    ((20 . "#437c7c")
     (40 . "#336c6c")
     (60 . "#205070")
     (80 . "#2f4070")
     (100 . "#1f3060")
     (120 . "#0f2050")
     (140 . "#a080a0")
     (160 . "#806080")
     (180 . "#704d70")
     (200 . "#603a60")
     (220 . "#502750")
     (240 . "#401440")
     (260 . "#6c1f1c")
     (280 . "#935f5c")
     (300 . "#834744")
     (320 . "#732f2c")
     (340 . "#6b400c")
     (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty" :foundry "PfEd" :slant normal :weight normal :height 143 :width normal)))))

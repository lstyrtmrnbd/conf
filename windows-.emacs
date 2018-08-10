(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (borland-blue)))
 '(custom-safe-themes
   (quote
    ("02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1" "b486c4c5f8597e0b8401bc960cc878151a335d20e052ba3c9da27f12881877a2" "b825687675ea2644d1c017f246077cdd725d4326a1c11d84871308573d019f67" "5dd70fe6b64f3278d5b9ad3ff8f709b5e15cd153b0377d840c5281c352e8ccce" "1aad0f86167806e27b9267ccca814ee0b2f623cfef32422f1ea0e5d3f3dd7650" default)))
 '(fringe-mode 6 nil (fringe))
 '(inhibit-startup-screen t)
 '(linum-format " %7d ")
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (js-comint company omnisharp org quasi-monochrome-theme purple-haze-theme borland-blue-theme magit paredit smartparens ac-slime auto-complete)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ricty" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;; set margin widths
(setq-default left-margin-width 0 right-margin-width 0)

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
   If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, "C:"
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(setq make-backup-file-name-function 'my-backup-file-name)

;; MELPA packaging
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

;; SLIME
(require 'slime)
(add-to-list 'slime-contribs 'slime-repl)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
; (setq inferior-lisp-program "C:\\clisp-2.49\\Clisp.exe")
(setq slime-lisp-implementations
      '((sbcl ("C:\\Program Files\\SBCL\\sbcl.exe" "--core" "C:\\Program Files\\SBCL\\sbcl.core"))
        (clisp ("C:\\clisp-2.49\\Clisp.exe"))))

;; ac-slime SLIME autocompletion package
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; paredit added to lisp modes
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; js-comint for javascript repl
(require 'js-comint)
(setq inferior-js-program-command "C:/Program Files/nodejs/node.exe")

;; use GLSL mode for proper filetypes
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; use omnisharp for C# intellisense
(add-hook 'csharp-mode-hook 'omnisharp-mode)
;; server can alternatively be started manually with M-x omnisharp-start-omnisharp-server

;; Org Mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; execute C, C++, D, python, and js source blocks in org mode
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (python . t)
                             (js . t)))

;; don't use tabs for indentation
(setq-default indent-tabs-mode nil)

;; does this even work?
(setq-default default-directory "C:\\Users\\user0\\")
(put 'dired-find-alternate-file 'disabled nil)

;; Note that c-mode-hook runs for C source files only
;; use c++-mode-hook for C++ sources, java-mode-hook for Java sources, etc.
;; If you want the same customizations to be in effect in all languages supported by cc-mode
;; use c-mode-common-hook.
(defun indent-the-way-i-like-please ()
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook 'indent-the-way-i-like-please)

(setq explicit-shell-file-name "C:\\msys64\\usr\\bin\\bash.exe")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

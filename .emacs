;;; MELPA & Package=============================================

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

;;; General=====================================================

;; based
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; required
(xah-fly-keys 1)

;; show key commands
(require 'which-key)
(which-key-mode)

;; backup file location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))

;; don't ring bell
(setq ring-bell-function 'ignore)

;; add elisp file path and Dired+
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "dired+")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; makes titlebar the same as theme background on OSX
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; don't use tabs for indentation
(setq-default indent-tabs-mode nil)

(scroll-bar-mode -1) ; remove scrollbar

(electric-pair-mode 1) ; pair brackets outside of paredit

(column-number-mode 1) ; show column num

;; refresh buffer when disk copy changes
(global-auto-revert-mode 1)

;; color src code in org mode
(setq org-src-fontify-natively t)

;; show full filepath of buffer in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; for convenience
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;;; Javascript==================================================

(require 'js-comint)
(require 'js2-mode)
(setq js-comint-program-command "/usr/local/opt/node@10/bin/node")
(setq js-comint-program-arguments '("--experimental-repl-await"))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'js-send-buffer)
            (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'js-load-file-and-go)))

;; Fixes node prompt in shell
(setenv "NODE_NO_READLINE" "1")

;;; Custom======================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#c0c0c0" "#336c6c" "#806080" "#0f2050" "#732f2c" "#23733c" "#6c1f1c" "#232333"])
 '(column-number-mode t)
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (greymatters)))
 '(custom-safe-themes
   (quote
    ("3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "28fa7536c8f563c6d6296989937a8e87a2dc6477fd7b366e0336a8997a521094" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "4e7e04c4b161dd04dc671fb5288e3cc772d9086345cb03b7f5ed8538905e8e27" "6c5a5c47749e7992b4da3011595f5470f33e19f29b10564cd4f62faebbe36b91" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c" "6c0d748fb584ec4c8a0a1c05ce1ae8cde46faff5587e6de1cc59d3fc6618e164" "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "1dacaddeba04ac1d1a2c6c8100952283b63c4b5279f3d58fb76a4f5dd8936a2c" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739" "2ae4b0c50dd49a5f74edeae3e49965bf8853954b63c5712a7967ea0a008ecd5b" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "f738c3eb5cfc7e730fea413f9cd8ba0624bd8b4837451660fe169f13f77c7814" "ed17fef69db375ae1ced71fdc12e543448827aac5eb7166d2fd05f4c95a7be71" "6515fcc302292f29a94f6ac0c5795c57a396127d5ea31f37fc5f9f0308bbe19f" "bdb4509c123230a059d89fc837c40defdecee8279c741b7f060196b343b2d18d" "5a45c8bf60607dfa077b3e23edfb8df0f37c4759356682adf7ab762ba6b10600" "341b2570a9bbfc1817074e3fad96a7eff06a75d8e2362c76a2c348d0e0877f31" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "7824eb15543c5c57c232c131ca64c4f25bfeeeda6744f71b999787a9172fa74e" "e6ccd0cc810aa6458391e95e4874942875252cd0342efd5a193de92bfbb6416b" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" default)))
 '(fci-rule-color "#c7c7c7")
 '(global-display-line-numbers-mode t)
 '(js2-bounce-indent-p nil)
 '(linum-format " %5i ")
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (dakrone-light-theme greymatters-theme color-theme-modern goose-theme which-key xah-fly-keys xah-find monotropic-theme nofrils-acme-theme tao-theme soft-stone-theme gandalf-theme racket-mode js-comint js2-mode yaml-mode magit anti-zenburn-theme sublime-themes)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(show-paren-mode t)
 '(speedbar-obj-do-check nil)
 '(speedbar-show-unknown-files t)
 '(speedbar-vc-do-check nil)
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
 '(default ((t (:inherit nil :stipple nil :background "#f9fbfd" :foreground "#2f2f2f" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Ricty"))))
 '(show-paren-match ((t (:background "DarkSeaGreen3")))))

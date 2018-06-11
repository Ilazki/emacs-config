;; TODO:  Figure out necessary steps to turn custom-config.el (and other deps) into org-babel files
;; Probably have to (require 'org) here and then load the file, tangle it, then load that.
;; Need to rethink directory layout a bit in that case.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD CUSTOM CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(package-initialize)

;; Wrap file load in a couple speed-up tweaks
(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold 20971520)	; Raise GC cap to 20mb during init.
  (load "~/.emacs.d/custom-config.el")
;;  (setq gc-cons-threshold 5242880)		; Then restore to something lower (5mb) after if wanted.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS CUSTOMISATION, DO NOT TOUCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#f03669" "#9fc43d" "#ffbe4a" "#02a2ff" "#dc84c0" "#7fcfd3" "#e8e8e8"])
 '(backup-directory-alist (quote (("." . "~/tmp/backups"))))
 '(backward-delete-char-untabify-method (quote hungry))
 '(cider-auto-mode nil)
 '(cider-auto-select-error-buffer nil)
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(cider-eval-spinner-type (quote vertical-breathing))
 '(cider-font-lock-dynamically (quote (macro function var deprecated core)))
 '(cider-lein-parameters "trampoline repl :headless :host localhost")
 '(cider-repl-display-help-banner nil)
 '(custom-enabled-themes (quote (base16-irblack)))
 '(custom-safe-themes
   (quote
    ("3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "7790dbc91156dd9a5c7f2ee99e5f7e6549f244038b46ed6352d7693be2e0aec6" "aa87469691932ff791f966bffb885ecd97ebfa4dc4d42e479f3819ac4a3fbcaf" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "9f6750057fefba39c184783c7b80ddd9c63bc6e8064846b423b4362c9e930404" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" "40a783fd65a506a532c32423cd5ca3619d8192cac030b1f1cc2a609d7521bc9d" "f0eda5a0cbe43d2febfbbeed6830c6205f4b3edd1a3597a002c57f79f36cff89" "f782ed87369a7d568cee28d14922aa6d639f49dd676124d817dd82c8208985d0" "9558f71c706fba7b136e75d9c5e73ddd2c9d91e76e2b18f733d4ab2f388f3b72" "33bb2c9b6e965f9c3366c57f8d08a94152954d4e2124dc621953f5a8d7e9ca41" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "588b1ec3f63dfbd7ab2ba7eda4b1b6009dd1c8ed6a321fa98c492d8a63f1bba7" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "c4156b408e636a1286e0e8ed9531b4767dc2c8aa225d2012959e2a8610272cdc" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "357d5abe6f693f2875bb3113f5c031b7031f21717e8078f90d9d9bc3a14bcbd8" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" default)))
 '(default-justification (quote full))
 '(dired-listing-switches "-alhF")
 '(dired-omit-files "^\\(?:\\.[^.]+\\)\\|\\.?#")
 '(ecb-options-version "2.40")
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(elfeed-db-directory "~/.emacs.d/elfeed/")
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(fci-rule-color "#37474f")
 '(fill-column 78)
 '(frame-resize-pixelwise t)
 '(fringe-mode 3 nil (fringe))
 '(global-hl-line-mode t)
 '(indent-guide-char "│")
 '(indent-guide-recursive t)
 '(indent-tabs-mode t)
 '(inf-clojure-generic-cmd (quote ("localhost" . 5555)))
 '(inhibit-startup-screen t)
 '(linum-delay t)
 '(linum-format "%3i ")
 '(minimap-recenter-type (quote middle))
 '(minimap-update-delay 0.1)
 '(minimap-window-location (quote right))
 '(neo-persist-show nil)
 '(neo-theme (quote arrow))
 '(nlinum-format "%3i|")
 '(ob-translate:default-dest "de,fr,it,ja")
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(org-startup-with-inline-images t)
 '(package-selected-packages
   (quote
    (calfw calfw-cal calfw-gcal calfw-ical calfw-org leuven-theme zen-and-art-theme zeal-at-point yaml-mode xterm-color x509-mode writeroom-mode wn-mode windsize windresize windata wiki-nav which-key weechat web-search web-mode wanderlust w3m vmd-mode vline vdirel utop undo-tree tuareg tree-mode tao-theme tabbar sublime-themes smartparens smart-tabs-mode sauron sane-term rainbow-mode rainbow-delimiters racket-mode quasi-monochrome-theme python-environment psci psc-ide projectile popup-imenu popup-complete pixie-mode php-mode persp-mode pcre2el pastels-on-dark-theme paren-face paperless pandoc package-lint ox-pandoc ox-asciidoc origami org-pdfview org-pandoc org-mind-map org-link-minor-mode org-fstree org-ac olivetti ob-translate ob-crystal nov nlinum nix-mode neotree nav monroe monokai-theme molokai-theme minimap minimal-theme markdown-toc markdown-preview-mode markdown-mode+ make-color lush-theme lorem-ipsum lice lacarte kotlin-mode kosmos-theme json-mode indicators indent-guide imenu-list imenu-anywhere ido-vertical-mode ido-ubiquitous idle-highlight-mode hy-mode hexrgb helm-swoop helm-nixos-options helm-google helm-emmet helm-descbinds helm-clojuredocs helm-cider ham-mode gntp git-commit gh-md fsharp-mode fountain-mode fish-mode espresso-theme epc ensime elm-mode elfeed-web elfeed-org elfeed-goodies elf-mode ecb direx dired-single dired-rainbow dired-filter diminish ctags-update ctags company-qml company-nixos-options company-lua company-inf-ruby column-marker color-theme-solarized color-theme-sanityinc-tomorrow clues-theme clomacs clojure-quick-repls clojure-mode-extra-font-locking clojure-cheatsheet clojars clj-refactor cherry-blossom-theme bubbleberry-theme bbdb-vcard base16-theme badwolf-theme babel-repl babel ascii-art-to-unicode aria2 android-mode aggressive-indent adoc-mode adaptive-wrap ac-js2 ac-ispell ac-html ac-haskell-process ac-etags ac-emmet ac-cider 4clojure)))
 '(persp-add-buffer-on-after-change-major-mode nil)
 '(persp-interactive-completion-system (quote ido))
 '(persp-keymap-prefix "")
 '(persp-mode t nil (persp-mode))
 '(persp-mode-prefix-key "")
 '(persp-modestring-dividers (quote ("" "" "|")))
 '(persp-nil-name "main")
 '(persp-set-ido-hooks t)
 '(persp-when-remove-buffer-switch-to-other-buffer t)
 '(pixie-inf-lisp-program "pxi")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(rainbow-ansi-colors (quote auto))
 '(rainbow-ansi-colors-major-mode-list (quote (sh-mode c-mode c++-mode clojure-mode)))
 '(rainbow-html-colors (quote auto))
 '(rainbow-html-colors-major-mode-list
   (quote
    (html-mode css-mode php-mode nxml-mode xml-mode clojure-mode org-mode)))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 4)
 '(tabbar-separator (quote (0.5)))
 '(term-default-bg-color nil)
 '(term-default-fg-color nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(cider-error-highlight-face ((t (:inherit nil :underline "red"))))
 '(cider-result-overlay-face ((t (:background "grey20" :foreground "medium spring green" :box (:line-width -1 :color "grey30")))))
 '(cider-warning-highlight-face ((t (:inherit nil :underline "yellow"))))
 '(hl-line ((t (:background "gray17"))))
 '(link ((t (:foreground "#02a2ff" :underline t))))
 '(linum ((t (:inherit (shadow default) :background "gray20" :foreground "gray60" :family "Input"))))
 '(minimap-active-region-background ((t (:stipple nil :background "gray25"))))
 '(org-link ((t (:foreground "#02a2ff"))))
 '(parenthesis ((t (:inherit font-lock-comment-delimiter-face))))
 '(persp-face-lighter-buffer-not-in-persp ((t (:underline t :slant italic :weight bold))))
 '(persp-face-lighter-default ((t (:inherit italic :underline nil :weight bold))))
 '(scroll-bar ((t (:background "#383838"))))
 '(term-color-black ((t (:background "#040404" :foreground "#040404"))))
 '(term-color-blue ((t (:background "#02a2ff" :foreground "#02a2ff"))))
 '(term-color-cyan ((t (:background "#7fd0d3" :foreground "#7fd0d3"))))
 '(term-color-green ((t (:background "#9fc53c" :foreground "#9fc53c"))))
 '(term-color-magenta ((t (:background "#dc84c1" :foreground "#dc84c1"))))
 '(term-color-red ((t (:background "#f03669" :foreground "#f03669"))))
 '(term-color-white ((t (:background "#d7d7d7" :foreground "#d7d7d7"))))
 '(term-color-yellow ((t (:background "#ffa402" :foreground "#ffa402"))))
 '(tool-bar ((t (:background "#393939" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(trailing-whitespace ((t (:background "#262626" :foreground "#ffffb6"))))
 '(variable-pitch ((t (:height 1.15 :family "Ubuntu"))))
 '(vline ((t (:inherit hl-line)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

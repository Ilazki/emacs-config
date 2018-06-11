;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FACE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: colours darker than #202020 sometimes shift slightly with emacs-gtk.
;; It's usually barely perceptible, but once you notice it, it's incredibly
;; annoying.
;; No idea why this happens, but it doesn't affect emacs-lucid.

;; Can't set relevant faces until they're loaded.
(require 'helm)
(require 'powerline)

(set-face-attribute
  'helm-selection nil
  :distant-foreground "black"
  :background "gray30")


(set-face-attribute
  'fringe nil
  :foreground "#506080"
  :background "#1c1c1c"		;; Matches my manually-set default face
  )


(set-face-attribute
  'mode-line nil
;  :box '(nil :foreground "#918f88" :background "#484844")
  :foreground "#918f88"
  :background "#484844")

(set-face-attribute
  'powerline-active1 nil
  :background "grey22"
  :inherit 'mode-line)

(set-face-attribute
  'powerline-active2 nil
  :background "grey40"
  :inherit 'mode-line)

(set-face-attribute
 'mode-line-inactive nil
 :foreground "#918f88"
 :background "#484844")

(set-face-attribute
 'powerline-inactive1 nil
 :background "grey11"
 :inherit 'mode-line-inactive)

(set-face-attribute
 'powerline-inactive2 nil
 :background "grey20"
 :inherit 'mode-line-inactive)

(set-face-attribute
 'mode-line-buffer-id nil
 :foreground "medium spring green")

(set-face-attribute
 'mode-line-highlight nil
 :foreground "medium spring green")

;; Setting these manually because, for some reason, the comment colours
;; were showing up different on the terminal.  This fixes it.

(set-face-attribute
 'font-lock-comment-delimiter-face nil
 :foreground "#484844")

(set-face-attribute
 'font-lock-comment-face nil
 :foreground "#6c6c66")

(set-face-attribute
 'font-lock-doc-face nil
 :foreground "#71cbff")

;; Sets the vertical divider FG and BG colours to match the mode-line.
;; In a terminal, this hides the pipe characters used, making the divider
;; look like a solid bar.
;; The foreground colour is also used in GUI emacs, making it blend in
;; more nicely there as wel.
(set-face-attribute 'vertical-border nil :background (face-attribute 'mode-line :background))
(set-face-attribute 'vertical-border nil :foreground (face-attribute 'mode-line :background))


;; I originally was setting these via customize-face, but it has a tendency
;; to completely obliterate the default face at random because of my use of
;; variable-pitch as well.  No idea why but it's easier to set it here than
;; it is to constantly fight with customize.

(set-face-attribute
  'default nil
  :family "Input"
  :foundry "unknown"
  :slant 'normal
  :weight 'normal
  :height 120
  :width 'semi-condensed
  :foreground "#fff"
  :background "#1c1c1c")

(set-face-attribute
  'variable-pitch nil
  :height 1.20
  :family "Ubuntu")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabbar configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tabbar)

(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))

(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))

;; Decided to try making current tab blend in differently.
;; Leaving this for reversal if desired.
;; (set-face-attribute
;;  'tabbar-selected nil
;;  :background "gray75"
;;  :foreground "black"
;;  :box '(:line-width 5 :color "gray75" :style nil))
;;
;; (set-face-attribute
;;  'tabbar-selected-modified nil
;;  :background "gray75"
;;  :foreground "Violetred4"
;;  :box '(:line-width 5 :color "gray75" :style nil))


(set-face-attribute
 'tabbar-selected nil
 :background "#1c1c1c"
 :foreground "white"
 :box '(:line-width 5 :color "#1c1c1c" :style nil))

(set-face-attribute
 'tabbar-selected-modified nil
 :background "1c1c1c"
 :foreground "Turquoise1"
 :box '(:line-width 5 :color "#1c1c1c" :style nil))

(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))

;; (set-face-attribute
;;  'tabbar-button nil
;;  :box '(:line-width 1 :color "gray20" :style nil)
;;  :inherit 'fixed-pitch)

(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

(set-face-attribute
 'tabbar-modified nil
 :background "gray30"
 :foreground "Turquoise1"
 :box '(:line-width 5 :color "gray30" :style nil))

(set-face-attribute
 'tabbar-button nil
 :foreground "white"
 :background "gray30"
 :family "Monospace"
 :height 1.5
 :box '(:line-width 1 :color "gray30" :style nil)
 :inherit 'tabbar-default)

(set-face-attribute
 'tabbar-button-highlight nil
 :foreground "Turquoise1"
 :background "gray30"
 :family "Monospace"
 :height 1.5
 :box '(:line-width 1 :color "gray30" :style nil)
 :inherit 'tabbar-default)

;; :inherit 'fixed-pitch

;; Disable images for a speed-up
(setq tabbar-use-images nil)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for tab.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)

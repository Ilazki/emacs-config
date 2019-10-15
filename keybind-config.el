;;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;;;;;;;;;;;;;;;;;

;; Generic prog-mode binds
(add-hook 'prog-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-;") 'comment-region)
			))

;; tuareg-mode (ocaml) keybindings
(add-hook 'tuareg-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-return>")   'tuareg-eval-phrase)	; Code eval. not in-line :(
            (local-set-key (kbd "C-j")          'tuareg-eval-phrase)	; C-return for terminal
            (local-set-key (kbd "<C-S-return>") 'tuareg-eval-buffer)	; Evaluate entire file
			(local-set-key (kbd "<C-M-return>") 'merlin-type-enclosing)
            (local-set-key (kbd "<M-RET>")      'auto-complete)
            (local-set-key (kbd "C-M-J")        'auto-complete)
            ))

;; cider-mode (clojure) keybindings
(add-hook 'cider-mode-hook
		  (lambda ()
			(local-set-key (kbd "<C-return>")   'cider-eval-defun-at-point)	; Like LT's inline eval
			(local-set-key (kbd "C-j")          'cider-eval-defun-at-point)	; C-return for terminal
			(local-set-key (kbd "<C-S-return>") 'cider-load-buffer)			; Evaluate entire file
			(local-set-key (kbd "<C-M-return>") 'cider-inspect)				; Inspect symbol
			(local-set-key (kbd "<M-RET>")      'complete-symbol)			; Autocomplete symbol
  			(local-set-key (kbd "C-M-j")        'complete-symbol)			; C-M-RET for terminal
			))

;; inf-clojure-minor-mode (clojure/pixie) keybindings.
(add-hook 'inf-clojure-minor-mode-hook
		  (lambda ()
			(local-set-key (kbd "<C-return>")   'inf-clojure-eval-defun)	; Eval current form
			(local-set-key (kbd "<C-S-return>") 'inf-clojure-eval-buffer)	; Eval entire buffer
			(local-set-key (kbd "<C-M-return>") 'inf-clojure-eval-buffer)	; Eval entire buffer
			(local-set-key (kbd "<M-RET>")      'inf-clojure-eval-defun)	; Eval current form
			(local-set-key (kbd "C-j")			'inf-clojure-eval-defun)	; C-return on term
			))

;; hy-mode (hy, clojure-ish lisp on python) keybindings.  Again, as similar as possible to clojure's
(add-hook 'hy-mode-hook
		  (lambda ()
			"hy-mode keybindings"
			(local-set-key (kbd "<C-return>") 'lisp-eval-defun)     ; Eval current form
			(local-set-key (kbd "<M-RET>")    'lisp-eval-defun)     ; Eval current form
			(local-set-key (kbd "C-j")        'lisp-eval-defun)		; C-return for term
			))

;; racket-mode: same thing.
(add-hook 'racket-mode-hook
		  (lambda ()
			"racket-mode keybindings"
			(local-set-key (kbd "<C-return>") 'racket-send-last-sexp)
			(local-set-key (kbd "<M-RET>")    'racket-send-last-sexp)
			(local-set-key (kbd "C-j")        'racket-send-last-sexp)
			))

;; lisp-interaction-mode, because it used C-j, which works for terminal
;; but not for the GUI.
(add-hook 'lisp-interaction-mode-hook
		  (lambda () 
			(local-set-key (kbd "<C-return>")   'eval-print-last-sexp)))


;; dired keybinds
(add-hook 'dired-mode-hook
		  (lambda ()
			(local-set-key (kbd ")") 'dired-omit-mode)
			(dired-omit-mode)))


;; Add narrow-or-widen-dwim to narrowing sub map.
;; Replaces default narrow-region binding.
(global-set-key (kbd "C-x n n") 'narrow-or-widen-dwim)

;; easier keybind for emacs' alt-tab equivalent, C-x o
(global-set-key (kbd "M-`")             'other-window)

;; Extra tmux and screen-esque keybindings for window management.
;; Persp-mode already uses the same prefix, so I'm just piggybacking on top of
;; its key chording for now.
(define-key persp-key-map (kbd "|")     'split-window-right)
(define-key persp-key-map (kbd "-")     'split-window-below)
(define-key persp-key-map (kbd "<tab>") 'other-window)
(define-key persp-key-map (kbd "x")     'delete-window)

;; Keybindings to force-insert tabs
(global-set-key (kbd "<C-tab>")         'tab-to-tab-stop) ; ctrl-tab
(global-set-key (kbd "<C-iso-lefttab>") 'tab-to-tab-stop) ; ctrl-shift tab

;; Move back-to-indentation to S-tab,
;; so that I can use M-m to set mark,
;; so I can use ctrl+space for M-x
(global-set-key (kbd "<backtab>")       'back-to-indentation)
(global-set-key (kbd "<C-SPC>")         'helm-M-x)
(global-set-key (kbd "C-@")             'helm-M-x) ; C-SPC in terms

;; Trying some other mark keys because I don't love M-m
(global-set-key (kbd "<C-S-SPC>")       'set-mark-command)
(global-set-key (kbd "C-t")             'set-mark-command)

;; Repurpose M-m for rectangle mark mode.  Also C-S-t
(global-set-key (kbd "M-m")             'rectangle-mark-mode)
(global-set-key (kbd "C-S-t")           'rectangle-mark-mode)


;; C-' to select all in addition to C-x h
(global-set-key (kbd "C-'") 'mark-whole-buffer)

;; F11 toggle fullscreen, following convention of other applications.
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)

;; (un)highlight symbol at point
(global-set-key (kbd "M-h")				'highlight-symbol-at-point)
(global-set-key (kbd "M-H")				'unhighlight-regexp)


;; Require helm because helm-map needs it
(require 'helm)
;; Shortcuts for selecting files, buffers
;; I hate using crap like C-v, C-f, and C-b for movement, so I may as
;; well make them useful to me.
(global-set-key (kbd "C-v") 'helm-find-files)   ; _v_isit file
(global-set-key (kbd "C-b") 'helm-buffers-list) ; _b_uffer list
(global-set-key (kbd "C-f") 'helm-buffers-list) ; No good mnemonic, just use it a lot

;; Move the original M-x to a new key as a backup in case helm explodes.
(global-set-key (kbd "M-X")           'execute-extended-command)

;; Helm replacements for emacs features
(global-set-key (kbd "M-x")             'helm-M-x)				; command complete
(global-set-key (kbd "C-x b")           'helm-buffers-list)
(global-set-key (kbd "C-x C-f")         'helm-find-files)
(global-set-key (kbd "C-M-/")           'helm-dabbrev)
(global-set-key (kbd "C-.")            #'helm-imenu-anywhere)	; Requires imenu-anywhere

;; Keybinds for controlling helm
;; Rationale:  I like tab completion for filenames and find the normal behaviour of
;; TAB = helm-select-action to be annoying.  So I'm moving that to C-TAB and changing
;; TAB to helm-execute-persistent-action.
(define-key helm-map (kbd "TAB")             'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>")           'helm-execute-persistent-action)

(define-key helm-map (kbd "C-TAB")           'helm-select-action)
(define-key helm-map (kbd "<C-tab>")         'helm-select-action)

;; imenu-list
(global-set-key (kbd "M-.") #'imenu-list-minor-mode)


;; Rearranging the default tabbar mwheel bindings.
;; * Wheel for tab swap within group
;; * shift-wheel for tab group swap
;; * ctrl-wheel for tab swap that will cycle through groups too.
(eval-after-load 'tabbar
  (lambda ()		       
	(define-key tabbar-mwheel-mode-map (kbd "<header-line> <mouse-4>")
	  'tabbar-mwheel-forward-tab)
	(define-key tabbar-mwheel-mode-map (kbd "<header-line> <mouse-5>")
	  'tabbar-mwheel-backward-tab)

	(define-key tabbar-mwheel-mode-map (kbd "<header-line> C-<mouse-4>")
	  'tabbar-mwheel-forward)
	(define-key tabbar-mwheel-mode-map (kbd "<header-line> C-<mouse-5>")
	  'tabbar-mwheel-backward)

	(define-key tabbar-mwheel-mode-map (kbd "<header-line> S-<mouse-4>")
	  'tabbar-mwheel-forward-group)
	(define-key tabbar-mwheel-mode-map (kbd "<header-line> S-<mouse-5>")
	  'tabbar-mwheel-backward-group)
	))

;; Global Neotree
(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "<f7>") 'neotree-find)

(add-hook 'neotree-mode-hook
		  (lambda ()
			"Local binds for Neotree"
            ;; More logical mnemonic for refresh.
			(local-set-key (kbd "r") 'neotree-refresh)
            ;; Moved off r
			(local-set-key (kbd "R") 'neotree-change-root)
            ;; g as refresh seems weird to me; I blame elinks.
            ;; Using it as 'go to' instead.
			(local-set-key (kbd "g") 'neotree-change-root)
			))

;; smartparens

(add-hook 'smartparens-mode-hook
		  (lambda ()
			"Local binds for smartparens"
			(local-set-key (kbd "<M-left>")		'sp-forward-barf-sexp)
			(local-set-key (kbd "<M-right>")	'sp-forward-slurp-sexp)
			(local-set-key (kbd "<M-S-left>")	'sp-backward-slurp-sexp)
			(local-set-key (kbd "<M-S-right>")	'sp-backward-barf-sexp)
			))

;; Shortcuts to call my alignment functions
(global-set-key (kbd "M-r") 'align-regexp)
(global-set-key (kbd "M-v") 'align-values)

;; Shortcuts for org-link-minor-mode links
(global-set-key (kbd "C-c l") 'org-insert-link-global)
(global-set-key (kbd "C-l") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)
(global-set-key (kbd "C-c g") 'org-open-at-point-global)

(with-eval-after-load 'undo-tree
  (define-key undo-tree-map (kbd "M-/") 'undo-tree-redo)
  )


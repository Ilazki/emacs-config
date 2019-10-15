;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT AND AUTOLOADING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
	  '(
		("gnu" . "http://elpa.gnu.org/packages/")
;;		("marmalade" . "https://marmalade-repo.org/packages/")
;;		("melpa stable" . "http://stable.melpa.org/packages/")
		("melpa" . "http://melpa.org/packages/")
	))
(package-initialize)

;; Add local-elisp and all subdirs to the load path
(let ((default-directory "~/.emacs.d/local-elisp/"))
  (add-to-list 'load-path (expand-file-name default-directory))
  (normal-top-level-add-subdirs-to-load-path))

(autoload 'nov-mode  "nov"  nil t)
(require 'livedown)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DEFINITIONS AND SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unset-tty-background (&rest f)
  "Removes emacs background in terminal frames, restoring the terminal's
background colour.  Used to retain terminal background transparency."
  (interactive)
  (if (not (display-graphic-p))
	  (set-frame-parameter (selected-frame) 'background-color nil)))

(advice-add 'server-create-tty-frame :after
			#'unset-tty-background)

(unset-tty-background)

(defun align-values (start end)
  "Vertically aligns region based on lengths of the first value of each line.
Example output:

	foo        bar
	foofoo     bar
	foofoofoo  bar"
  (interactive "r")
  (align-regexp start end
				"\\S-+\\(\\s-+\\)"
				1 1 nil))

(defun align-checklist (start end)
  "Vertically aligns org-mode list entries within a region."
  (interactive "r")
  (align-regexp start end
				"^\\s-*\\(?:\\-\\|\\+\\|\\(?:[[:digit:]]+[\\.\\)]\\)\\)\\(\\s-+\\)"
				1 1 nil))

;; Align-values at the nth column.
;; Not mine, could be buggy.  ref:
;; https://www.reddit.com/r/emacs/comments/5aio50/align_text_with_alignregexp/d9vp5i1/
;; Says it has problems with one field being shorter.

(defun align-nth-field (n beg end)
  "Align lines in region to the Nth field separated by whitespace."
  (interactive "*p\nr")
  (align-regexp beg end
                (rx-to-string
                 `(seq bol (* blank)
                       (+ (not blank))
                       ,(if (> n 1) `(repeat ,(1- n) (seq (+ blank) (+ (not blank)))) "")
                       (group (+ blank))))))

(defun show-trailing-whitespace ()
  "Toggles the show-trailing-whitespace variable to show or hide end-of-line whitespace"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Commands to terminate emacs.  
(defun exit-emacs ()
  "Exit emacs after confirmation by calling kill-emacs. Convenience function
because 'Quit' and 'Exit' are more commonly used terms for program termination
outside of the emacs world"
  (interactive)
  (if (y-or-n-p "Really exit emacs?")
	  (kill-emacs)
	))
(defalias 'quit-emacs 'exit-emacs)

;; Reduces some repetition with add-hook definitions.  Or will if I ever use it.
;; TODO:  actually use this.
(defun lambda- (expr)
  "Wrap expr in a lambda to reduce some boilerplate code."
	`(lambda () ,expr))

;; Memory-friendly aliases that I'm much more likely to remember and/or notice
;; than "helm-ucs" when using M-x
(defalias 'helm-unicode 'helm-ucs)
(defalias 'helm-character-select 'helm-ucs)

;; All-in-one narrow/widen function shamelessly taken from
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;;;;;;;;;;;;;;;;;
;; EXAMPLE CODE
;;;;;;;;;;;;;;;;;;

;; Some miscellaneous stuff I might want later, or tend to forget.

;; Turns on lexical binding for an elisp file, required for using closures:
;; (setq lexical-binding t)
;; Also possible via file variables in the first line, e.g.
;; "-*- lexical-binding: t; -*-"

;; Closure example for function aliases
;; (let ((f 'message))
;;   (funcall f "hello"))

;; Pseudo namespaces via closures
;; (defun pseudo-ns (ns)
;;   (lambda (s &rest args)
;;     (apply (intern (concat (symbol-name  ns) "-" (symbol-name s))) args)))
;;
;; (defun s (f &rest args)
;;   (apply (pseudo-ns 'string) f args))
;;
;; (s 'to-list "Hello")
;; => (72 101 108 108 111)

;; Both concepts can be combined and used together, e.g.
;; (funcall s 'to-list "Hello") inside a let binding, but it's really only
;; useful for shortening really long package "namespaces"



;;;;;;;;;;;;;;
;; LUA-MODE
;;;;;;;;;;;;;;

(add-hook 'lua-mode-hook (lambda()
                           (setq indent-tabs-mode nil)
                           ))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUAREG-MODE (OCAML)
;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'merlin-mode "merlin" "Merlin mode" t)

(add-hook 'tuareg-mode-hook 'paren-face-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
;; This will hopefully toggle orgtbl back OFF.
;; I like it in most modes, but it has issues with ocaml.
(add-hook 'tuareg-mode-hook (lambda () (orgtbl-mode -1)))

(add-hook 'caml-mode-hook   'merlin-mode)
(add-hook 'caml-mode-hook   (lambda () (orgtbl-mode -1)))

;; Alias because I tend to forget the dumb mode name.
(defalias 'ocaml-mode 'tuareg-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE EXTENSION ASSOCIATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.adoc$"     . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.epub$"     . nov-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIABLE-WIDTH FONT HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some types of files are better viewed variable-width.
;; Notably, markdown and asciidoc files.
(add-hook 'adoc-mode-hook     (lambda() (buffer-face-mode t)))
(add-hook 'markdown-mode-hook (lambda() (buffer-face-mode t)))
(add-hook 'ham-mode-hook      (lambda() (buffer-face-mode t)))
;; Giving variable-width font in org-mode a try.
(add-hook 'org-mode-hook      (lambda() (buffer-face-mode t)))



;;;;;;;;;;;;;;;;;;;;
;; MOUSE SETTINGS
;;;;;;;;;;;;;;;;;;;;

;; Moving S-mouse1 to S-mouse3 to free it for left mouse use
(global-set-key (kbd "<S-down-mouse-3>") 'mouse-appearance-menu)

;; ctrl + mouse scrolling.  Currently not using.
;; (global-set-key (kbd "<C-down-mouse-1>") 'mouse-drag-throw)

;; Focus follows mouse, to match my window manager behaviour.
(setq mouse-wheel-follow-mouse t)

;; Enable xterm mouse support.
(xterm-mouse-mode)



;;;;;;;;;;;;;;;;;;;
;; BELL SETTINGS
;;;;;;;;;;;;;;;;;;;

;; Disables bell for specific functions.
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort
                        abort-recursive-edit
                        exit-minibuffer
                        keyboard-quit))
          (ding))))

;; Disable bell universally instead.
(setq ring-bell-function 'ignore)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORIGAMI MODE (CODE FOLDING)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'origami)
(global-origami-mode)
(add-hook 'origami-mode-hook
		  (lambda ()
			"origami mode (code folding) keybindings"
			(local-set-key (kbd "<double-mouse-1>")   'origami-toggle-node)
			(local-set-key (kbd "<S-double-mouse-1>") 'origami-toggle-all-nodes)
			(local-set-key (kbd "C-c TAB")            'origami-toggle-node)
			))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No tab/space mixing in prog modes.
(add-hook 'prog-mode-hook (lambda()
                            (setq indent-tabs-mode nil)
                            ))



;;;;;;;;;;;;;;;;;;;;;;;
;; PARENTHESES MODES
;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on parentheses highlighting and make it instant.
(show-paren-mode)
(setq show-paren-delay 0)

;; Dim delimiters universally
(global-paren-face-mode t)

;; Automatic pairing and s-expression movement
(require 'smartparens)
(add-hook 'prog-mode-hook (lambda () (smartparens-mode t)))

;; smartparens does a bit more than I want.  This limits the mode
;; to only pairing (), {}, and [] by default, no quotes/etc.

(sp-pair "'" 		nil :actions :rem)
(sp-pair "\"" 		nil :actions :rem)
(sp-pair "\\\"" 	nil :actions :rem)
(sp-pair "\\\{" 	nil :actions :rem)
(sp-pair "\\\\(" 	nil :actions :rem)
(sp-pair "\\(" 		nil :actions :rem)
(sp-pair "\`"		nil :actions :rem)

;; Unfortunately, the above causes an unexpected problem.  If sp
;; doesn't have a match pair that starts with ` or ', then its
;; sp-backward-barf-sexp function breaks.  So this is a "pair" that
;; is highly unlikely to be matched but meets the requirements of
;; this odd bug. It's a dumb hack, but I'd rather do this than
;; deal with paredit.
(sp-pair "`--!!!~~~--" "`--!!!~~~--")



;;;;;;;;;;;;;;;;;;;;
;; LINE NUMBERING
;;;;;;;;;;;;;;;;;;;;

;; Hook-based linum modes.  Doesn't work for nlinum-mode due to daemon bug.
;; TODO:  verify if nlinum bug still present.
(add-hook 'prog-mode-hook #'linum-mode)
(add-hook 'css-mode-hook  #'linum-mode)
(add-hook 'text-mode-hook #'linum-mode)



;;;;;;;;;;;;;;;;;;
;; RAINBOW MODE
;;;;;;;;;;;;;;;;;;

;; Displays colour for things like #0055CC.

;; Require is necessary to make it diminish properly.
(require 'rainbow-mode)

;; Modes where I find rainbow useful.
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'org-mode-hook  #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)
(add-hook 'css-mode-hook  #'rainbow-mode)
(add-hook 'help-mode-hook #'rainbow-mode)



;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOW MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: Some windo managers have trouble with resizing emacs to full
;; sizes.  One way to fix this is to set the WM (like kwin) to ignore
;; requested geometry, but depending on the WM that may not always be
;; possible.
;; Another fix is emacs' frame-resize-pixelwise setting, in the Windows
;; group.  If set to true, allows emacs to resize itself more traditionally.

;; window navigation extension, for more convenient M+[1-9] shortcuts
;; for window swapping.
(wn-mode)

;; Resize windows with S-C-[arrows]
(require 'windsize)
(windsize-default-keybindings)

;; Make CIDER's console and stacktrace buffers appear in their own frames
;; the first time.  Slightly less annoying than having them steal my buffer
;; views.  Once they pop up I can either move them or combine them as I
;; please and kill off any extra frames I don't want.

(add-to-list 'special-display-buffer-names "*cider-error*")			; cider error popup
(add-to-list 'special-display-regexps      "^\*cider-repl\s?.+\*$")	; cider repl
;; Also doing same for some other modes
(add-to-list 'special-display-buffer-names "*inf-clojure*")			; inf-clojure repl, for pixie/clojure
(add-to-list 'special-display-buffer-names "*inferior-lisp*")
(add-to-list 'special-display-buffer-names "*HS-Error*")			; Error popup for interactive-haskell-mode

;; Change the title bar a bit
;; Default, for reference: (setq-default frame-title-format '(multiple-frames "%b" ("" invocation-name "@" system-name))
(setq-default frame-title-format '("%b <" invocation-name "@" system-name ">"))



;;;;;;;;;;;;;;;
;; POWERLINE
;;;;;;;;;;;;;;;

;; Enable powerline custom modeline
(powerline-default-theme)

;; Set the section seperator; only applicable to GUI emacs.
;; Options: arrow, arrow-fade, bar, box, brace, butt, chamfer, contour
;; curve, rounded, roundstub, slant, wave, zigzag, utf-8, nil
(setq powerline-default-separator 'arrow)

;; Like above, but UTF-8 seperator  on the terminal.  The default sucks,
;; so I use triangles to emulate the GUI version.
(setq powerline-utf-8-separator-left  #x25B6)
(setq powerline-utf-8-separator-right #x25C0)

;; Alternately, this can give a light gradient look.
;;(setq powerline-utf-8-separator-left #x2591)
;;(setq powerline-utf-8-separator-right #x2591)

;; TODO: figure out how adjust powerline's mode-line-format to visually
;; differentiate major and minor modes by changing colour or something.
;; I'd also like to be able to make the bar wider without messing up the
;; separators.



;;;;;;;;;;;;;;;;;
;; PERSPECTIVE
;;;;;;;;;;;;;;;;;

(persp-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABBAR CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Moved tabbar configuration to its own file due to size
(load "~/.emacs.d/tabbar-config.el")



;;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;;;;;;;;;;;;;;;;;

;; Moved to separate file due to size
(load "~/.emacs.d/keybind-config.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FACE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Moved to its own file for cleanliness
(load "~/.emacs.d/face-config.el")



;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMET [ZEN CODING]
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE, CIDER, ETC.
;;;;;;;;;;;;;;;;;;;;;;;;;;



(require 'auto-complete)
(require 'clojure-mode-extra-font-locking)
;;(require 'ac-cider)           ;; Requiring here forces it into use whenever clojure-mode starts.  Not wanted.
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
 '(progn
    (add-to-list 'ac-modes 'cider-mode)
    (add-to-list 'ac-modes 'cider-repl-mode)))

;; Pixie-mode hook for REPL access via inf-clojure mode
(add-hook 'pixie-mode-hook #'inf-clojure-minor-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRETTIFY-SYMBOLS MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turns c into ·c for prettify-symbols-alist.
;; Useful if the truncation causes indentation problems as some have complained.
;; Mostly obsoleted by prettify-string since I can now do (prettify-string " →")
(defun prettify-pad (c)
  `(?\s (Br . Bl) ,c))

;; Local package from [[https://github.com/Ilazki/prettify-utils.el/blob/master/prettify-utils.el][here]] to make prettify-symbols-alist creation nicer.
(require 'prettify-utils)

;; Can't figure out how to make the list I want global so this will have to do.
(defun prettify-set ()
  (setq prettify-symbols-alist
		(prettify-utils-generate
		 ("lambda"	"λ")
		 ("|>"		"▷")
		 ("<|"		"◁")
		 ("->>"		"↠")
		 ("->"		"→")
		 ("<-"		"←")
		 ("=>"		"⇒")
		 ("<="		"≤")
		 (">="		"≥")
;        ("*"   . ?×)					; Nope. Nice but confusing.
         )))

;; You can either set the full list or  push new symbols onto the list
;; during add-hook, along with enabling the mode for that hook.
;; I'm using the same list universally, so this calls prettify-set
;; for all prog modes,s o they all get get the same list.
(add-hook 'prog-mode-hook 'prettify-set)
(global-prettify-symbols-mode t)

;; Org-mode checkbox change
(prettify-utils-add-hook org-mode 
                         ("[ ]" "☐")
                         ("[X]" "☑")
                         ("[-]" "❍"))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELFEED + ELFEED ORG
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elfeed)
(require 'elfeed-goodies)
(require 'elfeed-org)

(elfeed-org)
(elfeed-goodies/setup)
(setq rmh-elfeed-org-files '( "~/.emacs.d/elfeed/feeds/main.org"
							  "~/.emacs.d/elfeed/feeds/comics.org"
							 ))



;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;; Set languages for inline evaluation via org-babel.
(org-babel-do-load-languages 'org-babel-load-languages
							 '(
							   (emacs-lisp . t)
							   (translate . t)
							   (ruby . t)
							   (perl . t)
							   (clojure . t)
							   ))

;; Enable org link style and org tables in other files
(require 'org-link-minor-mode)
(add-hook 'prog-mode-hook 'org-link-minor-mode)
(add-hook 'prog-mode-hook 'orgtbl-mode)
;; Link mode bindings set in keybind-config.el

;;;;;;;;;;;;;;;;;;;;;;;;
;; POLY-MODE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;

;; Poly modes get their own file
(load "~/.emacs.d/polymode-config.el")



;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISCELLANEOUS SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Edit server for edit-with-emacs extensions
(require 'edit-server)
(edit-server-start)

;; Use Perl-compatible regexps in any interactive regexps
(pcre-mode 1)

;; Disable scroll bars
(scroll-bar-mode 0)

;; Global undo-tree
(global-undo-tree-mode 1)

;; Global visual line mode.  Nicer word wrapping IMO
(global-visual-line-mode 1)

;; Current-line highlighting for everything
(global-hl-line-mode 1)

;; When set to '1', selections get deleted by typing instead of requiring
;; explicitly hitting something like delete or backspace.  This conforms
;; with other editor behaviour.
(delete-selection-mode 1)

;; Create clickable links in any mode based off text-mode or prog-mode
(add-hook 'text-mode-hook #'goto-address-mode)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

;; Auto-complete for prog mode
(add-hook 'prog-mode-hook #'auto-complete-mode)

;; Replace the full "yes or no" prompts with quick y/n.  May be a bad
;; idea, but I got tired of typing out yes/no for things.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Window change undo/redo, for when something creates a new window and
;; completely ruins a layout.  Use C-c left/right arrows for undo/redo.
(winner-mode 1)

;; Begone, foul toolbar.
(tool-bar-mode -1)
;; Menu bar too, if I get tired of it.
;; (menu-bar-mode -1)

;; Disables the 'ad-handle-definition: `tramp-read-passwd got redefined'
;; error helm causes, among other things.  It's pointless noise.
;; As a bonus, removing it also seems to speed up initial helm invocation.
(setq ad-redefinition-action 'accept)

;; Replaces the default safe URL of "\\`cid:", which only allows inline images.
(setq mime-w3m-safe-url-regexp nil)



;;;;;;;;;;;;;;;;;;;;
;; DIMINISH MODES
;;;;;;;;;;;;;;;;;;;;

;; Hide the display of certain minor modes.
;; This is supposed to be at end of file, so that other modes are
;; loaded first.
(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'rainbow-mode)
(diminish 'visual-line-mode)
(diminish 'org-link-minor-mode)
(diminish 'pcre-mode)



;;;;;;;;;;;;;;;;;;;;;
;; DAEMONISE EMACS
;;;;;;;;;;;;;;;;;;;;;

;; Start the daemon so emacsclient can work
(server-start)
;; and disable that annoying buffer kill query that comes with it.
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)




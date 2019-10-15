(require 'polymode)

;; Ref: https://polymode.github.io/defining-polymodes/


;; A basic zim+code block polymode.
;; Currently only defines ocaml, can add more as needed.

(define-hostmode poly-zim-hostmode
  :mode 'text-mode)

(define-innermode poly-zim-ocaml-innermode
  :mode 'tuareg-mode
  :head-mode 'host
  :tail-mode 'host
  :head-matcher "^{{{code: lang=\"objective-caml\" linenumbers=\"True\""
  :tail-matcher "^}}}\n"
  )

;; Incomplete, doesn't seem to be necessary for a basic setup.
;; (define-auto-innermode poly-zim-ocaml-innermode-code
;;   :head-matcher (cons )
;;   :tail-matcher
;;   :mode-matcher
;;   :head-mode 'host
;;   :tail-mode 'host
;;   )

(define-polymode poly-zim-mode
  :hostmode 'poly-text-hostmode
  :innermodes '(poly-zim-ocaml-innermode)
  )

(when (< emacs-major-version 24)
  (error "Need Emacs v24 or higher. Currently have: v%d" emacs-major-version))

; TODO(zmanji): Put non-package configuration elsewhere.

; Remove unused GUI components.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

; Uniquify Buffer Names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; Show matching parens like vim
(show-paren-mode 1)
(setq show-paren-delay 0)

; Don't use tabs for indentation
(setq-default indent-tabs-mode nil)

; Save curor position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

; Shorter y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Show whitespace
; (global-whitespace-mode t)
; (setq-default show-trailing-whitespace t)
; Minimal style (similar to vim) see
; http://ergoemacs.org/emacs/whitespace-mode.html
; (setq whitespace-style (quote (face tabs trailing newline space-mark tab-mark newline-mark)))
; (setq whitespace-display-mappings
;       '(
;         (space-mark 32 [183])
;         (newline-mark 10 [172 10])
;         (tab-mark 9 [9655 9])
;         ))

; TODO(zmanji): Strip trailing whitespace
; TODO(zmanji): Show line numbers

(require 'package)

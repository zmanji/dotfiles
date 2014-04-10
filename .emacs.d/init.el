(when (< emacs-major-version 24)
  (error "Need Emacs v24 or higher. Currently have: v%d" emacs-major-version))

(setq user-full-name "Zameer Manji")
(setq user-mail-address "zmanji@gmail.com")

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


; TODO(zmanji) Show tabs as ▸ and eol as ¬
; TODO(zmanji): Figure out how to show trailing whitespace
; TODO(zmanji): Strip trailing whitespace
; TODO(zmanji): Show line numbers
; TODO(zmanji): Show vim style 'ruler'
; TODO(zmanji): Set encoding of everything to utf-8
; TODO(zmanji): Show soft line breaks
; TODO(zmanji): autoread/autowrite
; TODO(zmanji): Enable mouse in the terminal
; TODO(zmanji): Don't wrap long lines insert hard newlines
; TODO(zmanji): Integrate with system clipboard
; TODO(zmanji): Setup custom spelling
; TODO(zmanji): Setup backup/undo
; TODO(zmanji): Configure whitespace to be <tab> = 2 spc (tab-width) ?
; TODO(zmanji): Configure whitespace to be textwidth = 80
; TODO(zmanji): Highlight the current line and 81st column
; TODO(zmanji): Possibly investigate disabling highlighting on unfocus
; TODO(zmanji): Setup file type specific preferences
; TODO(zmanji): Check .vimrc for anything else that is needed

(require 'package)
; TODO(zmanji): Add evil-mode
; TODO(zmanji): Add auto-complete or company-mode
; TODO(zmanji): Configure buffer auto completion for dictionary words
; TODO(zmanji): Add YASnipped
; TODO(zmanji): Add projectile
; TODO(zmanji): Add helm/ido
; TODO(zmanji): Configure helm/ido for M-x or get smex
; TODO(zmanji): Investigate and setup RTM integration
; TODO(zmanji): Investigate org-mode
; TODO(zmanji): Setup graphical undo tree (global-undo-tree-mode) ?
; TODO(zmanji): Setup a powerline
; TODO(zmanji): Inestigate magit
; TODO(zmanji): Figure out way to show kill/yank ring
; TODO(zmanji): Investigate ace-jump (easymotion style)
; TODO(zmanji): Investigate exec-path-with-shell because GUI Emacs might not use
; the shell's $PATH on OSX.
; TODO(zmanji): Add language specific modes: c, c++, rust, pandoc, markdown,
; haml, ruby, python, scala, latex,

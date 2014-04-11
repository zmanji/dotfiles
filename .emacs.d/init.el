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
; TODO(zmanji): Investigate the following:
; (setq uniquify-separator "/")
; (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
; (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

; Show matching parens like vim
(show-paren-mode 1)
(setq show-paren-delay 0)

; Don't use tabs for indentation
(setq-default indent-tabs-mode nil)
; Set tab display to be equal to 2 spaces
(setq-default tab-width 2)

; Save curor position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

; Shorter y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Position/Size in modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

; Reload buffers which have been modified externally
(global-auto-revert-mode t)

; Highlight the current line
; TODO(zmanji): enable this when the color scheme doesn't make it unreadable.
;(global-hl-line-mode +1)

; Show line numbers
(global-linum-mode t)
; Put a space after the number so it is seperated from the buffer.
(setq linum-format "%d ")

; TODO(zmanji): Take more core config from:
; https://github.com/bling/dotemacs/blob/master/config/init-core.el

; TODO(zmanji): Show tabs as ▸ and eol as ¬
; TODO(zmanji): Show line-wraps as ↪
; TODO(zmanji): Figure out how to show trailing whitespace
; TODO(zmanji): Strip trailing whitespace
; TODO(zmanji): Set encoding of everything to utf-8
; TODO(zmanji): Figure out how to highlight TODO/XXX/HACK etc
; TODO(zmanji): Show soft line breaks
; TODO(zmanji): autowrite style functionality
; TODO(zmanji): Enable mouse in the terminal
; TODO(zmanji): Don't wrap long lines insert hard newlines
; TODO(zmanji): Integrate with system clipboard
; TODO(zmanji): Setup custom spelling
; TODO(zmanji): Setup backup/undo
; TODO(zmanji): Configure whitespace to be textwidth = 80
; TODO(zmanji): Highlight the 81st column
; TODO(zmanji): Possibly investigate disabling highlighting on unfocus
; TODO(zmanji): Setup file type specific preferences
; TODO(zmanji): Figure out how to change the cursor in terminal when entering/exiting insert mode.
; TODO(zmanji): Bind '\\' to comment/uncomment line/region
; TODO(zmanji): Check .vimrc for anything else that is needed
; TODO(zmanji): Figure out folding
; TODO(zmanji): Use <TAB> to autocomplete and indent (tab-always-indent 'complete) ?
; TODO(zmanji): Get smartparens (auto insert ']' after '[' )
; TODO(zmanji): Investigate 'savehist or similar for saving some state between sessions
; TODO(zmanji): Investigate setting up 'tramp for remote access
; TODO(zmanji): Investigate anzu: https://github.com/syohex/emacs-anzu to show how many search
; matches are in buffer
; TODO(zmanji): Investigate dired and dired-x
; TODO(zmanji): Configure 'eshell:
; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el#L363

(require 'package)
; TODO(zmanji): Consider using the stable melpa repo and pinning certain
; packages (ie evil-mode) to that repo. Can pin certain packages to certain
; repos using package-pinned-packages.
; See https://github.com/milkypostman/melpa#stable-packages for stable package repo
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
; Package management philosophy: Just use M-x package install and git commit the
; state of the elpa folder delete/update folder as needed.

(require 'evil)
(evil-mode 1)
; TODO(zmanji): Configure evil-mode
; TODO(zmanji): look at evil-overriding-maps and evil-intercept-maps
; TODO(zmanji): investigate default evil-states for motions via
; evil-*-state-modes like evil-emacs-state-modes
; TODO(zmanji): Invesigate evil-motion-state-map defaults
; TODO(zmanji): Investigate key mappings here: http://stackoverflow.com/a/10166400/2874
; TODO(zmanji): Take evil configuration from:
; https://github.com/bling/dotemacs/blob/master/config/init-evil.el
; TODO(zmanji): Take bindings from
; https://github.com/bling/dotemacs/blob/master/config/init-bindings.el
; TODO(zmanji): Investigate elscreen to emulate vim tabs
; TODO(zmanji): Investigate flyspell
; TODO(zmanji): Add auto-complete or company-mode and look at hippie-expand
; TODO(zmanji): Configure buffer auto completion for dictionary words
; TODO(zmanji): Add YASnipped
; TODO(zmanji): Add projectile See:
; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el#L264 for basic config
; TODO(zmanji): Add helm/ido
; TODO(zmanji): Configure helm/ido for M-x or get smex
; TODO(zmanji): Consider helm extentions like helm-swoop, helm-config
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
; TODO(zmanji): Investigate 'diminsh to simplify modeline
; TODO(zmanji): Investigate 'pretty-symbols, 'color-identifiers-mode, 'fancy-narrow

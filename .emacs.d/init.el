(when (< emacs-major-version 24)
  (error "Need Emacs v24 or higher. Currently have: v%d" emacs-major-version))

(setq user-full-name "Zameer Manji")
(setq user-mail-address "zmanji@gmail.com")

; TODO(zmanji): Consider using use-package to cleanup package mangement code:
; https://github.com/jwiegley/use-package

; Package management at the start so we can assume everything is loaded.
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


; TODO(zmanji): Put non-package configuration elsewhere.

; Remove unused GUI components.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
; Show it in GUI OSX because it doesn't take up any space
(when (eq window-system 'ns)
  (menu-bar-mode 1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

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
(global-hl-line-mode +1)

; Show line numbers
(global-linum-mode t)
; Put a space after the number so it is seperated from the buffer.
(setq linum-format "%d ")

; UTF-8 everything taken from: http://stackoverflow.com/a/2903256/2874
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
 (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

; Improve scrolling
(setq scroll-conservatively 9999)
(setq scroll-preserve-screen-position t)

; Seed the random number generator
(random t)

; Terminal Emacs on OSX needs to use pbpaste/pbcopy
(when (eq system-type 'darwin)
  ; When in a terminal
  (unless (display-graphic-p)
    ; Mainly from https://gist.github.com/the-kenny/267162
    (defun zmanji/copy-from-osx ()
      "Copies the current clipboard content using the `pbcopy` command"
      (shell-command-to-string "pbpaste"))

    (defun zmanji/paste-to-osx (text &optional push)
          "Copies the top of the kill ring stack to the OSX clipboard"
          (let ((process-connection-type nil))
            (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
              (process-send-string proc text)
              (process-send-eof proc))))

    (setq interprogram-cut-function 'zmanji/paste-to-osx)
    (setq interprogram-paste-function 'zmanji/copy-from-osx)
))

; Don't clobber system clipboard on kill
(setq save-interprogram-paste-before-kill t)

; On OSX, GUI Emacs does not have the same environment as terminal environment
(when (eq system-type 'darwin)
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

; Enable smartparens for smart pair matching.
; TODO(zmanji): Enable for latex/html as well
(require 'smartparens)
(require 'smartparens-config)
(add-hook 'prog-mode-hook (lambda () (smartparens-mode +1)))

; Color parens differently depending on nesting
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Enable auto indentation on newline globally
; NOTE: can use 'electric-indent-local-mode' to disable in modes that don't play well
(electric-indent-mode 1)

; TODO(zmanji): Consider 'auto-mode-alist for automatically activating major modes.
; TODO(zmanji): Show tabs as ▸ and eol as ¬
; TODO(zmanji): Show line-wraps as ↪
; TODO(zmanji): Figure out how to show trailing whitespace
; TODO(zmanji): Strip trailing whitespace:
; (add-hook 'write-file-hooks 'delete-trailing-whitespace) ?
; TODO(zmanji): Figure out how to highlight TODO/XXX/HACK etc
; TODO(zmanji): Show soft line breaks
; TODO(zmanji): autowrite style functionality
; TODO(zmanji): Enable mouse in the terminal
; TODO(zmanji): Don't wrap long lines insert hard newlines
; TODO(zmanji): Setup custom spelling
; TODO(zmanji): Setup backup/undo
; TODO(zmanji): Configure whitespace to be textwidth = 80
; TODO(zmanji): Highlight the 81st column
; TODO(zmanji): Setup file type specific preferences
; TODO(zmanji): Bind '\\' to comment/uncomment line/region
; TODO(zmanji): Check .vimrc for anything else that is needed
; TODO(zmanji): Figure out folding
; TODO(zmanji): Use <TAB> to autocomplete and indent (tab-always-indent 'complete) ?
; TODO(zmanji): Investigate 'savehist or similar for saving some state between sessions
; TODO(zmanji): Investigate setting up 'tramp for remote access
; TODO(zmanji): Investigate anzu: https://github.com/syohex/emacs-anzu to show how many search
; matches are in buffer
; TODO(zmanji): Investigate dired and dired-x
; TODO(zmanji): Configure 'eshell:
; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el#L363
; TODO(zmanji): Setup (global-font-lock-mode) for syntax highlighting?
; TODO(zmanji): Consider 'dired 'dired-x and similar

(require 'evil)
(evil-mode 1)
; Change the cursor to be a pipe in insert mode and a block when in
; normal mode when in the terminal. This uses the DECSCUSR escape
; codes on iTerm2/xterm. See http://git.io/zvDeWQ for example code.
(defun zmanji/evil-terminal-cursor-change ()
  (when (and (null (getenv "TMUX")) (null (display-graphic-p)))
    ; TODO(zmanji): Put a guard to ensure we are in an xterm like terminal.
    ; Enter Insert Mode (Cursor Shape: vertical bar)
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\e[6 q")))
    ; Leave Insert Mode (Cursor Shape: block)
    (add-hook 'evil-insert-state-exit-hook (lambda () (send-string-to-terminal "\e[2 q")))
    ; Restore cursor shape to vertical bar when quitting
    (add-hook 'kill-emacs-hook (lambda () (send-string-to-terminal "\e[6 q")))
    ; Ensure the cursor is a block shape when we enter normal mode or motion mode
    (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\e[2 q")))
    (add-hook 'evil-motion-state-entry-hook (lambda () (send-string-to-terminal "\e[2 q")))
))
(zmanji/evil-terminal-cursor-change)

; ESC quits everything as expected taken from: http://stackoverflow.com/a/10166400/2874
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

; TODO(zmanji): Configure evil-mode
; TODO(zmanji): look at evil-overriding-maps and evil-intercept-maps
; TODO(zmanji): investigate default evil-states for motions via
; evil-*-state-modes like evil-emacs-state-modes
; TODO(zmanji): Invesigate evil-motion-state-map defaults
; TODO(zmanji): Take evil configuration from:
; https://github.com/bling/dotemacs/blob/master/config/init-evil.el
; TODO(zmanji): Take bindings from
; https://github.com/bling/dotemacs/blob/master/config/init-bindings.el
; TODO(zmanji): Investigate elscreen to emulate vim tabs
; TODO(zmanji): Investigate flyspell
; TODO(zmanji): Add auto-complete or company-mode and look at hippie-expand
; TODO(zmanji): Configure buffer auto completion for dictionary words
; TODO(zmanji): Add YASnipped

(require 'projectile)
(projectile-global-mode t)
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
; TODO(zmanji): Add language specific modes: c, c++, rust, pandoc, markdown,
; haml, ruby, python, scala, latex,
; TODO(zmanji): Investigate 'diminsh to simplify modeline
; TODO(zmanji): Investigate 'pretty-symbols or 'pretty-mode,
; 'color-identifiers-mode, 'fancy-narrow


; To load themes installed manually we need to add the location of the themes to
; 'custom-theme-load-path.
; See: http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(load-theme 'base16-tomorrow-dark t)
; Use terminal background when possible instead of base16 bg
(custom-set-faces (if (not window-system) '(default ((t (:background "nil"))))))

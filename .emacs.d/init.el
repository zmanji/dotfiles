; Load Package Management
(require 'package)

; Packages to install
(setq zmanji-packages
      '(melpa ; Package for the melpa repo, adds utility functions
        evil
        solarized-theme
        auctex
        magit
        python
        slime
        auto-complete
        clojure-mode
        ac-slime
        pandoc-mode
         ))

; Add the MELPA
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

; Known problem with MELPA
(setq url-http-attempt-keepalives nil)


; Boot Packages
(package-initialize)

(require 'melpa)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg zmanji-packages)
  (when (and (not (package-installed-p pkg))
          (assoc pkg package-archive-contents))
    (package-install pkg)))

; Some UI Tweaks

; Hide the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; Disable the startup screen
(setq inhibit-startup-screen t)

; Nicer scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

; Always have line number in mode line
(line-number-mode t)
; Always have column number in mode line
(column-number-mode t)
; Always have file size in mode line
(size-indication-mode t)

(require 'solarized-theme)
; Use solarized as theme
(load-theme 'solarized-dark t)

; Set up evil
(require 'evil)
(evil-mode 1)

; From http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters
(setq evil-default-cursor t)

; Port some of my .vimrc here

; shift width is 2 by default
(setq evil-shift-width 2)

; Navigate Windows by using HJKL. I don't use those keys
; and can't use C-h because it conflicts with help

(define-key evil-normal-state-map "H" 'evil-window-left)
(define-key evil-normal-state-map "J" 'evil-window-down)
(define-key evil-normal-state-map "K" 'evil-window-up)
(define-key evil-normal-state-map "L" 'evil-window-right)

; Swap ; and :
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

; Enable Line Numbers globally
(global-linum-mode 1)

; Disable line wrapping
(setq default-truncate-lines t)

; Fix $PATH on OSX.
; See: http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
          (replace-regexp-in-string "[[:space:]\n]*$" ""
                                    (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (equal system-type 'darwin)
  (set-exec-path-from-shell-PATH))

; Everyone tells me I need this
(require 'cl)
(require 'ansi-color)

; Don't use tabs to indent
(setq-default indent-tabs-mode nil)

; Automatically re-load files that have been changed externally
(global-auto-revert-mode t)

; Indenting and matching braces
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

; highlight the current line
(global-hl-line-mode +1)

; ido-mode
(ido-mode t)
(setq
  ido-everywhere t
  ido-enable-flex-matching t)

; auto-completion in minibuffer
(icomplete-mode +1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq x-select-enable-clipboard t)

; Set up autocomplete
(require 'auto-complete-config)
(ac-config-default)

; Clojure/Slime Autocomplete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

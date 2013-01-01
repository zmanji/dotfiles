(require 'cl)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar zmanji-packages
  '(evil
    autopair
    markdown-mode
    pandoc-mode)
  "A list of packages that must be installed at launch.")

(defun zmanji-packages-installed-p ()
  "Returns true if and only if all the packages in 'zmanji-packages'
are installed."
  (loop for p in zmanji-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun zmanji-install-packages ()
  (unless (zmanji-packages-installed-p)
    (message "Refreshing package database...")
    (package-refresh-contents)
    (message " done.")
    (dolist (p zmanji-packages)
      (unless (package-installed-p p)
	(package-install p)))))

(zmanji-install-packages)

(add-to-list 'custom-theme-load-path (expand-file-name "vendor/base16-emacs"
					  (file-name-directory load-file-name)))

(load-theme 'base16-tomorrow t)

(require 'autopair)
(autopair-global-mode)

(require 'evil)
(evil-mode 1)

; Customizations to Evil
; Swap : and ;
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

; Ctrl-hjkl for moving among splits
; (define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
; (define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
; This Shadows the default help for Emacs
; (define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
; (define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

; Basic Emacs Configuration
; Highlight matching pairs of parens
(show-paren-mode t)
; No delay to show the matching pairs
(setq show-paren-delay 0)
; Turn off the bell
(setq ring-bell-function 'ignore)
; Integrate to System Clipboard
(setq x-select-enable-clipboard t)
; Remove Splash Screen
(setq inhibit-splash-screen t)
; Show Line of cursor in the mode line
(line-number-mode 1)
; Show Column of cursor in mode line
(column-number-mode 1)
; Highlight curent line
(global-hl-line-mode)
; Show line numbers to the left
(global-linum-mode 1)
; http://stackoverflow.com/q/13908192/2874
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("'[-a-zA-Z_][-a-zA-Z0-9_]*\\>" 0 'font-lock-constant-face)))

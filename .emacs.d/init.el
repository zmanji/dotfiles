; Load Package Management
(require 'package)

; Packages to install
(setq zmanji-packages
      '(evil
         ))

; Add the Marmalade package repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
; Add the ELPA
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))

; Boot Packages
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg zmanji-packages)
  (when (and (not (package-installed-p pkg))
          (assoc pkg package-archive-contents))
    (package-install pkg)))


; Set up evil
(require 'evil)
(evil-mode 1)

; Port some of my .vimrc here

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

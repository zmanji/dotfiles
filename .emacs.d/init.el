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

(require 'evil)
(evil-mode 1)

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

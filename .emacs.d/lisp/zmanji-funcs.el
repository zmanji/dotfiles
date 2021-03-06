; Functions to be used elsewhere

; Taken from http://stackoverflow.com/a/2903256/2874
(defun zmanji//utf8-everywhere ()
  ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (setq utf-translate-cjk-mode nil)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
      (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

(defun zmanji/setup-gui ()
  "Useful GUI fixes"
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (setq-default cursor-in-non-selected-windows nil)

  ;; Typing out 'yes' is too much
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Make alt/option meta and command do nothing
  ;; https://gist.github.com/railwaycat/3498096
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  (setq x-stretch-cursor t)

  ; When creating a new emacsclient frame, focus it
  (add-hook 'server-after-make-frame-hook 'raise-frame)

  (setq frame-title-format "%b %F")

  ;; use existing emacs frames
  (setq pop-up-frames nil)

)

(defun zmanji/setup-modeline ()
  "Tweaks to the modeline"
  ; Position/Size in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
)

(defun zmanji/setup-editing ()
  "Changes needed for editing"
  (zmanji//utf8-everywhere)
  ; Don't make backups
  (setq make-backup-files nil)
  ; Save only file buffers on focus lost.
  (add-hook 'focus-out-book (lambda () (save-some-buffers t nil)))
  ; Have to set the evil shift-width too
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default evil-shift-width 2)
  (setq-default fill-column 80)
  (global-display-line-numbers-mode t)
)

(defun zmanji/disable-custom ()
  "Disable the custom file"
  (setq custom-file (make-temp-file "emacs-custom"))
)

(defun zmanji//send-string-to-terminal-if-in-terminal (str)
  "Allow for blindly setting hooks that write to terminal"
  (when (and (null (getenv "TMUX")) (not (display-graphic-p)))
    (send-string-to-terminal str)
  )
)

; Change the cursor to be a pipe in insert mode and a block when in
; normal mode when in the terminal. This uses the DECSCUSR escape
; codes on iTerm2/xterm. See http://git.io/zvDeWQ for example code.
(defun zmanji/evil-terminal-cursor-change ()
  ; TODO(zmanji): Put a guard to ensure we are in an xterm like terminal.
  ; Enter Insert Mode (Cursor Shape: vertical bar)
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (zmanji//send-string-to-terminal-if-in-terminal "\e[6 q")))
  ; Leave Insert Mode (Cursor Shape: block)
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (zmanji//send-string-to-terminal-if-in-terminal "\e[2 q")))
  ; Restore cursor shape to vertical bar when quitting
    (add-hook 'kill-emacs-hook
              (lambda () (zmanji//send-string-to-terminal-if-in-terminal "\e[6 q")))
  ; Ensure the cursor is a block shape when we enter normal mode or motion mode
    (add-hook 'evil-normal-state-entry-hook
              (lambda () (zmanji//send-string-to-terminal-if-in-terminal "\e[2 q")))
    (add-hook 'evil-motion-state-entry-hook
              (lambda () (zmanji//send-string-to-terminal-if-in-terminal "\e[2 q")))
    )

(defun zmanji/delete-file-and-buffer ()
  "Kills the current buffer and deletes the associated file if any."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (delete-file filename)
      )
    )
  (kill-buffer)
  ;; TODO(zmanji): Consider deleting the current window as well.
  )

(defun zmanji/set-default-directory-projectile-root ()
  (let ((dir (projectile-project-root)))
    (when dir
       (setq-local default-directory dir)
      )
    )
  )


(defun zmanji/vsplit-eshell ()
  "vertically splits the window to open an ehell buffer for the current project"
  (interactive)
  (evil-window-vsplit)
  (projectile-run-eshell nil)

  )

(provide 'zmanji-funcs)

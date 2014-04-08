(when (< emacs-major-version 24)
  (error "Need Emacs v24 or higher. Currently have: v%d" emacs-major-version))

; Remove unused GUI components.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

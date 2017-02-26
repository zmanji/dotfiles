(setq user-full-name "Zameer Manji")
(setq user-mail-address "zmanji@gmail.com")

;; Add my own code to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'zmanji-funcs)

;; TODO(zmanji): Consider flycheck
;; TODO(zmanji): Create evil friendly bindings for occur-mode
;; TODO(zmanji): Consider view-mode: https://www.emacswiki.org/emacs/ViewMode
;; http://pragmaticemacs.com/emacs/view-mode-makes-for-great-read-only-reading/
;; TODO(zmanji): Consider creating a `view' like binary.
;; TODO(zmanji): Consider setting up `ediff' and `ediff-evil' and magit
;; integration: http://oremacs.com/2015/01/17/setting-up-ediff/
;; https://github.com/justbur/evil-ediff

(require 'package)
;; TODO(zmanji): Consider using stable melpa
;; TODO(zmanji): Consider versioning elpa/ directory.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package general
  ;; Good README here: https://github.com/noctuid/general.el
  :config
  (general-evil-setup))

(use-package server
  :config
  ;; Start an emacs server if not running
  (unless (server-running-p) (server-start)))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  (general-nmap
   :prefix ","
   "u" 'undo-tree-visualize)
)

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  ;; ESC quits everything as expected taken from: http://stackoverflow.com/a/10166400/2874
  (define-key minibuffer-local-isearch-map [escape] 'zmanji/minibuffer-keyboard-quit)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  ;; Fixes some jank in the terminal.
  (define-key evil-insert-state-map "\e" 'evil-normal-state)
  (define-key minibuffer-local-map [escape] 'zmanji/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'zmanji/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'zmanji/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'zmanji/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'zmanji/minibuffer-keyboard-quit)

  (define-key evil-window-map "q" 'evil-window-delete)

  (zmanji/evil-terminal-cursor-change)

  (evil-ex-define-cmd "fullscreen" 'toggle-frame-fullscreen)

  (setq evil-vsplit-window-right t)

  (use-package evil-commentary
    :init
    (evil-commentary-mode)
    :diminish evil-commentary-mode
    )

  (use-package evil-anzu
    :init
    (global-anzu-mode +1)
    :diminish anzu-mode
    )

  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1)
    )

  ;; TOOD(zmanji): Consider using advice here
  (evil-define-operator zmanji/evil-shift-left (beg end &optional count preserve-empty)
    "Wrapper around evil-shift-left that preserves visual selection"
    :type line
    (interactive "<r><vc>")
    (if (eq evil-state 'visual)
        (progn
          (evil-shift-left beg end count preserve-empty)
          (evil-normal-state)
          (evil-visual-restore)
          )
      (evil-shift-left beg end count preserve-empty)
      )
    )

  (evil-define-operator zmanji/evil-shift-right (beg end &optional count preserve-empty)
    "wrapper around evil-shift-right that preserves visual selection"
    :type line
    (interactive "<r><vc>")
    (if (eq evil-state 'visual)
        (progn
          (evil-shift-right beg end count preserve-empty)
          (evil-normal-state)
          (evil-visual-restore)
          )
      (evil-shift-right beg end count preserve-empty)
      )

    )

  (define-key evil-normal-state-map [remap evil-shift-right] 'zmanji/evil-shift-right)
  (define-key evil-normal-state-map [remap evil-shift-left] 'zmanji/evil-shift-left)

)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-show-operator-state-maps t))

(use-package base16-theme
  :init
  (load-theme 'base16-tomorrow-night t))

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 1)
  (smooth-scrolling-mode))

(use-package pbcopy)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  ;; Ignore special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package paren
  :config
  ;; Show matching parens like vim
  (show-paren-mode 1)
  (setq show-paren-delay 0))

;; TODO(zmanji): Investigate `org-journal', `org-agenda', `org-habit'
;; Reading Material:
;; http://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; https://github.com/tarleb/evil-rebellion/blob/master/evil-org-rebellion.el
;; https://writequit.org/eos/eos-org.html
;; https://github.com/jcf/emacs.d/blob/master/init-org.org
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-org.el
;; https://github.com/aaronbieber/dotfiles/blob/master/configs/emacs.d/lisp/init-org.el
;; http://pragmaticemacs.com/emacs/org-mode-basics-structuring-your-notes/
;; http://www.cachestocaches.com/2016/9/my-workflow-org-agenda/
(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-log-done 'time)
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    :config
    (setq org-bullets-bullet-list '("◉" "○")))
  ;; Some evil friendly keybindings taken from evil-org-mode:
  ;; https://github.com/edwtjo/evil-org-mode
  (general-mmap
   :keymaps 'org-mode-map
   "$" 'org-end-of-line
   "^" 'org-beginning-of-line
   ;; TODO(zmanji): Determine if this works as intended
   ;; "<" 'org-metaleft
   ;; ">" 'org-metaright
   )

  (general-nmap
   :keymaps 'org-mode-map
   "M-l" 'org-metaright
   "M-h" 'org-metaleft
   "M-k" 'org-metaup
   "M-j" 'org-metadown

   "M-L" 'org-shiftmetaright
   "M-H" 'org-shiftmetaleft
   "M-K" 'org-shiftmetaup
   "M-J" 'org-shiftmetadown
   )

  (general-imap
   :keymaps 'org-mode-map
   "TAB" 'indent-for-tab-command
   )

  ;; https://github.com/company-mode/company-mode/issues/50
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

  (setq org-startup-indented t)
  (add-hook 'org-mode-hook 'org-indent-mode)

  (setq org-hide-emphasis-markers t)

  (setq org-startup-folded nil)

  (setq org-return-follows-link t)
  (general-nmap
   :keymaps 'org-mode-map
   "<return>" 'org-return
   ))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :init
  (global-whitespace-mode)
  :config
  (setq whitespace-action '(warn-if-read-only auto-cleanup))
  (setq whitespace-display-mappings
  '((space-mark 32 [183])
  (space-mark 160 [164])
  (newline-mark 10 [172 10])
  (tab-mark 9 [9656 9])))
  ;; TODO(zmanji): Change `whitespace-space-regexp` and enable `spaces`?
  (setq whitespace-style
  '(face
  trailing
  ;spaces
  lines-tail
  newline
  ;empty
  space-after-tab
  space-before-tab
  ;space-mark
  tab-mark
  newline-mark)))

(use-package saveplace
  :init
  (save-place-mode))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package ag
  :config
  (setq ag-highlight-search t)
)

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (use-package counsel-projectile
    :config
    (general-nmap
    :prefix ","
    "f" 'counsel-projectile-find-file)
    )
)

;; XXX: See `ispell-help' for more information
;; See http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(use-package flyspell
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  ;; TODO(zmanji): Check that hunspell exists
  (setq ispell-program-name "/usr/local/bin/hunspell")
  (setq ispell-silently-savep t)

  (general-nmap
   :keymaps 'flyspell-mode-map
   "]s" 'flyspell-goto-next-error
   )
  ;; TODO(zmanji): Add a binding `zg' to store misspelled word in
  ;; personal dictionary
)

(use-package browse-url
  :config
  ;; Open contents in system browser
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "open"))

;; Reload buffers which have been modified externally
(use-package autorevert
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode t)
)

(use-package hl-line
  :init
  (global-hl-line-mode)
)

(use-package linum
  :init
  (global-linum-mode)
)

(use-package restart-emacs
  :init
  (evil-ex-define-cmd "restart" 'restart-emacs)
)

(use-package eyebrowse
  :init
  (eyebrowse-mode)
  :config
  (general-mmap
   "gt" 'eyebrowse-next-window-config
   "gT" 'eyebrowse-prev-window-config)
  (setq eyebrowse-wrap-around t)
  ;; A new workspace should just be the scratch buffer
  (setq eyebrowse-new-workspace t)

  (evil-ex-define-cmd "tab" 'eyebrowse-create-window-config)
  (evil-ex-define-cmd "tabc[lose]" 'eyebrowse-close-window-config)
  (evil-ex-define-cmd "tabr[ename]" 'eyebrowse-rename-window-config)

  (evil-define-command zmanji/evil-quit (&optional force)
    "Wrapper around evil-quit that attempts to close the current
eyebrowse tab before calling the actual function."
    :repeat nil
    (interactive "<!>")
    (let* ((window-configs (eyebrowse--get 'window-configs nil))
           (num-tabs (length window-configs))
           (num-windows (count-windows))
           )

      ;; If there are multiple windows delegate to the original
      ;; function. Otherwise close the current window config if there
      ;; is more than one.
      (if (eq num-windows 1)
          (if (> num-tabs 1)
              (call-interactively 'eyebrowse-close-window-config)
            (evil-quit force)
            )
        (evil-quit force)
        )
      )
    )

  (define-key eyebrowse-mode-map [remap evil-quit] 'zmanji/evil-quit)

  )

(use-package with-editor
  :config
  ;; Bind :cq to something reasonable
  (define-key with-editor-mode-map [remap evil-quit-all-with-error-code] 'with-editor-cancel)
  (define-key with-editor-mode-map [remap evil-quit] 'with-editor-finish)
)

(use-package magit
  :config
  (use-package evil-magit)

  (general-nmap
   :prefix ","
   "g" 'magit-status)
)

(use-package dired
  :config
  (use-package dired-x)
  (use-package dired+)
  ;; Use coreutils for dired
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
)

;; TODO(zmanji): Consider more key bindings with ivy/swiper/counsel.
;; TODO(zmanji): Consider adding custom ivy action that opens file in split.
;; TODO(zmanji): Read docs here: http://oremacs.com/swiper/
(use-package ivy
  :init
  (ivy-mode)
  :diminish ivy-mode
  :config
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'zmanji/minibuffer-keyboard-quit)

  (general-nmap
   :prefix ","
   "b" 'ivy-switch-buffer)
 )

(use-package counsel
  :init
  (counsel-mode)
  :diminish counsel-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-count-format "[%d/%d] ")
)

(use-package winner
  :init
  (winner-mode)
  :config
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "r" 'winner-redo)
  (setq winner-boring-buffers '("*Completions*" "*Help*"))
  )

(use-package deft
  :config
  (setq deft-extentions '("org"))
  (setq deft-default-extension "org")
  (setq deft-directory "~/deft/")
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  ;; Make sure deft starts in insert mode by default
  (add-to-list 'evil-insert-state-modes 'deft-mode)
  )

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (general-imap
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   )

  (general-imap
   :keymaps 'company-mode-map
   "TAB" 'company-indent-or-complete-common
   )

  ;; NOTE: Order of `company-backends' matters, the first backend is preferred.
  (setq company-backends
        '(company-semantic
          company-capf
          (company-files
           company-dabbrev-code
           company-dabbrev
           company-keywords
           company-ispell)))

  (define-key company-mode-map
    [remap indent-for-tab-command] 'company-indent-or-complete-common)
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  ;; NOTE: add future backends here on a mode by mode basis
  )

(use-package origami
  :init
  (global-origami-mode)
  )

(zmanji/setup-gui)
(zmanji/setup-modeline)
(zmanji/setup-editing)
(zmanji/disable-custom)

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

;; Readonly prompts
(setq comint-prompt-read-only t)

(setq-default tab-always-indent 'complete)

(setq ring-bell-function 'ignore)

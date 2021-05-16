(setq user-full-name "Zameer Manji")
(setq user-mail-address "zmanji@gmail.com")

;; Add my own code to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'zmanji-funcs)

;; TODO(zmanji): Consider flycheck
;; TODO(zmanji): Consider view-mode: https://www.emacswiki.org/emacs/ViewMode
;; http://pragmaticemacs.com/emacs/view-mode-makes-for-great-read-only-reading/
;; TODO(zmanji): Consider creating a `view' like binary.
;; TODO(zmanji): Consider setting up `ediff' and `ediff-evil' and magit
;; integration: http://oremacs.com/2015/01/17/setting-up-ediff/
;; https://github.com/justbur/evil-ediff


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
  (when (memq window-system '(mac ns x nil))
    (exec-path-from-shell-initialize)))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-auto-save-history t)
  (setq evil-undo-system 'undo-tree)
  (setq undo-tree-enable-undo-in-region nil)
)



;; TODO(zmanji): Consider making space the leader key and not ','
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  ;; Fixes some jank in the terminal.
  (define-key evil-insert-state-map "\e" 'evil-normal-state)

  (define-key evil-window-map "q" 'evil-window-delete)

  ;; Swap ; and :
  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-motion-state-map ":" 'evil-repeat-find-char)


  (zmanji/evil-terminal-cursor-change)

  (evil-ex-define-cmd "fullscreen" 'toggle-frame-fullscreen)
  (evil-ex-define-cmd "rm" 'zmanji/delete-file-and-buffer)

  (setq evil-vsplit-window-right t)

  (use-package evil-collection
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init)
    ; More consistent c-j / c-k in minibuffer
    (general-imap
      :keymaps 'evil-ex-completion-map
      "C-j" 'next-complete-history-element
      "C-k" 'previous-complete-history-element
     )

    (general-nmap
      :keymaps 'evil-ex-completion-map
      "C-j" 'next-history-element
      "C-k" 'previous-history-element
    )


    )


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

  ;; Evil friendly bindings for occur-mode
  (add-to-list 'evil-motion-state-modes 'occur-mode)
  (general-mmap
   :keymaps 'occur-mode-map
   "RET" 'occur-mode-goto-occurrence
   )

  ;; Make leader keyspace
  ;; Pattern from https://github.com/noctuid/evil-guide#leader-key
  (define-prefix-command 'zmanji/leader-map)
  ;; Unbind ',' in motion mode so it can be a leader key
  (define-key evil-motion-state-map "," 'zmanji/leader-map)
  (define-key zmanji/leader-map "j" 'org-journal-new-entry)
  (define-key zmanji/leader-map "f" 'counsel-projectile-find-file)
  (define-key zmanji/leader-map "g" 'magit-status)
  (define-key zmanji/leader-map "b" 'ivy-switch-buffer)
  (define-key zmanji/leader-map "u" 'undo-tree-visualize)
  (define-key zmanji/leader-map "p" 'counsel-projectile-switch-project)

)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-show-operator-state-maps t))

(use-package base16-theme
  :init
  (setq base16-theme-256-color-source "base16-shell")

  ;; Need to load theme to get colors defined
  (load-theme 'base16-tomorrow-night t)

  ;; Set the cursor color based on the evil state
  (setq evil-emacs-state-cursor   `(,(plist-get base16-tomorrow-night-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get base16-tomorrow-night-colors :base0D) bar)
        evil-motion-state-cursor  `(,(plist-get base16-tomorrow-night-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get base16-tomorrow-night-colors :base0B) box)
        evil-replace-state-cursor `(,(plist-get base16-tomorrow-night-colors :base08) bar)
        evil-visual-state-cursor  `(,(plist-get base16-tomorrow-night-colors :base09) box))


  ;; the package doesn't style these faces so we do it our selves
  (base16-set-faces
   'base16-tomorrow-night
   base16-tomorrow-night-colors
   `(
     (tab-bar :background base01 :foreground base01)
     (tab-bar-tab  :background base00 :foreground base06)
     (tab-bar-tab-inactive :background base01 :foreground base05)
     (tab-line  :background base0D)
     )
   )

  ;; Need to re-enable theme to get this to stick.
  (enable-theme 'base16-tomorrow-night)

  )

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 1)
  (smooth-scrolling-mode))

(use-package pbcopy)

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-after-kill-buffer-p t)
  ;; Ignore special buffers
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

(use-package paren
  :config
  ;; Show matching parens like vim
  (show-paren-mode 1)
  (setq show-paren-delay 0))

;; TODO(zmanji): Investigate `org-agenda', `org-habit'
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
  :config
  (setq org-log-done 'time)
  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    :config
    (setq org-bullets-bullet-list '("◉" "○")))
  ;; Simple journal integrated with org-mode, vimwiki style
  ;; https://github.com/bastibe/org-journal
  (use-package org-journal
    :config
    ;; Need to use use `custom-set-variables' here.
    ;; See http://emacs.stackexchange.com/a/134/8591
    ;; Changing these custom variables should trigger some lambdas.
    (custom-set-variables
     '(org-journal-dir "~/journal")
     '(org-journal-file-format "%Y-%m-%d.org")
     '(org-journal-date-format "%A, %d-%m-%Y")
     '(org-journal-hide-entries-p nil)
     ;; TODO(zmanji): Consider setting `org-journal-date-prefix' to be `#+TITLE'
     ;; so the files have titles.
     )

    ;; To ensure that evil folding works correctly, we need to add
    ;; org-journal-mode to the list of fold entries. We just call whatever
    ;; the action is for org-mode
      (add-to-list 'evil-fold-list
                   `((org-journal-mode)
                     :open-all   ,(lambda () (evil-fold-action (org-mode) :open-all))
                     :close-all  ,(lambda () (evil-fold-action (org-mode) :close-all))
                     :toggle     ,(lambda () (evil-fold-action (org-mode) :toggle))
                     :open       ,(lambda () (evil-fold-action (org-mode) :open))
                     :open-rec   ,(lambda () (evil-fold-action (org-mode) :open-rec))
                     :close      ,(lambda () (evil-fold-action (org-mode) :close))))


    )
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
   )

  ;; Auto hard wrap text in org mode files.
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  )

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package whitespace
  :straight (:type built-in)
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
  :straight (:type built-in)
  :init
  (save-place-mode))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-require-project-root nil)
  (use-package counsel-projectile)
  ;; projectile-mode-hook doesn't run for every buffer
  ;; Emacs has no generic "open file buffer hook" so this is a weak emulation
  ;; If this doesn't work need to advice create-file-buffer like uniquify
  (add-hook 'prog-mode-hook 'zmanji/set-default-directory-projectile-root)
  (add-hook 'text-mode-hook 'zmanji/set-default-directory-projectile-root)


  (evil-ex-define-cmd "vsh" 'zmanji/vsplit-eshell)
)

;; XXX: See `ispell-help' for more information
;; See http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; NOTE: The dictionaries have to come from here: http://wordlist.aspell.net/dicts/
(use-package flyspell
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  ;; TODO(zmanji): Check that hunspell exists
  (setq ispell-program-name "/usr/local/bin/hunspell")
  (setq ispell-silently-savep t)

  (general-define-key
    :keymaps 'flyspell-mode-map
    :states '(normal)
    :prefix "]"
    "s" 'flyspell-goto-next-error
    )
  ;; TODO(zmanji): Add a binding `zg' to store misspelled word in
  ;; personal dictionary
  ;; See this for inspiration: https://stackoverflow.com/a/22116480/2874
)

(use-package browse-url
  :config
  ;; Open contents in system browser
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "open"))

;; Reload buffers which have been modified externally
(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode t)
)

(use-package hl-line
  :straight (:type built-in)
  :init
  (global-hl-line-mode)
  )

(use-package tab-bar
  :straight (:type built-in)
  :init
  (tab-bar-mode t)
  :config
  (setq tab-bar-show 1)
  (setq tab-bar-new-tab-choice "*scratch*")
  (evil-ex-define-cmd "tab" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabc[lose]" 'tab-bar-close-tab)
  (evil-ex-define-cmd "tabs" 'tab-switcher)

  (evil-define-command zmanji/evil-quit (&optional force)
  "Wrapper around evil-quit that attempts to close the current
  tab before calling the actual function."
  :repeat nil
  (interactive "<!>")
  (let* ((num-tabs (length (tab-bar-tabs)))
         (num-windows (count-windows))
         )
    (if (eq num-windows 1)
        (if (> num-tabs 1)
            (call-interactively 'tab-bar-close-tab)
          (evil-quit force)
          )
      (evil-quit force)

      )
    )
  )

  (evil-ex-define-cmd "q[uit]" 'zmanji/evil-quit)

  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)

  )

(use-package eshell
  :straight (:type built-in)
  :config
  (setq eshell-error-if-no-glob t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-scroll-to-bottom-on-input 'this)
  (setq eshell-list-files-after-cd t)

  ;; eshell-mode-map is reset every time the mode starts
  (add-hook 'eshell-first-time-mode-hook (lambda ()
    (general-nmap
    :keymaps 'eshell-mode-map
    "M-j" 'eshell-next-prompt
    "M-k" 'eshell-previous-prompt
    "j" 'eshell-next-input
    "k" 'eshell-previous-input
    "C-j" 'evil-next-line
    "C-k" 'evil-previous-line
    "^"   'eshell-bol
    "<return>" 'eshell-send-input
    )
    ))

  (add-hook 'eshell-mode-hook (lambda ()
    (setenv "PAGER" "cat")
  ))
)

(use-package with-editor
  :config
  ;; Bind :cq to something reasonable
  (define-key with-editor-mode-map [remap evil-quit-all-with-error-code] 'with-editor-cancel)
  (define-key with-editor-mode-map [remap evil-quit] 'with-editor-finish)
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  )

(use-package magit)

(use-package dired
  :straight (:type built-in)
  :config
  (use-package dired-x
    :straight (:type built-in)
  )
  ;; Use coreutils for dired
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (general-nmap
    :keymaps 'dired-mode-map
    ";" 'evil-ex)

)

;; TODO(zmanji): Consider more key bindings with ivy/swiper/counsel.
;; TODO(zmanji): Consider adding custom ivy action that opens file in split.
;; TODO(zmanji): Read docs here: http://oremacs.com/swiper/
(use-package ivy
  :init
  (ivy-mode)
  :diminish ivy-mode
  :config
  (general-imap
    :keymaps 'ivy-minibuffer-map
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
   )
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
  :straight (:type built-in)
  :init
  (winner-mode)
  :config
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "r" 'winner-redo)
  (setq winner-boring-buffers '("*Completions*"))
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
  (origami-mode 1)
  )

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc -f commonmark_x -t html5 -s --shift-heading-level-by=-1")
  (setq markdown-header-scaling t)

  (evil-declare-motion 'markdown-forward-paragraph)
  (evil-declare-motion 'markdown-backward-paragraph)


  (general-mmap
  :keymaps 'markdown-mode-map
    "gj" 'markdown-forward-same-level
    "gk" 'markdown-backward-same-level
    "gh" 'markdown-outline-up
    "gl" 'markdown-outline-next
    "M-j" 'markdown-move-down
    "M-k" 'markdown-move-up
    "M-h" 'markdown-promote
    "M-l" 'markdown-demote
    "}" 'markdown-forward-paragraph
    "{" 'markdown-backward-paragraph
    )

  (defun zmanji/format-markdown ()
    "pipes a document though pandoc for formatting"
    (interactive)
    (shell-command-on-region
     (point-min)
     (point-max)
     "pandoc -f commonmark_x -t  commonmark_x --reference-links --reference-location=section --columns=79 -s"
     (current-buffer)
     t
     )
    )

  )

(use-package edit-server
  :init
  (edit-server-start)
  :config
  ;; Bind :cq and :q to something reasonable
  (define-key edit-server-edit-mode-map [remap evil-quit-all-with-error-code] 'edit-server-abort)
  (define-key edit-server-edit-mode-map [remap evil-quit] 'edit-server-done)
  (define-key edit-server-edit-mode-map [remap evil-save] 'edit-server-save)
  (define-key edit-server-edit-mode-map [remap evil-write] 'edit-server-save)
  (define-key edit-server-edit-mode-map [remap evil-save-and-close] 'edit-server-done)
  (define-key edit-server-edit-mode-map [remap evil-save-modified-and-close] 'edit-server-done)
  )

(use-package comint
  :straight (:type built-in)
  :init
  (general-nmap
   :keymaps 'comint-mode-map
   "C-l" 'comint-clear-buffer
   "C-j" 'comint-next-input
   "C-k" 'comint-previous-input
   )
  (general-imap
   :keymaps 'comint-mode-map
   "C-l" 'comint-clear-buffer
   )
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

;; Prevent disaster with deletes
(setq delete-by-moving-to-trash t)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
(set-face-attribute 'default nil :family "DejaVu Sans Mono")
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")


;; Ensure 'q' in help mode does the same thing as C-w q
;; NOTE: This seems undesirable since help mode can also replace an existing
;; window
(general-nmap
  :keymaps 'help-mode-map
  "q" 'evil-window-delete
  )


;; Make compilation mode tolerable, not evil defaults it to motion mode
(unbind-key "g" compilation-mode-map)
(general-define-key
  :keymaps 'compilation-mode-map
  :states '(motion)
  "h" 'evil-backward-char
  "r" 'recompile
  )

;; When in the ZSH cli a command is edited with edit-command-line (aka v in
;; vicmd mode), the filename is /tmp/zshXXXXXX where the X is a random character
(add-to-list 'auto-mode-alist '("/tmp/zsh[[:alnum:]]\\{6\\}" . sh-mode))

;;; post-init.el --- This file is loaded after init.el. It is useful for additional configurations or package setups that depend on the configurations in init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; fork from https://github.com/jamescherti/minimal-emacs.d
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (setq compile-angel-verbose t)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded.
  (compile-angel-on-load-mode))

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))
(add-hook 'kill-emacs-hook #'recentf-cleanup)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

(setq auto-save-interval 300)
(setq auto-save-timeout 30)

(setq auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity
(auto-save-visited-mode 1)

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package vim-tab-bar
  :ensure t
  :commands vim-tab-bar-mode
  :init
  (defun my/setup-vim-tab-bar ()
    (vim-tab-bar-mode 1)
    (setq tab-bar-tab-hints t))
  :hook (after-init . my/setup-vim-tab-bar))

(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-buffers
             vdiff-buffers3
             vdiff-quit
             vdiff-files
             vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

;; The evil-surround package simplifies handling surrounding characters, such as parentheses, brackets, quotes, etc.
;; It provides key bindings to easily add, change, or delete these surrounding characters in pairs.
;; For instance, you can surround the currently selected text with double quotes in visual state using S" or gS":
(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

;; You can also add the following code to enable commenting and uncommenting by pressing gcc
;; in normal mode and gc in visual mode
;; (thanks you to the Reddit user u/mistakenuser for this contribution, which replaces the evil-commentary package):
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

;(use-package evil-snipe
;  :defer t
;  :commands evil-snipe-mode
;  :hook (after-init . evil-snipe-mode))

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;; Display of line numbers in the buffer:
(setq display-line-numbers-mode 1)

(use-package which-key
  :ensure nil ; builtin
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;; Display the time in the modeline
(display-time-mode 1)

;; Paren match highlighting
(show-paren-mode 1)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(winner-mode 1)

;; Replace selected text with typed text
(delete-selection-mode 1)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; httpsni//www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Dired buffers: Automatically hide file details (permissions, size,
;; modification date, etc.) and all the files in the `dired-omit-files' regular
;; expression for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-hlt --time-style=long-iso")

;; Hide files from dired
(setq dired-omit-files (concat "\\`[.]\\'"
                               "\\|\\(?:\\.js\\)?\\.meta\\'"
                               "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                               "\\|^\\.DS_Store\\'"
                               "\\|^\\.\\(?:svn\\|git\\)\\'"
                               "\\|^\\.ccls-cache\\'"
                               "\\|^__pycache__\\'"
                               "\\|^\\.project\\(?:ile\\)?\\'"
                               "\\|^flycheck_.*"
                               "\\|^flymake_.*"))
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; Enable on-the-fly spell checking (Flyspell mode).
(add-hook 'text-mode-hook #'flyspell-mode)

;; Configures Aspell's suggestion mode to "ultra", which provides more
;; aggressive and detailed suggestions for misspelled words. The language
;; is set to "en_US" for US English, which can be replaced with your desired
;; language code (e.g., "en_GB" for British English, "de_DE" for German).
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Enabled backups save your changes to a file intermittently
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)

(use-package org
  :ensure t
  :defer t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :config
  (add-to-list 'org-modules 'org-habit)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated nil)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t))

(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; My Customization
;(load-theme 'dracula t)

;; ;; Using doom monokai theme
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (nerd-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;; 
;;   ;; (load-theme 'doom-monokai-pro t))
;;   (load-theme 'doom-one t))

;;; modus theme
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs t)

  ;; Configure the Modus Themes' appearance
  ;; frome https://systemcrafters.net/emacs-from-scratch/the-modus-themes/
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold intense)
        modus-themes-completions 'opinionated
        modus-themes-org-blocks 'tinted-background
        modus-themes-scale-headings t
        Modus-themes-region '(bg-only)
        modus-themes-headings
        '((1 . (rainbow overline background 1.4))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1))))    
     
  ;; Apply more colorful foreground to some headings (headings 0-8).
  ;; Level 0 is for Org #+title and related.
  (setq modus-themes-common-palette-overrides
        '((fg-heading-1 blue-warmer)
          (bg-heading-1 bg-blue-nuanced)
          (overline-heading-1 blue)
          (fg-heading-2 yellow-cooler)
          (fg-heading-3 cyan-cooler)
          (prose-done fg-dim)))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; restart emacs command
(use-package restart-emacs
  :ensure t)


;; Load OS-specific configurations
(cond
 ((eq system-type 'windows-nt)
  (load (expand-file-name "windows-config.el" user-emacs-directory)))
 ((eq system-type 'darwin)
  (load (expand-file-name "macos-config.el" user-emacs-directory)))
 ((eq system-type 'gnu/linux)
  (load (expand-file-name "linux-config.el" user-emacs-directory))))

;; Save/restore window size and position
(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'frameset-data)

;; load org config file
(load (expand-file-name "org-config.el" user-emacs-directory))

;; org journal
(use-package org-journal
  :ensure t)

(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-type 'weekly)
(setq org-journal-file-format "%Y%mW%V%d.org")
(setq org-journal-date-format "%A, %Y/%m/%d")
(setq org-journal-time-format "%Y%m%dT%H%M")
(setq org-journal-enable-agenda-integration t)
(setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STRT\"|TODO=\"WAIT\"")

(defun insert-timestamp ()
  "Insert current timestamp in format YYYYMMDDTHHMM."
  (interactive)
  (insert (format-time-string "%Y%m%dT%H%M")))


;; denote
;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
  :ensure t)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Obsidian/Note/"))
(setq denote-save-buffers nil)
(setq denote-known-keywords '("emacs" "git" "software" "network" "ai" "economics"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type 'markdown-yaml) ; Org is the default, set others here
(setq denote-prompts '(title keywords))
(setq denote-excluded-directories-regexp nil)
(setq denote-excluded-keywords-regexp nil)
(setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

;; Read this manual for how to specify `denote-templates'.  We do not
;; include an example here to avoid potential confusion.

(setq denote-date-format nil) ; read doc string

;; By default, we do not show the context of links.  We just display
;; file names.  This provides a more informative view.
(setq denote-backlinks-show-context t)

;; Also see `denote-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
            (expand-file-name "~/Documents/books")))

;; Generic (great if you rename files Denote-style in lots of places):
(add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
(denote-rename-buffer-mode 1)

;; denote
(use-package zoxide
  :ensure t)

(defun dired-jump-with-zoxide (&optional other-window)
   (interactive "P")
   (zoxide-open-with nil (lambda (file) (dired-jump other-window file)) t))

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (gfm-mode gfm-view-mode markdown-mode markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

;; Author: James Cherti
;; URL: https://www.jamescherti.com/emacs-markdown-table-of-contents-update-before-save/
;; License: MIT

;; Configure the markdown-toc package
(use-package markdown-toc
  :ensure t
  :defer t
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-delete-toc
             markdown-toc--toc-already-present-p))

;; The following functions and hooks guarantee that any existing table of
;; contents remains current whenever changes are made to the markdown file,
;; while also ensuring that both the window start and cursor position remain
;; unchanged.
(defun my-markdown-toc-gen-if-present ()
    (when (markdown-toc--toc-already-present-p)
      (let* ((window (selected-window))
             (buffer-in-selected-window (eq (window-buffer window)
                                            (current-buffer)))
             (window-hscroll nil)
             (lines-before nil))
        (when buffer-in-selected-window
          (setq window-hscroll (window-hscroll))
          (setq lines-before (count-screen-lines
                              (save-excursion (goto-char (window-start))
                                              (vertical-motion 0)
                                              (point))
                              (save-excursion (vertical-motion 0)
                                              (point))
                              nil
                              window)))
        (unwind-protect
            (markdown-toc-generate-toc)
          (when buffer-in-selected-window
            (set-window-start window
                              (save-excursion
                                (vertical-motion 0)
                                (line-move-visual (* -1 lines-before))
                                (vertical-motion 0)
                                (point)))
            (set-window-hscroll window window-hscroll))))))

  (defun my-setup-markdown-toc ()
    "Setup the markdown-toc package."
    (add-hook 'before-save-hook #'my-markdown-toc-gen-if-present -100 t))

  (add-hook 'markdown-mode-hook #'my-setup-markdown-toc)
  (add-hook 'markdown-ts-mode-hook #'my-setup-markdown-toc)

(use-package rg
  :ensure t)

(use-package vterm
   :ensure t
   :config
   (setq vterm-shell "/bin/zsh"))

(use-package multi-vterm
  :ensure t)

;; Bind M-1 through M-9 globally to switch tabs
;; Assumes built-in tab-bar-mode or tab-line-mode
(dotimes (i 9)
  (let ((key (format "M-%d" (1+ i))) ; Create key string like "M-1", "M-2", etc.
        (tab-number (1+ i)))        ; Tab index (1-based)
    (global-set-key (kbd key)
                    ;; Define a command to select the corresponding tab
                    `(lambda () (interactive) (tab-bar-select-tab ,tab-number)))))

;; Ensure the same bindings work within vterm buffers
;; This overrides vterm's default behavior for these keys
(with-eval-after-load 'vterm
  (dotimes (i 9)
    (let ((key (format "M-%d" (1+ i)))
          (tab-number (1+ i)))
      (define-key vterm-mode-map (kbd key)
                  `(lambda () (interactive) (tab-bar-select-tab ,tab-number))))))

(use-package magit
  :ensure t
  :config
  ;; Full screen magit-status
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

;; Enable hunk-level Ediff in Magit
(setq magit-ediff-dwim-show-on-hunks t)

;; Bind Super+v to paste (yank)
(global-set-key (kbd "s-v") 'yank)
;; Disable the space key in Dired so that it can be used as a leader key.
(evil-collection-define-key 'normal 'dired-mode-map " " 'nil)

;; Set leader key
(evil-set-leader 'motion (kbd "SPC"))

(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
      (apply orig-fn beg end type ?_ args))

;; Bind Alt+1 through Alt+9 to tab-bar-select-tab
(dotimes (i 9)
  (let ((n (number-to-string (1+ i))))
    (global-set-key (kbd (concat "M-" n))
                    `(lambda () (interactive) (tab-bar-select-tab ,(1+ i))))))

;; customize key
(evil-define-key nil 'global
    ;; <leader>
    (kbd "<leader> :")  '("M-x" . execute-extended-command)
    (kbd "<leader> .")  '("Fine file" . find-file)
    (kbd "<leader> ,")  '("Switch buffer" . switch-to-buffer)
    (kbd "<leader> ;")  '("Insert timestamp" . insert-timestamp)

    ;; <leader> q --- quit/session
    (kbd "<leader> qq")  '("Quit Emacs" . save-buffers-kill-terminal)
    (kbd "<leader> qr")  '("Restart Emacs" . restart-emacs)

    ;; comment
    (kbd "<leader> /")  '("Commentary" . my-evil-comment-or-uncomment)

    ;; buffer
    (kbd "<leader> bn") '("Next buffer" . evil-next-buffer)
    (kbd "<leader> bp") '("Prev buffer" . evil-prev-buffer)
    (kbd "<leader> bs") '("Save buffer" . basic-save-buffer)
    (kbd "<leader> ba") '("Save all buffers" . evil-write-all)
    (kbd "<leader> bk") '("Kill current buffer" . kill-current-buffer)
    (kbd "<leader> bro") '("Read only mode" . read-only-mode)
    (kbd "<leader> bm") '("View message buffer" . view-echo-area-messages)
    (kbd "<leader> be") '("Eval buffer" . eval-buffer)

    ;; window
    (kbd "<leader> w") '("Window" . evil-window-map)

    ;; <leader> o --- org
    (kbd "<leader> oa") '("Agenda" . org-agenda)
    (kbd "<leader> ol") '("Todo list" . org-todo-list)
    (kbd "<leader> om") '("Tags search" . org-tags-view)
    (kbd "<leader> ov") '("View search" . org-search-view)
    (kbd "<leader> ot") '("Todo change" . org-todo)
    (kbd "<leader> oc") '("Capture" . org-capture)
    (kbd "<leader> od") '("Insert deadline" . org-deadline)
    (kbd "<leader> os") '("Insert schedule" . org-schedule)
    (kbd "<leader> or") '("Refile" . org-refile)
    (kbd "<leader> oil") '("Inser link" . org-insert-link)
    (kbd "<leader> op") '("Change priority" . org-priority)

    ;; <leader> c --- clock
    (kbd "<leader> ct") '("Update time" . org-clock-update-time-maybe)
    (kbd "<leader> ci") '("Start clock" . org-clock-in)
    (kbd "<leader> co") '("Stop clock" . org-clock-out)
    (kbd "<leader> cpi") '("Punch in clock" . bh/punch-in)
    (kbd "<leader> cpo") '("Punch out clock" . bh/punch-out)
    (kbd "<leader> cg") '("Go to clock" . org-clock-goto)
    (kbd "<leader> clt") '("Clock in the interrupted task" . bh/clock-in-last-task)
    (kbd "<leader> cs") '("Switch task" . kk/org-clock-in-switch-task)

    ;; denote
    (kbd "<leader> dn") '("Creat a denote" . denote)
    (kbd "<leader> dr") '("Rename file" . denote-rename-file)

    ;; zoxide
    (kbd "<leader> zf") '("Find file under a path saved in zoxide" . zoxide-find-file)
    (kbd "<leader> zt") '("Travel to a path saved in zoxide" . zoxide-travel)
    (kbd "<leader> zc") '("Change working directory to a path" . zoxide-cd)

    ;; delete
    (kbd "<leader> dd") '("Kill line" . kill-line)

    ;; tab
    (kbd "<leader> tn") '("Create new tab" . tab-new)
    (kbd "<leader> tc") '("Close tab" . tab-close)

    ;; vterm
    (kbd "<leader> vt") '("Create new vterm" . multi-vterm)

    ;; git
    (kbd "<leader> gs") '("Show status" . magit-status)

    ;; org journal
    (kbd "<leader> jn") '("Creat a entry" . org-journal-new-entry)
    (kbd "<leader> jo") '("Open current journal" . org-journal-open-current-journal-file))

;; Configure hjkl for Org Agenda
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "h") 'evil-backward-char)    ; Move left
  (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)  ; Move down
  (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line) ; Move up
  (define-key org-agenda-mode-map (kbd "l") 'evil-forward-char))    ; Move right


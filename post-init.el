;;; post-init.el --- This file is loaded after init.el. It is useful for additional configurations or package setups that depend on the configurations in init.el -*- no-byte-compile: t; lexical-binding: t; -*-

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

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

(setq auto-save-interval 300)
(setq auto-save-timeout 30)

(setq auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity
(auto-save-visited-mode 1)

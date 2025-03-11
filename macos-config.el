;; macos-config.el - macOS specific settings

;; macOS specific key bindings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)

;; macOS specific font settings
(set-face-attribute 'default nil :font "MonoLisa Nerd Font-16")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq default-directory "~/org/")

;; Fix dired on macOS
(setq dired-use-ls-dired nil)

;; macOS specific paths
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(provide 'macos-config)


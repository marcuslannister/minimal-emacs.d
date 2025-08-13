;; macos-config.el - macOS specific settings

;; macOS specific key bindings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
;; (setq mac-control-modifier 'super)

; macOS specific font settings
(set-face-attribute 'default nil :font "MonoLisa Nerd Font-16")
;; (set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Mono-16")
;; (set-face-attribute 'default nil :font "CaskaydiaMono Nerd Font Mono-16")
;; (set-face-attribute 'default nil :font "IosevkaTerm Nerd Font Mono-16")

(add-hook 'vterm-mode-hook
  (lambda ()
    ;; (face-remap-add-relative 'default '(:family "IosevkaTerm Nerd Font Mono" :height 160))))
    ;; (face-remap-add-relative 'default '(:family "IosevkaTerm Nerd Font Mono" :height 180))))
    (face-remap-add-relative 'default '(:family "AporeticSansMono Nerd Font" :height 180))))
    ;; (face-remap-add-relative 'default '(:font "IosevkaTerm Nerd Font Mono-16"))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/refile.org")

(setq default-directory "~/org/")

;; Fix dired on macOS
(setq dired-use-ls-dired nil)

;; macOS specific paths
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(provide 'macos-config)


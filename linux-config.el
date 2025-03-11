;; linux-config.el - Linux specific settings

;; Linux specific font settings
(set-face-attribute 'default nil :font "DejaVu Sans Mono-12")

;; Linux specific browser settings
(setq browse-url-browser-function 'browse-url-xdg-open)

;; X clipboard integration
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Check for WSL (Windows Subsystem for Linux)
(when (string-match "Microsoft" (shell-command-to-string "uname -r"))
  ;; WSL specific settings
  (setq browse-url-browser-function 'browse-url-default-windows-browser)
  ;; Use Windows path for certain operations
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (setq browse-url-generic-program cmd-exe
          browse-url-generic-args cmd-args)))

;; Other Linux specific settings...

(provide 'linux-config)


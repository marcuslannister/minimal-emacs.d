;; windows-config.el - Windows specific settings

;; Windows specific font settings
(set-face-attribute 'default nil :font "Consolas-12")

;; Windows paths with forward slashes
(setq default-directory "c:/Users/YourUsername/Documents/")

;; Default browser on Windows
(setq browse-url-browser-function 'browse-url-default-windows-browser)

;; Fix performance issues on Windows
(setq w32-get-true-file-attributes nil)
(setq inhibit-compacting-font-caches t)

;; Set cursor color
(set-face-attribute 'cursor nil :background "#d00000")

;; Other Windows specific settings...

(provide 'windows-config)

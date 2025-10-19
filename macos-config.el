;; macos-config.el - macOS specific settings

;; macOS specific key bindings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
;; (setq mac-control-modifier 'super)

; macOS specific font settings
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (and (eq system-type 'darwin) (display-graphic-p))
  (cl-loop for font in '("MonoLisa Nerd Font" "Cascadia Code" "SF Mono" "Source Code Pro"
                         "Fira Code" "Menlo" "Monaco" "Dejavu Sans Mono"
                         "Lucida Console" "Consolas" "SAS Monospace")
           when (font-installed-p font)
           return (set-face-attribute
                   'default nil
                   :font (font-spec :family font
                                    :weight 'normal
                                    :slant 'normal
                                    :size 16.0)))
  (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Apple Color Emoji"
                         "Segoe UI Emoji" "EmojiOne Color" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode
                                    (font-spec :family font
                                               :size 18.0)
                                    nil 'prepend))
  (cl-loop for font in '("MapleMono NF CN" "思源黑体 CN" "思源宋体 CN" "Source Han Sans CN" "Source Han Serif CN"
                         "微软雅黑 CN" "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                         "Microsoft Yahei UI" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size 18.0)))
  (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
           when (font-installed-p font)
           return (set-fontset-font t '(#x20000 . #x2A6DF)
                                    (font-spec :name font
                                               :weight 'normal
                                               :slant 'normal
                                               :size 18.0))))

;; Fallback to original settings if above doesn't work
;; (set-face-attribute 'default nil :font "IosevkaTerm Nerd Font Mono-16")

;; (add-hook 'vterm-mode-hook
;;   (lambda ()
;;     ;; (face-remap-add-relative 'default '(:family "IosevkaTerm Nerd Font Mono" :height 180))))
;;     (face-remap-add-relative 'default '(:family "AporeticSansMono Nerd Font" :height 180))))
;;     ;; (face-remap-add-relative 'default '(:font "IosevkaTerm Nerd Font Mono-16"))))

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ;; vterm configuration for macOS
;; (use-package vterm
;;   :ensure t
;;   :config
;;   (setq vterm-shell "/bin/zsh"))
;;
;; (use-package multi-vterm
;;   :ensure t)

;; ;; Ensure the same bindings work within vterm buffers
;; ;; This overrides vterm's default behavior for these keys
;; (with-eval-after-load 'vterm
;;   (dotimes (i 9)
;;     (let ((key (format "M-%d" (1+ i)))
;;           (tab-number (1+ i)))
;;       (define-key vterm-mode-map (kbd key)
;;                   `(lambda () (interactive) (tab-bar-select-tab ,tab-number))))))

(provide 'macos-config)

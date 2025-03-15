;; org-config.el - Org Mode settings

;; Show one week list in org agenda view
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-files
    (seq-filter (lambda(x) (not (string-match "/.stversions/"(file-name-directory x))))
     (directory-files-recursively "~/org/" "\\.org$")
     ))
(setq org-agenda-clockreport-parameter-plist
    (quote (:maxlevel 5 :fileskip0 t :compact t :narrow 80 :formula % )))

(run-at-time "5 min" (* 5 60) #'org-agenda-redo)

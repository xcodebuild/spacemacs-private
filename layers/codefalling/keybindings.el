(define-key global-map (kbd "<f1>") 'codefalling/hotspots)

(define-key global-map (kbd "M-\\") 'org-ctrl-c-ret)

;;(define-key evil-leader--default-map "oy" 'youdao-dictionary-search-at-point+)
(evil-leader/set-key "SPC" 'avy-goto-char)

(define-key global-map (kbd "M-c") 'copy-region-as-kill)
(define-key global-map (kbd "M-v") 'evil-paste-after)

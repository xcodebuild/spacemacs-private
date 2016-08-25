(setq css-indent-offset 2)

(with-eval-after-load "web-mode"
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(with-eval-after-load "evil-matchit-mode"
  (global-evil-matchit-mode))

(with-eval-after-load ""AA)

(add-hook 'after-init-hook
          (lambda () (moe-theme-set-color 'w/b)))

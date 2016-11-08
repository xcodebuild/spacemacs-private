(setq css-indent-offset 2)

(with-eval-after-load "web-mode"
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(with-eval-after-load "evil-matchit-mode"
  (global-evil-matchit-mode))


;; (defun codefalling//reset-eslint-rc ()
;;   (interactive)
;;   (let ((rc-path (concat (projectile-project-root) ".eslintrc")))
;;     (if (file-exists-p rc-path)
;;         (setq flycheck-eslintrc rc-path))))


(defun codefalling//init ()
  ;; (moe-theme-set-color 'w/b)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'prog-mode-hook 'editorconfig-mode)
  (add-hook 'prog-mode-hook 'editorconfig-apply)
  )

(add-hook 'after-init-hook 'codefalling//init)

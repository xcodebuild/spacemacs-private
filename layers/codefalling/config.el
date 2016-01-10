;;; Markdown mode
(when (configuration-layer/layer-usedp 'markdown)
  (setq auto-mode-alist (cons '("\\.text$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.md$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.mdown$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.mdt$" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.markdown$" . gfm-mode) auto-mode-alist)))

(add-to-list 'auto-mode-alist '("\\inbox.txt\\'" . org-mode))

(setq erc-nick "codefalling")
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs-cn")))


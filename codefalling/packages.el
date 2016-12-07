;;; packages.el --- codefalling layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: codefalling <codefalling@mbp-wifi.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `codefalling-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `codefalling/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `codefalling/pre-init-PACKAGE' and/or
;;   `codefalling/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst codefalling-packages
  '(
    dtrt-indent
    parinfer)
    
  "The list of Lisp packages required by the codefalling layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun codefalling/init-dtrt-indent ()
  (use-package dtrt-indent
    :defer 1
    :init
    (dtrt-indent-mode)))
    

(defun codefalling/init-parinfer ()
  (use-package parinfer
    :defer t
    :diminish parinfer-mode
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
      (add-hook 'clojure-mode-hook 'parinfer-mode)
      (add-hook 'common-lisp-mode-hook 'parinfer-mode)
      (add-hook 'scheme-mode-hook 'parinfer-mode)
      (add-hook 'lisp-mode-hook 'parinfer-mode)

      (spacemacs|add-toggle parinfer-indent
        :evil-leader "tP"
        :documentation "Enable Parinfer Indent Mode."
        :if (bound-and-true-p parinfer-mode)
        :status (eq parinfer--mode 'indent)
        :on (parinfer-toggle-mode)
        :off (parinfer-toggle-mode))
      (setq parinfer-extensions '(defaults pretty-parens evil smart-yank)))))
;;; packages.el ends here

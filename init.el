;;; package -- init

;;; Commentary:
;;; init.el --- Custom Emacs Configuration
;;; Author: Alamin Mahamud
;;; Version: 3.0.0
;;; Keywords: Configuration Emacs
;;; URL: https://github.com/AlaminMahamud/.emacs.d/init.el

;;; Code:
;;;
(if
    (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(if
    (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if
    (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq inhibit-startup-message t)

(package-initialize)

(require 'ob-tangle)
(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))
;;; init.el ends here

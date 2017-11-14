;;; init.el --- Custom Emacs Configuration

;; Author: Md. Alamin Mahamud [AlaminMahamud]
;; Version: 1.0.0
;; Keywords: configuration emacs
;; URL: https://github.com/AlaminMahamud/.emacs.d/init.el
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(package-initialize)

(require 'org)
(require 'ob-tangle)

(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(org-babel-load-file (expand-file-name "alamin.org" init-dir))

;;; init.el ends here

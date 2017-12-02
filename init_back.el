;;; init.el --- Custom Emacs Configuration

;; Author: Md. Alamin Mahamud [AlaminMahamud]
;; Version: 2.0.0
;; Keywords: configuration emacs
;; URL: https://github.com/AlaminMahamud/.emacs.d/init.el
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;;; init.el ends here

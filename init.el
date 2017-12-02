;;; init.el --- Custom Emacs Configuration
;;; Author: Md. Alamin Mahamud
;;; Version: 3.0.0
;;; Keywords: Configuration Emacs
;;; URL: https://github.com/AlaminMahamud/.emacs.d/init.el


;;; Commentary
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'ob-tangle)
(org-babel-load-file(
		      expand-file-name
		      "~/.emacs.d/README.org"))

;;; init.el ends here

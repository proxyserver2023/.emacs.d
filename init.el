;;; init.el --- Custom Emacs Configuration
;;; Author: Md. Alamin Mahamud
;;; Version: 3.0.0
;;; Keywords: Configuration Emacs
;;; URL: https://github.com/AlaminMahamud/.emacs.d/init.el


;;; Commentary
;;; Code:

(require 'ob-tangle)
(org-babel-load-file(
		      expand-file-name
		      "~/.emacs.d/README.org"))

;;; init.el ends here

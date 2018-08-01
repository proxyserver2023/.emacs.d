;;; package --- init

;;; Commentary:
;;; init.el --- Custom Emacs Configuration
;;; Author   :  Md Alamin Mahamud
;;; Version  :  4.0.0
;;; Keywords :  Utility | Lots of customization
;;; URL      :  https://github.com/AlaminMahamud/.emacs.d/blob/master/init.el


;; Directory Structure:
;;
;; ~/.emacs.d/                   user directory
;; ~/.emacs.d/init.el            init file
;; ~/.emacs.d/README.org         config in literate style
;; ~/.emacs.d/users              user specific settings
;; ~/.emacs.d/custom.el          customized variables and faces
;; ~/.emacs.d/snippets           yasnippets backup files
;; ~/.emacs.d/themes             custom themes
;; ~/.emacs.d/backups            backups


;;; Code:
;;;

(package-initialize)

(require 'ob-tangle)
(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))

;;; init.el ends here

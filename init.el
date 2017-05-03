;;; My Custom Config
;;; thanks to Mike Zamanasky
;;; Date: 03-05-2017






;;; ignore splash screen
(setq inhibit-startup-message t)

;;; telling emacs to require packages - why?
(require 'package)
;;; why?
(setq package-enable-at-startup nil)
;;; adding melpa
(add-to-list 'package-archives
	     '("melpa". "https://melpa.org/packages/"))

(package-initialize)

;;; use-package is helpful to install package with ease
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; try
(use-package try
  :ensure t)
;;; which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

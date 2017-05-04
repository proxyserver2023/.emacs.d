;;; My Custom Config
;;; thanks to Mike Zamanasky
;;; Date: 03-05-2017

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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

;; ido-mode
;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; ibuffer
;;
(defalias 'list-buffers 'ibuffer-other-window)


;; ace-window
;;
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;; it looks like counsel is a requirement for swiper
;;
(use-package counsel
  :ensure t
)

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind(("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :bind(("C-s" . swiper)
	("C-r" . swiper)
	("C-c C-r" . ivy-resume)
	("M-x" . counsel-M-x)
	("C-x C-f" . counsel-find-file))
  :config
  (progn

   (ivy-mode 1)
   (setq ivy-use-virtual-buffers t)
   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;; Avy
;;
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char))
  :config
)


;;; ---------------------------------------------------
;;; Org Mode Stuff
;;; ---------------------------------------------------
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'setup-local)

;; org mode stuff
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-log-done 'time)

;; Ace window for easy window switching
(use-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-scope 'frame)
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
    ))

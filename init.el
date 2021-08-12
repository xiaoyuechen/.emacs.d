(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq user-full-name "Xiaoyue Chen")
(setq user-mail-address "xiaoyue.chen.0484@student.uu.se")

(let ((smart-theme "~/.emacs.d/hacking/smart-theme.el"))
  (load smart-theme))

(let ((fringe-bitmaps-hi-res "~/.emacs.d/hacking/fringe-bitmaps-hi-res.el"))
  (load fringe-bitmaps-hi-res))

(setq magit-section-visibility-indicator
      '(fringe-bitmap-plus-hi-res . fringe-bitmap-minus-hi-res))
(defun my-magit-mode-hook ()
  (setq left-fringe-width 32))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

(setq backup-by-copying t)

(which-key-mode)
(global-auto-revert-mode)
(global-visual-line-mode)
(delete-selection-mode)
(setq show-paren-delay 0)
(show-paren-mode)
(electric-pair-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-+") 'er/expand-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

(setq projectile-project-search-path '("~/repos"))
(projectile-mode)
(define-key projectile-mode-map (kbd "s-j") 'projectile-command-map)

(setq vterm-buffer-name-string "vterm %s")

(setq company-minimum-prefix-length 2
      company-idle-delay 0.0)

(setq lsp-idle-delay 0.1
      lsp-headerline-breadcrumb-enable nil)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-text-mode-hook ()
  (flyspell-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-prog-mode-hook ()
  (flyspell-prog-mode)
  (flycheck-mode)
  (company-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-common-hook ()
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline 1)
  (lsp))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-cmake-mode-hook ()
  (lsp))
(add-hook 'cmake-mode-hook 'my-cmake-mode-hook)

(pdf-loader-install)
(add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(setq TeX-electric-math '("\\(" . "\\)"))
(setq TeX-electric-sub-and-superscript t)
(add-hook 'TeX-after-compilation-finished-functions
	  'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook 'prettify-symbols-mode)
(setq LaTeX-electric-left-right-brace t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; hacking packages
(add-to-list 'load-path "~/.emacs.d/hacking")
(load "smart-theme")
(load "hdfb")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq user-full-name "Xiaoyue Chen")
(setq user-mail-address "xiaoyue.chen.0484@student.uu.se")

(fringe-mode 16)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq x-underline-at-descent-line t)
(setq backup-by-copying t)
(global-auto-revert-mode)
(global-visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete-selection-mode)
(setq show-paren-delay 0)
(show-paren-mode)
(electric-pair-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(setq gdb-many-windows t)
(which-key-mode)
(global-set-key (kbd "C-+") 'er/expand-region)

;; flymake
(setq flymake-error-bitmap '(hdfb-double-exclamation-mark compilation-error))
(setq flymake-warning-bitmap '(hdfb-exclamation-mark compilation-warning))
(setq flymake-note-bitmap '(hdfb-exclamation-mark compilation-info))

;; magit
(setq magit-section-visibility-indicator
      '(hdfb-plus . hdfb-minus))
(defun my-magit-mode-hook ()
  (setq left-fringe-width 32))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; projectile
(setq projectile-project-search-path '("~/repos"))
(projectile-mode)
(define-key projectile-mode-map (kbd "s-j") 'projectile-command-map)

;; vterm
(setq vterm-buffer-name-string "vterm %s")

;; company
(setq company-minimum-prefix-length 2
      company-idle-delay 0.0)

(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; eglot
(defun my-eglot-managed-mode-hook ()
  (yas-minor-mode))
(add-hook 'eglot-managed-mode-hook 'my-eglot-managed-mode-hook)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
  (add-to-list 'eglot-server-programs '((c-mode c++mode) . "clangd"))
  (add-to-list 'eglot-server-programs '(java-mode . "jdtls"))
  (add-to-list 'eglot-server-programs '(csharp-mode . ("omnisharp" "-lsp"))))

;; compilation buffer
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-text-mode-hook ()
  (flyspell-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-prog-mode-hook ()
  (flymake-mode)
  (flyspell-prog-mode)
  (company-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-common-hook ()
  (c-toggle-electric-state 1)
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-hook ()
  (eglot-ensure))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (eglot-ensure))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-java-mode-hook ()
  (eglot-ensure))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-csharp-mode-hook ()
  (eglot-ensure))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(defun my-python-mode-hook ()
  (eglot-ensure))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))

(pdf-loader-install)
(defun my-pdf-view-mode-hook ()
  (pdf-view-themed-minor-mode))
(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)

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

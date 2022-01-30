(setq user-full-name "Xiaoyue Chen")
(setq user-mail-address "xiaoyue.chen.0484@student.uu.se")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; hacking packages
(add-to-list 'load-path "~/.emacs.d/hacks")
(load "hdfb")
(load "liceheader")

;; global key bindings
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c o") 'find-file-at-point)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "C-c V") 'view-mode)
(global-set-key (kbd "C-c L") 'lice)
(global-set-key (kbd "C-c R") 'rename-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C") 'counsel-compile)
(global-set-key (kbd "C-c t") 'vterm)

;; mouse wheel
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; modes
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode)
(fringe-mode '(16 . 0))
(setq-default fill-column 80)
(setq x-underline-at-descent-line t)
(setq sentence-end-double-space nil)
(auto-insert-mode)
(global-auto-revert-mode)
(global-visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete-selection-mode)
(setq show-paren-delay 0)
(show-paren-mode)
(electric-pair-mode)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(setq gdb-many-windows t)
(which-key-mode)
(setq imenu-auto-rescan t)
(setq recentf-max-saved-items 200)
(recentf-mode)
(setq-default indent-tabs-mode nil)
(setq enable-recursive-minibuffers t)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(counsel-mode)

;; bash completion
(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)

;; tramp
(with-eval-after-load 'tramp
  (add-to-list 'tramp-connection-properties
               (list nil "remote-shell" "/bin/bash"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; flyspell
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

;; flymake
(setq flymake-error-bitmap '(hdfb-double-exclamation-mark compilation-error))
(setq flymake-warning-bitmap '(hdfb-exclamation-mark compilation-warning))
(setq flymake-note-bitmap '(hdfb-exclamation-mark compilation-info))
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c e n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c e p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c e l") 'flymake-show-diagnostics-buffer))

;; vterm
(setq vterm-buffer-name-string "*Vterm %s*")
(setq vterm-max-scrollback 10000)
(defun vterm-shell-command (cmd)
  (vterm)
  (vterm-send-string (concat "exec /bin/sh -c " cmd))
  (vterm-send-return))

;; dired
(defun xdg-open-dired ()
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c o") 'xdg-open-dired))

;; eldoc
(eldoc-add-command 'c-electric-paren)

;; magit
(setq magit-section-visibility-indicator
      '(hdfb-plus . hdfb-minus))
(defun my-magit-mode-hook ()
  (setq left-fringe-width 32))
(add-hook 'magit-mode-hook 'my-magit-mode-hook)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; company
(setq company-minimum-prefix-length 3
      company-idle-delay 0.0)

;; yasnippet
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; eglot
(setq eglot-confirm-server-initiated-edits nil)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (add-to-list
   'eglot-server-programs
   '((c-mode c++mode) .
     ("clangd" "-background-index" "-clang-tidy" "-completion-style=detailed"
      "-cross-file-rename" "-header-insertion=iwyu"
      "-header-insertion-decorators")))
  (add-to-list 'eglot-server-programs '(java-mode . my-jdtls-contact))
  (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server"))))

(defun my-jdtls-contact (interactive)
  (let ((workspace (expand-file-name (md5 (project-root (project-current))) "/tmp")))
    (list "jdtls" "-data" workspace)))

;; compilation buffer
;; (defun colorize-compilation-buffer ()
;;   (let ((inhibit-read-only t))
;;     (ansi-color-apply-on-region (point-min) (point-max))))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-emacs-lisp-mode-hook ()
  (flymake-mode))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(defun my-text-mode-hook ()
  (flyspell-mode)
  (auto-fill-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-prog-mode-hook ()
  (flyspell-prog-mode)
  (company-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-c-mode-common-hook ()
  (c-toggle-electric-state 1)
  (local-set-key (kbd "C-c O") 'ff-find-other-file)
  (require 'disaster))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-java-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure))
(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-python-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun my-haskell-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))
(defun my-minizinc-mode-hook ()
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1))
(add-hook 'minizinc-mode-hook 'my-minizinc-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

(add-to-list 'auto-mode-alist '("makefile" . makefile-gmake-mode))

(pdf-loader-install)

;; disaster
(with-eval-after-load 'disaster
  (define-key c-mode-base-map (kbd "C-c d") 'disaster))

;; LaTeX
(defun my-TeX-command-run-all ()
  (interactive)
  (save-buffer)
  (TeX-command-run-all nil))
(with-eval-after-load 'tex
  (define-key TeX-mode-map (kbd "C-c c") 'my-TeX-command-run-all)
  (add-to-list 'TeX-view-program-selection '(output-pdf "xdg-open")))
(setq-default TeX-master nil)
(setq-default TeX-engine 'luatex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-source-correlate-start-server t)
(setq TeX-electric-math '("\\(" . "\\)"))
(setq TeX-electric-sub-and-superscript t)
(setq LaTeX-electric-left-right-brace t)
(setq reftex-plug-into-AUCTeX t)
(defun my-LaTeX-mode-hook ()
  (yas-minor-mode)
  (eglot-ensure)
  (company-mode)
  (turn-on-reftex)
  (turn-on-auto-fill)
  (LaTeX-math-mode)
  (setq prettify-symbols-unprettify-at-point t)
  (prettify-symbols-mode)
  (TeX-fold-mode)
  (TeX-source-correlate-mode))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)
(add-hook 'TeX-after-compilation-finished-functions
	  'TeX-revert-document-buffer)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; hacking packages
(add-to-list 'load-path "~/.emacs.d/hacks")
;; (load "smart-theme")
(load "hdfb")
(load "liceheader")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq user-full-name "Xiaoyue Chen")
(setq user-mail-address "xiaoyue.chen@it.uu.se")

(global-set-key (kbd "C-c v") 'view-mode)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "C-c l") 'lice)
(global-set-key (kbd "C-c R") 'rename-buffer)

;; (desktop-save-mode 1)
(fringe-mode 16)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq process-connection-type nil)
(setq x-underline-at-descent-line t)
(setq sentence-end-double-space nil)
(auto-insert-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete-selection-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(setq gdb-many-windows t)
(which-key-mode 1)
(setq imenu-auto-rescan t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; tramp
(with-eval-after-load 'tramp
  (add-to-list 'tramp-connection-properties
               (list nil "remote-shell" "/bin/bash"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-default-proxies-alist
	       '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
	       '((system-name) "\\`root\\'" nil)))

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
  (define-key flymake-mode-map (kbd "C-c e l") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

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
(setq company-minimum-prefix-length 2
      company-idle-delay 0.0)

;; yasnippet
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; eglot
(setq eglot-confirm-server-initiated-edits nil)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c t") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (setenv "CLASSPATH"
	  (concat
	   (expand-file-name
	    (car
	     (file-expand-wildcards
	      "~/.emacs.d/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_*.jar")))
	   ":"
	   (getenv "CLASSPATH")))
  (add-to-list
   'eglot-server-programs
   '((c-mode c++mode) .
     ("clangd" "-background-index" "-clang-tidy" "-completion-style=detailed"
      "-cross-file-rename" "-header-insertion=iwyu"
      "-header-insertion-decorators")))
  (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server"))))

;; compilation buffer
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-emacs-lisp-mode-hook ()
  (flymake-mode))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(defun my-text-mode-hook ()
  (flyspell-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

(defun my-prog-mode-hook ()
  (flyspell-prog-mode)
  (company-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(define-key prog-mode-map (kbd "C-c c") 'compile)

(defun my-c-mode-common-hook ()
  (c-toggle-electric-state 1)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
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

(add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))
(defun my-minizinc-mode-hook ()
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1))
(add-hook 'minizinc-mode-hook 'my-minizinc-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

(pdf-loader-install)
(defun my-pdf-view-mode-hook ()
  (pdf-view-themed-minor-mode))
(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)

;; disaster
(with-eval-after-load 'disaster
  (define-key c-mode-base-map (kbd "C-c d") 'disaster))

;; LaTeX
(defun my-TeX-command-run-all ()
  (interactive)
  (save-buffer)
  (TeX-command-run-all nil))
(with-eval-after-load 'tex
  (define-key TeX-mode-map (kbd "C-c c") 'my-TeX-command-run-all))
(setq-default TeX-master nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
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

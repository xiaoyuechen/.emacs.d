(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; hacking packages
(add-to-list 'load-path "~/.emacs.d/hacks")
;; (load "smart-theme")
(load "hdfb")
(load "liceheader")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq user-full-name "Xiaoyue Chen")
(setq user-mail-address "xiaoyue.chen.0484@student.uu.se")

(set-face-attribute 'default nil :height 160)
(global-set-key (kbd "C-c v") 'view-mode)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c m") 'man)
(global-set-key (kbd "C-c l") 'lice)
(global-set-key (kbd "C-c R") 'rename-buffer)

(setq display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
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
(ido-mode 1)

;; Chinese input
(setq default-input-method "pyim")
(with-eval-after-load 'pyim
  (pyim-default-scheme 'quanpin)
  (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-length 5)
  (require 'pyim-basedict)
  (pyim-basedict-enable))

;; EXWM
(require 'exwm)

(require 'exwm-config)
(exwm-config-ido)

(setq exwm-workspace-number 4)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-input-global-keys
      `(
        ([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))))
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ([?\C-s] . [?\C-f])))

(exwm-enable)

(require 'exwm-randr)
(defun my-exwm-randr-screen-change-hook ()
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--primary" "--auto"
         "--output" default-output "--off")
        (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))
(add-hook 'exwm-randr-screen-change-hook 'my-exwm-randr-screen-change-hook)
(exwm-randr-enable)

;; using xim input
(require 'exwm-xim)
(exwm-xim-enable)
(push ?\C-\\ exwm-input-prefix-keys)

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
  (define-key TeX-mode-map (kbd "C-c c") 'my-TeX-command-run-all))
(setq-default TeX-master nil)
(setq-default TeX-engine 'luatex)
;; Use pdf-tools to open PDF files
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
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

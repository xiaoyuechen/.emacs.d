(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'dbus)
(defun theme-switcher-dbus-method-handler (theme-name)
  (let ((theme (intern theme-name)))
    (enable-theme theme)))

(dbus-register-method
 :session dbus-service-emacs
 (concat dbus-path-emacs "/ThemeSwitcher")
 (concat dbus-interface-emacs ".ThemeSwitcher")
 "SetTheme"
 #'theme-switcher-dbus-method-handler)

(setq backup-by-copying t)

(add-hook 'after-init-hook 'global-visual-line-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'after-init-hook 'delete-selection-mode)
(global-set-key (kbd "C-+") 'er/expand-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

(add-hook 'after-init-hook 'which-key-mode)

(setq show-paren-delay 0)
(add-hook 'after-init-hook 'show-paren-mode)

(add-hook 'after-init-hook 'electric-pair-mode)

(add-hook 'after-init-hook 'yas-global-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq projectile-project-search-path '("~/repos"))
(add-hook 'after-init-hook 'projectile-mode)
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "s-z") 'projectile-command-map))

(setq vterm-buffer-name-string "*vterm %s*")

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

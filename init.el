;;; init.el --- My personal Emacs init file          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Xiaoyue Chen

;; Author: Xiaoyue Chen <xiaoyue.chen@it.uu.se>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq user-full-name "Xiaoyue Chen"
      user-mail-address "xiaoyue.chen@it.uu.se")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq use-package-hook-name-suffix nil)
(setq x-underline-at-descent-line t)
(setq enable-recursive-minibuffers t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq delete-by-moving-to-trash t)
(setq async-shell-command-buffer 'new-buffer)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package gnus
  :defer
  :init
  (setq gnus-select-method
        '(nnimap "mail.uu.se"
                 (nnimap-stream ssl))))

(use-package mu4e-alert
  :config
  (setq mu4e-alert-email-notification-types '(count))
  (mu4e-alert-set-default-style 'notifications)
  :hook
  (after-init-hook . mu4e-alert-enable-notifications))

(use-package mml-sec
  :init
  (setq mml-default-sign-method "smime"
        mml-default-encrypt-method "smime"
        mml-secure-smime-sign-with-sender t)
  :config
  (defun sign-mail ()
    (let* ((ctx (mu4e-context-current))
           (name (if ctx (mu4e-context-name ctx))))
      (when name
        (cond
         ((equal name "uu")
          (mml-secure-sign))))))
  :hook
  (mu4e-compose-mode-hook . sign-mail))

(use-package mu4e
  :demand
  :bind
  (("C-c m" . mu4e))
  :hook
  (dired-mode-hook . turn-on-gnus-dired-mode)
  :init
  (setq mail-user-agent 'mu4e-user-agent)
  (setq read-mail-command 'mu4e)
  :config
  (setq mu4e-headers-fields '((:human-date . 10)
                              (:flags . 4)
                              (:mailing-list . 10)
                              (:from-or-to . 22)
                              (:thread-subject)))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval 30)
  (setq mu4e-hide-index-messages t)
  (setq mu4e-change-filenames-when-moving t)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-maildir-shortcuts
        '((:maildir "/uu/Inbox" :key ?u)
          (:maildir "/outlook/Inbox" :key ?i)))
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "uu"
            :match-func
            (lambda (msg)
              (when msg
                (string-match-p "^/uu"
                                (mu4e-message-field msg :maildir))))
            :vars
            '((user-mail-address . "xiaoyue.chen@it.uu.se")
              (mu4e-sent-folder . "/uu/Sent Items")
              (mu4e-drafts-folder . "/uu/Drafts")
              (mu4e-trash-folder . "/uu/Deleted Items")
              (mu4e-refile-folder . "/uu/Archive")
              (mu4e-sent-messages-behavior . sent)
              (smtpmail-smtp-server . "mail.uu.se")
              (smtpmail-smtp-service . 587)
              (smtpmail-stream-type . starttls)
              (mu4e-compose-signature
               . (concat "Xiaoyue Chen, PhD Student\n"
                         "Division of Computer Systems\n"
                         "Department of Information Technology\n"
                         "Uppsala University"))))
          ,(make-mu4e-context
            :name "outlook"
            :match-func
            (lambda (msg)
              (when msg
                (string-match-p "^/outlook"
                                (mu4e-message-field msg :maildir))))
            :vars
            '((user-mail-address . "xiaoyue_chen@outlook.com")
              (mu4e-sent-folder . "/outlook/Sent")
              (mu4e-drafts-folder . "/outlook/Drafts")
              (mu4e-trash-folder . "/outlook/Deleted")
              (mu4e-refile-folder . "/outlook/Archive")
              (mu4e-sent-messages-behavior . delete)
              (smtpmail-smtp-server . "smtp-mail.outlook.com")
              (smtpmail-smtp-service . 587)
              (smtpmail-stream-type . starttls)
              (mu4e-compose-signature . t)))))
  (mu4e t))

(use-package auth-source
  :init
  (setq auth-sources '("secrets:default")))

(use-package desktop
  :init
  (setq desktop-restore-frames nil)
  :config
  (desktop-save-mode))

(use-package doom-modeline
  :config
  (doom-modeline-mode))

(use-package eshell
  :defer
  :config
  (use-package esh-module
    :config
    (add-to-list 'eshell-modules-list 'eshell-tramp))
  (use-package em-term
    :init
    (setq eshell-destroy-buffer-when-process-dies t)
    :config
    (dolist (command '("vim" "vifm" "nmtui" "alsamixer"))
      (add-to-list 'eshell-visual-commands command))
    (dolist (subcommand '(("aur" "sync")))
      (add-to-list 'eshell-visual-subcommands subcommand))))

(use-package bash-completion
  :config
  (defun bash-completion-eshell-capf ()
    (append (bash-completion-dynamic-complete-nocomint
             (save-excursion (eshell-bol) (point))
             (point) t)
            '(:exclusive no)))

  (defun bash-completion-from-eshell ()
    (add-hook 'completion-at-point-functions
              'bash-completion-eshell-capf 0 t))
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete)
  (eshell-mode-hook . bash-completion-from-eshell))

(use-package savehist
  :config
  (savehist-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package ido
  :init
  (setq ido-use-filename-at-point 'guess)
  (setq ido-use-url-at-point t)
  :config
  (ido-mode))

(use-package simple
  :demand
  :config
  (advice-add 'async-shell-command :after
              (lambda (command &optional output-buffer error-buffer)
                (unless output-buffer
                  (let ((output-buffer
                         (concat "*CMD " command "*")))
                    (with-current-buffer shell-command-buffer-name-async
                      (rename-buffer output-buffer t))))))
  :hook
  (before-save-hook . delete-trailing-whitespace))

(use-package paren
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package locate
  :bind
  (("C-c F" . locate)))

(use-package recentf
  :init
  (setq recentf-max-saved-items 1000)
  (setq recentf-menu-filter 'recentf-arrange-by-dir)
  (setq recentf-auto-cleanup nil)
  (recentf-mode)
  :bind
  (("C-c h" . recentf-open-files)))

(use-package ibuffer
  :config
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
  :bind
  (("C-x C-b" . ibuffer)))

(use-package imenu
  :init
  (setq imenu-auto-rescan t)
  :bind
  (nil
   :map prog-mode-map
   ("C-c i" . imenu)))

(use-package compile
  :bind
  (("C-c c" . compile)))

(use-package mwheel
  :init
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse 't))

(use-package gdb-mi
  :init
  (setq gdb-many-windows t))

(use-package autorevert
  :config
  (global-auto-revert-mode))

(use-package comint
  :init
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 20000))

(use-package autoinsert
  :init
  (let ((insert-lice (lambda () (lice lice:default-license)
                       (insert "\n")))
        (c-header-condition
         '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header"))
        (c-source-condition
         '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")))
    (define-auto-insert c-header-condition insert-lice)
    (define-auto-insert c-source-condition insert-lice))
  :config
  (auto-insert-mode))

(use-package copyright
  :hook
  (before-save-hook . copyright-update))

(use-package elide-head
  :hook
  (file-file-hook . elide-head))

(use-package org
  :init
  (setq org-adapt-indentation t)
  :config
  (use-package oc-biblatex)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  :bind
  (("C-c l" . org-store-link)))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package flyspell
  :bind
  (nil
   :map flyspell-mode-map
   ("C-." . nil)
   ("C-M-i" . nil))
  :hook
  ((text-mode-hook . flyspell-mode)
   (prog-mode-hook . flyspell-prog-mode)))

(use-package flymake
  :bind
  (nil
   :map flymake-mode-map
   ("C-c e n" . flymake-goto-next-error)
   ("C-c e p" . flymake-goto-prev-error)
   ("C-c e l" . flymake-show-diagnostics-buffer))
  :hook
  (prog-mode-hook . flymake-mode))

(use-package dired
  :config
  (defun xdg-open-from-dired ()
    (interactive)
    (call-process "xdg-open" nil 0 nil
                  (dired-get-filename nil t)))
  :bind
  (nil
   :map dired-mode-map
   ("C-c o" . xdg-open-from-dired))
  :hook
  (dired-mode-hook . turn-on-gnus-dired-mode))

(use-package eldoc
  :config
  (eldoc-add-command 'c-electric-paren))

(use-package company
  :init
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.0)
  :hook
  (prog-mode-hook . company-mode)
  (latex-mode-hook . company-mode))

(use-package yasnippet
  :defer t
  :config
  (yas-reload-all))

(use-package hideshow
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package eglot
  :init
  (setq eglot-confirm-server-initiated-edits nil)
  :bind
  (nil
   :map eglot-mode-map
   ("C-c r" . eglot-rename)
   ("C-c f" . eglot-format)
   ("C-c a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs
               '(cmake-mode . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs
               '(java-mode . (lambda ()
                               (interactive)
                               (let ((workspace
                                      (expand-file-name
                                       (md5 (project-root (project-current)))
                                       "/tmp")))
                                 (list "jdtls" "-data" workspace)))))
  :hook
  ((c-mode-hook c++-mode-hook java-mode-hook python-mode-hook haskell-mode-hook)
   . eglot-ensure))

(use-package text-mode
  :config
  (auto-fill-mode))

(use-package cc-mode
  :config
  (dolist (map '(c-mode-map c++-mode-map))
    (bind-key "C-c o" 'ff-find-other-file map)
    (bind-key "C-c d" 'disaster map))
  :hook
  (c-mode-common-hook . (lambda ()
                          (setq-default fill-column 80)
                          (c-toggle-electric-state 1)
                          (c-toggle-comment-style 1)))
  (minizinc-mode-hook . (lambda ()
                          (c-toggle-electric-state -1))))

(use-package minizinc-mode
  :mode
  "\\.mzn\\'")

(use-package opencl-mode
  :mode
  "\\.cl\\'")

(use-package make-mode
  :mode
  ("makefile" . makefile-gmake-mode))

(use-package langtool
  :init
  (setq langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(use-package tex
  :init
  (setq-default TeX-master nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-electric-math '("\\(" . "\\)"))
  (setq TeX-electric-sub-and-superscript t)
  (setq LaTeX-electric-left-right-brace t)
  (setq reftex-plug-into-AUCTeX t)
  (setq prettify-symbols-unprettify-at-point t)
  :config
  (add-to-list 'TeX-view-program-selection '(output-pdf "xdg-open"))
  :bind
  (nil
   :map TeX-mode-map
   ("C-c c" . (lambda ()
                (interactive)
                (save-buffer)
                (TeX-command-run-all nil))))
  :hook
  (LaTeX-mode-hook . (lambda ()
                       (turn-on-reftex)
                       (turn-on-auto-fill)
                       (LaTeX-math-mode)
                       (prettify-symbols-mode)
                       (TeX-fold-mode)
                       (TeX-source-correlate-mode))))

(add-to-list 'command-switch-alist
             '("--denv" . (lambda (_) (load "~/.emacs.d/denv.el"))))

;;; init.el ends here

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

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package eshell
  :defer
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-visual-commands
        (seq-union '("vim" "nmtui" "alsamixer")
                   eshell-visual-commands)))

(use-package bash-completion
  :hook
  (shell-dynamic-complete-functions . bash-completion-dynamic-complete))

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
  (:map prog-mode-map
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
  (:map flyspell-mode-map
        ("C-." . nil)
        ("C-M-i" . nil))
  :hook
  ((text-mode-hook . flyspell-mode)
   (prog-mode-hook . flyspell-prog-mode)))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c e n" . flymake-goto-next-error)
        ("C-c e p" . flymake-goto-prev-error)
        ("C-c e l" . flymake-show-diagnostics-buffer))
  :hook
  (prog-mode-hook . flymake-mode))

(defun xdg-open () (interactive)
       (call-process "xdg-open" nil 0 nil
                     (dired-get-filename nil t)))

(use-package dired
  :bind
  (:map dired-mode-map
        ("C-c o" . xdg-open)))

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
  (:map eglot-mode-map
        ("C-c r" . eglot-rename)
        ("C-c f" . eglot-format)
        ("C-c a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs
               '(cmake-mode . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs
               '(java-mode . (lambda () (interactive)
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
  (:map TeX-mode-map
        ("C-c c" . (lambda () (interactive)
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

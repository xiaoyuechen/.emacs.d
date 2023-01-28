;;; Init.el --- My personal Emacs init file          -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023  Xiaoyue Chen

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

(setq use-package-hook-name-suffix nil)
(setq use-package-enable-imenu-support t)

(add-to-list 'command-switch-alist
             '("--denv" . (lambda (_) (load "~/.emacs.d/denv.el"))))

(use-package emacs
  :demand
  :config
  (setq x-underline-at-descent-line t)
  (setq max-mini-window-height 0.6)
  (setq enable-recursive-minibuffers t)
  (setq sentence-end-double-space nil)
  (setq require-final-newline t)
  (setq-default indent-tabs-mode nil)
  (setq tab-always-indent 'complete)
  (setq delete-by-moving-to-trash t)
  (setq async-shell-command-buffer 'new-buffer)
  (setq backup-by-copying-when-linked t)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

(use-package consult
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c x" . consult-mode-command)
   ("C-c k" . consult-kmacro)

   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)

   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop

   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-make)
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)

   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history)                 ;; orig. previous-
   )

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file . ((styles . (basic partial-completion))))
                                        (eglot . ((styles . (orderless))))))
  (setq orderless-component-separator "[ &]"))

(use-package recentf
  :init
  (setq recentf-max-menu-items 100)
  (recentf-mode))

(use-package windmove
  :config
  (windmove-default-keybindings '(meta shift)))

(use-package window
  :config
  (setq split-height-threshold nil))

(use-package engine-mode
  :init
  (engine-mode)
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine wiktionary
    (concat "https://www.wikipedia.org/search-redirect.php?"
            "family=wiktionary&language=en&go=Go&search=%s")
    :keybinding "t"))

(use-package newcomment
  :defer
  :config
  (setq comment-empty-lines t))

(use-package calendar
  :defer
  :config
  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face)
        calendar-intermonth-header
        (propertize "Wk"
                    'font-lock-face 'font-lock-keyword-face)))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package mu4e-alert
  :init
  ;; (setq mu4e-alert-email-notification-types '(count subjects))
  ;; (mu4e-alert-set-default-style 'notifications)
  :hook
  ;; (after-init-hook . mu4e-alert-enable-notifications)
  (after-init-hook . mu4e-alert-enable-mode-line-display))

(use-package mml-sec
  :hook
  (mu4e-compose-mode-hook . sign-mail)
  :config
  (defun sign-mail ()
    (let* ((ctx (mu4e-context-current))
           (name (if ctx (mu4e-context-name ctx))))
      (when name
        (cond
         ((equal name "uu")
          (mml-secure-sign)))))))

(use-package mu4e
  :demand
  :hook
  (dired-mode-hook . turn-on-gnus-dired-mode)
  (after-init-hook . (lambda ()
                       (mu4e-update-mail-and-index t)))
  :config
  (setq mu4e-update-interval 300)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (setq read-mail-command 'mu4e)
  (setq mu4e-headers-fields '((:human-date . 11)
                              (:flags . 4)
                              (:mailing-list . 10)
                              (:from-or-to . 22)
                              (:thread-subject)))
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-hide-index-messages t)
  (setq mu4e-change-filenames-when-moving t)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-date-format "%F")
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
  :bind
  (("C-c m" . mu4e)
   :map mu4e-main-mode-map
   ("q" . bury-buffer)))

(use-package auth-source
  :config
  (setq auth-sources '("secrets:Login")))

(use-package pyvenv
  :config
  (setq pyvenv-default-virtual-env-name ".pyenv")
  (pyvenv-mode))

(use-package ediff
  :defer
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package desktop
  :init
  (setq desktop-restore-frames nil)
  (setq desktop-load-locked-desktop t)
  (desktop-save-mode))

(use-package mood-line
  :config
  (mood-line-mode))

(use-package vterm
  :defer
  :config
  (setq vterm-buffer-name-string "*vterm %s*")
  :bind
  ("C-c t" . vterm))

(use-package eshell-vterm
  :init
  (eshell-vterm-mode))

(use-package esh-module
  :defer
  :config
  (dolist (module '(eshell-tramp eshell-elecslash))
    (add-to-list 'eshell-modules-list module)))

(use-package em-term
  :defer
  :config
  (setq eshell-destroy-buffer-when-process-dies t)
  (dolist (command '("vim" "vifm" "nmtui" "alsamixer" "gh"))
    (add-to-list 'eshell-visual-commands command))
  (dolist (subcommand '(("aur" "sync")))
    (add-to-list 'eshell-visual-subcommands subcommand)))

(use-package eshell
  :hook
  (eshell-mode-hook . rename-eshell-buffer)
  (eshell-directory-change-hook . rename-eshell-buffer)
  (eshell-expand-input-functions . eshell-expand-history-references)
  :init
  (setq eshell-history-size 10000)
  (defun rename-eshell-buffer ()
    (unless eshell-non-interactive-p
      (rename-buffer (format "*eshell: %s*"
                             (abbreviate-file-name (directory-file-name
                                                    default-directory)))
                     t)))

  (defun spawn-eshell (display)
    "Create a frame with a dedicated Eshell window."
    (select-frame (make-frame-on-display display))
    (let* ((default-directory "~")
           (buffer (eshell)))
      (set-window-dedicated-p (get-buffer-window buffer) t))))

(use-package pcomplete
  :init
  (setq shell-dynamic-complete-functions
        '(pcomplete-completions-at-point)))

(use-package pcmpl-args
  :after
  (pcomplete))

(use-package pcmpl-gnu
  :after
  (pcomplete))

(use-package bash-completion
  :after
  (pcomplete)

  :hook
  (eshell-mode-hook . bash-completion-from-eshell)

  :init
  (add-to-list 'shell-dynamic-complete-functions
               'bash-completion-dynamic-complete
               t)

  :config
  (defun bash-completion-eshell-capf ()
    (bash-completion-dynamic-complete-nocomint
     (save-excursion (eshell-bol) (point))
     (point) t))

  (defun bash-completion-from-eshell ()
    (setq-local pcomplete-default-completion-function 'ignore)
    (add-hook 'completion-at-point-functions 'bash-completion-eshell-capf 0 t)))

(use-package savehist
  :init
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

(use-package simple
  :demand
  :config
  (setq set-mark-command-repeat-pop t)
  (advice-add 'async-shell-command :after
              (lambda (command &optional output-buffer error-buffer)
                (unless output-buffer
                  (let ((output-buffer
                         (concat "*CMD " command "*")))
                    (with-current-buffer shell-command-buffer-name-async
                      (rename-buffer output-buffer t))))))
  (setq visual-line-fringe-indicators '(left-curly-arrow right-arrow))
  :hook
  (before-save-hook . delete-trailing-whitespace))

(use-package visual-fill-column
  :config
  (setq visual-fill-column-enable-sensible-window-split t)
  :hook
  (visual-line-mode-hook . visual-fill-column-mode))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package ibuffer
  :config
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
  :bind
  (("C-x C-b" . ibuffer)))

(use-package imenu
  :defer
  :config
  (setq imenu-auto-rescan t))

(use-package compile
  :bind
  (("C-c c" . compile)))

(use-package pixel-scroll
  :init
  (pixel-scroll-precision-mode))

(use-package gdb-mi
  :defer
  :config
  (setq gdb-many-windows t))

(use-package autorevert
  :config
  (global-auto-revert-mode))

(use-package comint
  :defer
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 20000))

(use-package lice)

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
  :commands
  (my-agenda)
  :hook
  (org-mode-hook . (lambda ()
                     (setq-local fill-column 80)
                     (visual-line-mode)))
  (mu4e-compose-mode-hook . turn-on-orgtbl)

  :init
  (setq org-directory "~/Org")
  (setq org-notes-directory (expand-file-name "notes" org-directory))
  (setq org-template-directory (expand-file-name "templates" org-directory))
  (setq org-default-notes-file (expand-file-name "notes" org-notes-directory))
  (setq org-agenda-files (expand-file-name "agenda-file-list" org-directory))
  (setq org-babel-tangle-use-relative-file-links nil)
  (defun org-template-arg (name)
    (when name
      `(file ,(expand-file-name name org-template-directory))))

  :config
  (setq org-cite-global-bibliography
        (list (expand-file-name "ca.bib"
                                (expand-file-name "bib" org-directory))))

  (setq org-hide-emphasis-markers nil)
  (setq org-startup-folded 'nofold)
  (setq org-image-actual-width 800)

  (dolist (x '((latex biblatex "ieee" "ieee")
               (t csl "ieee.csl" "ieee.csl")))
    (add-to-list 'org-cite-export-processors x))

  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-indented t)
  (setq org-log-done 'time)
  (setq org-catch-invisible-edits 'smart)
  (setq org-attach-store-link-p 'attached)
  (setq org-attach-dir-relative t)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-tag-persistent-alist
        '(("research" . ?i)
          ("study" . ?s)
          ("teaching" . ?t)
          ("hobby" . ?h)
          ("programming" . ?p)
          ("writing" . ?w)
          ("reading" . ?r)
          ("meeting" . ?m)
          ("errand" . ?e)))
  (setq org-capture-templates
        (mapcar
         (lambda (config)
           `(,(plist-get config 'key)
             ,(plist-get config 'description)
             entry
             (file ,(expand-file-name (plist-get config 'file)
                                      org-notes-directory))
             ,(org-template-arg (plist-get config 'template))
             :empty-lines 1))
         '(( key "t"
             description "Task"
             file "tasks.org"
             template "task")
           ( key "f"
             description "Fleeting note"
             file "notes.org"
             template "note"))))
  (setq org-fast-tag-selection-single-key 'expert)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (haskell . t)
     (C . t)
     (python . t)
     (bibtex . nil)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-cite-csl-styles-dir "~/Repos/csl-styles")
  (dolist (module '(org-id org-attach oc-biblatex oc-csl ox-reveal))
    (add-to-list 'org-modules module))

  (defun my-agenda ()
    (interactive)
    (org-agenda nil "n"))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . my-agenda)
   ("C-c p" . org-capture)
   :map dired-mode-map
   ("C-c C-x a" . org-attach-dired-to-subtree)))

(use-package ox-reveal
  :defer
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package org-roam
  :defer
  :init
  (setq org-roam-directory (expand-file-name "roam" org-directory))
  :config
  (setq org-roam-node-display-template
        (concat "${title:60*} "
                (propertize "${tags}" 'face 'org-tag)))
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))
  (setq org-roam-capture-templates
        (mapcar
         (lambda (config)
           `(,(plist-get config 'key)
             ,(plist-get config 'description)
             plain
             ,(org-template-arg (plist-get config 'template))
             :target
             (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        ,(format
                          (concat "#+title: ${title}\n"
                                  "#+filetags: :%s:\n")
                                 (mapconcat 'identity
                                            (plist-get config 'tags)
                                            ":")))
             :unnarrowed t
             :empty-lines 1))
         `(( key "l"
             description "Literature note"
             template "literature"
             tags ("literature"))
           ( key "c"
             description "Permanent note"
             template nil
             tags ("permanent"))
           ( key "e"
             description "Entry point"
             template nil
             tags ("entry"))
           ( key "v"
             description "CIV4 game"
             template "civ"
             tags ("game")))))

  (org-roam-db-autosync-mode)
  :bind
  ("C-c r i" . org-roam-node-insert)
  ("C-c r f" . org-roam-node-find)
  ("C-c r p" . org-roam-capture)
  ("C-c r b" . org-roam-buffer-toggle))

(use-package org-roam-ui
  :defer
  :init
  (setq org-roam-ui-open-on-start nil)
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-follow-mode nil)))

(use-package vertico
  :init
  (vertico-mode))

(use-package ffap
  :config
  (ffap-bindings))

(use-package files
  :config
  (setq auto-save-file-name-transforms nil))

(use-package tramp
  :defer
  :config
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not
                (let ((method (file-remote-p name 'method)))
                  (when (stringp method)
                    (member method '("su" "sudo"))))))))

  (defun rename-tramp-buffer ()
    (when (tramp-tramp-file-p buffer-file-name)
      (let* ((tramp (tramp-dissect-file-name buffer-file-name))
             (host (tramp-file-name-host tramp))
             (file (file-name-nondirectory (tramp-file-name-localname tramp))))
        (rename-buffer (generate-new-buffer-name
                        (format "@%s: %s" host file))))))

  (add-hook 'find-file-hooks 'rename-tramp-buffer)
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
  ( :map flymake-mode-map
    ("M-g n" . flymake-goto-next-error)
    ("M-g p" . flymake-goto-prev-error))
  :hook
  (emacs-lisp-mode-hook . flymake-mode))

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
  :init
  (setq eldoc-echo-area-display-truncation-message nil)
  :config
  (eldoc-add-command 'c-electric-paren))

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (corfu-separator ?\&)          ;; Orderless field separator
  (corfu-popupinfo-delay t)

  :init
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil
                  corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (add-to-list 'savehist-additional-variables 'corfu-history)

  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)

  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))

  :hook
  (eshell-mode-hook . (lambda ()
                        (setq-local corfu-auto nil))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style
   '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind
  ( :map minibuffer-local-map
    ("M-A" . marginalia-cycle)))

(use-package racket-mode
  :defer
  :hook
  (racket-mode-hook . racket-xp-mode))

(use-package racket-xp
  :defer
  :hook
  (racket-xp-mode-hook
   . (lambda ()
       (remove-hook 'pre-redisplay-functions
                    #'racket-xp-pre-redisplay
                    t))))

(use-package scheme
  :defer
  :init
  (setq auto-mode-alist
        (remove '("\\.rkt\\'" . scheme-mode) auto-mode-alist)))

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package hideshow
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package which-func
  :init
  (setq which-func-modes '(c-mode c++-mode))
  (which-function-mode))

(use-package eglot
  :bind
  ( :map eglot-mode-map
    ("C-c s r" . eglot-rename)
    ("C-c s f" . eglot-format)
    ("C-c s a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs
               '(cmake-mode . ("cmake-language-server")))

  (setq-default eglot-workspace-configuration
                '(:haskell
                  ( :maxCompletions 50
                    :formattingProvider "fourmolu")))

  :hook
  (eglot-managed-mode-hook . (lambda ()
                               (setq-local eldoc-documentation-strategy
                                           'eldoc-documentation-compose))))

(use-package cc-mode
  :config
  (dolist (map '(c-mode-map c++-mode-map))
    (bind-key "C-c o" 'ff-find-other-file map))
  (c-add-style "m5"
	       '((c-basic-offset . 4)
                 (indent-tabs-mode . nil)
	         (c-offsets-alist . ((substatement-open . 0)
				     (inline-open . 0)
				     (block-open . -4)
				     (case-label . 2)
				     (label . 2)
				     (statement-case-intro . 2)
				     (statement-case-open . 2)
				     (access-label . -2)
				     (innamespace . 0)))))
  :hook
  (c-mode-common-hook . (lambda ()
                          (setq-local fill-column 80)
                          (c-toggle-electric-state 1)
                          (c-toggle-comment-style 1)))
  (minizinc-mode-hook . (lambda ()
                          (c-toggle-electric-state -1))))

(use-package minizinc-mode
  :mode
  "\\.mzn\\'")

(use-package make-mode
  :mode
  ("makefile" . makefile-gmake-mode))

(use-package verilog-mode
  :hook
  (verilog-mode-hook
   . (lambda ()
       (setq-default compilation-error-regexp-alist
                     (mapcar 'cdr verilog-error-regexp-emacs-alist))))
  :config
  (setq verilog-auto-newline nil)
  (setq verilog-linter "verilator --lint-only -Wall"))

(use-package haskell-mode
  :bind
  ( :map haskell-mode-map
    ("C-c C" . haskell-compile)
    ("C-c L" . haskell-process-load-file)
    ("C-c H" . haskell-hoogle))
  :config
  (setq haskell-compile-command
        "ghc -O3 -dynamic -Wall -ferror-spans -fforce-recomp %s")
  (setq haskell-hoogle-command
        "hoogle -n 1024 --numbers"))

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
                       (LaTeX-math-mode)
                       (prettify-symbols-mode)
                       (TeX-fold-mode)
                       (TeX-source-correlate-mode))))

(use-package vc
  :defer
  :init
  (setq vc-follow-symlinks t))

(use-package geiser
  :defer
  :config
  (setq geiser-repl-current-project-function 'ignore))

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
